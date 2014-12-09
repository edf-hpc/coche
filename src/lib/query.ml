(****************************************************************************)
(*  Copyright (C) 2013-2014 EDF S.A.                                        *)
(*                                                                          *)
(*  This file is part of Coche.                                             *)
(*                                                                          *)
(*  Coche is free software: you can redistribute it and/or modify it        *)
(*  under the terms of the GNU General Public License as published by the   *)
(*  Free Software Foundation, either version 3 of the License, or (at your  *)
(*  option) any later version.                                              *)
(*                                                                          *)
(*  Coche is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or   *)
(*  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   *)
(*  for more details.                                                       *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with Coche.  If not, see <http://www.gnu.org/licenses/>.          *)
(*                                                                          *)
(****************************************************************************)

module Dtd = Ast.Dtd
module Result = Ast.Result
module Report = Ast.Report

open Unix
open Ast.Base
open Dtd
open Utils
open Result
open Units

let contains s1 s2 =
  ExtString.String.exists s2 s1

let ok t = Ast.Result_info.Ok t
let fail (t1,t2) = Ast.Result_info.Fail (t1,t2)

module SMap = Map.Make(String)

(*
 * packages
 *)
let q_packages packages =
  let output_lines = read_process_lines "dpkg -l| egrep -v '[|\\/]' | grep -v '+-='| awk '{ print $1,$2 }'" in
  let nb_packages = List.length output_lines in
  let dpkg_l = List.fold_left
    (fun map line ->
      let status, package = match (ExtString.String.nsplit line " ") with
	| status::package::_ -> status, package
	| _ -> assert false
      in
      assert(String.length(status) > 0);
      let status = if status.[0] = 'i' then `Installed else `Absent in
      SMap.add package status map
    )
    SMap.empty
    output_lines
  in
  let l_st = List.map
    (fun (pkg, status) ->
      try
	let found = SMap.find pkg dpkg_l in
	pkg, status, found
      with Not_found ->
	pkg, status, `Absent
    )
    packages.Dtd.p_status
  in
  let cond_status = List.for_all
    (fun (pkg, status, found) -> status = found)
    l_st
  in
  let cond_match =
    if packages.Dtd.p_match = `Subset
    then cond_status
    else cond_status && nb_packages = List.length packages.Dtd.p_status
  in
  let result_status =
    if cond_status
    then ok packages.Dtd.p_status
    else
      let fail_l = List.fold_left
	(fun acc elt ->
	  let pkg, status, found = elt in
	  if status <> found
	  then (pkg,found)::acc
	  else acc
	)
	[]
	l_st
      in
      fail (fail_l, packages.Dtd.p_status) (* double fail *)
  in
  let result_match = if cond_match then ok packages.Dtd.p_match  else fail (`Subset, packages.Dtd.p_match) in
  { p_status = result_status ;
    p_match = result_match;
  }

(*
 * extraction des résultats pour le daemon
 *)
let q_daemon daemon =
  let demon = daemon.d_name in
  let out  = read_process("/etc/init.d/"^demon^" status") in
  let status =
    if contains out "running" then
      `Running
    else
      `Stopped
  in
  let cond = daemon.d_status = status in
  if cond
  then
    ok daemon
  else
    let r_daemon = {d_name = demon; d_status = status }in
    fail (r_daemon, daemon)

(*
 * mount
 *)
let q_mount mount =
  let output_lines = read_process_lines "mount | awk '{ print $1,$3,$4,$5 }'" in
  let elem = List.find (fun s -> contains s mount.m_name) output_lines in
  let mount_point = (List.nth (ExtString.String.nsplit " " elem) 1) in
  let mount_fstype = (List.nth (ExtString.String.nsplit " " elem) 2) in
  let mount_option = (List.nth (ExtString.String.nsplit " " elem) 3) in
  if (mount.m_options = Some mount_option &&
      mount.m_fstype = Some mount_fstype &&
      mount.m_mountpoint = mount_point)
  then ok mount
  else
    fail ({ m_name = mount.m_name;
	    m_options= Some mount_option;
	    m_mountpoint= mount_point;
	    m_fstype = Some mount_fstype;
	    m_device = mount.m_device;
	    m_size = mount.m_size;
            m_quota = mount.m_quota},
          mount)

(*
 * fichier
 *)
let q_file : (Dtd.file ->  Result.file) = fun file ->
  let file_name = file.f_name in
  let siz = Unix.stat file.f_name in
  let owner = getpwuid (siz.st_uid) in
  let owner = owner.pw_name in
  let group = getgrgid (siz.st_gid) in
  let group = group.gr_name in
  let kind = siz.st_kind in
  let perm = siz.st_perm in
  let file_o = {f_name = file.f_name;
	   f_owner = file.f_owner;
	   f_group = file.f_group;
	   f_same = Digest.file "/dev/null";
	   f_perms = file.f_perms;
	   f_type = file.f_type } in
  let file_r = {f_name = file.f_name;
	   f_owner = Some owner;
	   f_group = Some group;
	   f_same = Digest.file file_name;
	   f_perms = Some perm;
	   f_type = kind } in
  if (file.f_owner = Some owner &&
      file.f_group = Some group &&
      file.f_type = kind &&
      file.f_perms = Some perm)
  then
    ok file_r
  else
    fail (file_r, file_o)

 (*
  * netdevice
  *)
let q_netdevice netdevice =
  let nom = netdevice.Dtd.nd_name in
  let out = read_process ("/sbin/ifconfig "^nom)in
  let state =
    if contains out "Up"
    then
      `Up
    else
      `Down
  in
  let reslt = read_process ("/sbin/ifconfig "^nom^"|grep 'inet '| awk  '{print $2 }'") in
  let st =
    try
      let adres = Network.ip (String.sub reslt 4 ((String.length reslt)-4)) in
      let l = Network.expand_range(netdevice.Dtd.nd_target.Ast.a_range) in
      if Network.compare adres (Network.ip (List.nth l 0)) > 0
         && Network.compare adres (Network.ip (List.nth (List.rev l) 0)) < 0
         && state = netdevice.Dtd.nd_state
      then
        ok netdevice.Dtd.nd_state
      else fail (state, netdevice.Dtd.nd_state)
    with Invalid_argument "String.sub" ->
      fail (state, netdevice.Dtd.nd_state)
  in
  {Result.nd_name = nom ;
   nd_target = netdevice.Dtd.nd_target;
   nd_state = st }

(*
 *disk
 *)
let  q_disk disk =
  let output_lines = read_process_lines "df -h -P 2>/dev/null | grep -E '(sd)'| awk '{print $1, $2}'" in
  try
    let elem = List.find (fun s -> contains s disk.device) output_lines in
    let elem = (ExtString.String.strip elem) in
    let siz = (Units.Size.make (List.nth (ExtString.String.nsplit " " elem) 1)) in
    match disk.size with
    | Some size ->
       if (Units.Size.compare siz size) = 0
       then ok disk
       else fail ({device = disk.device ; size = Some siz}, disk)
    | None -> ok disk
  with Not_found -> fail ({device = disk.device ; size = Some (Units.Size.make "0")}, disk)


(*
 * Memoire
 *)
let q_memory memory =
  let mem  = read_process "sed -n 's/MemTotal:[ ]*//p' /proc/meminfo" in
  let swap = read_process "sed -n 's/SwapTotal:[ ]*//p' /proc/meminfo" in
  let mem = (Units.Size.make mem) in
  let swap = (Units.Size.make swap) in
  let mem_rslt = {swap = Some swap ; ram = Some mem } in
  match memory.swap, memory.ram  with
    | Some m_swap, Some ram -> if (Units.Size.compare m_swap swap) = 0 && (Units.Size.compare ram mem) = 0
      then ok memory
      else fail (mem_rslt, memory)
    | None, None  ->  ok memory
    | None, Some ram ->  if (Units.Size.compare ram mem) = 0
      then ok memory
      else fail (mem_rslt, memory)
    | Some m_swap, None ->  if (Units.Size.compare m_swap swap) = 0
      then ok memory
      else fail (mem_rslt, memory)

(*
 *cpu
 *)
let  q_cpu cpu =
  let cpu1  = read_process "egrep -c '^processor' /proc/cpuinfo" in
  let cpu1 = (int_of_string (ExtString.String.strip cpu1)) in
  let maxf  = read_process "lscpu |grep 'CPU MHz'| awk '{ print $3}'" in
  let maxf = (Units.Freq.make((ExtString.String.strip maxf)^"MHz")) in
  let core = read_process "cat /proc/cpuinfo | grep 'cpu cores' | awk '{s+=$4} END {print s}'" in
  let core = (int_of_string (ExtString.String.strip core)) in
  let socket = read_process "grep 'physical id' /proc/cpuinfo | sort -ru | awk '{print $4}'" in
  let socket = (((1 + int_of_string (ExtString.String.strip socket))*cpu1)) in
  let thread = read_process "lscpu |grep 'Thread(s) per core'| awk '{ print $4}'" in
  let thread = (((int_of_string (ExtString.String.strip thread))*core)) in
  match cpu.maxfreq with
    |Some maxfreq ->
      if Units.Freq.compare maxfreq maxf = 0 &&
	 cpu.ncores = Some core &&
	 cpu.nsockets = Some socket &&
	 cpu.nthreads = Some thread
      then
	ok cpu
      else
	fail ({ maxfreq = Some maxf;
	        ncores = Some core;
	        nsockets = Some socket;
	        nthreads = Some thread
	     }, cpu)
    | None -> ok cpu


(*
 * system
 *)
let q_sysconfig sysconfig =
  let vers = read_process "uname -r " in
  let arch = read_process "uname -m"  in
  match sysconfig with
    | Ast.Base.Kernel kernel ->
      if (kernel.k_version = vers && kernel.k_arch = Some arch)
      then
	ok sysconfig
      else
        let sysref = Ast.Base.Kernel { k_version = vers; k_arch = Some arch } in
	fail (sysref, sysconfig)

let q_system system =
  let syname = system.Dtd.sys_name in
  let list_sys_conf = system.Dtd.sys_config in
  let lst = List.fold_left
    (fun acc elt ->
      let elt = q_sysconfig elt in
      match elt with
      | (Ast.Result_info.Ok _) -> elt::acc
      | (Ast.Result_info.Fail _) -> acc
    )
    []
    list_sys_conf in
  {Result.sys_name = syname ; Result.sys_config = lst }

(*
 * Construction de q_config
 *)
let q_hardware_desc hardware_desc =
  match hardware_desc with
    |Dtd.Memory memory -> Memory (q_memory memory)
    |Dtd.Disk disk -> Disk (q_disk disk)
    |Dtd.Cpu cpu -> Cpu (q_cpu cpu)

let q_hardware hardware =
  let name = hardware.Dtd.h_name in
  let list_res= List.map q_hardware_desc hardware.Dtd.h_desc in
  { h_name = name;
    h_desc = list_res;
    h_classes = hardware.Dtd.h_classes }

let q_node_desc node_desc =
    match node_desc with
      |Dtd.Mount mount ->  Mount (q_mount mount)
      |Dtd.Daemon daemon -> Daemon (q_daemon daemon)
      |Dtd.Packages packages -> Packages (q_packages packages)
      |Dtd.System system -> System (q_system system)
      |Dtd.File file -> File (q_file file)

let q_node node =
  let list_nde = List.map q_node_desc node.Dtd.n_desc  in
  { n_role = node.Dtd.n_role;
    n_classes = node.Dtd.n_classes;
    n_type = node.Dtd.n_type;
    n_ha = node.Dtd.n_ha;
    n_desc = list_nde }

let q_service service =
  let list_serv = List.map q_node service.Dtd.s_nodes in
  {s_name = service.Dtd.s_name;
   s_nodes = list_serv }


let q_netconfig netconfig =
  let list_conf = List.map q_netdevice netconfig.Dtd.nc_devices in
  let kind = ok netconfig.Dtd.nc_kind in
  { nc_name = netconfig.Dtd.nc_name;
    nc_classes = netconfig.Dtd.nc_classes;
    nc_kind = kind;
    nc_devices = list_conf}

let cluster_to_result cluster =
  List.map
    (fun config ->
      match config with
	|Dtd.Netconfig netconfig -> Netconfig (q_netconfig netconfig)
	|Dtd.Hardware hardware -> Hardware (q_hardware hardware)
	|Dtd.Service service -> Service (q_service service)
    )
    cluster.Dtd.config

(*
 * Report creation, starting from the results
 *)

open Report

(*
 * recupération du hostname
 *)
let hostname = read_process "hostname"

let r_result = function
  | Ast.Result_info.Ok elm ->
    { Ast.Report_info.value = elm;
      Ast.Report_info.good = hostname::[];
      Ast.Report_info.bad = []
    }
  | Ast.Result_info.Fail (elm, good) ->
    { Ast.Report_info.value = good;
      Ast.Report_info.good = [];
      Ast.Report_info.bad = (elm, hostname::[])::[]
    }

(*
 * packages
 *)
let r_packages packages =
  let p_status = r_result packages.Result.p_status in
  let p_match = r_result packages.Result.p_match in
  { Report.p_status = p_status;
    Report.p_match = p_match}

(*
 * file
 *)
let r_file file =
  r_result file

(*
 * netdevice
 *)
let r_netdevice netdevice =
  let state = r_result netdevice.Result.nd_state in
  { Report.nd_name = netdevice.Result.nd_name;
    Report.nd_target = netdevice.Result.nd_target;
    Report.nd_state = state
  }

(*
 * system
 *)
let r_system system =
  let list = List.map
    (fun elm ->
      r_result elm
    ) system.Result.sys_config
  in
  { Report.sys_name = system.Result.sys_name;
    Report.sys_config = list
  }

(*
 * r_config
 *)
let r_hardware_desc hardware_desc =
  match hardware_desc with
    | Result.Memory memory -> Report.Memory (r_result memory)
    | Result.Disk disk -> Report.Disk (r_result disk)
    | Result.Cpu cpu -> Report.Cpu (r_result cpu)

let r_hardware hardware =
  let name = hardware.Result.h_name in
  let list_res = List.map r_hardware_desc hardware.Result.h_desc in
  { Report.h_name = name;
    Report.h_classes = hardware.Result.h_classes;
    Report.h_desc = list_res
  }

let r_node_desc node_desc =
  match node_desc with
    | Result.Mount mount -> Report.Mount (r_result mount)
    | Result.Daemon daemon -> Report.Daemon (r_result daemon)
    | Result.Packages packages -> Report.Packages (r_packages packages)
    | Result.System system -> Report.System (r_system system)
    | Result.File file -> Report.File (r_file file)

let r_node node =
  let list_nde = List.map r_node_desc node.Result.n_desc  in
  {  Report.n_role = node.Result.n_role;
     Report.n_classes = node.Result.n_classes;
     Report.n_type = node.Result.n_type;
     Report.n_ha = node.Result.n_ha;
     Report.n_desc = list_nde
  }

let r_service service =
  let list_serv = List.map r_node service.Result.s_nodes in
  { Report.s_name = service.Result.s_name;
    Report.s_nodes = list_serv
  }
let r_netconfig netconfig =
  let list_conf = List.map r_netdevice netconfig.Result.nc_devices in
  let kind = r_result netconfig.Result.nc_kind
  in
  { Report.nc_name = netconfig.Result.nc_name;
    Report.nc_classes = netconfig.Result.nc_classes;
    Report.nc_kind = kind;
    Report.nc_devices = list_conf
  }

let result_to_report cluster =
  List.map
    (fun config ->
      match config with
	| Result.Netconfig netconfig -> Report.Netconfig (r_netconfig netconfig)
	| Result.Hardware hardware -> Report.Hardware (r_hardware hardware)
	| Result.Service service -> Report.Service (r_service service)
    )
    cluster

(*
 * merge of two results :
 * merge_results:
 * 'a Report.result ->
 * 'a Report.result ->
 * 'a Report.result
 *)

let merge_results r1 r2 =
  let good = r1.Ast.Report_info.good @ r2.Ast.Report_info.good in
  let bad =
    List.fold_left
      (fun acc (resultat, hosts) ->
	try
	  let h = List.assoc resultat acc in
          let host_result = hosts @ h in
	  (resultat, host_result) :: (List.remove_assoc resultat acc)

	with Not_found ->
	  (resultat, hosts) :: acc
      )
      r1.Ast.Report_info.bad
      r2.Ast.Report_info.bad
  in
  { Ast.Report_info.value = r1.Ast.Report_info.value;
    Ast.Report_info.good = good;
    Ast.Report_info.bad = bad;
  }

(*
 * packages
 *)
let mr_packages packages1 packages2 =
  let p_status = merge_results packages1.Report.p_status packages2.Report.p_status  in
  let p_match = merge_results packages1.Report.p_match packages2.Report.p_match  in
  { Report.p_status = p_status;
    Report.p_match = p_match}

(*
 * file
 *)
let mr_file file1 file2 =
  merge_results file1 file2

(*
 * netdevice
 *)
let mr_netdevice netdevice1 netdevice2 =
  let state = merge_results netdevice1.Report.nd_state netdevice1.Report.nd_state   in
  { Report.nd_name = netdevice1.Report.nd_name;
    Report.nd_target = netdevice1.Report.nd_target;
    Report.nd_state = state
  }

(*
 * system
 *)
let mr_system system1 system2 =
  let list = List.map2
    (fun elm1 elm2->
      merge_results elm1 elm2
    )
    system1.Report.sys_config
    system2.Report.sys_config
  in
  { Report.sys_name = system1.Report.sys_name;
    Report.sys_config = list
  }

(*
 * config
 *)
let mr_hardware_desc hardware_desc1 hardware_desc2 =
  match hardware_desc1 ,hardware_desc2  with
    | Report.Memory memory1, Report.Memory memory2 ->
      Report.Memory (merge_results memory1 memory2)
    | Report.Disk disk1, Report.Disk disk2 ->
      Report.Disk (merge_results disk1 disk2)
    | Report.Cpu cpu1, Report.Cpu cpu2 ->
      Report.Cpu (merge_results cpu1 cpu2)
    | _ -> Errors.raise (Errors.Cannot_merge_two_different_tags "hardware")

let mr_hardware hardware1 hardware2 =
  let name = hardware1.Report.h_name in
  let list_res =
    List.map2 mr_hardware_desc hardware1.Report.h_desc hardware2.Report.h_desc in
  { Report.h_name = name;
    Report.h_classes = hardware1.Report.h_classes;
    Report.h_desc = list_res
  }

let mr_node_desc node_desc1 node_desc2 =
  match node_desc1, node_desc2 with
    | Report.Mount mount1, Report.Mount mount2->
      Report.Mount (merge_results mount1 mount2)
    | Report.Daemon daemon1, Report.Daemon daemon2 ->
      Report.Daemon (merge_results daemon1 daemon2)
    | Report.Packages packages1, Report.Packages packages2->
      Report.Packages (mr_packages packages1 packages2)
    | Report.System system1, Report.System system2->
      Report.System (mr_system system1 system2)
    | Report.File file1, Report.File file2  -> Report.File (mr_file file1 file2)
    |  _ -> Errors.raise (Errors.Cannot_merge_two_different_tags "node_desc")

let mr_node node1 node2 =
  let list_nde = List.map2 mr_node_desc node1.Report.n_desc node2.Report.n_desc  in
  {  Report.n_role = node1.Report.n_role;
     Report.n_classes = node1.Report.n_classes;
     Report.n_type = node1.Report.n_type;
     Report.n_ha = node1.Report.n_ha;
     Report.n_desc = list_nde
  }

let mr_service service1 service2 =
  let list_serv = List.map2 mr_node service1.Report.s_nodes service2.Report.s_nodes in
  { Report.s_name = service1.Report.s_name;
    Report.s_nodes = list_serv
  }
let mr_netconfig netconfig1 netconfig2=
  let list_conf = List.map2 mr_netdevice netconfig1.Report.nc_devices netconfig2.Report.nc_devices in
  let kind = merge_results netconfig1.Report.nc_kind netconfig2.Report.nc_kind
  in
  { Report.nc_name = netconfig1.Report.nc_name;
    Report.nc_classes = netconfig1.Report.nc_classes;
    Report.nc_kind = kind;
    Report.nc_devices = list_conf
  }

(*
 * merge of two report :
 * merge_reports:
 * Report.t ->
 * Report.t ->
 * Report.t
 *)

let merge_reports cluster1 cluster2 =
  try
    List.map2 (fun elm1 elm2 ->
      match elm1, elm2 with
	| Report.Netconfig netconfig1, Report.Netconfig netconfig2 ->
	  Report.Netconfig (mr_netconfig netconfig1 netconfig2)
	| Report.Hardware hardware1, Report.Hardware hardware2 ->
	  Report.Hardware (mr_hardware hardware1 hardware2)
	| Report.Service service1, Report.Service service2 ->
	  Report.Service (mr_service service1 service2)
	| _ ->
	  Errors.raise (Errors.Cannot_merge_two_different_reports "config")

    )
      cluster1
      cluster2
  with Invalid_argument e ->
    Errors.raise (Errors.Cannot_merge_two_different_reports "merge_reports")
(*
 * Print report function
 * let print_report r =
 *)
let print_element elm  =
  if List.length elm.Ast.Report_info.good > 0
  then
    begin
      Printf.printf "Good values : [";
      let () = List.iter (fun elm -> (Printf.printf "%s; " elm)) elm.Ast.Report_info.good in
      Printf.printf "]";
    end
  else
    if List.length elm.Ast.Report_info.bad > 0
    then
      begin
	Printf.printf "Bad values : [";
	List.iter
	  ( fun (a, elm) ->
	    Printf.printf "[";
	    List.iter
	      (fun elm1 ->
		Printf.printf "%s; " elm1
	      ) elm ;
	    Printf.printf "]"
	  ) elm.Ast.Report_info.bad;
	Printf.printf "]";
      end
    else ()


 (*
 *packages
 *)
let print_packages packages =
  print_element packages.Report.p_status;
  print_element packages.Report.p_match

(*
 *file
 *)
let print_file file =
  print_element file

(*
 * netdevice
 *)
let print_netdevice netdevice =
print_element netdevice.Report.nd_state

(*
 *system
 *)
let print_system system =
  Printf.printf "value :%s " system.Report.sys_name ;
  List.iter ( fun elm -> print_element elm ) system.Report.sys_config

(*
 * r_config
 *)
let print_hardware_desc hardware_desc =
  match hardware_desc with
    | Report.Memory memory -> print_element memory
    | Report.Disk disk -> print_element disk
    | Report.Cpu cpu -> print_element cpu

let print_hardware hardware =
  Printf.printf "value :%s " hardware.Report.h_name ;
  List.iter print_hardware_desc hardware.Report.h_desc

let print_node_desc node_desc =
  match node_desc with
    | Report.Mount mount ->
      (print_element mount)
    | Report.Daemon daemon ->
      (print_element daemon)
    | Report.Packages packages ->
      (print_packages packages)
    | Report.System system ->
      (print_system system)
    | Report.File file ->
      (print_file file)

let print_node node =
  List.iter print_node_desc node.Report.n_desc

let print_service service =
  List.iter print_node service.Report.s_nodes

let print_netconfig netconfig =
  List.iter print_netdevice netconfig.Report.nc_devices;
  print_element netconfig.Report.nc_kind

let print_report config =
  List.iter
    (fun config ->
      match config with
	| Report.Netconfig netconfig ->
	  (print_netconfig netconfig)
	| Report.Hardware hardware ->
	  (print_hardware hardware)
	| Report.Service service ->
	  (print_service service)
    ) config
