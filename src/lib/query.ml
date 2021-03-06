(****************************************************************************)
(*  Copyright (C) 2013-2015 EDF S.A.                                        *)
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

open Unix
open Ast.Base
open Dtd
open Utils
open Result
open Units

(*
 * General use functions
 *)
let contains str sub =
  ExtString.String.exists str sub

let skip t = Ast.Result_info.Skip t
let ok t = Ast.Result_info.Ok t
let fail (t1,t2) = Ast.Result_info.Fail (t1,t2)

let run_or_skip run f t =
  if run then
    f t
  else
    skip t

let __ = Printf.sprintf

(*
 * Services section
 *)

let q_packages run packages =
  if not run then
    { p_status = skip packages.Dtd.p_status;
      p_match = skip packages.Dtd.p_match;
    }
  else
    let output_lines = read_process_lines "dpkg -l | awk 'NR > 5 { print $1,$2 }'" in
    let nb_packages = List.length output_lines in
    let dpkg_l =
      List.fold_left
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
    let l_st =
      List.map
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
        let fail_l =
          List.fold_left
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

let q_mount mount =
  let output_lines = read_process_lines "mount | awk '{ print $1,$3,$4,$5 }'" in
  let failed = { m_name = mount.m_name;
                 m_options = None;
                 m_mountpoint = mount.m_mountpoint;
                 m_fstype = None;
                 m_device = mount.m_device;
                 m_size = None;
                 m_quota = [] (* FIXME: Quota check is not implemented! *)
               }
  in
  try
    let elem = List.find (fun s -> contains s mount.m_device) output_lines in
    let size = read_process (__ "df -h -P 2>/dev/null | sed -n 's@^%s *\\([^ ]*\\).*$@\\1@p' | sed 's/,/./g'" mount.m_device) in
    let size = Units.Size.make (if size = "" then "0" else size^"B") in
    begin match ExtString.String.nsplit elem " " with
          | _ :: mount_point :: "type" :: mount_fstype :: mount_options :: [] ->
             if option_eq mount.m_options mount_options (* FIXME: Options can be in a different order *)
                && option_eq mount.m_fstype mount_fstype
                && mount.m_mountpoint = mount_point
             then
               ok mount
             else fail ({ failed with m_options    = option_same mount.m_options mount_options;
                                      m_mountpoint = mount_point;
                                      m_fstype     = option_same mount.m_fstype mount_fstype;
                                      m_size       = option_same mount.m_size size;
                        }
                       , mount)
          | _ :: mount_point :: "type" :: mount_fstype :: [] ->
             if mount.m_options = None
                && option_eq mount.m_fstype mount_fstype
                && mount.m_mountpoint = mount_point
             then
               ok mount
             else fail ({ failed with m_options    = None;
                                      m_mountpoint = mount_point;
                                      m_fstype     = option_same mount.m_fstype mount_fstype;
                                      m_size       = option_same mount.m_size size;
                        }
                       , mount)
          | _ ->
             fail (failed, mount)
    end
  with _ ->
       fail (failed, mount)

let q_file run file =
  let file_o = {f_name  = file.f_name;
                f_owner = file.f_owner;
                f_group = file.f_group;
                f_same  = Digest.file "/dev/null";
                f_perms = file.f_perms;
                f_type  = file.f_type } in
  if not run then
    skip file_o
  else
    try
      let file_name = file.f_name in
      let siz = Unix.stat file.f_name in
      let owner = getpwuid (siz.st_uid) in
      let owner = owner.pw_name in
      let group = getgrgid (siz.st_gid) in
      let group = group.gr_name in
      let kind = siz.st_kind in
      let perm = siz.st_perm in
      let file_r = {f_name  = file.f_name;
                    f_owner = option_same file.f_owner owner;
                    f_group = option_same file.f_group group;
                    f_same  = Digest.file file_name;
                    f_perms = option_same file.f_perms perm;
                    f_type  = kind } in
      if option_eq file.f_owner owner
         && option_eq file.f_group group
         && file.f_type = kind
         && option_eq file.f_perms perm
      then
        ok file_r
      else
        fail (file_r, file_o)
    with _ ->
         let file_r = {f_name  = file.f_name;
                       f_owner = None;
                       f_group = None;
                       f_same  = Digest.file "/dev/null";
                       f_perms = None;
                       f_type  = file.f_type } in
         fail (file_r, file_o)

let q_sysconfig sysconfig =
  let vers = read_process "uname -r " in
  let arch = read_process "uname -m"  in
  match sysconfig with
    | Ast.Base.Kernel kernel ->
      if kernel.k_version = vers && option_eq kernel.k_arch arch
      then
        ok sysconfig
      else
        let sysref = Ast.Base.Kernel { k_version = vers; k_arch = option_same kernel.k_arch arch } in
        fail (sysref, sysconfig)

let q_system run system =
  let syname = system.Dtd.sys_name in
  let list_sys_conf = system.Dtd.sys_config in
  let lst = List.fold_left
    (fun acc elt ->
      let elt = run_or_skip run q_sysconfig elt in
      elt :: acc
    )
    []
    list_sys_conf in
  {Result.sys_name = syname ; Result.sys_config = lst }

let q_node_desc run node_desc =
    match node_desc with
      |Dtd.Mount mount ->  Mount (run_or_skip run q_mount mount)
      |Dtd.Daemon daemon -> Daemon (run_or_skip run q_daemon daemon)
      |Dtd.Packages packages -> Packages (q_packages run packages)
      |Dtd.System system -> System (q_system run system)
      |Dtd.File file -> File (q_file run file)

let q_node in_classes node =
  let run = in_classes node.Dtd.n_classes in
  let list_nde = List.map (q_node_desc run) node.Dtd.n_desc  in
  { n_role = node.Dtd.n_role;
    n_classes = node.Dtd.n_classes;
    n_type = node.Dtd.n_type;
    n_ha = node.Dtd.n_ha;
    n_desc = list_nde }

let q_service in_classes service =
  let list_serv = List.map (q_node in_classes) service.Dtd.s_nodes in
  {s_name = service.Dtd.s_name;
   s_nodes = list_serv }

(*
 * Netconfig section
 *)

let q_netdevice should_run netdevice =
  let name = netdevice.Dtd.nd_name in
  let st = if should_run then
    begin
       let out = read_process (__ "/bin/ip link show dev %s | sed -nr '1 s/.*<([^>]*)>.*/\\1/p'" name) in
       let flag_mach = ExtString.String.nsplit out "," in
       let flag_user = netdevice.Dtd.nd_flags in
       let state = List.for_all (fun x -> List.mem x flag_mach) flag_user
      in
      let result = read_process (__ "/bin/ip addr show dev %s | sed -n 's@[ ]*inet6\\?[ ]*\\([^ /]*\\).*$@\\1@pg'" name) in
      let addrs = ExtString.String.nsplit result "\n" in
      let addrs = List.fold_left (fun addrs addr -> try (Network.ip addr) :: addrs with _ -> addrs) [] addrs in
      let my_ips = List.filter (fun (host, ip) -> ExtString.String.ends_with host (Utils.get_hostname ()))
                    (List.combine
                       (Network.expand_hosts netdevice.Dtd.nd_target.Ast.a_hosts)
                       (Network.expand_range netdevice.Dtd.nd_target.Ast.a_range)
                    )
      in
      match my_ips with
      | [_,my_ip] when List.mem (Network.ip my_ip) addrs && state = true ->
        ok netdevice.Dtd.nd_flags
      | _ ->
        fail (flag_mach, netdevice.Dtd.nd_flags)
    end
  else
    skip netdevice.Dtd.nd_flags
  in
  {Result.nd_name = name;
   nd_target = netdevice.Dtd.nd_target;
   nd_flags = st }

let q_netconfig in_classes netconfig =
  let should_run = in_classes netconfig.Dtd.nc_classes in
  let list_conf = List.map (q_netdevice should_run) netconfig.Dtd.nc_devices in
  let kind = (if should_run then ok else skip) netconfig.Dtd.nc_kind in
  { nc_name = netconfig.Dtd.nc_name;
    nc_classes = netconfig.Dtd.nc_classes;
    nc_kind = kind;
    nc_devices = list_conf}

(*
 * Hardware section
 *)

let q_baseboard baseboard =
  let vendor =
    Std.input_file "/sys/devices/virtual/dmi/id/board_vendor" $
      ExtString.String.strip
  in
  let name =
    Std.input_file "/sys/devices/virtual/dmi/id/board_name" $
      ExtString.String.strip
  in
  if baseboard.b_vendor = vendor
     && option_eq baseboard.b_name name
  then
    ok baseboard
  else
    fail ({b_vendor = vendor; b_name = option_same baseboard.b_name name}, baseboard)

let q_pci p =
  let pci_devices = read_process_lines "lspci" in
  let devices = List.filter (fun l -> ExtString.String.exists l p.field) pci_devices in
  let devices =
    List.map
      (fun l ->
       try
         snd (ExtString.String.split l ": ")
       with _ ->
         l
      )
      devices in
  if List.mem p.desc devices then
    ok p
  else
    fail ({field = ""; desc = ""}, p)

let q_disk disk =
  let output = read_process (__ "lsblk -n -d -o SIZE %s 2>/dev/null | sed 's@,@.@'" disk.device) in
  let output = output ^ "B" in
  try
    let siz = Units.Size.make output in
    match disk.size with
    | Some size ->
       if Units.Size.compare siz size = 0
       then ok disk
       else fail ({device = disk.device ; size = Some siz}, disk)
    | None -> ok disk
  with _ -> fail ({device = disk.device ; size = None}, disk)

let q_memory memory =
  let mem  = read_process "sed -n 's/MemTotal:[ ]*//p' /proc/meminfo" in
  let swap = read_process "sed -n 's/SwapTotal:[ ]*//p' /proc/meminfo" in
  let ram_speeds = read_process_lines "dmidecode -t memory | sed -n 's/^\tSpeed: \\([0-9].*\\)/\\1/p' | sort -ru" in
  let ram_speed =
    match ram_speeds with
    | [] -> Units.Freq.make "0"
    | f::_ -> Units.Freq.make f
  in
  let ram_modules = read_process "dmidecode -t memory | grep Size | grep -v 'No Module Installed' | wc -l" in
  let mem = Units.Size.make mem in
  let swap = Units.Size.make swap in
  let mem_rslt = { swap = option_same memory.swap swap;
                   ram = option_same memory.ram mem;
                   ram_speed = option_same memory.ram_speed ram_speed;
                   ram_modules = option_same memory.ram_modules (int_of_string ram_modules);
                   ram_delta = memory.ram_delta
                 }
  in
  match memory.swap, memory.ram, memory.ram_delta with
  | Some m_swap, Some ram, Some delta ->
      let mem = Units.Size.sub mem memory.ram_delta in
      if (Units.Size.compare m_swap swap = 0)
        && (Units.Size.compare ram mem) >= 0
      then ok memory
      else fail (mem_rslt, memory)
  | Some m_swap, Some ram, None ->
      if (Units.Size.compare m_swap swap = 0)
        && (Units.Size.compare ram mem) = 0
      then ok memory
      else fail (mem_rslt, memory)
  | None, None, _  ->  ok memory
  | None, Some ram, Some delta ->
      let mem = Units.Size.sub mem memory.ram_delta in
      if (Units.Size.compare ram mem) >= 0
      then ok memory
      else fail (mem_rslt, memory)
  | None, Some ram, None ->
      if (Units.Size.compare ram mem) = 0
      then ok memory
      else fail (mem_rslt, memory)
  | Some m_swap, None, _ ->
      if (Units.Size.compare m_swap swap) = 0
      then ok memory
      else fail (mem_rslt, memory)

let q_cpu cpu =
  let model = read_process "sed -n '/^model name/{s/.*: //pg;q;}' /proc/cpuinfo" in
  let maxf  = read_process "lscpu | sed -n 's/CPU MHz:[ ]*//p'" in
  let maxf = Units.Freq.make (maxf^"MHz") in
  let core = read_process "lscpu | sed -n 's@Core(s) per socket:[ ]*@@p'" in
  let core = int_of_string core in
  let socket = read_process "lscpu | sed -n 's@Socket(s):[ ]*@@p'" in
  let socket = int_of_string socket in
  let thread = read_process "lscpu | sed -n 's@Thread(s) per core:[ ]*@@p'" in
  let thread = int_of_string thread in
  match cpu.maxfreq with
  | Some maxfreq ->
     if Units.Freq.compare maxfreq maxf = 0
        && option_eq cpu.model model
        && option_eq cpu.cores core
        && option_eq cpu.sockets socket
        && option_eq cpu.threads thread
     then
       ok cpu
     else
       fail ({ model   = option_same cpu.model model;
               maxfreq = option_same cpu.maxfreq maxf;
               cores   = option_same cpu.cores core;
               sockets = option_same cpu.sockets socket;
               threads = option_same cpu.threads thread
             }, cpu)
  | None -> ok cpu

let q_hardware_desc run hardware_desc =
  match hardware_desc with
  | Dtd.Baseboard baseboard -> Baseboard (run_or_skip run q_baseboard baseboard)
  | Dtd.Pci pci -> Pci (run_or_skip run q_pci pci)
  | Dtd.Memory memory -> Memory (run_or_skip run q_memory memory)
  | Dtd.Disk disk -> Disk (run_or_skip run q_disk disk)
  | Dtd.Cpu cpu -> Cpu (run_or_skip run q_cpu cpu)

let q_hardware in_classes hardware =
  let should_run = in_classes hardware.Dtd.h_classes in
  let name = hardware.Dtd.h_name in
  let list_res= List.map (q_hardware_desc should_run) hardware.Dtd.h_desc in
  { h_name = name;
    h_desc = list_res;
    h_classes = hardware.Dtd.h_classes }

(*
 * Main function of the module to run the entire set of tests.
 *)

let run ~hostname:my_hostname cluster =
  let in_class a_classname =
    try
      let a_class = List.find (fun c -> c.Ast.c_name = a_classname) cluster.Dtd.classes in
      let check area = Network.is_host_member my_hostname area.Ast.a_hosts in
      List.exists check a_class.Ast.c_areas
    with Not_found -> false
  in
  let in_classes classes =
    List.exists (fun c -> in_class c.Ast.c_name) classes
  in
  List.map
    (fun config ->
      match config with
      | Dtd.Netconfig netconfig ->
         Netconfig (q_netconfig in_classes netconfig)
      | Dtd.Hardware hardware ->
         Hardware (q_hardware in_classes hardware)
      | Dtd.Service service ->
         Service (q_service in_classes service)
    )
    cluster.Dtd.config
