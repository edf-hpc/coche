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

open Ast
open Ast.Base
open Dtd
open Utils
open Errors

let dtd_file = ref "/usr/share/coche/coche.dtd"
let xml_file = ref "coche.xml"

let validate xml_file =
  let dtd_file =
    if Sys.file_exists !dtd_file then
      !dtd_file
    else
      Filename.concat (Filename.dirname xml_file) "coche.dtd"
  in
  let command = Printf.sprintf
    "xmllint --nonet --valid --dtdvalid %s %s 2>&1 >/dev/null"
    dtd_file
    xml_file
  in
  match Unix.system command with
    | Unix.WEXITED n when n <> 0 ->
        raise (XML_file_not_DTD_compliant xml_file)
    | _ -> ()

let pop input = ignore (Xmlm.input input)
let flat attrs = List.map (fun ((_,key),value) -> key, value) attrs

let string_of_signal = function
  | `El_end -> Printf.sprintf "El_end"
  | `Dtd (Some dtd) -> Printf.sprintf "Dtd(%s)" dtd
  | `Dtd None -> Printf.sprintf "Dtd"
  | `El_start (("", tag), _) ->
      Printf.sprintf "El_Start(%s)" tag
  | `El_start ((ns, tag), _) ->
      Printf.sprintf "El_Start(%s:%s)" ns tag
  | `Data data -> Printf.sprintf "Data(%s)" data

let xml_error input tag signal =
  raise (Erroneous_tag_found (!xml_file, Xmlm.pos input, string_of_signal signal, tag))

let read_data tag input =
  let rec read_my_data tag data input =
    match Xmlm.peek input, data with
      | `Data data, _ ->
          pop input;
          read_my_data tag (Some data) input
      | `El_end, Some data ->
          pop input;
          data
      | `El_start ((_, tag'), _), _ when tag' = tag ->
          pop input;
          read_my_data tag data input
      | signal, _ ->
          xml_error input tag signal
  in read_my_data tag None input

let get_attrs attrs names =
  let attrs = flat attrs in
  List.map (fun name -> List.assoc name attrs) names

let get_attrs_option attrs names =
  let attrs = flat attrs in
  List.map (fun name ->
    try
      Some (List.assoc name attrs)
    with _ ->
      None)
    names

let read_element_base read_attrs element attr_names input =
  match Xmlm.peek input with
    | `El_start ((_, tag), attrs) when tag = element ->
        pop input;
        let attr_values = read_attrs attrs attr_names in
        assert (List.length attr_values = List.length attr_names);
        pop input; (* Pops the forthcoming `El_end *)
        attr_values
    | _ as a -> xml_error input element a

let read_element = read_element_base get_attrs
let read_element_option = read_element_base get_attrs_option

let unique_named_element name names l =
  let module M = Map.Make(ExtString.String) in
  let rec dups s = function
  | [] -> s
  | h::l -> if M.mem h s then
        M.add h (M.find h s + 1) s
    else
        M.add h 1 s
  in
  let dups = dups M.empty names in
  let dups = M.fold (fun k v a -> (k,v)::a) dups [] 
    $ List.filter (fun (k,v) -> v > 1)
    $ List.map fst in
  match dups with
    | [] -> l
    | _ -> raise (Duplicate_elements (name, dups))

let find_classes classes_names classes =
  let nc_classes = ExtString.String.nsplit classes_names "," in
  let classes = List.map (fun cl -> cl.c_name, cl) classes in
  List.map (fun cl ->
    try
      List.assoc cl classes
    with _ ->
      raise (Class_not_found cl)
  ) nc_classes

let find_class class_name classes =
  let classes = List.map (fun cl -> cl.c_name, cl) classes in
  try
    List.assoc class_name classes
  with _ ->
    raise (Class_not_found class_name)

let merge_2_areas a1 a2 =
  { a1 with a_hosts = Network.merge_hosts a1.a_hosts a2.a_hosts;
            a_range = Network.merge_range a1.a_range a2.a_range
  }

let merge_2_nodes n1 n2 =
  { n1 with n_desc = n1.n_desc @ n2.n_desc }

let find_area area_name classes =
  let areas = List.fold_left
    (fun areas netclass ->
      try
        let new_areas = List.map (fun area -> area.a_name, area) netclass.c_areas in
        let new_area = List.assoc area_name new_areas in
        let final_area = begin try
            merge_2_areas new_area (List.assoc area_name areas)
          with _ ->
            new_area
        end in
        (area_name, final_area) :: (List.remove_assoc area_name areas)
      with _ ->
        areas
    )
    []
    classes in
  try
    List.assoc area_name areas
  with _ ->
    raise (Area_not_found area_name)

let merge_areas a1 a2 =
  let a1 = List.map (fun a -> a.a_name, a) a1 in
  let a2 = List.map (fun a -> a.a_name, a) a2 in
  let new_a =
    List.fold_left
      (fun areas (name, area) ->
        try
          let old_area = List.assoc name areas in
          let new_area = merge_2_areas area old_area in
          (name, new_area)::(List.remove_assoc name areas)
        with _ ->
          (name, area)::areas
      )
      a1
      a2 in
  List.map snd new_a

let merge_nodes n1 n2 =
  let n1 = List.map (fun n -> n.n_role, n) n1 in
  let n2 = List.map (fun n -> n.n_role, n) n2 in
  let new_n =
    List.fold_left
      (fun nodes (name, node) ->
        try
          let old_node = List.assoc name nodes in
          let new_node = merge_2_nodes node old_node in
          (name, new_node)::(List.remove_assoc name nodes)
        with _ ->
          (name, node)::nodes
      )
      n1
      n2 in
  List.map snd new_n

let read_areas class_name classes netlist areas input =
  let rec read_areas classes netlist default areas input =
    match Xmlm.peek input with
      | `El_start ((_, "area"), attrs) ->
          let attr_values = read_element "area" ["name"; "network"; "hosts"; "iprange"] input in
          begin try
              let network = List.assoc (List.nth attr_values 1) netlist in
              let a_hosts = Network.hosts (List.nth attr_values 2) in
              let a_range = Network.range (List.nth attr_values 3) in
              if   (Network.expand_hosts a_hosts $ List.length)
                <> (Network.expand_range a_range $ List.length)
              then
                raise (Hostlist_and_range_have_different_sizes
                          (Network.string_of_hosts a_hosts,
                           Network.string_of_range a_range))
              else if Network.are_members a_range network.n_cidr then
                let areas =
                  { a_name = List.nth attr_values 0;
                    a_network = network;
                    a_hosts = a_hosts;
                    a_range = a_range }
                  :: areas in
                read_areas classes netlist default areas input
              else
                raise (Range_is_not_in_defined_network
                          (Network.string_of_range a_range,
                           Network.string_of_cidr network.n_cidr))
            with Not_found ->
              raise (Network_not_found (List.nth attr_values 1))
          end
      | `El_start ((_, "include"), _) ->
          let included_class = read_data "include" input in
          let included_class = find_class included_class classes in
          let areas = merge_areas areas included_class.c_areas in
          read_areas classes netlist default areas input
      | `El_start ((_, "default"), _) ->
          let default = read_data "default" input in
          read_areas classes netlist (Some default) areas input
      | `El_end ->
          pop input;
          let areasl = List.map (fun a -> a.a_name, a) areas in
          begin match default with
            | Some default ->
                begin try
                    let default = List.assoc default areasl in
                    areas, default
                with _ ->
                  raise (Missing_defaut_area class_name)
                end
            | None -> raise (Missing_defaut_area class_name)
          end
      | _ as a -> xml_error input "area" a
  in
  let areas = read_areas classes netlist None areas input in
  let area_names = List.map (fun a -> a.a_name) (fst areas) in
  unique_named_element "area" area_names areas

let rec read_classes netlist classes input =
  match Xmlm.peek input with
    | `El_start ((_, "class"), attrs) ->
        pop input;
        let attrs = flat attrs in
        let name = List.assoc "name" attrs in
        let ctype = List.assoc "type" attrs in
        let areas, default = read_areas name classes netlist [] input in
        let classes =
          { c_name = name;
            c_type = ctype;
            c_areas = areas;
            c_default = default }
          :: classes in
        read_classes netlist classes input
    | `El_end ->
        pop input;
        classes
    | _ ->
        let class_names = List.map (fun c -> c.c_name) classes in
        unique_named_element "class" class_names classes

let rec read_devices classes devices input =
  match Xmlm.peek input with
    | `El_start ((_, "device"), attrs) ->
        let attr_values = read_element "device" ["name"; "target"; "flags"] input in
        let flags = ExtString.String.nsplit (List.nth attr_values 2) "," in
        let flags = List.map String.uppercase flags in
        let target = find_area (List.nth attr_values 1) classes in
        let devices = { nd_name = List.nth attr_values 0;
                        nd_target = target;
                        nd_flags = flags } :: devices
        in
        read_devices classes devices input
    | _ ->
        let device_names = List.map (fun d -> d.nd_name) devices in
        unique_named_element "device" device_names devices

let rec read_hardware hard input =
  match Xmlm.peek input with
    | `El_start ((_, "baseboard"), attrs) ->
        let attr_values = read_element_option "baseboard" ["vendor"; "name"] input in
        let hard = Baseboard { b_vendor = Option.get (List.nth attr_values 0);
                               b_name = List.nth attr_values 1 } :: hard
        in
        read_hardware hard input
    | `El_start ((_, "pci"), attrs) ->
        let attr_values = read_element_option "pci" ["type"; "desc"] input in
        let hard = Pci { field = Option.get (List.nth attr_values 0);
                         desc = Option.get (List.nth attr_values 1) } :: hard
        in
        read_hardware hard input
    | `El_start ((_, "memory"), attrs) ->
        let attr_values = read_element_option "memory" ["swap"; "ram"; "ram-speed"; "ram-modules"; "ram-delta"] input in
        let hard = Memory { swap = Option.map Units.Size.make (List.nth attr_values 0);
                            ram = Option.map Units.Size.make (List.nth attr_values 1);
                            ram_speed = Option.map Units.Freq.make (List.nth attr_values 2);
                            ram_modules = Option.map int_of_string (List.nth attr_values 3);
                            ram_delta = Option.map Units.Size.make (List.nth attr_values 4);
                          } :: hard
        in
        read_hardware hard input
    | `El_start ((_, "disk"), attrs) ->
        let attr_values = read_element_option "disk" ["device"; "size"] input in
        let hard = Disk { device = Option.get (List.nth attr_values 0);
                          size = Option.map Units.Size.make (List.nth attr_values 1) } :: hard
        in
        read_hardware hard input
    | `El_start ((_, "cpu"), attrs) ->
        let attr_values = read_element_option "cpu" ["maxfreq"; "sockets"; "cores"; "threads"; "model"] input in
        let hard = Cpu { maxfreq = Option.map Units.Freq.make (List.nth attr_values 0);
                         sockets = Option.map int_of_string (List.nth attr_values 1);
                         cores = Option.map int_of_string (List.nth attr_values 2);
                         threads = Option.map int_of_string (List.nth attr_values 3);
                         model = List.nth attr_values 4;
                       } :: hard
        in
        read_hardware hard input
    | _ -> hard

let rec read_quotas quotas input =
  match Xmlm.peek input with
    | `El_start ((_, "quota"), attrs) ->
        let attr_values = read_element_option "quota" ["type"; "target"; "size"] input in
        let q_type = Option.default "soft" (List.nth attr_values 0) in
        let target = Option.default "user" (List.nth attr_values 1) in
        let quotas = { q_type = if q_type = "soft" then `Soft else `Hard;
                       q_target = if target = "user" then `User else `Group;
                       q_size = Units.Size.make (Option.get (List.nth attr_values 2)) } :: quotas in
        read_quotas quotas input
    | _ -> quotas

let rec read_system config input =
  match Xmlm.peek input with
    | `El_start ((_, "kernel"), attrs) ->
        let attr_values = read_element_option "kernel" ["version"; "arch"] input in
        let version = Option.get (List.nth attr_values 0) in
        let arch = List.nth attr_values 1 in
        let config = Kernel { k_version = version; k_arch = arch } :: config in
        read_system config input
    | _ -> config

let rec read_packages packages input =
  match Xmlm.peek input with
    | `El_start ((_, "list"), attrs) ->
        let list = read_data "list" input
          $ ExtString.String.nsplit ","
          $ List.map ExtString.String.strip in
        read_packages (ExtList.List.unique list :: packages) input
    | `El_start ((_, "include"), attrs) ->
        let file = read_data "include" input in
        begin try
            let () = Unix.access file [ Unix.R_OK ] in
            let lines = Std.input_list (open_in file) in
            let lines = List.filter
              (fun l -> not (ExtString.String.starts_with l "#"))
              lines in
            let lines = List.map ExtString.String.strip lines in
            read_packages (ExtList.List.unique lines :: packages) input
          with Unix.Unix_error _ ->
            warn (File_not_readable_or_not_found file);
            read_packages packages input
        end
    | _ -> packages

let rec read_nodes nodes input =
  match Xmlm.peek input with
    | `El_start ((_, "mount"), attrs) ->
        pop input;
        let attrs = get_attrs_option attrs
          ["name"; "options"; "mountpoint"; "device"; "fstype"; "size"] in
        let quotas = read_quotas [] input in
        let nodes = Mount { m_name = Option.get (List.nth attrs 0);
                            m_options = List.nth attrs 1;
                            m_mountpoint = Option.get (List.nth attrs 2);
                            m_device = Option.get (List.nth attrs 3);
                            m_fstype = List.nth attrs 4;
                            m_size = Option.map Units.Size.make (List.nth attrs 5);
                            m_quota = quotas } :: nodes in
        pop input;
        read_nodes nodes input
    | `El_start ((_, "daemon"), attrs) ->
        let attr_values = read_element_option "daemon" ["name"; "status"] input in
        let status = Option.get (List.nth attr_values 1) in
        let nodes = Daemon { d_name = Option.get (List.nth attr_values 0);
                             d_status = if status = "running" then `Running else `Stopped }
          :: nodes in
        read_nodes nodes input
    | `El_start ((_, "packages"), attrs) ->
        pop input;
        let attr_values = get_attrs attrs ["status"; "match"] in
        let p_status = if String.lowercase (List.nth attr_values 0) = "installed"
          then `Installed
          else `Absent
        in
        let p_match = if String.lowercase (List.nth attr_values 1) = "exact"
          then `Exact
          else `Subset
        in
        let packages = read_packages [] input $ List.flatten $ ExtList.List.unique in
        let packages = List.map (fun p -> p, p_status) packages in
        let nodes = Packages { p_status = packages; p_match = p_match }
          :: nodes in
        pop input;
        read_nodes nodes input
    | `El_start ((_, "system"), attrs) ->
        pop input;
        let attr_values = get_attrs attrs ["type"] in
        let system_config = read_system [] input in
        let nodes = System { sys_name = List.nth attr_values 0; sys_config = system_config }
          :: nodes in
        pop input;
        read_nodes nodes input
    | `El_start ((_, "file"), attrs) ->
        let attr_values = read_element_option "file"
          ["name"; "owner"; "group"; "perms"; "same"; "type"] input in
        let same = match (List.nth attr_values 4) with
          | Some r when String.lowercase r = "yes" -> true
          | _ -> false in
        let f_type =
          match Option.map String.lowercase (List.nth attr_values 5) with
          | Some "file" -> Unix.S_REG
          | Some "directory" -> Unix.S_DIR
          | Some "chrdev" -> Unix.S_CHR
          | Some "blkdev" -> Unix.S_BLK
          | Some "symlink" -> Unix.S_LNK
          | Some "pipe" -> Unix.S_FIFO
          | Some "socket" -> Unix.S_SOCK
          | _ -> Unix.S_REG in
        let perms = match (List.nth attr_values 3) with
          | Some p -> Some (int_of_string ("0o" ^ p))
          | None -> None in
        let nodes = File { f_name = Option.get (List.nth attr_values 0);
                           f_owner = List.nth attr_values 1;
                           f_group = List.nth attr_values 2;
                           f_perms = perms;
                           f_same = same;
                           f_type = f_type } :: nodes in
        read_nodes nodes input
    | _ -> nodes

let rec read_service services classes nodes input =
  match Xmlm.peek input with
    | `El_start ((_, "node"), attrs) ->
        pop input;
        let attrs = flat attrs in
        let n_classes = List.assoc "classes" attrs in
        let n_role = List.assoc "role" attrs in
        let n_type = try Some (List.assoc "type" attrs) with _ -> None in
        let n_ha = try Some (List.assoc "ha" attrs) with _ -> None in
        let n_ha = match Option.map String.lowercase n_ha with
          | Some "active-passive" -> Some Active_passive
          | Some "active-active" -> Some Active_active
          | _ -> None in
        let nodes_desc = read_nodes [] input in
        let nodes = { n_classes = find_classes n_classes classes;
                      n_role = n_role;
                      n_type = n_type;
                      n_ha = n_ha;
                      n_desc = nodes_desc } :: nodes in
        pop input;
        read_service services classes nodes input
    | `El_start ((_, "include"), attrs) ->
        let included = read_data "include" input in
        begin try
            let included = List.assoc included services in
            let nodes = merge_nodes nodes included.s_nodes in
            read_service services classes nodes input
          with Not_found ->
            raise (Service_not_found included)
        end
    | _ ->
        let node_names = List.map (fun n -> n.n_role) nodes in
        unique_named_element "node" node_names nodes

let unique_config config =
  let services, hardware, netconfig = List.fold_left
    (fun (services, hardware, netconfig) -> function
    | Service s -> (s.s_name :: services), hardware, netconfig
    | Hardware h -> services, (h.h_name :: hardware), netconfig
    | Netconfig n -> services, hardware, (n.nc_name :: netconfig)
    )
    ([],[],[])
    config in
  ignore (unique_named_element "service" services config);
  ignore (unique_named_element "hardware" hardware config);
  unique_named_element "netconfig" netconfig config

let find_services list =
  List.fold_left
    (fun acc -> function
    | Service s -> (s.s_name, s) :: acc
    | _ -> acc
    )
    []
    list

let rec read_config classes config input =
  match Xmlm.peek input with
    | `El_start ((_, "netconfig"), attrs) ->
        pop input;
        let attrs = flat attrs in
        let name = List.assoc "name" attrs in
        let nc_type =
          try
            List.assoc "type" attrs
          with _ -> "physical" in
        let nc_type = if nc_type = "physical" then Physical else Virtual in
        let nc_classes =
          let nc_classes = List.assoc "classes" attrs in
          find_classes nc_classes classes
        in
        let devices = read_devices nc_classes [] input in
        pop input;
        let netconfig = { nc_name = name;
                          nc_classes = nc_classes;
                          nc_kind = nc_type;
                          nc_devices = devices } in
        let config = Netconfig netconfig :: config in
        read_config classes config input
    | `El_start ((_, "hardware"), attrs) ->
        pop input;
        let name = List.assoc "name" (flat attrs) in
        let n_classes = List.assoc "classes" (flat attrs) in
        let hardware = read_hardware [] input in
        let hardware = Hardware {h_name = name;
                                 h_classes = find_classes n_classes classes;
                                 h_desc = hardware } in
        let config = hardware :: config in
        pop input;
        read_config classes config input
    | `El_start ((_, "service"), attrs) ->
        pop input;
        let attrs = flat attrs in
        let name = List.assoc "name" attrs in
        let nodes = read_service (find_services config) classes [] input in
        let config = Service { s_name = name; s_nodes = nodes } :: config in
        pop input;
        read_config classes config input
    | `El_end ->
        pop input;
        unique_config config
    | _ ->
        unique_config config

let rec read_networks networks input =
  match Xmlm.peek input with
    | `El_start ((_, "network"), attrs) ->
        let attr_values = read_element "network" ["name"; "description"] input in
        let cidr = Network.cidr (List.nth attr_values 1) in
        let networks = {n_name = List.nth attr_values 0; n_cidr = cidr } :: networks in
        read_networks networks input
    | `El_start ((_, tag), _) when tag <> "network" ->
        let netlist = List.map (fun n -> n.n_name, n) networks in
        let network_names = List.map fst netlist in
        unique_named_element "network" network_names (networks, netlist)
    | _ as a-> xml_error input "network" a

let rec read_cluster input =
  match Xmlm.input input with
    | `Dtd _ | `El_end | `Data _ -> read_cluster input
    | `El_start ((_, "cluster"), attrs) ->
        let attr_values = get_attrs attrs ["name"; "version"] in
        let networks, netlist = read_networks [] input in
        let classes = read_classes netlist [] input in
        let config = read_config classes [] input in
        { name = List.nth attr_values 0;
          version = List.nth attr_values 1;
          networks = networks;
          classes = classes;
          config = config;
        }
    | _ as a -> xml_error input "cluster" a

let read file =
  let () = if !Flags.check_against_dtd then validate file in
  xml_file := file;
  let ic = open_in file in
  let input = Xmlm.make_input ~strip:true (`Channel ic) in
  try
    let config = read_cluster input in
    let () = close_in ic in
    config
  with e ->
    close_in ic;
    if not !Flags.check_against_dtd then Printf.eprintf "E: Did you validate %s against the DTD?\n" file;
    Pervasives.raise e
