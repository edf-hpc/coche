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

type 'a check = Ok of 'a | Fail of 'a *'a

type t = config list

and config =
    | Netconfig of netconfig
    | Hardware of hardware
    | Service of service
 
and netconfig = {
  nc_name: string;
  nc_kind: Dtd.kind check;
  nc_devices: netdevice list;
}

and netdevice = {
  nd_name: string;
  nd_target: Dtd.area;
  nd_state: [ `Up | `Down ] check;
}

and hardware = {
  h_name: string;
  h_desc: hardware_desc list;
}

and hardware_desc =
    | Memory of memory
    | Disk of disk
    | Cpu of cpu

and memory = Dtd.memory check

and disk = Dtd.disk check

and cpu = Dtd.cpu check

and service = {
  s_name: string;
  s_nodes: node list;
}

and node = {
  n_role: string;
  n_type: string option;
  n_ha: Dtd.ha option;
  n_desc: node_desc list;
}

and node_desc =
    | Mount of mount
    | Daemon of daemon
    | Packages of packages
    | System of system
    | File of file

and mount = Dtd.mount check
and daemon = Dtd.daemon check
and file = Dtd.file check * Digest.t

and packages = {
  p_status: (string * [ `Installed | `Absent ]) list check;
  p_match: [ `Exact | `Subset ] check;
}

and system = {
  sys_name: string;
  sys_config: Dtd.sysconfig check list;
}
