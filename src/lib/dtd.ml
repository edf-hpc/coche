(****************************************************************************)
(*  Copyright (C) 2013 EDF S.A.                                             *)
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

type cluster = {
  name: string;
  version: string;
  networks: network list;
  classes: classes;
  config: config list;
}

and network = {
  n_name: string;
  n_cidr: Network.cidr;
}

and classes = netclass list

and netclass = {
  c_name: string;
  c_type: string;
  c_areas: area list;
  c_default: area;
}

and area = {
  a_name: string;
  a_network: network;
  a_hosts: Network.hosts;
  a_range: Network.range;
}

and config =
    | Netconfig of netconfig
    | Hardware of hardware
    | Service of service

and netconfig = {
  nc_name: string;
  nc_classes: classes;
  nc_kind: kind;
  nc_devices: netdevice list;
}

and kind = Physical | Virtual

and netdevice = {
  nd_name: string;
  nd_target: area;
  nd_state: [ `Up | `Down ];
}

and hardware = {
  h_name: string;
  h_desc: hardware_desc list;
}

and hardware_desc =
    | Memory of memory
    | Disk of disk
    | Cpu of cpu

and memory = {
  swap: Units.Size.t option;
  ram: Units.Size.t option;
}

and disk = {
  device: string;
  size: Units.Size.t option;
}

and cpu = {
  maxfreq: Units.Freq.t option;
  ncores: int option;
  nsockets: int option;
  nthreads: int option;
}

and service = {
  s_name: string;
  s_nodes: node list;
}

and node = {
  n_classes: classes;
  n_role: string;
  n_type: string option;
  n_ha: ha option;
  n_desc: node_desc list;
}

and ha = Active_active | Active_passive

and node_desc =
    | Mount of mount
    | Daemon of daemon
    | Packages of packages
    | System of system
    | File of file

and mount = {
  m_name: string;
  m_options: string option;
  m_mountpoint: string;
  m_device: string;
  m_fstype: string option;
  m_size: Units.Size.t option;
  m_quota: quota list;
}

and quota = {
  q_type: [ `Soft | `Hard ];
  q_target: [ `User | `Group ];
  q_size: Units.Size.t;
}

and daemon = {
  d_name: string;
  d_status: [ `Running | `Stopped ];
}

and packages = {
  p_status: [ `Installed | `Absent ];
  p_match: [ `Exact | `Subset ];
  p_list: packages_desc;
}

and pstatus = Installed | Absent
and pmatch = Exact | Subset

and packages_desc = string list

and system = {
  sys_name: string;
  sys_config: sysconfig list;
}

and sysconfig =
    | Kernel of kernel

and kernel = {
  k_version: string;
  k_arch: string option;
}

and file = {
  f_name: string;
  f_owner: string option;
  f_group: string option;
  f_same: bool;
  f_perms: Unix.file_perm option;
  f_type: Unix.file_kind;
}
