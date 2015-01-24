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

module type OrderedType = sig
  type 'a t
  type file_info
  val compare : 'a t -> 'a t -> int
end

type network = {
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

module Base = struct
  type mount = {
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

  and memory = {
    swap: Units.Size.t option;
    ram: Units.Size.t option;
    ram_speed: Units.Freq.t option;
    ram_modules: int option;
  }

  and baseboard = {
    vendor: string;
    name: string option;
  }

  and disk = {
    device: string;
    size: Units.Size.t option;
  }

  and cpu = {
    model: string option;
    maxfreq: Units.Freq.t option;
    cores: int option;
    sockets: int option;
    threads: int option;
  }

  and sysconfig =
  | Kernel of kernel

  and kernel = {
    k_version: string;
    k_arch: string option;
  }

  and kind = Physical | Virtual

  and ha = Active_active | Active_passive

  and 'a file_t = {
    f_name: string;
    f_owner: string option;
    f_group: string option;
    f_same: 'a;
    f_perms: Unix.file_perm option;
    f_type: Unix.file_kind;
  }

end

module Make(Ord: OrderedType) = struct

  type 'a info = 'a Ord.t

  type cluster = {
    name: string;
    version: string;
    networks: network list;
    classes: classes;
    config: config list;
  }

  and t = config list

  and config =
  | Netconfig of netconfig
  | Hardware of hardware
  | Service of service

  and netconfig = {
    nc_name: string;
    nc_classes: classes;
    nc_kind: Base.kind info;
    nc_devices: netdevice list;
  }

  and netdevice = {
    nd_name: string;
    nd_target: area;
    nd_state: [ `Up | `Down ] info;
  }

  and hardware = {
    h_name: string;
    h_classes: classes;
    h_desc: hardware_desc list;
  }

  and hardware_desc =
  | Baseboard of baseboard
  | Memory of memory
  | Disk of disk
  | Cpu of cpu

  and service = {
    s_name: string;
    s_nodes: node list;
  }

  and node = {
    n_classes: classes;
    n_role: string;
    n_type: string option;
    n_ha: Base.ha option;
    n_desc: node_desc list;
  }

  and node_desc =
  | Mount of mount
  | Daemon of daemon
  | Packages of packages
  | System of system
  | File of file

  and packages = {
    p_status: (string * [ `Installed | `Absent ]) list info;
    p_match: [ `Exact | `Subset ] info;
  }

  and system = {
    sys_name: string;
    sys_config: Base.sysconfig info list;
  }

  and file = (Ord.file_info Base.file_t) info

  and mount = Base.mount info
  and quota = Base.quota
  and daemon = Base.daemon info
  and memory = Base.memory info
  and baseboard = Base.baseboard info
  and disk = Base.disk info
  and cpu = Base.cpu info
  and kernel = Base.kernel

end

module Id = struct
  type 'a t = 'a
  type file_info = bool
  let compare = Pervasives.compare
end

module Dtd = Make(Id)

module Result_info = struct
  type 'a t = Ok of 'a | Fail of 'a * 'a | Skip of 'a
  type file_info = Digest.t
  let compare = Pervasives.compare
end

module Result = Make(Result_info)
