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

type range
type hosts
type ipv4
type prefix
type cidr

val cidr : string -> cidr
val ip : string -> ipv4
val range : string -> range
val hosts : string -> hosts

val ip_from_cidr : cidr -> ipv4
val prefix_from_cidr : cidr -> prefix

val valid_ip : ipv4 -> bool
val valid_cidr : cidr -> bool

val to_int : ipv4 -> int32
val from_int : int32 -> ipv4

val string_of_range : range -> string
val string_of_hosts : hosts -> string
val string_of_cidr : cidr -> string
val string_of_ipv4 : ipv4 -> string
val string_of_prefix : prefix -> string

val expand_range : range -> string list
val expand_hosts : hosts -> string list
val expand_ip_range : range -> ipv4 list

val fold_hosts : string list -> hosts

val block_of_cidr : cidr -> ipv4 * ipv4
val block_of_range : range -> (ipv4 * ipv4) list

val merge_range : range -> range -> range
val merge_hosts : hosts -> hosts -> hosts

val compare : ipv4 -> ipv4 -> int

val is_member : ipv4 -> cidr -> bool
val are_members : range -> cidr -> bool

val is_host_member : string -> hosts -> bool
