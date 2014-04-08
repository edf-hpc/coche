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

type position = int * int

type error =
    | Cannot_mix_different_kind_of_ranges
    | IP_ranges_with_different_depths
    | IP_range_with_incorrect_depth
    | IP_range_with_non_ordered_elements
    | Format_not_supported of string
    | Invalid_hosts_description of string
    | Empty_IP_range
    | Invalid_network_description of string
    | Invalid_IP_address_description of string
    | Invalid_IP_range_description of string
    | XML_file_not_DTD_compliant of string
    | Class_not_found of string
    | Area_not_found of string
    | Service_not_found of string
    | Network_not_found of string
    | Hostlist_and_range_have_different_sizes of string * string
    | Range_is_not_in_defined_network of string * string
    | Duplicate_elements of string * string list
    | Erroneous_tag_found of string * position * string * string
    | Missing_defaut_area of string
    | Error_while_reading_file of string
    | Invalid_size_multiplier of char
    | Invalid_size_description of string
    | Invalid_freq_multiplier of char
    | Invalid_freq_description of string
    | File_not_readable_or_not_found of string
    | Forkpty_failed of Unix.error
    | Unix of Unix.error

exception Error of error

val string_of_error : error -> string

val raise : error -> 'a
val warn : error -> unit
