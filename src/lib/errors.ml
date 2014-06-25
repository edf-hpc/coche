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
    | Authentification_failed of string
    | Lost_connection of string
    | Unix of Unix.error

exception Error of error

open Printf

let string_of_error = function
    | Cannot_mix_different_kind_of_ranges ->
        sprintf "Cannot mix different kind of ranges"
    | IP_ranges_with_different_depths ->
        sprintf "IP ranges with different depths"
    | IP_range_with_incorrect_depth ->
        sprintf "IP range with incorrect depth"
    | IP_range_with_non_ordered_elements ->
        sprintf "IP range with non-ordered elements"
    | Format_not_supported s ->
        sprintf "Format not supported: %s" s
    | Invalid_hosts_description s ->
        sprintf "Invalid hosts description: %s" s
    | Empty_IP_range ->
        sprintf "Empty IP range found"
    | Invalid_IP_address_description s ->
        sprintf "Invalid IP address description: %s" s
    | Invalid_IP_range_description s ->
        sprintf "Invalid IP range description: %s" s
    | Invalid_network_description s ->
        sprintf "Invalid network description: %s" s
    | XML_file_not_DTD_compliant s ->
        sprintf "XML file %s is not DTD compliant" s
    | Class_not_found s ->
        sprintf "Class %s not found" s
    | Area_not_found s ->
        sprintf "Area %s not found" s
    | Service_not_found s ->
        sprintf "Service %s not found" s
    | Network_not_found s ->
        sprintf "Network %s not found" s
    | Hostlist_and_range_have_different_sizes (hosts, range) ->
        sprintf "Hostlist %s and address range %s have different size"
          hosts
          range
    | Range_is_not_in_defined_network (range, network) ->
        sprintf "Addresses in range %s are not members of %s"
          range
          network
    | Duplicate_elements (tag, names) ->
        sprintf "Not uniquely defined %s elements: %s"
          tag
          (ExtString.String.join ", " names)
    | Erroneous_tag_found (file, (line, pos), signal, tag) ->
        sprintf "File \"%s\", line %d, characters 1-%d:\nErroneous tag %s found under a <%s ...>"
          file
          line
          pos
          signal
          tag
    | Missing_defaut_area s ->
        sprintf "Missing default area in class %s" s
    | Error_while_reading_file s ->
        sprintf "Error while reading file %s" s
    | Invalid_size_multiplier c ->
        sprintf "Invalid size multiplier %c" c
    | Invalid_size_description s ->
        sprintf "Invalid size description %s" s
    | Invalid_freq_multiplier c ->
        sprintf "Invalid freq multiplier %c" c
    | Invalid_freq_description s ->
        sprintf "Invalid freq description %s" s
    | File_not_readable_or_not_found s ->
        sprintf "File %s not found or not readable" s
    | Forkpty_failed error ->
        sprintf "Forkpty failed: %s" (Unix.error_message error)
    | Authentification_failed host ->
        sprintf "Authentification error on host %s" host
    | Lost_connection host ->
        sprintf "Lost connection to %s" host
    | Unix error ->
        sprintf "Unix error: %s" (Unix.error_message error)

let raise e =
  raise (Error e)

let report flag fmt args =
  Printf.eprintf "%s: " flag;
  Printf.eprintf fmt args;
  Printf.eprintf "\n%!"

let warn e =
  report "W" "%s" (string_of_error e)

let () =
  Printexc.register_printer
    (function
    | Error exn -> Some ("E: " ^ (string_of_error exn))
    | _ -> None
    )
