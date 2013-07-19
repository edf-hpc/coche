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
    | Class_not_unique of string
    | Erroneous_tag_found of string * (int * int) * string * string
    | Missing_defaut_area of string
    | Error_while_reading_file of string
    | Invalid_size_multiplier of char
    | Invalid_size_description of string
    | Invalid_freq_multiplier of char
    | Invalid_freq_description of string

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
    | Class_not_unique s ->
        sprintf "Class %s is not uniquely defined" s
    | Erroneous_tag_found (file, (line, pos), signal, tag) ->
        sprintf "File \"%s\", line %d, characters 1-%d:\nErroneous tag %s found under a <%s ...>"
          file
          line
          pos
          signal
          tag
    | Missing_defaut_area s (* class name *) ->
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
