module Arg = CocheArg

let name = "check"
let xml_file = ref None

let set_xml f =
  if Sys.file_exists f
  then xml_file := Some f
  else raise (Arg.Bad "Specified XML file doesn't exist")

let spec = [
  "-xml", Arg.String set_xml, " Sets XML file";
]

let main () = ()

let () = Subcommand.register {
  Subcommand.name = name;
  Subcommand.description = "Read an XML file and run specified tests";
  Subcommand.main = main;
  Subcommand.spec = spec;
}
