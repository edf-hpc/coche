module Arg = CocheArg

let name = "range"

let spec = []

let main () = ()

let () = Subcommand.register {
  Subcommand.name = name;
  Subcommand.description = "Compute hostlists and IP ranges operations";
  Subcommand.main = main;
  Subcommand.spec = spec;
}
