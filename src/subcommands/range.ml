module Arg = CocheArg

let spec = []

let main () = ()

let () = Subcommand.register {
  Subcommand.name = "range";
  Subcommand.usage = "[options] host_expr";
  Subcommand.description = "Compute hostlists and IP ranges operations";
  Subcommand.main = main;
  Subcommand.spec = spec;
  Subcommand.anon = fun _ -> ();
}
