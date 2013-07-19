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

let verbose = ref false
let slave   = ref false
let port    = ref 1875
let master  = ref "localhost"
let config  = ref None

let set_remote m =
  if String.contains m ' ' || String.length m = 0
  then raise (Arg.Bad "Bad remote hostname")
  else master := m

let set_config c =
  if Sys.file_exists c
  then config := Some c
  else raise (Arg.Bad "Specified configuration file doesn't exist")

let spec = Arg.align [
  "-port", Arg.Set_int port, " Sets port number";
  "-slave", Arg.Clear slave, " Sets current instance as a slave";
  "-config", Arg.String set_config, " Sets configuration file";
  "-verbose", Arg.Set verbose, " Sets verbose mode"
]

let () = Arg.parse
  spec
  (fun master ->
    (* Ideally, one would get a "remote:port" string where
       the remote address and its allocated port number are
       clearly distinguishable. That way, we would not bother
       implementing bizarre stuff to make both mandatory (this
       is mainly due to a limitation of the module Arg).

       It is also possible to say that my default port number
       is always 1875 and not bother if it is not specified.
    *)
    set_remote master
  )
  "Configuration checker"

let check =
  (* Remote address is mandatory *)
  if !master = ""
  then raise (Arg.Bad "Please specify an address for master")

let host_addrs =
  let addrs_l = Std.input_all (Unix.open_process_in ("hostname -I"))
  in ExtString.String.nsplit addrs_l " "

let hostname = Unix.gethostname ()

let i_am_master () =
  not !slave && List.mem !master host_addrs

let socket =
  let inet = Unix.inet_addr_of_string !master in
  Unix.ADDR_INET (inet, !port)

let master_wait =
  def x () & y () = reply to x
  in x

let registered_nodes = ref []

def register_node (node) =
  if not (List.mem node !registered_nodes) then begin
    registered_nodes := node :: !registered_nodes;
    Printf.printf "Registered %s...\n%!" node;
  end;
  0

let master_node () =
  def master (slave) =
    spawn register_node (slave); reply hostname to master
  in
  let () = Join.Ns.register Join.Ns.here "master" master in
  let () = Join.Site.listen socket in
  spawn register_node (hostname);
  master_wait ()

let worker_node () =
  let server = Join.Site.there socket in
  let ns = Join.Ns.of_site server in
  let register = (Join.Ns.lookup ns "master" : string -> string) in
  print_endline (register hostname)

let main =
  if i_am_master ()
  then master_node ()
  else worker_node ()
