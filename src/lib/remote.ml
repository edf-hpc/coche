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

external dumpFd : Unix.file_descr -> int = "%identity"

external setControllingTerminal : Unix.file_descr -> unit =
  "setControllingTerminal"
external c_openpty : unit -> Unix.file_descr * Unix.file_descr =
  "c_openpty"

let openpty () =
  try
    Some (c_openpty ())
  with Unix.Unix_error _ ->
    None

let passwordRx =
  ".*assword:[ ]*"
let passphraseRx =
  "Enter passphrase for key.*"
let authenticityRx =
  "The authenticity of host .* continue connecting \\(yes/no\\)\\? "

(* Utility functions copied from ocaml's unix.ml because they are not exported :-| *)
let rec safe_dup fd =
  let new_fd = Unix.dup fd in
  if dumpFd new_fd >= 3 then
    new_fd
  else begin
    let res = safe_dup fd in
    Unix.close new_fd;
    res
  end

let safe_close fd = try Unix.close fd with Unix.Unix_error _ -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let newnewstdin = safe_dup new_stdin in
  let newnewstdout = safe_dup new_stdout in
  let newnewstderr = safe_dup new_stderr in
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr;
  Unix.dup2 newnewstdin Unix.stdin; Unix.close newnewstdin;
  Unix.dup2 newnewstdout Unix.stdout; Unix.close newnewstdout;
  Unix.dup2 newnewstderr Unix.stderr; Unix.close newnewstderr

(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
let create_session cmd args new_stdin new_stdout new_stderr =
  match openpty () with
    None ->
      (None,
       Unix.create_process cmd args new_stdin new_stdout new_stderr)
    | Some (masterFd, slaveFd) ->
        begin match Unix.fork () with
            0 ->
              begin try
                  Unix.close masterFd;
                  ignore (Unix.setsid ());
                  setControllingTerminal slaveFd;
                  (* WARNING: SETTING ECHO TO FALSE! *)
                  let tio = Unix.tcgetattr slaveFd in
                  tio.Unix.c_echo <- false;
                  Unix.tcsetattr slaveFd Unix.TCSANOW tio;
                  perform_redirections new_stdin new_stdout new_stderr;
                  Unix.execvp cmd args (* never returns *)
                with _ ->
                  Printf.eprintf "Some error in create_session child\n";
                  flush stderr;
                  exit 127
              end
          | childPid ->
              Unix.close slaveFd;
              (Some masterFd, childPid)
        end
