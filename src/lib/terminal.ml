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

open Errors

external c_forkpty : unit -> int * Unix.file_descr =
  "c_forkpty"
let forkpty() = try Some (c_forkpty ()) with Unix.Unix_error _ -> None

(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
let create_session env cmd args =
  match forkpty () with
  | None ->
      raise (Errors.Forkpty_failed (Unix.EUNKNOWNERR 0))
  | Some (0, _) ->
      begin try
        Unix.execvpe cmd args env (* never returns *)
      with Unix.Unix_error (error, _, _) ->
        raise (Errors.Forkpty_failed error)
     end
  | Some (childPid, masterFd) ->
      (Some masterFd, childPid)

let passwordRx =
  Pcre.regexp ".*assword:[ ]*$"
let passphraseRx =
  Pcre.regexp "Enter passphrase for key.*"
let authenticityRx =
  Pcre.regexp "continue connecting \\(yes/no\\)\\?[ ]*$"
let knownhostsRx =
  Pcre.regexp "Warning: Permanently added .* to the list of known hosts."

let buf_size = 4096

let rec read_all buf fd =
  try
    let temp = String.create buf_size in
    let len = Unix.read fd temp 0 buf_size in
    if len <= 0 then
      Pervasives.raise Exit
    else
      let () = Buffer.add_substring buf temp 0 len in
      read_all buf fd
  with _ ->
    let final = Buffer.contents buf in
    Buffer.reset buf;
    final

let read_msg fd =
  try
    let buffer = String.create buf_size in
    let len = Unix.read fd buffer 0 buf_size in
    if len <= 0 then
      raise (Errors.Unix Unix.EBADF)
    else begin
      let s = String.sub buffer 0 len in
      s
    end
  with
    | Unix.Unix_error (_,"read","") -> ""
    | Unix.Unix_error (e,_,_) -> raise (Errors.Unix e)

let write_msg fd msg =
  ignore (Unix.write fd (msg ^ "\n") 0 (String.length msg + 1))

let rec interact fdTerm host password =
  let first_msg = read_msg fdTerm in
  if ExtString.String.exists first_msg "lost connection" then
    raise (Lost_connection host)
  else
  let msg =
    if Pcre.pmatch ~rex:authenticityRx first_msg then begin
      write_msg fdTerm "yes";
      let tmp = read_msg fdTerm in
      if tmp = "\r\n" || Pcre.pmatch ~rex:knownhostsRx tmp then
        read_msg fdTerm
      else
        tmp
    end
    else
      first_msg
  in
  let msg =
    if Pcre.pmatch ~rex:passwordRx msg || Pcre.pmatch ~rex:passphraseRx msg then
      begin
        write_msg fdTerm password;
        let tmp = read_msg fdTerm in
        if tmp = "\r\n" then
          read_msg fdTerm
        else tmp
      end
    else
      msg
  in
  if Pcre.pmatch ~rex:passwordRx msg || Pcre.pmatch ~rex:passphraseRx msg then
    raise (Errors.Authentification_failed host)
  else
    let buffer = Buffer.create buf_size in
    let () = Buffer.add_string buffer msg in
    read_all buffer fdTerm

let run env host password cmd args =
  match fst (create_session env cmd args) with
  | Some fdTerm ->
    interact fdTerm host password
  | None ->
    assert false (* create_session nevers returns None *)

let common_args host =
  let env = [|
    Printf.sprintf "%s=%s" "SSH_AGENT_PID" (Unix.getenv "SSH_AGENT_PID");
    Printf.sprintf "%s=%s" "SSH_AUTH_SOCK" (Unix.getenv "SSH_AUTH_SOCK");
	    |] in
  [| "-e none";
     "-o ConnectTimeout=1";
     "-o ConnectionAttempts=1";
     "-q";
     host;
  |], env

let ssh host password command =
  let common_args, env = common_args host in
  let args = Array.append common_args command in
  run env host password "ssh" args

let scp host password files destination =
  let common_args, env = common_args "-r" in
  let args = Array.concat
    [ common_args;
      files;
      [| Printf.sprintf "%s:%s" host destination |]
    ] in
  run env host password "scp" args
