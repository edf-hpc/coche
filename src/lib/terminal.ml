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

open Errors

external c_forkpty : unit -> int * Unix.file_descr =
  "c_forkpty"
let forkpty() = try Some (c_forkpty ()) with Unix.Unix_error _ -> None

(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
let create_session cmd args =
  match forkpty () with
  | None ->
      raise (Errors.Forkpty_failed (Unix.EUNKNOWNERR 0))
  | Some (0, _) ->
      begin try
        Unix.execvp cmd args (* never returns *)
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

let read_msg fd =
  let buf_len = 1024 in
  let buffer = String.create buf_len in
  let len = Unix.read fd buffer 0 buf_len in
  if len <= 0 then
    raise (Errors.Unix Unix.EBADF)
  else begin
    let s = String.sub buffer 0 len in
    s
  end

let write_msg fd msg =
  ignore(Unix.write fd (msg^"\n") 0 (String.length msg + 1))

let rec interact fdTerm =
  let first_msg = read_msg fdTerm in
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
  let rec auth msg =
    if Pcre.pmatch ~rex:passwordRx msg || Pcre.pmatch ~rex:passphraseRx msg then begin
      write_msg fdTerm (Unix.getenv "COCHE_PASSWORD")
    end
    else
      let msg = read_msg fdTerm in
      if msg <> "\r\n"
      then auth msg
  in
  let () = auth msg in
  let msg =
    let tmp = read_msg fdTerm in
    if tmp = "\r\n" then begin
      read_msg fdTerm
    end else
      tmp
  in
  if Pcre.pmatch ~rex:passwordRx msg then
    failwith "password failed"
  else begin
    Printf.printf "Got msg (last): %s\n%!" msg;
    let msg = read_msg fdTerm in
    Printf.printf "Got msg (last): %s\n%!" msg;
  end

let run cmd args =
  match fst (create_session cmd args) with
  | Some fdTerm ->
    interact fdTerm
  | None ->
    assert false (* create_session nevers returns None *)

let _ (* main *) =
  run "ssh" [| "-e none"; "-q"; Unix.getenv "COCHE_HOST"; "ls"; "-a"; "/tmp"|]
