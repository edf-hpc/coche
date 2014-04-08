/*
 * Original file "pty.c", from Unison 2.32.52.
 *
 * Changed to suit our own needs. For instance, we don't use openpty(3)
 * any longer and replaced it by forkpty(3) which is more convenient.
 * 
 * Copyright © 1999-2009 Benjamin Pierce et al
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>    // alloc_tuple
#include <caml/memory.h>   // Store_field
#include <caml/fail.h>     // failwith
#include <errno.h>         // ENOSYS

extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

// openpty
#if defined(__linux)
#include <pty.h>
#define HAS_OPENPTY 1
#endif

#if defined(__APPLE__) || defined(__NetBSD__)
#include <util.h>
#define HAS_OPENPTY 1
#endif

#ifdef __FreeBSD__
#include <sys/types.h>
#include <libutil.h>
#define HAS_OPENPTY 1
#endif

#ifdef HAS_OPENPTY

#include <sys/ioctl.h>
#include <sys/types.h>

/* setControllingTerminal : Unix.file_descr -> unit */
CAMLprim value setControllingTerminal(value fdVal) {
  int fd = Int_val(fdVal);
  if (ioctl(fd, TIOCSCTTY, (char *) 0) < 0)
    uerror("ioctl", (value) 0);
  return Val_unit;
}

/* c_forkpty: unit -> (int * Unix.file_descr) */
CAMLprim value c_forkpty() {
  int master;
  pid_t pid;
  value pair;
  if ((pid = forkpty(&master,NULL,NULL,NULL)) < 0)
    uerror("forkpty", (value) 0);
  pair = alloc_tuple(2);
  Store_field(pair,0,Val_int(pid));
  Store_field(pair,1,Val_int(master));
  return pair;
}

#else // not HAS_OPENPTY

CAMLprim value setControllingTerminal(value fdVal) {
  unix_error (ENOSYS, "setControllingTerminal", NULL);
}

CAMLprim value c_forkpty() {
  unix_error (ENOSYS, "forkpty", NULL);
}

#endif
