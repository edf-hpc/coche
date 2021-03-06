############################################################################
#  Copyright (C) 2013-2015 EDF S.A.                                        #
#                                                                          #
#  This file is part of Coche.                                             #
#                                                                          #
#  Coche is free software: you can redistribute it and/or modify it        #
#  under the terms of the GNU General Public License as published by the   #
#  Free Software Foundation, either version 3 of the License, or (at your  #
#  option) any later version.                                              #
#                                                                          #
#  Coche is distributed in the hope that it will be useful, but WITHOUT    #
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or   #
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   #
#  for more details.                                                       #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with Coche.  If not, see <http://www.gnu.org/licenses/>.          #
#                                                                          #
############################################################################

# Configuration
NAME := coche
PREFIX := /usr/local

# Auto-detection
ifeq ($(OCAMLBEST),)
HAS_OPT := $(shell if which ocamlopt > /dev/null; then echo yes; fi)
else ifeq ($(OCAMLBEST),native)
HAS_OPT := yes
else
HAS_OPT :=
endif

CLASSIC := $(if $(INSIDE_EMACS), -classic-display)
ARCH := $(if $(HAS_OPT),native,byte)
OCAMLBUILD := ocamlbuild$(CLASSIC)$(if $(HAS_OPT),, -byte-plugin)

# Build
TARGETS := cocheLib.cma
ifeq ($(ARCH),native)
TARGETS += cocheLib.cmxa
endif
TARGETS += $(NAME).$(ARCH)

# C stubs magic for bytecode
export CAML_LD_LIBRARY_PATH=$(CURDIR)/_build/src

# Installation
BINDIR := $(DESTDIR)$(PREFIX)/bin

all: build

build:
	$(OCAMLBUILD) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

# Test
toplevel:
	rlwrap ocaml -init tests/toplevel.ml

# Headers update
headers:
	headache -r -h _header Makefile \
		$(filter-out lib/cocheArg.ml lib/cocheArg.mli,$(wildcard *.ml lib/*.ml lib/*.mli lib/*.mly lib/*.mll))
