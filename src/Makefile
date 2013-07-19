# Configuration
NAME := coche
PREFIX := /usr/local

# Auto-detection
ifeq ($(OCAMLBEST),)
HAS_OPT := $(shell if which jocamlopt > /dev/null; then echo yes; fi)
else ifeq ($(OCAMLBEST),native)
HAS_OPT := yes
else
HAS_OPT :=
endif

CLASSIC := $(if $(INSIDE_EMACS), -classic-display)
ARCH := $(if $(HAS_OPT),native,byte)
OCAMLBUILD := ocamlbuild$(CLASSIC)$(if $(HAS_OPT),, -byte-plugin)
OCAMLBUILD_ENV := OCAMLFIND_COMMANDS='ocamlc=jocamlc ocamlopt=jocamlopt ocamldep=jocamldep'

# Build
TARGETS := $(NAME).$(ARCH)

# C stubs magic for bytecode
export CAML_LD_LIBRARY_PATH=$(CURDIR)/_build/src

# Installation
BINDIR := $(DESTDIR)$(PREFIX)/bin

all: build

build:
	$(OCAMLBUILD_ENV) $(OCAMLBUILD) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

headers:
	headache -r -h _header Makefile \
		$(wildcard *.ml lib/*.ml lib/*.mli lib/*.mly lib/*.mll)
