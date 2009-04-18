# This source is part of Percepio -- the build system inspector
#
# Copyright (C) 2009 Alan Falloon <alan.falloon@gmail.com>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

all:

OCAMLFIND = ocamlfind
OCAMLC    = ocamlc
OCAMLOPT  = ocamlopt

OCAML_COMPILE_FLAGS = -w -g
OCAML_LINK_FLAGS    = -g
OCAMLC_FLAGS        =
OCAMLOPT_FLAGS      =

PACKAGES = camlp4
SYNTAX   = camlp4o

OCAMLFIND_PACKAGES = $(if $(PACKAGES),-package $(PACKAGES))
OCAMLFIND_SYNTAX   = $(if $(SYNTAX),-syntax $(SYNTAX))
OCAMLFIND_LINKPKG  = $(if $(PACKAGES),-linkpkg)

BUILD_DIR = build

SRC    = strace_parser.ml
NATIVE = $(BUILD_DIR)/percepio-parser
BYTE   = $(BUILD_DIR)/percepio-parser.byte

CMO = $(SRC:%.ml=$(BUILD_DIR)/%.cmo)
CMX = $(SRC:%.ml=$(BUILD_DIR)/%.cmx)

$(NATIVE): $(CMX) | $(BUILD_DIR)
	$(OCAMLFIND) $(OCAMLOPT) $(OCAML_LINK_FLAGS) $(OCAMLOPT_FLAGS) $(OCAMLFIND_LINKPKG) $(OCAMLFIND_PACKAGES) -o $@ $^
$(BYTE): $(CMO) | $(BUILD_DIR)
	$(OCAMLFIND) $(OCAMLC) $(OCAML_LINK_FLAGS) $(OCAMLC_FLAGS) $(OCAMLFIND_LINKPKG) $(OCAMLFIND_PACKAGES) -o $@ $^
$(CMX): $(BUILD_DIR)/%.cmx: %.ml | $(BUILD_DIR)
	$(OCAMLFIND) $(OCAMLOPT) $(OCAML_COMPILE_FLAGS) $(OCAMLOPT_FLAGS) $(OCAMLFIND_PACKAGES) $(OCAMLFIND_SYNTAX) -o $@ -c $<
$(CMO): $(BUILD_DIR)/%.cmo: %.ml | $(BUILD_DIR)
	$(OCAMLFIND) $(OCAMLC) $(OCAML_COMPILE_FLAGS) $(OCAMLC_FLAGS) $(OCAMLFIND_PACKAGES) $(OCAMLFIND_SYNTAX) -o $@ -c $<
$(BUILD_DIR):
	mkdir $@

test: $(BYTE) test_data
	$< < test_data

clean::
	rm -r $(NATIVE) $(BYTE) $(CMO) $(CMX)

all:    $(NATIVE) $(BYTE)
native: $(NATIVE)
byte:   $(BYTE)

.PHONY: all native byte clean test
