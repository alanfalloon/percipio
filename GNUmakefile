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

CABAL=cabal
CABAL_CONFIGURE_FLAGS=--user --prefix=$(PREFIX)
PREFIX=$(HOME)/local

PERCIPIO=dist/build/percipio/percipio
CONFIG=dist/setup-config

$(PERCIPIO): force $(CONFIG)
	$(CABAL) build $(CABAL_BUILD_FLAGS)

$(CONFIG): Setup.hs percipio.cabal
	$(CABAL) configure $(CABAL_CONFIGURE_FLAGS)

test: $(PERCIPIO) test_data
	$< < test_data

force:: ;

clean::
	rm -r dist

all:    $(PERCIPIO)

.PHONY: all clean test
