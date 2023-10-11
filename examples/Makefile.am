## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-INI.
##
## Guile-INI is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-INI is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-INI.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guilec

EXTRA_DIST = \
	ini-to-scm.in

SOURCES = \
	ini-to-scm.scm

examplesdir = $(pkgdatadir)/examples
examples_parenthesis_checkdir = $(pkgdatadir)/examples/
examples_parenthesis_check_DATA = $(SOURCES)

SUFFIXES = .in

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'
.in.scm:
	@echo "  GEN      $@"
	@$(MKDIR_P) "`dirname "$@"`"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

custom-fsm.scm: fsm.puml
	@echo "  SMC      $<"
	$(abs_top_builddir)/pre-inst-env $(abs_top_builddir)/utils/smc compile \
	--log-driver null \
	-L "$(abs_top_srcdir)/modules:$(abs_top_srcdir)/examples/parenthesis-check/" \
	-U "((context))" \
	--guile-smc-path $(abs_top_srcdir)/modules/ \
	--target guile-standalone \
	--fsm-name custom-fsm \
	--fsm-module "(custom-fsm)" \
	--validate \
	$< > $@

CLEANFILES = \
	custom-fsm.scm		\
	main.scm

# Makefile.am ends here

