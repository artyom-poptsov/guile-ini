## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
	fsm.scm	\
	fsm-context.scm

EXTRA_DIST += \
	fsm.scm	\
	fsm.puml

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/ini/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ini/

SUFFIXES = .puml

.puml.scm:
	@echo "  SMC      $<"
	@smc compile \
		-L $(top_srcdir)/modules \
		-U "((ini fsm-context) (smc context char-context))" \
		--fsm-name ini-fsm \
		--fsm-module "(ini fsm)" \
		--validate fsm.puml > fsm.scm

CLEANFILES += \
	fsm.scm

### Makefile.am ends here.
