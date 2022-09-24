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
	fsm-context-ini.scm

BUILT_SOURCES = \
	fsm-context.scm

INSTALL += \
	fsm-context.scm

EXTRA_DIST += \
	fsm.scm	\
	fsm.puml

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/ini/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ini/

GUILE_SMC_CONTEXT_ARGS = \
	--log-driver file \
	--log-opt "file=fsm-context.log" \
	-m "(ini fsm-context)"

GUILE_SMC_ARGS = \
	--log-driver file \
	--log-opt "file=fsm.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name ini-fsm \
	--fsm-module "(ini fsm)" \
	-U "((ini fsm-context) (ini fsm-context-ini))" \
	--validate

GUILE_SMC_ENV = GUILE_AUTO_COMPILE=0

if ENABLE_SMC_STANDALONE_MODE
GUILE_SMC_CONTEXT_ARGS += \
	--standalone
GUILE_SMC_ARGS += \
	--target=guile-standalone
endif

fsm-context.scm:
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc context $(GUILE_SMC_CONTEXT_ARGS) > $@

fsm.scm: fsm-context.scm fsm.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_ARGS) fsm.puml > $@

CLEANFILES += \
	fsm.scm	\
	fsm.log	\
	fsm-context.scm	\
	fsm-context.log

### Makefile.am ends here.
