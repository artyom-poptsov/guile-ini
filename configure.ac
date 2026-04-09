# Guile-SSH -- An SSH library for GNU Guile.
#
# Process this file with autoconf to produce a configure script.
#
# Copyright © 2021-2023, 2025-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
# Copyright © 2021 Aleix Conchillo Flaqué <aleix@papayapay.com>

# -------------------------------------------------------------------------------
# Autoconf initialization.
# -------------------------------------------------------------------------------

AC_INIT([Guile-INI],
  [0.5.6],
  [poptsov.artyom@gmail.com],
  [guile-ini],
  [https://github.com/artyom-poptsov/guile-ini])

# -------------------------------------------------------------------------------

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux/m4])

AM_INIT_AUTOMAKE([gnu silent-rules -Wall -Wno-portability
                      color-tests])

m4_ifdef(
  [AM_SILENT_RULES],
  [AM_SILENT_RULES([yes])],
  [AC_SUBST([AM_DEFAULT_VERBOSITY],1)])

AC_ARG_WITH([guilesitedir],
  [AS_HELP_STRING([--with-guilesitedir],
     [use the specified installation path for Guile modules])],
  [case "x$withval" in
     xyes|xno) guilesitedir="";;
     *)        guilesitedir="$withval";;
   esac],
  [guilesitedir=""])

# -------------------------------------------------------------------------------
# Guile-SMC standalone mode.

AC_ARG_ENABLE(
  [standalonemode],
  [AS_HELP_STRING(
     [--enable-standalonemode],
        [Use the standalone mode for the Guile state machine compiler
         (Guile-SMC)])])

AS_IF(
  [test "x$enable_standalonemode" = "xyes"],
  [AC_MSG_NOTICE([Guile-SMC standalone mode enabled])
   AC_DEFINE(SMC_STANDALONE_MODE, 1, [Guile-SMC standalone mode enabled])],
  [AC_DEFINE(SMC_STANDALONE_MODE, 0, [Guile-SMC standalone mode disabled])])

AM_CONDITIONAL(
  [ENABLE_SMC_STANDALONE_MODE],
  [test "x$enable_standalonemode" = "xyes"])

# -------------------------------------------------------------------------------

GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR

GUILE_MODULE_REQUIRED([smc fsm])

if test "x$guilesitedir" = "x"; then
   guilesitedir="$datadir/guile/site/$GUILE_EFFECTIVE_VERSION"
fi
AC_SUBST([guilesitedir])

AC_CONFIG_FILES([
  Makefile
  build-aux/Makefile
  build-aux/am/Makefile
  build-aux/m4/Makefile
  doc/Makefile
  examples/Makefile
  modules/Makefile
  modules/ini/Makefile
  tests/Makefile
])

dnl texinfo package required to make the documentation in Texinfo format.
AC_CHECK_PROG(MAKEINFO_CHECK, makeinfo, yes)
AS_IF(
  [test x"$MAKEINFO_CHECK" != x"yes"],
  [AC_MSG_ERROR([Please install texinfo before configuring])])

AC_CONFIG_LINKS(
  [modules/ini/fsm.puml:modules/ini/fsm.puml])

if test "$guilesitedir" != "$GUILE_SITE"; then
  # Guile has a different prefix than this module
  AC_MSG_WARN([]
    [The Guile modules will be installed in ${guilesitedir}.]
    [You should probably re-run `configure' with]
    [`--with-guilesitedir=$GUILE_SITE']
    [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
    [variable.])
fi

# -------------------------------------------------------------------------------

# Generate a Makefile, based on the results.
AC_OUTPUT

dnl configure.ac ends here.
