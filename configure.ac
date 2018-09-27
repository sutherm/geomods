#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# Process this file with autoconf to produce a configure script

##################
# Package info
AC_INIT([GEOMODS],[0.3.0],[matthew.love@colorado.edu])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

#AC_PREREQ(2.60)
#AC_USE_SYSTEM_EXTENSIONS([_GNU_SOURCE])
#AC_CONFIG_HEADERS([config.h])
#AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_MACRO_DIR([m4])
AC_PROG_SED

##################
# Check fodnl r Programs
dnl PKG_PROG_PKG_CONFIG
dnl AC_CANONICAL_HOST
dnl AC_PROG_INSTALL
dnl AC_PROG_MKDIR_P
dnl AM_PROG_AR

# from guile.m4
# sets: GUILE_CFLAGS and GUILE_LDFLAGS
GUILE_PKG([2.2 2.0])
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

if test "x$GUILD" = "x"; then
   AC_MSG_WARN(['guild' binary not found; please check your guile-2.x installation.])
   AC_PATH_PROG([GUILD], guild)
fi

AC_MSG_CHECKING([the Guile effective version])
guile_effective_version="`$GUILE -c '(display (effective-version))'`"
AC_MSG_RESULT([$guile_effective_version])

################
# Guile has three directories that don't follow the GNU Filesystem
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([gnu-filesystem-hierarchy],
            [AS_HELP_STRING([--with-gnu-filesystem-hierarchy],
                            [Strictly follow GNU Filesystem Heirarchy when installing, instead of querying Guile to discover the install directories that it expects.])])

case "x$with_gnu_filesystem_hierarchy" in
     xyes)
       # Follow the GNU Filesystem Heirarchy Standard correctly
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir="\$(datadir)/guile/site/$guile_effective_version"
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir="$libdir/guile/$guile_effective_version"
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir="$libdir/guile/$guile_effective_version/site-ccache"
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
     *)
       # Get the installation directories from Guile
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir=$GUILE_SITE
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir=$GUILE_EXTENSION
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir=$GUILE_SITE_CCACHE
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
esac


AM_PATH_PYTHON

##################
#  Output

AC_CONFIG_FILES([Makefile modules/Makefile modules/geodesic/Makefile \
modules/hulls/Makefile modules/xyz/Makefile modules/geographic/Makefile 
modules/geographic/dem/Makefile modules/geographic/util/Makefile \
modules/geographic/scripts/Makefile modules/geographic/scripts/xyz/Makefile \
modules/geographic/scripts/dem/Makefile modules/geographic/scripts/fetch/Makefile \
scripts/Makefile scripts/dem/Makefile scripts/fetch/Makefile scripts/xyz/Makefile \
python/Makefile python/gdal/Makefile python/mbfetch/Makefile python/dcfetch/Makefile \
python/nosfetch/Makefile python/tnmfetch/Makefile python/srtmfetch/Makefile \
python/chartfetch/Makefile python/cudemfetch/Makefile python/gmrtfetch/Makefile])

AC_OUTPUT

if test "$with_gnu_filesystem_hierarchy" = "yes"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [This is because you ran configure with the `--with-gnu-filesystem-hierarchy' flag]
   []
   [Remember that you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
else
   AC_MSG_NOTICE([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [These directories are the ones expected by $GUILE])
fi
AC_SUBST([guilesitedir])
