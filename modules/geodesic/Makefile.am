## Process this file with automake to produce Makefile.in

### scheme files ###
pkgguilesitedir = $(guilesitedir)/geodesic
dist_pkgguilesite_DATA = formulae.scm geod.scm 

### compiled scheme files ###

pkgguileobjectdir = $(guileobjectdir)/geodesic
pkgguileobject_DATA = $(dist_pkgguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = -Warity-mismatch -Wformat -Wunused-variable
guilec_opts = --load-path=$(abs_top_srcdir)/modules
.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS) $(guilec_opts)\
	 -L "$(abs_builddir)/.." -L "$(abs_srcdir)/.." -o "$@" "$<"

ETAGS_ARGS = formulae.scm geod.scm

CLEANFILES = $(pkgguileobject_DATA)
