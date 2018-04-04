## Process this file with automake to produce Makefile.in

### scheme files ###
pkgguilesitedir = $(guilesitedir)/geographic/scripts/dem
dist_pkgguilesite_DATA = dem-build.scm hillshade.scm fuzzy-regions.scm guess-nodes.scm help.scm

### compiled scheme files ###

pkgguileobjectdir = $(guileobjectdir)/geographic/scripts/dem
pkgguileobject_DATA = $(dist_pkgguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = -Warity-mismatch -Wformat -Wunused-variable
guilec_opts = --load-path=$(abs_top_srcdir)/modules

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS) $(guilec_opts)\
	 -L "$(abs_builddir)/.." -L "$(abs_srcdir)/.." -o "$@" "$<"

ETAGS_ARGS = dem-build.scm hillshade.scm fuzzy-regions.scm guess-nodes.scm help.scm

CLEANFILES = $(pkgguileobject_DATA)
