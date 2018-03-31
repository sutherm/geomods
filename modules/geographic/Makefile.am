## Process this file with automake to produce Makefile.in

SUBDIRS=dem util scripts

### scheme files ###
pkgguilesitedir = $(guilesitedir)/geographic
dist_pkgguilesite_DATA = ogr-gmt.scm rasters.scm spatial.scm regions.scm color-table.scm

### compiled scheme files ###

pkgguileobjectdir = $(guileobjectdir)/geographic
pkgguileobject_DATA = $(dist_pkgguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = -Warity-mismatch -Wformat -Wunused-variable
guilec_opts = --load-path=$(abs_top_srcdir)/modules

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS) $(guilec_opts)\
	 -L "$(abs_builddir)/.." -L "$(abs_srcdir)/.." -o "$@" "$<"

ETAGS_ARGS = ogr-gmt.scm rasters.scm spatial.scm regions.scm color-table.scm

CLEANFILES = $(pkgguileobject_DATA)
