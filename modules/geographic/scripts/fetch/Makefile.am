## Process this file with automake to produce Makefile.in

### scheme files ###
pkgguilesitedir = $(guilesitedir)/geographic/scripts/fetch
dist_pkgguilesite_DATA = help.scm national-map.scm multibeam.scm hydro.scm digital-coast.scm srtm.scm charts.scm cudem.scm gmrt.scm

### compiled scheme files ###

pkgguileobjectdir = $(guileobjectdir)/geographic/scripts/fetch
pkgguileobject_DATA = $(dist_pkgguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = -Warity-mismatch -Wformat -Wunused-variable
guilec_opts = --load-path=$(abs_top_srcdir)/modules

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS) $(guilec_opts)\
	 -L "$(abs_builddir)/.." -L "$(abs_srcdir)/.." -o "$@" "$<"

ETAGS_ARGS = help.scm national-map.scm multibeam.scm hydro.scm digital-coast.scm srtm.scm charts.scm cudem.scm gmrt.scm

CLEANFILES = $(pkgguileobject_DATA)
