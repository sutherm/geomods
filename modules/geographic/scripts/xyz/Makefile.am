## Process this file with automake to produce Makefile.in

### scheme files ###
pkgguilesitedir = $(guilesitedir)/geographic/scripts/xyz
dist_pkgguilesite_DATA = dump.scm snarf.scm datalist.scm help.scm vectorize.scm filter.scm

### compiled scheme files ###

pkgguileobjectdir = $(guileobjectdir)/geographic/scripts/xyz
pkgguileobject_DATA = $(dist_pkgguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = -Warity-mismatch -Wformat -Wunused-variable
guilec_opts = --load-path=$(abs_top_srcdir)/modules

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS) $(guilec_opts)\
	 -L "$(abs_builddir)/.." -L "$(abs_srcdir)/.." -o "$@" "$<"

ETAGS_ARGS = dump.scm snarf.scm datalist.scm help.scm vectorize.scm filter.scm

CLEANFILES = $(pkgguileobject_DATA)
