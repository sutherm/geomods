## Process this file with automake to produce Makefile.in

### scheme files ###
pkgguilesitedir = $(guilesitedir)/xyz
dist_pkgguilesite_DATA = xyz.scm datalists.scm infos.scm

### compiled scheme files ###

pkgguileobjectdir = $(guileobjectdir)/xyz
pkgguileobject_DATA = $(dist_pkgguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = -Warity-mismatch -Wformat -Wunused-variable
guilec_opts = --load-path=$(abs_top_srcdir)/modules
.scm.go:
	GUILE_AUTO_COMPILE=0 \
	LTDL_LIBRARY_PATH=$(abs_builddir)/.libs \
	$(GUILD) compile --target="$(host)" $(GUILEC_FLAGS) $(guilec_opts)\
	 -L "$(abs_builddir)/.." -L "$(abs_srcdir)/.." -o "$@" "$<"

ETAGS_ARGS = xyz.scm datalists.scm infos.scm

CLEANFILES = $(pkgguileobject_DATA)
