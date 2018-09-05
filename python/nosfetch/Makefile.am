## Process this file with automake to produce Makefile.in

### python files ###

fetchdir = $(prefix)/share/$(PACKAGE)/fetch/

nosfetch.py: nosfetch.py.in
	if [ -f $@ ]; then [ -f $@ ] && rm -f $@; fi
	$(SED) -e "s:[@]fetchdir@:$(fetchdir):" $< > $@
	chmod a-w $@

fetch_DATA = nosfetch.gmt

bin_SCRIPTS = nosfetch.py
