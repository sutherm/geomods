## Process this file with automake to produce Makefile.in

### python files ###

fetchdir = $(prefix)/share/$(PACKAGE)/fetch/

chartfetch.py: chartfetch.py.in
	if [ -f $@ ]; then [ -f $@ ] && rm -f $@; fi
	$(SED) -e "s:[@]fetchdir@:$(fetchdir):" $< > $@
	chmod a-w $@

bin_SCRIPTS = chartfetch.py

fetch_DATA = chartfetch.gmt
