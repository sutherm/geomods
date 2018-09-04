## Process this file with automake to produce Makefile.in

### python files ###

fetchdir = $(prefix)/share/$(PACKAGE)/fetch/

dcfetch.py: dcfetch.py.in
	if [ -f $@ ]; then [ -f $@ ] && rm -f $@; fi
	$(SED) -e "s:[@]fetchdir@:$(fetchdir):" $< > $@
	chmod a-w $@

fetch_DATA = digital_coast.gmt
bin_SCRIPTS = dcfetch.py
