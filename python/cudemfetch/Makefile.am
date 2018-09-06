## Process this file with automake to produce Makefile.in

### python files ###

fetchdir = $(prefix)/share/$(PACKAGE)/fetch/

cudemfetch.py: cudemfetch.py.in
	if [ -f $@ ]; then [ -f $@ ] && rm -f $@; fi
	$(SED) -e "s:[@]fetchdir@:$(fetchdir):" $< > $@
	chmod a-w $@

fetch_DATA = cudem.gmt

bin_SCRIPTS = cudemfetch.py
