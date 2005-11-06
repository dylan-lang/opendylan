.h.dylan:
	$(parse-header) $<

library.dylan: library.dylan.in $(module-built-dylan-files) \
               $(export-files) $(makefiles)
	sed -e 's/@EXPORTS@/cat *.exp/e' $< > $@

$(lid-file): $(lid-file).in $(export-files) $(makefiles)
	sed -e 's/\(\s*\)@FILES@/ls -1 *.h | \
	        sed "s\/^\\(.\\+\\)\\.h\\$$\/\1\\1\/"/e' $< > $@

tidy:
	rm -f *.bak *.exp *.h copy-headers-stamp

clean: tidy
	rm -f $(built-files)

tags: TAGS
TAGS: $(wildcard *.dylan)
	$(DTAGS) *.dylan

build: all
	opendylan -build $(module)
