# This file is included into the makefiles for the generation of
# source files in each of the GTK+ 2.0 libraries.

# Copyright (C) 2005  Daniel Brockman

.SUFFIXES: .h .dylan .exp .src
.PHONY: all copy-headers

GEMA = gema
DTAGS = ../../../tools/gnuemacs/dtags

module-lowercase = $(shell echo $(module) | tr A-Z a-z)
module-deps-lowercase = $(shell echo $(module-deps) | tr A-Z a-z)

lid-file = $(module-lowercase).lid
built-dylan-files = library.dylan $(lid-file) $(module-built-dylan-files)
built-dylan-file-deps = $(makefiles) exclusions renamings ../patterns.gema
built-files = $(built-dylan-files) module.mk
common-makefiles = ../early.mk ../late.mk
makefiles = Makefile module.mk $(common-makefiles)

parse-header = $(GEMA) -f ../patterns.gema \
                 -exclude exclusions -rename renamings \
                 -module $(module) -otyp .dylan -odir .

all: $(built-files)

module.mk: copy-headers-stamp $(common-makefiles)
	( echo 'module-built-dylan-files = \' ; \
	  for header in *.h ; do \
	    echo -n '	' ; \
	    echo -n $$header | sed -e 's/h$$/dylan/' ; \
	    echo ' \' ; \
	  done ; \
	  echo '  # end of list' ; \
	  echo ; \
	  for header in *.h ; do \
	    echo -n $$header | sed -e 's/h$$/dylan/' ; \
	    echo ':' $$header '$$(built-dylan-file-deps)' ; \
	  done ) > $@

copy-headers: copy-headers-stamp
copy-headers-stamp:
	here=`pwd` ; cd $(headers-dir) && \
	  cp -p $(headers) $$here && \
	  touch $$here/copy-headers-stamp
