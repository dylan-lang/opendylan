
#  This file is included into the make files for the generation of source
#  files in each of the X window system libraries.
#
# Copyright (C) 1998 Functional Objects, Inc. All rights reserved.

GEMA=/u/dylan/local/gema

# directory containing the X11 header files
XHDIR=/usr/openwin/include/X11
# directory containing the Motif header files
MHDIR=/usr/dt/include/Xm

# directory for files common to the various X/Motif libraries:
COMDIR=../xlib

PATTERNS=$(COMDIR)/patterns.pat

XTOD=$(COMDIR)/x-to-dylan

.SUFFIXES: .h .dylan .exp .src

PATTERN-DEP=$(PATTERNS) $(COMDIR)/motif.pat $(COMDIR)/sources.mak

.h.dylan:
	$(GEMA) -f $(PATTERNS) -f $(COMDIR)/motif.pat \
		-f $(COMDIR)/modcopyr.pat  \
		-exclude $(COMDIR)/obsolete.text \
		-module Motif   -odir . -otyp .dylan $<

.src.dylan:
	$(GEMA) -f $(COMDIR)/include.pat -odir . -otyp .dylan $<

.dylan.exp:
	touch $@


# Can't re-export slots that were exported from the `Xlib' module.
COMSLOT=$(COMDIR)/common-slots.pat
$(COMSLOT): $(COMDIR)/library.dylan
	$(GEMA) -match -idchars '_-$$<>@%&*+=/?' \
	  -p 'unmap:-=_' \
	  -p '<I>-value-setter\I=export-slot\:\\C@unmap{$$1}\=$$1-value\@end\n;<I>=' \
	  $(COMDIR)/library.dylan | sort -u > $(COMSLOT)


