
#  This file is included into the make files for the generation of source
#  files in each of the MacOS libraries.
#
# Copyright (C) 1999 Functional Objects, Inc. All rights reserved.

GEMA=..\..\..\tools\gema\gema.exe

# directory containing the MacOS header files
MACOSHDIR=..\headers

# directory for files common to the various MacOS libraries:
COMDIR=..\interface

PATTERNS=$(COMDIR)\patterns.pat

MACTOD=$(GEMA) -f ..\interface\patterns.pat -f ..\interface\modcopyr.pat -exclude obsolete.text -rename renamings.text -otyp .dylan

.SUFFIXES: .dylan .exp .src .h

COMMONDEPENDENCIES=$(PATTERNS) $(COMDIR)\common.mak

.src.dylan:
	$(GEMA) -f $(COMDIR)\include.pat -odir . -otyp .dylan $<

%.dylan: $(MACOSHDIR)\%.h $(DEPENDENCIES)
	$(INTERFACETOD) $< $@

{$(MACOSHDIR)}.h{}.dylan:
	$(INTERFACETOD) $< $@

.dylan.exp:
	touch $@

# Can't re-export slots that were exported from the 'macos-interface' module.
COMSLOT=$(COMDIR)\common-slots.pat
