
#  This file is included into the make files for the generation of source
#  files in each of the Win32 libraries.
#
# Copyright: 1998 Functional Objects, Inc.  All rights reserved.
#
# $HopeName: !sources.mak(D-kan.3) $
# $Date: 2004/03/12 00:10:22 $


# GEMA=/u/dylan/local/gema
GEMA=gema

# directory containing the header files from the Microsoft Win32 SDK:
# (Originally, these were not placed in the current directory because the
#  old Dylan C back-end compiler would over-write them with different files
#  with the same name.)
MSH=..\microsoft-headers

# directory for files common to the various Win32 libraries:
COMDIR=..\win32-common

PATTERNS=$(COMDIR)\patterns.pat
WINPAT=$(COMDIR)\winpat.pat
COMSLOT=$(COMDIR)\common-slots.pat

.SUFFIXES: .h .dylan .exp .src

#.h.dylan:
#	$(GEMA) -f $(WINPAT) -f $(PATTERNS) -odir . -otyp .dylan $<

.src.dylan:
	$(GEMA) -f $(COMDIR)\include.pat -odir . -otyp .dylan $<

.dylan.exp:
	touch $@


# Use this option to generate library for selected subset:
SUBSET_OPTIONS=-only $(COMDIR)\appnames.text
# Or use this option to included everything except obsolete or
# platform-dependent names:
FULL_OPTIONS=-exclude $(COMDIR)\obsolete-names.text \
 -exclude $(COMDIR)\w95-only.text -exclude $(COMDIR)\nt-only.text \
 -exclude $(COMDIR)\ce-only.text 

# Old values for subset:
#OPTIONS=$(SUBSET_OPTIONS)
#OPTDEP=$(COMDIR)\appnames.text

# New values for full API:
OPTIONS=$(FULL_OPTIONS)
OPTDEP=$(COMDIR)\obsolete-names.text $(COMDIR)\w95-only.text \
 $(COMDIR)\nt-only.text $(COMDIR)\ce-only.text


# Can't re-export slots that were exported from the `Win32-common' module.
$(COMSLOT): $(COMDIR)\comlib.dylan
	$(GEMA) -match -idchars '_-$$<>@%&*+=/?' \
		-p '<I>-value-setter\I=export-slot\:\\C$$1\=\$$0-value\@end\n;<I>=' \
		$(COMDIR)\comlib.dylan | sort -u > $(COMSLOT)


