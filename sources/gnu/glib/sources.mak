
#  This file is included into the make files for the generation of source
#  files in each of the X window system libraries.
#
# Copyright (C) 1998 Functional Objects, Inc. All rights reserved.

GEMA=/u/dylan/local/gema

# directory containing the GTK header files
GTKPLUSHDIR=../headers

# directory containing the GLIB header files
GLIBHDIR=$(GTKPLUSHDIR)/glib

# directory containing the GDK header files
GDKHDIR=$(GTKPLUSHDIR)/gdk

# directory containing the GTK header files
GTKHDIR=$(GTKPLUSHDIR)/gtk

# directory for files common to the various GTK libraries:
COMDIR=../glib
GTKCOMMONDIR=../gtk-common

PATTERNS=$(COMDIR)/patterns.pat

XTOD=$(COMDIR)/x-to-dylan

.SUFFIXES: .h .dylan .exp .src

PATTERN-DEP=$(PATTERNS) $(COMDIR)/sources.mak

.h.dylan:
	$(GEMA) -f $(PATTERNS) \
		-f $(COMDIR)/modcopyr.pat  \
		-exclude obsolete.text \
		-module GLib   -odir . -otyp .dylan $<

.src.dylan:
	$(GEMA) -f $(COMDIR)/include.pat -odir . -otyp .dylan $<

.dylan.exp:
	touch $@


# Can't re-export slots that were exported from the `Glib' module.
COMSLOT=$(COMDIR)/common-slots.pat
$(COMSLOT): $(COMDIR)/library.dylan
	$(GEMA) -match -idchars '_-$$<>@%&*+=/?' \
	  -p 'unmap:-=_' \
	  -p '<I>-value-setter\I=export-slot\:\\C@unmap{$$1}\=$$1-value\@end\n;<I>=' \
	  $(COMDIR)/library.dylan | sort -u > $(COMSLOT)

# Can't re-export slots that were exported from the `Gtk-Common' module.
GTKSLOT=$(GTKCOMMONDIR)/gtk-common-slots.pat
$(GTKSLOT): $(GTKCOMMONDIR)/library.dylan
	$(GEMA) -match -idchars '_-$$<>@%&*+=/?' \
	  -p 'unmap:-=_' \
	  -p '<I>-value-setter\I=export-slot\:\\C@unmap{$$1}\=$$1-value\@end\n;<I>=' \
	  $(GTKCOMMONDIR)/library.dylan | sort -u > $(GTKSLOT)
