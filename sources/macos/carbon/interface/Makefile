
#  Dylan libraries for interfacing with MacOS Carbon APIs.
#
#  This file controls the generation of source files that are created
#  automatically by adapting information from the MacOS Carbon C header files.

# Copyright (C) 1998-1999 Functional Objects, Inc. All rights reserved.

all: $(INTERFACELIB) library.dylan common-slots.pat

!include ..\interface\common.mak

INTERFACELIB=MixedMode.dylan \
             MacTypes.dylan \
             Errors.dylan \
             OSUtils.dylan \
             DateTimeUtils.dylan \
             TextCommon.dylan \
             QuickdrawText.dylan \
             QDOffscreen.dylan \
             Quickdraw.dylan \
             Fonts.dylan \
             Events.dylan \
             Notification.dylan \
             AEDataModel.dylan \
             AppleEvents.dylan \
             Finder.dylan \
             Files.dylan \
             Processes.dylan \
             AppleTalk.dylan \
             Aliases.dylan \
             Collections.dylan \
             Drag.dylan \
             Menus.dylan \
	     CodeFragments.dylan \
             Icons.dylan \
             MacWindows.dylan \
	     TextEdit.dylan \
	     Controls.dylan \
	     Dialogs.dylan

INTERFACETOD=$(MACTOD) -module carbon-interface

library.dylan: library.src $(INTERFACELIB)

common-slots.pat: library.dylan
	$(GEMA) -match -idchars "_-$$<>@%&*+=/?" \
	  -p "unmap:-=_" \
	  -p "<I>-value-setter\I=export-slot\:\\C@unmap{$$1}\=$$1-value\@end\n;<I>=" \
	  $(COMDIR)\library.dylan | sort  > $(COMSLOT)

DEPENDENCIES=$(COMMONDEPENDENCIES) make-sources obsolete.text renamings.text

test.dylan: $(CARBONHDIR)\test.h $(DEPENDENCIES)

MixedMode.dylan: $(CARBONHDIR)\MixedMode.h $(DEPENDENCIES)

MacTypes.dylan: $(CARBONHDIR)\MacTypes.h $(DEPENDENCIES)

Errors.dylan: $(CARBONHDIR)\Errors.h $(DEPENDENCIES)

OSUtils.dylan: $(CARBONHDIR)\OSUtils.h $(DEPENDENCIES)

DateTimeUtils.dylan: $(CARBONHDIR)\DateTimeUtils.h $(DEPENDENCIES)

TextCommon.dylan: $(CARBONHDIR)\TextCommon.h $(DEPENDENCIES)

QuickdrawText.dylan: $(CARBONHDIR)\QuickdrawText.h $(DEPENDENCIES)

QDOffscreen.dylan: $(CARBONHDIR)\QDOffscreen.h $(DEPENDENCIES)

Quickdraw.dylan: $(CARBONHDIR)\Quickdraw.h $(DEPENDENCIES)

Fonts.dylan: $(CARBONHDIR)\Fonts.h $(DEPENDENCIES)

Events.dylan: $(CARBONHDIR)\Events.h $(DEPENDENCIES)

Notification.dylan: $(CARBONHDIR)\Notification.h $(DEPENDENCIES)

AEDataModel.dylan: $(CARBONHDIR)\AEDataModel.h $(DEPENDENCIES)

AppleEvents.dylan: $(CARBONHDIR)\AppleEvents.h $(DEPENDENCIES)

Finder.dylan: $(CARBONHDIR)\Finder.h $(DEPENDENCIES)

Files.dylan: $(CARBONHDIR)\Files.h $(DEPENDENCIES)

Processes.dylan: $(CARBONHDIR)\Processes.h $(DEPENDENCIES)

AppleTalk.dylan: $(CARBONHDIR)\AppleTalk.h $(DEPENDENCIES)

Aliases.dylan: $(CARBONHDIR)\Aliases.h $(DEPENDENCIES)

Collections.dylan: $(CARBONHDIR)\Collections.h $(DEPENDENCIES)

Drag.dylan: $(CARBONHDIR)\Drag.h $(DEPENDENCIES)

Menus.dylan: $(CARBONHDIR)\Menus.h $(DEPENDENCIES)

CodeFragments.dylan: $(CARBONHDIR)\CodeFragments.h $(DEPENDENCIES)

Icons.dylan: $(CARBONHDIR)\Icons.h $(DEPENDENCIES)

MacWindows.dylan: $(CARBONHDIR)\MacWindows.h $(DEPENDENCIES)

TextEdit.dylan: $(CARBONHDIR)\TextEdit.h $(DEPENDENCIES)

Controls.dylan: $(CARBONHDIR)\Controls.h $(DEPENDENCIES)

Dialogs.dylan: $(CARBONHDIR)\Dialogs.h $(DEPENDENCIES)

# remove all generated files
clean:
	del /f $(INTERFACELIB) *.bak *.exp 2>nul
