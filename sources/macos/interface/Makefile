
#  Dylan libraries for interfacing with MacOS.
#
#  This file controls the generation of source files that are created
#  automatically by adapting information from the MacOS C header files.

# Copyright (C) 1998-1999 Functional Objects, Inc. All rights reserved.

all: $(INTERFACELIB) library.dylan common-slots.pat

!include ..\interface\common.mak

INTERFACELIB=MixedMode.dylan \
             MacTypes.dylan \
             Errors.dylan \
             OSUtils.dylan \
             TextCommon.dylan \
             QuickdrawText.dylan \
             QDOffscreen.dylan \
             Quickdraw.dylan \
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
             MacWindows.dylan

INTERFACETOD=$(MACTOD) -module macos-interface

library.dylan: library.src $(INTERFACELIB)

common-slots.pat: library.dylan
	$(GEMA) -match -idchars "_-$$<>@%&*+=/?" \
	  -p "unmap:-=_" \
	  -p "<I>-value-setter\I=export-slot\:\\C@unmap{$$1}\=$$1-value\@end\n;<I>=" \
	  $(COMDIR)\library.dylan | sort  > $(COMSLOT)

DEPENDENCIES=$(COMMONDEPENDENCIES) make-sources obsolete.text renamings.text

test.dylan: $(MACOSHDIR)\test.h $(DEPENDENCIES)

MixedMode.dylan: $(MACOSHDIR)\MixedMode.h $(DEPENDENCIES)

MacTypes.dylan: $(MACOSHDIR)\MacTypes.h $(DEPENDENCIES)

Errors.dylan: $(MACOSHDIR)\Errors.h $(DEPENDENCIES)

OSUtils.dylan: $(MACOSHDIR)\OSUtils.h $(DEPENDENCIES)

TextCommon.dylan: $(MACOSHDIR)\TextCommon.h $(DEPENDENCIES)

QuickdrawText.dylan: $(MACOSHDIR)\QuickdrawText.h $(DEPENDENCIES)

QDOffscreen.dylan: $(MACOSHDIR)\QDOffscreen.h $(DEPENDENCIES)

Quickdraw.dylan: $(MACOSHDIR)\Quickdraw.h $(DEPENDENCIES)

Events.dylan: $(MACOSHDIR)\Events.h $(DEPENDENCIES)

Notification.dylan: $(MACOSHDIR)\Notification.h $(DEPENDENCIES)

AEDataModel.dylan: $(MACOSHDIR)\AEDataModel.h $(DEPENDENCIES)

AppleEvents.dylan: $(MACOSHDIR)\AppleEvents.h $(DEPENDENCIES)

Finder.dylan: $(MACOSHDIR)\Finder.h $(DEPENDENCIES)

Files.dylan: $(MACOSHDIR)\Files.h $(DEPENDENCIES)

Processes.dylan: $(MACOSHDIR)\Processes.h $(DEPENDENCIES)

AppleTalk.dylan: $(MACOSHDIR)\AppleTalk.h $(DEPENDENCIES)

Aliases.dylan: $(MACOSHDIR)\Aliases.h $(DEPENDENCIES)

Collections.dylan: $(MACOSHDIR)\Collections.h $(DEPENDENCIES)

Drag.dylan: $(MACOSHDIR)\Drag.h $(DEPENDENCIES)

Menus.dylan: $(MACOSHDIR)\Menus.h $(DEPENDENCIES)

MacWindows.dylan: $(MACOSHDIR)\MacWindows.h $(DEPENDENCIES)

# remove all generated files
clean:
	del /f $(INTERFACELIB) *.bak *.exp 2>nul
