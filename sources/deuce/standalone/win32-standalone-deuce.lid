Library:      standalone-deuce
Synopsis:     Standalone wrapper for DUIM-Deuce, for Win-32
Author:       Scott McKay
Files:  library
	module
	frame
	icons
	start
Linker-Options: $(guilflags)
Major-version: 2
Minor-version: 1
Comment:	'C-Header-Files' is a kludge to get the bitmaps linked in
RC-Files:	bitmaps.rc
C-Header-Files:	deuce.ico
		undo.ico
		redo.ico
		cut.ico
		copy.ico
		paste.ico
		find.ico
		replace.ico
		find-next.ico
		find-previous.ico
		new.ico
		open.ico
		save.ico
		potential-break.ico
		enabled-break.ico
		disabled-break.ico
		step-break.ico
		test-break.ico
		enabled-trace.ico
		disabled-trace.ico
		profile.ico
		current-location.ico
		prompt.ico
		values.ico
		warning.ico
		serious-warning.ico
Other-files: Open-Source-License.txt
Start-Function: ensure-deuce-started
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

