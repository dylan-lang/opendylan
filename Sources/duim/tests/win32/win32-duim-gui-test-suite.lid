Library:      win32-duim-gui-test-suite
Author:       Andy Armstrong, Scott McKay
Synopsis:     An interactive test-suite for Win32 DUIM
Files:	  library
	  module
          harness
          bitmaps
          list-control
          start-tests
Linker-Options: $(guilflags)
Start-Function: main
Comment:	'C-Header-Files' is a kludge to get the bitmaps linked in
RC-Files:	bitmaps.rc
C-Header-Files: bitmaps/cut.ico
		bitmaps/copy.ico
		bitmaps/paste.ico
	        bitmaps/wizard.ico
		bitmaps/current-location.bmp
		bitmaps/prompt.bmp
		bitmaps/values.bmp
Other-files: Open-Source-License.txt
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

