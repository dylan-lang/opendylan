Library:   win32-duim
Synopsis:  DUIM back-end for Microsoft Windows
Author:    Scott McKay, Andy Armstrong
Files:  library
	module
	win32-definitions
	win32-c-definitions
        ffi-bindings
	whandler
        wresources
	wutils
	wport
	wmirror
	wmedium
	wcolors
	wfonts
	wdraw
	wkeyboard
	wevents
	wframem
	wdisplay
	wpixmaps
        wtop
	wgadgets
        wcontrols
	wmenus
        wdialogs
	whelp
        wclipboard
Major-version: 2
Minor-version: 1
C-Source-Files: c-com.c
C-libraries: ole32.lib uuid.lib shell32.lib comdlg32.lib comctl32.lib user32.lib gdi32.lib advapi32.lib
Linker-Options: HTMLHELP.LIB
Library-Pack: GUI
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Other-files: Open-Source-License.txt

