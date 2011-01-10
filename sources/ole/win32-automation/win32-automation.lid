Library:      win32-Automation
Target-type:  DLL
Base-Address: 0x65590000
Library-Pack: OLE
major-version: 2
minor-version: 1
Compilation-mode: tight
Files:	library
	auto-intf
	util
	amisc
	auto-err
	strings
	arrays
        variant
C-Source-Files: c-auto.c
C-Header-Files: c-auto.h
                ../com/c-com.h
		auto-aux.c
C-libraries:	oleaut32.lib
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

