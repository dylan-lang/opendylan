Library:      Win32-user
Synopsis:     Win32 API for window functions implemented in "USER32.DLL".
Executable:   DxW32USR
Target-type:  DLL
Base-Address: 0x66860000
Library-Pack: Win32
RC-Files:     version.rc
Major-version: 2
Minor-version: 1
Compilation-mode: tight
Files:	library
	predecl
	winuser
	moreuser
	handler
C-Libraries: user32.lib
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

