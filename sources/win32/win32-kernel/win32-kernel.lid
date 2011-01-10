Library:      Win32-kernel
Synopsis:     Win32 API for non-GUI system services in "KERNEL32.DLL"
Executable:   DxW32KNL
Target-type:  DLL
Base-Address: 0x66900000
Library-Pack: Win32
RC-Files:     version.rc
Major-version: 2
Minor-version: 1
Compilation-mode: tight
Files:	library
	kernfirst
	winnt
	winbase
	winnls
	kernhack
C-Libraries: advapi32.lib
comment:  kernel32.lib is always included by the Dylan library.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

