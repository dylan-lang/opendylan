Library:      Win32-Registry
Synopsis:     Win32 API for the System Registry.
Executable:   DxW32REG
Target-type:  DLL
Base-Address: 0x665C0000
Library-Pack: Win32
RC-Files:     version.rc
Major-version: 2
Minor-version: 1
Compilation-mode: tight
Files:	library
	regconst
	winreg
	regutil
Comment: We need "advapi32.lib", but it is already included by "win32-kernel".
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Other-files: Open-Source-License.txt

