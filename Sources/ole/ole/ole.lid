Library:      OLE
Synopsis:     FFI interfaces to Microsoft Windows OLE features from "OLE2.H".
Executable:   DxOLE
Target-type:  DLL
Base-Address: 0x65680000
Library-Pack: OLE
RC-Files:     version.rc
Major-version: 2
Minor-version: 1
Compilation-mode: tight
Files:	library
	ole-err
	ole
	ole-intf
	omisc
C-Source-Files: c-ole.c
C-Header-Files: c-ole.h
		ole-aux.c
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Other-files: Open-Source-License.txt

