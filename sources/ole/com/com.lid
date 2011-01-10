Library:      COM
Synopsis:     Interface to the Microsoft "Component Object Model".
Executable:   DxCOM
Target-Type:  DLL
Base-Address: 0x65700000
Library-Pack: OLE
Major-Version: 2
Minor-Version: 1
Compilation-Mode: tight
Files:	library
	com-utils
        istream
	com
	cmisc
	com-err
	com-intf
	after
	factory
	dll-init
	custom
RC-Files: version.rc
C-Source-Files: c-com.c
C-Header-Files: c-com.h
C-Libraries: ole32.lib
	uuid.lib
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

