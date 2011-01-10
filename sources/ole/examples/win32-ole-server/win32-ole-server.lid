Library:   win32-ole-server
Synopsis:  Example of a simple OLE server application.  See the README file.
Files:	library
	app
	appobj
	program
	appdll
	appexe
Target-type: executable
RC-Files: simpsvr.rc
C-Header-Files: resource.h
		simpsvr.ico
Linker-Options: $(guilflags)
Start-function: main-program
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Other-files: README.html

