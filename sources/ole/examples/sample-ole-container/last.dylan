Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Run the main program.

define method main-program () => ()
  WinMain(application-instance-handle(),
	  $NULL-HINSTANCE,
	  application-command-line(),
	  application-show-window() );
end;

main-program();
