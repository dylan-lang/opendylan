Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Run the main program.

define method main-program () => ()
  WinMain(application-instance-handle(),
	  $NULL-HINSTANCE,
	  application-command-line(),
	  application-show-window() );
end;

main-program();
