Module:       win32-duim-gui-test-suite
Author:       Andy Armstrong, Scott McKay
Synopsis:     Win32 DUIM test suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main
    (arguments :: <sequence>) => (status-code :: <integer>)
  duim-debug-message("Starting Win32 DUIM GUI tests with arguments %=", arguments);
  let status-code = start-tests();
  duim-debug-message("Exiting Win32 DUIM GUI tests with status code %d", status-code);
  status-code
end method main;

//---*** Should we use the operating system library to get the
//---*** real arguments?
main(#[]);
