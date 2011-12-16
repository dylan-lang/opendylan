Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main
    (arguments :: <sequence>) => (status-code :: <integer>)
  duim-debug-message("Starting DUIM GUI tests with arguments %=", arguments);
  let status-code = start-tests();
  duim-debug-message("Exiting DUIM GUI tests with status code %d", status-code);
  status-code
end method main;

main(application-arguments());
