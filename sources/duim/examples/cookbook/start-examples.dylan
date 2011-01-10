Module:       duim-examples
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main
    (arguments :: <sequence>) => (status-code :: <integer>)
  debug-message("Starting DUIM examples with arguments %=", arguments);
  let status-code = start-examples();
  debug-message("Exiting DUIM examples with status code %d", status-code);
  status-code
end method main;

//---*** Should we use the operating system library to get the
//---*** real arguments?
main(#[]);
