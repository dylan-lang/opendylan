Module:    duim-graphics-benchmarks
Author:    Andy Armstrong
Synopsis:  Interactive benchmarks for DUIM graphics
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main
    (arguments :: <sequence>) => (status-code :: <integer>)
  debug-message("Starting benchmarks with arguments %=", arguments);
  let status-code = start-benchmarks();
  debug-message("Exiting benchmarks with status code %d", status-code);
  status-code
end method main;

//---*** Should we use the operating system library to get the
//---*** real arguments?
main(#[]);
