Module:    dll-wrap
Synopsis:  Wrapper for executing .dll projects.
Author:    1998/7/31 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main () => ()
  let args = application-arguments();
  if (args.size ~= 1)
    format-out("Usage: %s <dll-name>\n", application-name());
    exit-application(1);
  end if;
  let dll-location = args[0];
  format-out("Loading library %s...\n", dll-location);
  let dll-handle = load-library(dll-location);
  format-out("Library loaded, exiting.\n");
end method main;

begin
  main();
end;
