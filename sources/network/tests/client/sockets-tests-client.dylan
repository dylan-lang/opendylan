Module:       sockets-tests-client
Synopsis:     A brief description of the project.
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite sockets-test-suite ()
  test sockets-tests;
end suite;

define test sockets-tests ()
  run-clients();
end test;

define method main () => ()
  run-test-application(sockets-test-suite);
end method main;

begin
  main();
end;
