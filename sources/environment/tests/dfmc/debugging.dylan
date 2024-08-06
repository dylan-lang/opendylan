Module:    dfmc-environment-test-suite
Synopsis:  DFMC Environment Tests
Author:    Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $debugging-test-application = "cmu-test-suite";

define function open-debugging-test-project () => ()
  let application
    = open-project(test-project-location($debugging-test-application));
  open-project-compiler-database
    (application, error-handler: project-condition-handler);

  test-project-build(application, link?: #t);

  unless (open-project-compiler-database
            (application, error-handler: project-condition-handler))
    parse-project-source(application);
  end unless;
  *test-application* := application;
end function;

define function close-debugging-test-project () => ()
  close-project(*test-application*);
end function;

define suite dfmc-environment-debugging-suite
    (setup-function:   open-debugging-test-project,
     cleanup-function: close-debugging-test-project,
     when: method ()
             select ($os-name)
               #"win32" =>
                 #t;
               otherwise =>
                 values(#f, "debugging is not yet supported on this platform");
             end select
           end method)
end suite;
