Module:       test-report
Synopsis:     A tool to generate reports from test run logs
Author:	      Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method application-error
    (error-name :: <symbol>, format-string :: <string>, #rest args)
  format-out("\n");
  apply(format-out, format-string, args);
  format-out("\nUse %s -help for help on arguments.\n", application-name());
  exit-application(application-exit-code(error-name))
end method application-error;

// Just start it up

begin
  main(application-name(), application-arguments());
end;

