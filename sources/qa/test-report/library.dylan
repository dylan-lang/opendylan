Module:       dylan-user
Synopsis:     A tool to generate reports from test run logs
Author:       Shri Amit, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library test-report
  use common-dylan;
  use io;
  use system;
  use testworks;

  export test-report;
end library test-report;

define module test-report
  use common-dylan;
  use simple-io;
  use streams;
  use file-system;
  use operating-system;
  use threads,
    import: { dynamic-bind };
  use testworks;

  export read-log-file,
         perform-test-diff;
end module test-report;
