Module:    dylan-user
Synopsis:  Emulator TestWorks plus
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library testworks-plus
  use functional-dylan;
  use testworks, export: all;
  use io;

  export testworks-plus;
end;

define module testworks-plus
  use functional-dylan;
  use testworks, export: all;
  use format;
  use format-out;
  use streams;

  export run-test-application;
end;
