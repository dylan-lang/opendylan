Module:       dylan-user
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-test-suite
  use functional-dylan;

  use testworks;
  use testworks-specs;

  use duim-core;
  use duim-gadget-panes;
  use duim-extended-geometry;
  // use postscript-duim;	---*** No postscript for Kansas!

  export duim-test-suite;
end library duim-test-suite;
