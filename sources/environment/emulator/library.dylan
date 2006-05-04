Module:    Dylan-User
Synopsis:  Emulator Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library emulator-environment
  use functional-dylan;
  use system;

  use variable-search;

  use duim;
  use capi-duim;

  use editor-manager;

  use sectionizer;
  use environment-protocols;
  use environment-framework;
  use environment-tools;
  use environment-deuce;

  use release-info;
  use emulator-environment-backend;
end library emulator-environment;
