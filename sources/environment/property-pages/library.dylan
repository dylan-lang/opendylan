Module:    Dylan-User
Synopsis:  Environment property pages
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-property-pages
  use functional-dylan;
  use system;
  use duim-core;

  use file-source-records;

  use environment-protocols;
  use environment-manager;
  use environment-framework;
  use environment-tools;

  export environment-property-pages;
end library environment-property-pages;
