Module:       dylan-user
Author:       Andy Armstrong
Synopsis:     DUIM web browser
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module web-browser
  use functional-dylan;
  use file-system;
  use streams;
  use format;
  use http-streams;
  use duim-internals;

  export <web-browser>,
         browse-web;
end module web-browser;
