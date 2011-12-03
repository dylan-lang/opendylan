Module:       dylan-user
Author:       Andy Armstrong
Synopsis:     DUIM web browser
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module web-browser
  use common-dylan, exclude: { format-to-string };
  use file-system;
  use streams;
  use format;
  use http-streams;
  use duim-internals;

  export <web-browser>,
         browse-web;
end module web-browser;
