Module:    Dylan-User
Synopsis:  Environment code viewer
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-code-viewer
  use dylan;
  use functional-extensions;
  use streams;
  use format-out; //--- for debugging!

  use duim;
  use environment-protocols;

  export environment-code-viewer;
end library environment-code-viewer;
