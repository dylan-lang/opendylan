Module:    dylan-user
Synopsis:  A miniature DUIM
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library mini-duim
  use dylan;
  use functional-extensions;
  use equal-table;	//---*** Because C pointers don't compare with ==

  // For debugging
  use simple-streams;
  use simple-format;

  use transcendentals;

  use win32-common;
  use win32-user;
  use win32-gdi;
  use win32-kernel;
  use win32-dialog;

  //--- For 'window-callback-function' and 'enter-debugger'...
  //--- use idvm-application;

  export mini-duim;
end library mini-duim;
