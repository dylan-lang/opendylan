Module:       dylan-user
Synopsis:     GUI-TestWorks - a simple GUI wrapper for TestWorks
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gui-testworks
  use functional-dylan;
  use testworks, export: all;
  use duim;

  export gui-testworks;
end library gui-testworks;

define module gui-testworks
  use functional-dylan;
  use testworks, export: all;
  use threads;
  use duim;

  export <progress-window>,
	 *progress-window*,
	 gui-progress-display-message,
	 gui-progress-clear-all-messages,
	 gui-progress-pause,
	 gui-progress-pause-with-check-name,
	 gui-announce-function,
	 start-progress-window,
	 exit-progress-window,
	 gui-perform-suite,
	 gui-perform-test;
end module gui-testworks;
