Module:    win32-duim-regression-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  A regression test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bug 4195: DUIM: multiple selection bugs with list/table controls

define frame <bug-4195-frame> (<simple-frame>)
  pane list-control-pane (frame)
    make(<list-control>,
	 items: #[1, 2, 3, 4, 5],
	 selection-mode: #"multiple",
	 value-changed-callback: test-value-changed-callback,
	 activate-callback:      test-activate-callback);
  pane table-control-pane (frame)
    make(<table-control>,
	 items: #[1, 2, 3, 4, 5],
	 headings: #["Value", "Doubled"],
	 generators: vector(identity, method (x) x * 2 end),
	 selection-mode: #"multiple",
	 value-changed-callback: test-value-changed-callback,
	 activate-callback:      test-activate-callback);
  layout (frame)
    vertically (spacing: 4)
      frame.list-control-pane;
      frame.table-control-pane
    end;
end frame <bug-4195-frame>;

install-test
  (<bug-4195-frame>, 4195,
   "DUIM: multiple selection bugs with list/table controls",
   "Verify that extending the selection with shift-click and "
   "control-click does the expected thing.");
