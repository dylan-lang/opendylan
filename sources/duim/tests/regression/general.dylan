Module:    win32-duim-regression-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  A regression test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bug 2667: Keyboard navigation does not work properly in DUIM

define frame <bug-2667-frame> (<simple-frame>)
  pane radio-box-pane (frame)
    make(<radio-box>,
	 items: #[1, 2, 3, 4, 5],
	 value-changed-callback: test-value-changed-callback,
	 activate-callback:      test-activate-callback);
  pane group-pane (frame)
    grouping ("Group box")
      vertically (spacing: 4)
        make(<push-button>,
	     label: "Press me!",
	     activate-callback: test-activate-callback);
        make(<list-box>,
	     items: #[1, 2, 3, 4, 5],
	     value-changed-callback: test-value-changed-callback,
	     activate-callback:      test-activate-callback)
      end
    end;
  layout (frame)
    vertically (spacing: 4)
      frame.radio-box-pane;
      frame.group-pane
    end;
end frame <bug-2667-frame>;

install-test
  (<bug-2667-frame>, 2667,
   "Keyboard navigation does not work properly in DUIM",
   "Verify that tabbing between gadgets works as expected, and also "
   "that left-arrow and right-arrow cycle the input focus properly.");
