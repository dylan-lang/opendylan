Module:    win32-duim-regression-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  A regression test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bug 3171: right click doesn't work to invoke in right click menus

define frame <bug-3171-frame> (<simple-frame>)
  pane list-control-pane (frame)
    make(<list-control>,
	 items: #[1, 2, 3, 4, 5],
	 popup-menu-callback: test-popup-menu-callback);
  layout (frame)
    frame.list-control-pane;
  status-bar (frame)
    make(<status-bar>);
end frame <bug-3171-frame>;

install-test
  (<bug-3171-frame>, 3171, 
   "right click doesn't work to invoke in right click menus",
   "Right-click to get a menu and then verify that right clicking on "
   "the menu items invokes the callback such that a message appears "
   "in the status bar.");


/// Bug 3879: DUIM: Fails to display some menu separator items

define command-table *foo* (*global-command-table*)
  menu-item "Test" = test-command;
end command-table *foo*;

define command-table *bar* (*global-command-table*)
  menu-item "Test" = test-command;
end command-table *bar*;

define command-table *group-of-commands* (*global-command-table*)
  menu-item "Foo" = *foo*;
  menu-item "Bar" = *bar*;
  menu-item "Dog" = test-command;
  menu-item "Cat" = test-command;
end command-table *group-of-commands*;

define command-table *menu* (*global-command-table*)
  menu-item "Test" = test-command;
  include *group-of-commands*;
end command-table *menu*;

define command-table *menu-bar* (*global-command-table*)
  menu-item "Menu" = *menu*
end command-table *menu-bar*;

define frame <bug-3879-frame> (<simple-frame>)
  command-table (frame) *menu-bar*;
end frame <bug-3879-frame>;

install-test
  (<bug-3879-frame>, 3879, 
   "DUIM: Fails to display some menu separator items",
   "There should be only one separator between \"Test\" and \"Foo\".");


/// Bug 3884: DUIM: Mishandling '&' and 'mnemonic:'

define frame <bug-3884-frame> (<simple-frame>)
  pane test-button (frame)
    make(<menu-button>,
	 label: "Replace && Find Next",
	 mnemonic: 'i');
  pane test-menu (frame)
    make(<menu>,
	 label: "Test",
	 children: vector(frame.test-button));
  menu-bar (frame)
    make(<menu-bar>, children: vector(frame.test-menu));
end frame <bug-3884-frame>;

install-test
  (<bug-3884-frame>, 3884, 
   "DUIM: Mishandling '&' and 'mnemonic:'",
   "The menu item in this test window should have the letter 'i' as "
   "its mnemonic.");


/// Bug 4334: ENV: popup menu behavior is strange [and non-standard]

define frame <bug-4334-frame> (<simple-frame>)
  pane list-control-pane (frame)
    make(<list-control>,
	 items: #[1, 2, 3, 4, 5],
	 popup-menu-callback: test-popup-menu-callback);
  layout (frame)
    frame.list-control-pane;
end frame <bug-4334-frame>;

install-test
  (<bug-4334-frame>, 4334, 
   "ENV: popup menu behavior is strange [and non-standard]",
   "Right-click to get a menu and then right-click on a different item. "
   "You should get a new popup menu each time.");

/// Bug 9999: updating items and values fails in update-callback

define frame <bug-9999-frame> (<simple-frame>)
  pane test-menu-box (frame)
    make(<radio-menu-box>,
	 update-callback: method (gadget)
			    gadget-items(gadget) := #[1, 2, 3, 4];
			    gadget-value(gadget) := random(4) + 1
			  end,
	 value-changed-callback: test-value-changed-callback);
  pane test-menu (frame)
    make(<menu>, label: "Test", children: vector(frame.test-menu-box));
  menu-bar (frame)
    make(<menu-bar>, children: vector(frame.test-menu));
end frame <bug-9999-frame>;

install-test
  (<bug-9999-frame>, 9999, 
   "updating items and values fails in update-callback",
   "");
