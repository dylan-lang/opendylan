Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///----------------------------------------------------------------------------
/// Menu tests
///----------------------------------------------------------------------------

define test frame-menu-bar-test ()
  let works-component
    = make-test-pane(<menu-box>, items: #("update", "clone"));
  let works-menu 
    = make-test-pane(<menu>,
		     label: "works",
		     items: vector(works-component, "quit"));
  let help-menu
    = make-test-pane(<menu>,
		     label: "help",
		     items: #("about dylanworks"));
  let menu-bar = make-test-pane(<menu-bar>);
  add-child(menu-bar, works-menu);
  add-child(menu-bar, help-menu);
  let frame = make-test-frame(<test-frame>, menu-bar: menu-bar);
  frame
end test frame-menu-bar-test;

define test menu-parents-test ()
  let button = make-test-pane(<menu-button>, label: "test");
  let component
    = make-test-pane(<menu-box>,
		     selection-mode: #"single",
		     children: vector(button));
  check-equal("Initial menu button parent", sheet-parent(button), component);
  let menu = make-test-pane(<menu>, children: vector(component));
  check-equal("Initial menu component parent", 
	      sheet-parent(component), menu);
  let menu-bar = make-test-pane(<menu-bar>, children: vector(menu));
  check-equal("Initial menu parent", sheet-parent(menu), menu-bar);
  let frame = make-test-frame(<test-frame>, menu-bar: menu-bar);
  check-true("Initial menu bar parent",
	     subchild?(top-level-sheet(frame), menu-bar));
  frame
end test menu-parents-test;


/// Menu box pane testing

define method test-menu-box-panes-buttons ()
  test-gadget-box-pane-buttons(<push-menu-box>);
  test-gadget-box-pane-buttons(<radio-menu-box>);
  test-gadget-box-pane-buttons(<check-menu-box>);
end method test-menu-box-panes-buttons;

define method test-menu-box-panes-button-selections ()
  let rbp = make-collection-gadget(<radio-menu-box>);
  verify-gadget-box-pane-button-selection(rbp);
  let cbp = make-collection-gadget(<check-menu-box>,
				   selection: #(0, 2));
  verify-gadget-box-pane-button-selection(cbp);
  cbp
end method test-menu-box-panes-button-selections;

define test menu-box-panes-test ()
  test-menu-box-panes-buttons();
  test-menu-box-panes-button-selections();
end test menu-box-panes-test;



/// Define the menus test suite

define suite duim-menus-suite ()
  test frame-menu-bar-test;
  test menu-parents-test;
  test menu-box-panes-test;
end suite duim-menus-suite;
