Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Dialog class tests

define duim-frames class-test <dialog-frame> ()
  //---*** Fill this in...
end class-test <dialog-frame>;

define duim-frames class-test <property-frame> ()
  //---*** Fill this in...
end class-test <property-frame>;

define duim-frames class-test <property-page> ()
  //---*** Fill this in...
end class-test <property-page>;

define duim-frames class-test <wizard-frame> ()
  test-wizards()
end class-test <wizard-frame>;

define duim-frames class-test <wizard-page> ()
  //---*** Fill this in...
end class-test <wizard-page>;


/// Dialog test suites

define test notify-user-test ()
  //---*** Fill this in!
  #f
end test notify-user-test;

define test choose-file-test ()
  //---*** Fill this in!
  #f
end test choose-file-test;

define test choose-directory-test ()
  //---*** Fill this in!
  #f
end test choose-directory-test;

define test choose-color-test ()
  //---*** Fill this in!
  #f
end test choose-color-test;

define test choose-text-style-test ()
  //---*** Fill this in!
  #f
end test choose-text-style-test;

define test choose-from-dialog-test ()
  //---*** Fill this in!
  #f
end test choose-from-dialog-test;

define test choose-from-menu-test ()
  //---*** Fill this in!
  #f
end test choose-from-menu-test;


/// Wizard tests

define frame <test-wizard> (<wizard-frame>)
  pane first-button (frame)
    make(<button>, label: "One");
  pane second-button (frame)
    make(<button>, label: "Two");
  pane first-page (frame)
    vertically ()
      frame.first-button
    end;
  pane second-page (frame)
    vertically ()
      frame.second-button
    end;
end frame <test-wizard>;

define method initialize
    (wizard :: <test-wizard>, #key frame-manager: framem, #all-keys) => ()
  dialog-pages(wizard)
    := with-frame-manager (framem)
         vector(wizard.first-page, wizard.second-page)
       end;
  next-method();
end method initialize;

define method test-wizards
    () => ()
  let framem = find-test-frame-manager();
  let wizard = #f;
  check-true("wizard-frame first page is default",
	     begin
	       wizard := make-test-frame(<test-wizard>);
	       dialog-current-page(wizard) := wizard.first-page
	     end);
  when (wizard)
    check-false("wizard-frame back button is disabled and mapped",
	       begin
		 let back-button = dialog-back-button(wizard);
		 ~gadget-enabled?(back-button) & sheet-mapped?(back-button)
	       end);
    check-true("wizard-frame next button is enabled and mapped",
	       begin
		 let next-button = dialog-next-button(wizard);
		 gadget-enabled?(next-button) & sheet-mapped?(next-button)
	       end);
    check-true("wizard-frame exit button is enabled and unmapped",
	       begin
		 let exit-button = dialog-exit-button(wizard);
		 gadget-enabled?(exit-button) & ~sheet-mapped?(exit-button)
	       end);
    check-true("wizard-frame cancel button is enabled and mapped",
	       begin
		 let cancel-button = dialog-cancel-button(wizard);
		 gadget-enabled?(cancel-button) & sheet-mapped?(cancel-button)
	       end);
  end
end method test-wizards;


/// Define the menus test suite

define suite duim-dialogs-suite ()
  test notify-user-test;
  test choose-file-test;
  test choose-directory-test;
  test choose-color-test;
  test choose-text-style-test;
  test choose-from-dialog-test;
  test choose-from-menu-test;
end suite duim-dialogs-suite;
