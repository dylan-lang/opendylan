Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Dialog tests

define constant $default-title = "Test Suite";

define method yes-or-no? 
    (message :: <string>,
     #key title = $default-title,
          port = default-port(),
          owner)
 => (yes? :: <boolean>)
  with-frame-manager (frame-manager(owner))
    let yes-button
      = make(<push-button>, label: "Yes",
	     activate-callback: exit-dialog,
	     max-width: $fill);
    let no-button
      = make(<push-button>, label: "No",
	     activate-callback: cancel-dialog,
	     max-width: $fill);
    let dialog
      = make(<dialog-frame>,
	     title: title,
	     owner: owner,
	     resizable?: #f,
	     exit-buttons?: #f, 	// we'll do our own exit buttons
	     layout: vertically (x-alignment: #"center",
				 y-spacing: 5)
	               make(<label>, label: message);
                       horizontally (x-spacing: 2)
	                 yes-button; 
	                 no-button;
		       end
                     end);
    start-dialog(dialog) ~= #f
  end
end method yes-or-no?;

define method choose-string 
    (#key title = "Select a string",
          port = default-port(),
          owner)
 => (string :: false-or(<string>))
  with-frame-manager (frame-manager(owner))
    let text-field 
      = make(<text-field>, activate-callback: exit-dialog);
    let dialog
      = make(<dialog-frame>, 
	     title: title, 
	     owner: owner,
	     layout: text-field,
	     input-focus: text-field);
    start-dialog(dialog)
      & gadget-value(text-field)
  end
end method choose-string;

define method choose-integer
    (#key title = "Select an integer",
          port = default-port(),
          owner)
 => (integer :: false-or(<integer>))
  with-frame-manager (frame-manager(owner))
    let text-field 
      = make(<text-field>,
             value-type: <integer>,
             value-changing-callback: method (gadget)
                                        let dialog = sheet-frame(gadget);
                                        dialog-exit-enabled?(dialog)
                                          := gadget-value(gadget) ~= #f
                                      end,
	     activate-callback: exit-dialog);
    let dialog
      = make(<dialog-frame>, 
	     title: title, 
	     owner: owner,
	     layout: text-field,
	     input-focus: text-field);
    dialog-exit-enabled?(dialog) := gadget-value(text-field) ~= #f;
    start-dialog(dialog)
      & gadget-value(text-field)
  end
end method choose-integer;

define method new-choose-from-dialog
    (items :: <sequence>,
     #key title = "Select an item",
          label-key = identity,
          value-key = identity,
          port = default-port(),
          owner)
 => (item)
  with-frame-manager (frame-manager(owner))
    let chooser
      = make(<list-box>,
	     items: items,
	     label-key: label-key,
	     value-key: value-key,
	     activate-callback: exit-dialog);
    let dialog
      = make(<dialog-frame>, 
	     title: title, 
	     owner: owner,
	     layout: chooser,
	     input-focus: chooser);
    start-dialog(dialog)
      & gadget-value(chooser)
  end
end method new-choose-from-dialog;


/// Multiple values dialog

define frame <multiple-values-dialog> (<dialog-frame>)
  pane label-pane (frame)
    make(<option-box>, items: #("&Red", "&Green", "&Blue"));
  pane check-one (frame)
    make(<check-button>, label: "Check box test text");
  pane check-two (frame)
    make(<check-button>, label: "Check box test text");
  pane radio-box (frame)
    make(<radio-box>,
         items: #("Option &1", "Option &2", "Option &3", "Option &4"),
	 orientation: #"vertical");
  pane first-group-box (frame)
    grouping ("Group box", max-width: $fill)
      vertically (spacing: 4)
        make(<label>, label: "Label:");
        horizontally (spacing: 4, y-alignment: #"center")
          frame.label-pane;
          make(<button>, label: "Button");
        end;
        frame.check-one;
        frame.check-two;
      end
    end;
  pane second-group-box (frame)
    grouping ("Group box", max-width: $fill)
      frame.radio-box
    end;
  layout (frame)
    vertically (spacing: 4)
      frame.first-group-box;
      frame.second-group-box;
    end;
end frame <multiple-values-dialog>;

define method multiple-values-help
    (frame :: <multiple-values-dialog>) => ()
  notify-user("Help!", frame: frame)
end method multiple-values-help;

define method multiple-values-dialog-values
    (frame :: <multiple-values-dialog>)
 => (label :: <string>, check1? :: <boolean>, check2? :: <boolean>, radio1 :: <string>)
  values(frame.label-pane.gadget-value,
         frame.check-one.gadget-value,
         frame.check-two.gadget-value,
         frame.radio-box.gadget-value)
end method multiple-values-dialog-values;

define method choose-multiple-values
    (#key title = "Dialog Box",
          owner)
 => (label :: false-or(<string>),
     check1? :: <boolean>,
     check2? :: <boolean>, 
     radio1 :: false-or(<string>))
  with-frame-manager (frame-manager(owner))
    let dialog
      = make(<multiple-values-dialog>, 
             title: title,
             exit-buttons-position: #"right",
             help-callback: multiple-values-help);
    if (start-dialog(dialog))
      multiple-values-dialog-values(dialog)
    end
  end
end method choose-multiple-values;


/// Simple wizard test

define frame <simple-wizard-test> (<wizard-frame>)
  pane name-pane (frame)
    make(<text-field>);
  pane organization-pane (frame)
    make(<text-field>);
  pane job-description-pane (frame)
    make(<text-field>);
  pane years-employed-pane (frame)
    make(<text-field>, value-type: <integer>);
  pane first-page-layout (frame)
    make(<table-layout>,
         columns: 2,
         x-alignment: #(#"right", #"left"),
         children: vector(make(<label>, label: "Name:"),
                          frame.name-pane,
                          make(<label>, label: "Organization:"),
                          frame.organization-pane));
  pane second-page-layout (frame)
    make(<table-layout>,
         columns: 2,
         x-alignment: #(#"right", #"left"),
         children: vector(make(<label>, label: "Job Description:"),
                          frame.job-description-pane,
                          make(<label>, label: "Years Employed:"),
                          frame.years-employed-pane));
  pane first-page (frame)
    make(<wizard-page>, child: frame.first-page-layout);
  pane second-page (frame)
    make(<wizard-page>, child: frame.second-page-layout);
  pages (frame)
    vector(frame.first-page, frame.second-page);
  keyword title: = "Simple Wizard Test";
end frame <simple-wizard-test>;

define method initialize
    (frame :: <simple-wizard-test>, #key) => ()
  next-method();
  frame-input-focus(frame) := frame.name-pane;
end method initialize;

define method choose-from-wizard
    (#key title = "Employment Record",
          owner)
 => (name :: false-or(<string>),
     organization :: false-or(<string>),
     job-description :: false-or(<string>),
     years-worked :: false-or(<integer>))
  let frame
    = make(<simple-wizard-test>,
	   title: title, 
	   owner: owner);
  if (start-dialog(frame))
    let years-worked = frame.years-employed-pane;
    values(gadget-value(frame.name-pane),
           gadget-value(frame.organization-pane),
           gadget-value(frame.job-description-pane),
           gadget-value(years-worked))
  end
end method choose-from-wizard;


/// Simple property test

define frame <simple-property-test> (<property-frame>)
  pane name-pane (frame)
    make(<text-field>);
  pane organization-pane (frame)
    make(<text-field>);
  pane job-description-pane (frame)
    make(<text-field>);
  pane years-employed-pane (frame)
    make(<text-field>, 
         value-type: <integer>,
         value-changing-callback: method (gadget)
                                    let dialog = sheet-frame(gadget);
                                    let enabled? = (gadget-value(gadget) ~= #f);
                                    dialog-exit-enabled?(dialog) := enabled?
                                  end);
  pane first-page-layout (frame)
    make(<table-layout>,
         columns: 2,
         x-alignment: #(#"right", #"left"),
         children: vector(make(<label>, label: "Name:"),
                          frame.name-pane,
                          make(<label>, label: "Organization:"),
                          frame.organization-pane));
  pane second-page-layout (frame)
    make(<table-layout>,
         columns: 2,
         x-alignment: #(#"right", #"left"),
         children: vector(make(<label>, label: "Job Description:"),
                          frame.job-description-pane,
                          make(<label>, label: "Years Employed:"),
                          frame.years-employed-pane));
  pane first-page (frame)
    make(<tab-control-page>,
         label: "First",
         child: frame.first-page-layout);
  pane second-page (frame)
    make(<tab-control-page>,
         label: "Second",
         child: frame.second-page-layout);
  pages (frame)
    vector(frame.first-page, frame.second-page);
  keyword title: = "Simple Property Test";
  keyword apply-callback: = test-apply-callback;
end frame <simple-property-test>;

define method initialize
    (frame :: <simple-property-test>, #key) => ()
  next-method();
  dialog-exit-enabled?(frame) := #f;
  frame-input-focus(frame) := frame.name-pane;
end method initialize;

define method test-apply-callback
    (frame :: <simple-property-test>) => ()
  notify-user("Apply clicked!", owner: frame)
end method test-apply-callback;

define method display-properties
    (#key title = "Employment Record",
          owner)
 => (name :: false-or(<string>),
     organization :: false-or(<string>),
     job-description :: false-or(<string>),
     years-worked :: false-or(<integer>))
  let frame 
    = make(<simple-property-test>,
	   title: title,
	   owner: owner);
  if (start-dialog(frame))
    let years-worked = frame.years-employed-pane;
    values(gadget-value(frame.name-pane),
           gadget-value(frame.organization-pane),
           gadget-value(frame.job-description-pane),
           gadget-value(years-worked))
  end
end method display-properties;


/// Modeless dialog test

define function start-modeless-dialog
    (frame :: <frame>,
     #key title = "Modeless Dialog",
          owner) => ()
  with-frame-manager (frame-manager(owner))
    let text-field 
      = make(<text-field>,
             value-type: <integer>,
             value-changing-callback: method (gadget)
                                        let dialog = sheet-frame(gadget);
                                        dialog-exit-enabled?(dialog)
                                          := gadget-value(gadget) ~= #f
                                      end,
	     activate-callback: method (gadget)
                                  let text = gadget-text(gadget);
                                  frame-status-message(frame)
                                    := format-to-string("Activated gadget: '%s'", text)
                                end);
    let dialog
      = make(<dialog-frame>, 
             mode: #"modeless",
	     title: title,
	     owner: owner,
	     layout: text-field,
	     input-focus: text-field);
    start-dialog(dialog)
  end
end function start-modeless-dialog;


/// Simple Dialogs frame

define method show-result (frame :: <simple-frame>, value) => ()
  frame-status-message(frame) := format-to-string("Result: %=", value)
end method show-result;

define method test-yes-or-no? (frame :: <simple-frame>) => ()
  show-result(frame, yes-or-no?("Are you sure?", owner: frame))
end method test-yes-or-no?;

define method test-choose-string (frame :: <simple-frame>) => ()
  show-result(frame, choose-string(owner: frame))
end method test-choose-string;
  
define method test-choose-integer (frame :: <simple-frame>) => ()
  show-result(frame, choose-integer(owner: frame))
end method test-choose-integer;
  
define method test-new-choose-from-dialog (frame :: <simple-frame>)
  show-result
    (frame,
     new-choose-from-dialog(#("Red", "Green", "Blue"),
			    title: "Select a color",
			    owner: frame))
end method test-new-choose-from-dialog;
  
define method test-choose-multiple-values (frame :: <simple-frame>) => ()
  let (label, check1, check2, radio1) = choose-multiple-values(owner: frame);
  frame-status-message(frame)
    := format-to-string("Result: %=, %=, %=, %=",
			label, check1, check2, radio1)
end method test-choose-multiple-values;
  
define method test-choose-from-wizard (frame :: <simple-frame>) => ()
  let (name, organization, job-description, years-worked)
    = choose-from-wizard(owner: frame);
  frame-status-message(frame)
    := format-to-string("Result: %=, %=, %=, %=",
                        name, organization, job-description, years-worked)
end method test-choose-from-wizard;
  
define method test-display-properties (frame :: <simple-frame>) => ()
  let (name, organization, job-description, years-worked)
    = display-properties(owner: frame);
  frame-status-message(frame)
    := format-to-string("Result: %=, %=, %=, %=",
                        name, organization, job-description, years-worked)
end method test-display-properties;
  
define method test-modeless-dialog (frame :: <simple-frame>) => ()
  start-modeless-dialog(frame, owner: frame)
end method test-modeless-dialog;
  
define frame <simple-dialogs-frame> (<simple-frame>)
  pane tests (frame)
    make(<list-box>,
         items: vector(vector("Yes or No?",             test-yes-or-no?),
                       vector("Choose String",          test-choose-string),
                       vector("Choose Integer",         test-choose-integer),
                       vector("Choose from Dialog",     test-new-choose-from-dialog),
                       vector("Choose Multiple Values", test-choose-multiple-values),
                       vector("Choose from Wizard",     test-choose-from-wizard),
                       vector("Properties",             test-display-properties),
                       vector("Modeless Dialog",        test-modeless-dialog)),
         label-key: first,
         value-key: second,
	 scroll-bars: #"none",
         activate-callback: method (sheet :: <sheet>)
                              gadget-value(sheet)(sheet-frame(sheet))
                            end);
  pane main-layout (frame)
    frame.tests;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>);
end frame <simple-dialogs-frame>;

install-test(<simple-dialogs-frame>, "Dialogs");
