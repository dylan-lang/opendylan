Module:       duim-examples
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple dialog examples

define method yes-or-no? 
    (message :: <string>,
     #key port = default-port(),
          frame-manager: framem)
  with-frame-manager (framem)
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
	     exit-callback:   #f,
	     cancel-callback: #f,
	     layout: vertically (x-alignment: #"center",
				 y-spacing: 5)
	               make(<label>, label: message);
                       horizontally (x-spacing: 2)
	                 yes-button; 
	                 no-button;
		       end
                     end);
    start-frame(dialog) ~= #f
  end
end method yes-or-no?;

define method choose-string 
    (#key title = "Select a string",
          port = default-port(),
          frame-manager: framem)
  with-frame-manager (framem)
    let text-field 
      = make(<text-field>,
	     value-changed-callback: exit-dialog);
    let dialog = make(<dialog-frame>, title: title, layout: text-field);
    start-frame(dialog)
      & gadget-value(text-field)
  end
end method choose-string;

define method new-choose-from-dialog
    (items :: <sequence>,
     #key title = "Select an item",
          label-key = identity,
          value-key = identity,
          port = default-port(),
          frame-manager: framem)
  with-frame-manager (framem)
    let chooser
      = make(<list-box>,
	     items: items,
	     label-key: label-key,
	     value-key: value-key,
	     activate-callback: exit-dialog);
    let dialog = make(<dialog-frame>, title: title, layout: chooser);
    start-frame(dialog)
      & gadget-value(chooser)
  end
end method new-choose-from-dialog;


/// Multiple values dialog

define frame <multiple-values-dialog> (<dialog-frame>)
  pane label-pane (frame)
    make(<option-box>, items: #("Red", "Green", "Blue"));
  pane check-one (frame)
    make(<check-button>, label: "Check box example text");
  pane check-two (frame)
    make(<check-button>, label: "Check box example text");
  pane radio-box (frame)
    make(<radio-box>,
         items: #("Option one",
                  "Option two",
                  "Option three",
                  "Option four"),
         layout-class: <column-layout>);
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
    (#rest args)
  debug-message("Help: %=\n", args);
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
    (#key title = "Dialog Box", frame-manager: framem)
 => (label :: <string>, check1? :: <boolean>, check2? :: <boolean>, radio1 :: <string>)
  with-frame-manager (framem)
    let dialog
      = make(<multiple-values-dialog>, 
             title: title,
             exit-buttons-position: #"right",
             help-callback: multiple-values-help);
    if (start-frame(dialog))
      multiple-values-dialog-values(dialog)
    end
  end
end method choose-multiple-values;


/// Simple wizard example

define frame <simple-wizard-example> (<wizard-frame>)
  pane name-pane (frame)
    make(<text-field>);
  pane organization-pane (frame)
    make(<text-field>);
  pane job-description-pane (frame)
    make(<text-field>);
  pane years-employed-pane (frame)
    make(<text-field>,
         value-key: string-to-integer);
  pane first-page (frame)
    make(<table-layout>,
         columns: 2,
         x-alignment: #(#"right", #"left"),
         children: vector(make(<label>, label: "Name:"),
                          frame.name-pane,
                          make(<label>, label: "Organization:"),
                          frame.organization-pane));
  pane second-page (frame)
    make(<table-layout>,
         columns: 2,
         x-alignment: #(#"right", #"left"),
         children: vector(make(<label>, label: "Job Description:"),
                          frame.job-description-pane,
                          make(<label>, label: "Years Employed:"),
                          frame.years-employed-pane));
end frame <simple-wizard-example>;

define method initialize
    (frame :: <simple-wizard-example>, #key) => ()
  wizard-frame-pages(frame) := vector(frame.first-page, frame.second-page);
  next-method();
end method initialize;

define method choose-from-wizard
    (#key title = "Employment Record", frame-manager: framem)
 => (name :: <string>, organization :: <string>, job-description :: <string>,
     years-worked :: <integer>)
  let frame = make(<simple-wizard-example>, title: title, frame-manager: framem);
  if (start-frame(frame))
    let years-worked = frame.years-employed-pane;
    values(gadget-value(frame.name-pane),
           gadget-value(frame.organization-pane),
           gadget-value(frame.job-description-pane),
           gadget-value(years-worked))
  end
end method choose-from-wizard;


/// Simple Dialogs frame

define method show-result (frame :: <simple-frame>, value) => ()
  gadget-label(frame-status-bar(frame)) := format-to-string("Result: %=", value)
end method show-result;

define method test-yes-or-no? (frame :: <simple-frame>) => ()
  show-result(frame, yes-or-no?("Are you sure?", frame-manager: frame-manager(frame)))
end method test-yes-or-no?;

define method test-choose-string (frame :: <simple-frame>) => ()
  show-result(frame, choose-string(frame-manager: frame-manager(frame)))
end method test-choose-string;
  
define method test-new-choose-from-dialog (frame :: <simple-frame>)
  show-result
    (frame,
     new-choose-from-dialog(#("Red", "Green", "Blue"),
                            frame-manager: frame-manager(frame),
                            title: "Select a color"))
end method test-new-choose-from-dialog;
  
define method test-choose-multiple-values (frame :: <simple-frame>) => ()
  let (#rest values) = choose-multiple-values(frame-manager: frame-manager(frame));
  show-result(frame, values)
end method test-choose-multiple-values;
  
define method test-choose-from-wizard (frame :: <simple-frame>) => ()
  let (#rest values) = choose-from-wizard(frame-manager: frame-manager(frame));
  show-result(frame, values)
end method test-choose-from-wizard;
  
define frame <simple-dialogs-frame> (<simple-frame>)
  pane examples (frame)
    make(<list-control>,
         items: vector(vector("Yes or No?", test-yes-or-no?),
                       vector("Choose String", test-choose-string),
                       vector("Choose from Dialog", test-new-choose-from-dialog),
                       vector("Choose Multiple Values", test-choose-multiple-values),
                       vector("Choose from Wizard", test-choose-from-wizard)),
         label-key: first,
         value-key: second,
	 scroll-bars: #"none",
         activate-callback: method (sheet :: <sheet>)
                              gadget-value(sheet)(sheet-frame(sheet))
                            end);
  pane main-layout (frame)
    frame.examples;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>);
end frame <simple-dialogs-frame>;

install-example(<simple-dialogs-frame>, "Simple Dialog Examples");
