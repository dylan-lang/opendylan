Module:       duim-gui-test-suite
Author:       Andy Armstrong, Shri Amit
Synopsis:     An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Gadget tests

define constant $simple-gadgets-to-test
  = list(list(<label>,            "label"),
	 list(<push-button>,      "push button"),
	 list(<radio-button>,     "radio button"),
	 list(<check-button>,     "check button"),
	 list(<push-box>,         "push box"),
	 list(<radio-box>,        "radio box"),
	 list(<check-box>,        "check box"),
	 list(<text-field>,       "text field"),
	 list(<password-field>,   "password field"),
	 list(<text-editor>,      "text editor"),
	 list(<scroll-bar>,       "scroll bar"),
	 list(<list-box>,         "list box"),
	 list(<option-box>,       "option box"),
	 list(<combo-box>,        "combo box"));

define constant $new-gadgets-to-test
  = list(list(<list-control>,     "list control"),
	 list(<list-control>,     "list control",
	      label-key:, method (x) format-to-string("This is the long label for %s", x) end),
	 list(<progress-bar>,     "progress bar"),
	 list(<slider>,           "slider"),
	 list(<spin-box>,         "spin box"),
	 list(<tab-control>,      "tab control"),
	 list(<table-control>,    "table control"),
	 list(<tree-control>,     "tree control"));

define constant $vertical-gadgets-to-test
  = list(list(<push-box>,         "push box"),
	 list(<radio-box>,        "radio box"),
	 list(<check-box>,        "check box"),
	 list(<scroll-bar>,       "scroll bar"),
	 list(<slider>,           "slider"));

define constant $multiple-selection-gadgets-to-test
  = list(list(<check-box>,        "check box"),
         list(<list-box>,         "list box"),
         list(<list-control>,     "list control"),
         list(<table-control>,    "table control"));

define constant $test-gadget-items         = #("One", "Two", "Three");
define constant $test-gadget-default-value = $test-gadget-items[1];


/// Gadget Test Frame

define variable *gadget-test-foreground* = #f;
define variable *gadget-test-background* = #f;
define variable *gadget-test-text-style* = #f;

define frame <gadget-test-frame> (<simple-frame>)
  constant slot %enabled? = #t,
    init-keyword: enabled?:;
  constant slot %value = $test-gadget-items[1],
    init-keyword: value:;
  constant slot gadget-foreground = *gadget-test-foreground*,
    init-keyword: gadget-foreground:;
  constant slot gadget-background = *gadget-test-background*,
    init-keyword: gadget-background:;
  constant slot gadget-text-style = *gadget-test-text-style*,
    init-keyword: gadget-text-style:;
  pane enabling-button (frame)
    make(<check-button>,
         label: "Enabled?",
	 documentation: "Switch this button to enable or disable the gadgets",
         value: gadgets-enabled?(frame),
         value-changed-callback: update-gadget-enabling);
  pane value-button (frame)
    make(<radio-box>,
         items: $test-gadget-items,
	 documentation: "Switch this button to change all the gadgets values",
         value: gadgets-value(frame),
         value-changed-callback: update-gadgets-value);
  pane gadget-layout (frame)
    make-gadget-table(frame);
  pane main-layout (frame)
    vertically (spacing: 5)
      horizontally (x-alignment: #"center", spacing: 10)
        frame.enabling-button;
        horizontally (x-alignment: #"center")
          make(<label>, label: "Value:");
          frame.value-button;
        end
      end;
      make(<separator>);
      frame.gadget-layout
    end;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>);
end frame <gadget-test-frame>;

define method gadget-test-value-changed-callback
    (gadget :: <gadget>) => ()
  frame-status-message(sheet-frame(gadget))
    := format-to-string("Value changed to %=", gadget-value(gadget))
end method gadget-test-value-changed-callback;

define method gadget-test-value-changing-callback
    (gadget :: <gadget>) => ()
  frame-status-message(sheet-frame(gadget))
    := format-to-string("Value changing to %=", gadget-value(gadget))
end method gadget-test-value-changing-callback;

define method gadget-test-activate-callback
    (gadget :: <gadget>) => ()
  frame-status-message(sheet-frame(gadget))
    := format-to-string("Activated %=", gadget-value(gadget))
end method gadget-test-activate-callback;

define method compute-test-gadget-value
    (gadget :: <gadget>, value) => (value)
  value
end method compute-test-gadget-value;

define method compute-test-gadget-value
    (gadget :: <collection-gadget>, value) => (value)
  select (gadget-selection-mode(gadget))
    #"none"     => #f;
    #"single"   => value;
    #"multiple" => vector(value);
  end
end method compute-test-gadget-value;

define method compute-test-gadget-value
    (gadget :: <value-range-gadget>, value) => (value)
  let position = position($test-gadget-items, value, test: \=);
  if (position)
    position * 50;
  end
end method compute-test-gadget-value;

define generic make-test-gadget
    (label, class :: <class>, #rest keys, #key, #all-keys)
 => (gadget :: <abstract-gadget>);

define method make-test-gadget
    (label :: <string>, class :: <class>, #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        label: label,
        args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<text-field>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        text: label,
        value-changed-callback:  gadget-test-value-changed-callback,
        value-changing-callback: gadget-test-value-changing-callback,
        activate-callback:       gadget-test-activate-callback,
        args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<collection-gadget>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        items: $test-gadget-items,
        value-changed-callback: gadget-test-value-changed-callback,
        activate-callback:      gadget-test-activate-callback,
        args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<combo-box>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        text: label,
        items: $test-gadget-items,
        value-changed-callback:  gadget-test-value-changed-callback,
        value-changing-callback: gadget-test-value-changing-callback,
        activate-callback:       gadget-test-activate-callback,
        args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<list-control>), #rest args, #key label-key)
 => (gadget :: <gadget>)
  apply(make, class,
	items: $test-gadget-items,
        scroll-bars: #"none",
        value-changed-callback: gadget-test-value-changed-callback,
        activate-callback:      gadget-test-activate-callback,
        //--- This gets in the way of testing the other callbacks
        // key-press-callback:     gadget-test-key-press-callback,
	label-key: label-key | collection-gadget-default-label-key,
	args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<table-control>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
	items: range(from: 1, to: 5),
        headings: #("Squared", "Doubled", "Identity"),
	generators: vector(method (x) x * x end,
			   method (x) x + x end,
			   identity),
        scroll-bars: #"none",
        value-changed-callback: gadget-test-value-changed-callback,
        activate-callback:      gadget-test-activate-callback,
        //--- This gets in the way of testing the other callbacks
        // key-press-callback:     gadget-test-key-press-callback,
        popup-menu-callback:    select-table-control-view,
	args)
end method make-test-gadget;

define method select-table-control-view
    (table :: <table-control>, target, #key x, y) => ()
  local method change-view (box)
	  let view = gadget-value(box);
	  table-control-view(table) := view
	end method;
  let menu
    = if (target)
	make(<menu>,
	     owner: top-level-sheet(table),
	     children: vector(make(<push-menu-button>,
				   label: format-to-string("You clicked on %d", target))))
      else
	make(<menu>,
	     owner: top-level-sheet(table),
	     children: vector(make(<radio-menu-box>,
				   items: #(#"table", #"list", #"small-icon", #"large-icon"),
				   value: table-control-view(table),
				   label-key: method (x)
						select (x)
						  #"table"      => "Details";
						  #"list"       => "List";
						  #"small-icon" => "Small Icon";
						  #"large-icon" => "Large Icon";
						end
					      end method,
				   documentation: "Changes the view",
				   value-changed-callback: change-view)))
      end;
  display-menu(menu)
end method select-table-control-view;

define method make-test-gadget
    (label :: <string>, class :: subclass(<tree-control>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        roots: #(1),
        children-generator:     method (x) list(x * 2, 1 + x * 2) end,
        value-changed-callback: gadget-test-value-changed-callback,
        activate-callback:      gadget-test-activate-callback,
        //--- This gets in the way of seeing the other callbacks
        // key-press-callback:     gadget-test-key-press-callback,
        popup-menu-callback:    tree-control-popup-menu,
        args)
end method make-test-gadget;

define method tree-control-popup-menu
    (tree :: <tree-control>, target, #key x, y) => ()
  let menu
    = if (target)
	make(<menu>,
	     owner: top-level-sheet(tree),
	     children: vector(make(<push-menu-button>,
				   label: format-to-string("You clicked on %d", target))))
      else
	make(<menu>,
	     owner: top-level-sheet(tree),
	     children: vector(make(<push-menu-button>,
				   label: "You clicked on background")))
      end;
  display-menu(menu)
end method tree-control-popup-menu;

define method make-test-gadget
    (label :: <string>, class :: subclass(<value-range-gadget>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        value-range: range(from: 0, to: 100),
        value-changed-callback:  gadget-test-value-changed-callback,
        value-changing-callback: gadget-test-value-changing-callback,
        activate-callback:       gadget-test-activate-callback,
        args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<scroll-bar>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        value-range: range(from: 0, to: 100),
        slug-size: 20,
        value-changed-callback:  gadget-test-value-changed-callback,
        value-changing-callback: gadget-test-value-changing-callback,
        activate-callback:       gadget-test-activate-callback,
        args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<slider>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        value-range: range(from: 0, to: 100),
	tick-marks: 10,
        value-changed-callback:  gadget-test-value-changed-callback,
        value-changing-callback: gadget-test-value-changing-callback,
        activate-callback:       gadget-test-activate-callback,
        args)
end method make-test-gadget;

define method make-test-gadget
    (label :: <string>, class :: subclass(<tab-control>), #rest args, #key)
 => (gadget :: <gadget>)
  apply(make, class,
        labels: $test-gadget-items,
        pages: vector(make(<label>, label: "One"),
                      make(<button>, label: "Two"),
                      make(<radio-button>, label: "Three")),
        value-changed-callback: gadget-test-value-changed-callback,
        args)
end method make-test-gadget;

define method make-test-gadgets
    (frame :: <gadget-test-frame>, 
     #key gadget-classes = gadget-classes-to-test(frame),
          enabled? = #t)
 => (gadgets :: <sequence>)
  let no-of-children = size(gadget-classes) * 2;
  let children = make(<vector>, size: no-of-children);
  let foreground = gadget-foreground(frame);
  let background = gadget-background(frame);
  let text-style = gadget-text-style(frame);
  let orientation = frame-gadgets-orientation(frame);
  let selection-mode = frame-gadgets-selection-mode(frame);
  let value = $test-gadget-default-value;
  with-frame-manager (frame-manager(frame))
    for (gadget-class in gadget-classes,
         index from 0 by 2)
      let class = gadget-class[0];
      let label = gadget-class[1];
      let args  = rest(rest(gadget-class));
      children[index] := make(<label>, label: concatenate(label, ":"));
      let documentation = format-to-string("A test %s", label);
      let gadget
        = apply(make-test-gadget, label, class,
		documentation:  documentation,
		enabled?:       enabled?,
		foreground:     foreground,
		background:     background,
		text-style:     text-style,
                orientation:    orientation,
                selection-mode: selection-mode,
		args);
      update-gadget-value(gadget, value);
      children[index + 1] := gadget
    end
  end;
  if (orientation == #"vertical")
    let new-children = make(<vector>, size: no-of-children);
    for (i from 0 below no-of-children by 2,
         j from 0)
      new-children[j] := children[i]
    end;
    for (i from 1 below no-of-children by 2,
         j from floor/(no-of-children, 2))
      new-children[j] := children[i]
    end;
    new-children
  else
    children
  end
end method make-test-gadgets;

define method make-gadget-table 
    (frame :: <gadget-test-frame>) => (table :: <table-layout>)
  let orientation = frame-gadgets-orientation(frame);
  with-frame-manager (frame-manager(frame))
    let children = make-test-gadgets(frame, enabled?: gadgets-enabled?(frame));
    select (orientation)
      #"horizontal" =>
	make(<table-layout>, 
	     columns: 2,
	     x-spacing: 2, y-spacing: 5,
	     x-alignment: #(#"right", #"left"), y-alignment: #"center",
	     children: children);
      #"vertical" =>
	make(<table-layout>, 
	     rows: 2,
	     x-spacing: 10, y-spacing: 2,
	     x-alignment: #"center",
	     children: children);
    end
  end
end method make-gadget-table;

define method gadgets-enabled? 
    (frame :: <gadget-test-frame>) => (enabled? :: <boolean>)
  frame.%enabled?
end method gadgets-enabled?;

define method update-gadget-enabling 
    (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let enabled? = gadget-value(gadget);
  for (gadget in sheet-children(gadget-layout(frame)))
    gadget-enabled?(gadget) := enabled?
  end
end method update-gadget-enabling;

define method gadgets-value
    (frame :: <gadget-test-frame>) => (value :: <string>)
  frame.%value
end method gadgets-value;

define method update-gadget-value
    (gadget :: <gadget>, value) => ()
  //--- We can't update the value of arbitrary gadgets
  #f
end method update-gadget-value;

define method update-gadget-value
    (gadget :: <value-gadget>, value) => ()
  gadget-value(gadget) := compute-test-gadget-value(gadget, value)
end method update-gadget-value;

define method update-gadgets-value
    (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let value = gadget-value(gadget);
  for (gadget in sheet-children(gadget-layout(frame)))
    update-gadget-value(gadget, value)
  end
end method update-gadgets-value;

define method frame-gadgets-orientation
    (frame :: <gadget-test-frame>) => (orientation)
  #"horizontal"
end method frame-gadgets-orientation;

define method frame-gadgets-selection-mode
    (frame :: <gadget-test-frame>) => (selection-mode)
  #"single"
end method frame-gadgets-selection-mode;


/// Simple gadgets test

define class <simple-gadget-test-frame> (<gadget-test-frame>)
end class <simple-gadget-test-frame>;

define method gadget-classes-to-test
    (frame :: <simple-gadget-test-frame>) => (gadgets :: <sequence>)
  $simple-gadgets-to-test
end method gadget-classes-to-test;


/// New gadgets test

define class <new-gadget-test-frame> (<gadget-test-frame>)
end class <new-gadget-test-frame>;

define method gadget-classes-to-test
    (frame :: <new-gadget-test-frame>) => (gadgets :: <sequence>)
  $new-gadgets-to-test
end method gadget-classes-to-test;


/// Vertical gadgets test

define class <vertical-gadget-test-frame> (<gadget-test-frame>)
end class <vertical-gadget-test-frame>;

define method gadget-classes-to-test
    (frame :: <vertical-gadget-test-frame>) => (gadgets :: <sequence>)
  $vertical-gadgets-to-test
end method gadget-classes-to-test;

define method frame-gadgets-orientation
    (frame :: <vertical-gadget-test-frame>) => (orientation)
  #"vertical"
end method frame-gadgets-orientation;


/// Multiple selection gadgets test

define class <multiple-selection-gadget-test-frame> (<gadget-test-frame>)
end class <multiple-selection-gadget-test-frame>;

define method gadget-classes-to-test
    (frame :: <multiple-selection-gadget-test-frame>) => (gadgets :: <sequence>)
  $multiple-selection-gadgets-to-test
end method gadget-classes-to-test;

define method frame-gadgets-selection-mode
    (frame :: <multiple-selection-gadget-test-frame>) => (selection-mode)
  #"multiple"
end method frame-gadgets-selection-mode;


/// Advanced gadgets test

define class <advanced-gadget-test-frame> (<gadget-test-frame>)
end class <advanced-gadget-test-frame>;

define method make-test-gadgets
    (frame :: <advanced-gadget-test-frame>,
     #key gadget-classes, 
          enabled? = #t)
 => (gadgets :: <sequence>)
  ignore(gadget-classes);
  with-frame-manager (frame-manager(frame))
    let push-radio-box
      = make-test-gadget("push-style radio box", <radio-box>,
			 button-style:, #"push-button");
    let push-check-box
      = make-test-gadget("push-style check box", <check-box>,
			 button-style:, #"push-button");
    let item1 :: <string> = $test-gadget-items[0];
    let item2 :: <string> = $test-gadget-items[1];
    let item3 :: <string> = $test-gadget-items[2];
    let radio-box
      = make(<radio-box>,
             enabled?: enabled?,
             child: make(<column-layout>,
                         children:
			   vector(grouping ("First group", max-width: $fill)
				    make(<radio-button>, label: item1, id: item1)
				  end,
				  grouping ("Second group", max-width: $fill)
                                    vertically (spacing: 4)
			              make(<radio-button>, label: item2, id: item2);
			              make(<radio-button>, label: item3, id: item3)
                                    end
			          end)));
    let tab-control
      = make(<tab-control>,
             tabs-position: #"bottom",
             enabled?: enabled?,
             pages: vector(make(<tab-control-page>,
                                label: item1,
                                child: make(<button>, label: "One")),
                           make(<tab-control-page>, 
                                label: item2,
                                child: make(<list-box>, items: $test-gadget-items)),
                           make(<tab-control-page>, 
                                label: item3,
                                child: make(<ellipse-pane>))));

    vector(make(<label>, label: "push-style radio box"), push-radio-box,
           make(<label>, label: "push-style check box"), push-check-box,
           make(<label>, label: "complex radio box"), radio-box,
           make(<label>, label: "workspace"), tab-control)
  end
end method make-test-gadgets;


/// Gadgets frame

define variable $gadget-test-frame-tests
  = vector(vector("Simple gadgets",             <simple-gadget-test-frame>),
	   vector("Vertical gadgets",           <vertical-gadget-test-frame>),
	   vector("Multiple selection gadgets", <multiple-selection-gadget-test-frame>),
	   vector("New gadgets",                <new-gadget-test-frame>),
	   vector("Advanced gadgets",           <advanced-gadget-test-frame>));

define frame <gadgets-test-frame> (<simple-frame>)
  pane examples (frame)
    make(<list-control>,
         scroll-bars: #"none",
	 documentation: "Double-click on a test name to run it",
	 items: $gadget-test-frame-tests,
	 lines: size($gadget-test-frame-tests),
	 label-key: first,
	 activate-callback: method (gadget :: <gadget>)
                              let frame = sheet-frame(gadget);
                              let value = gadget-value(gadget);
                              let title = first(value);
                              let class = second(value);
                              let test-frame = make(class, title: title, owner: frame);
                              start-frame(test-frame)
			    end);
  pane main-layout (frame)
    frame.examples;
  layout (frame) frame.main-layout;
end frame <gadgets-test-frame>;

install-test(<gadgets-test-frame>, "Gadgets");

