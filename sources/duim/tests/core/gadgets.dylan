Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Gadget class tests

define sideways method make-test-instance
    (class :: subclass(<abstract-gadget>)) => (instance :: <gadget>)
  make-test-pane(class)
end method make-test-instance;

define sideways method make-test-instance
    (class == <table-control>) => (instance :: <table-control>)
  make-test-pane(<table-control>,
		 headings: #["Heading"],
		 generators: vector(identity))
end method make-test-instance;

define sideways method make-test-instance
    (class == <tree-control>) => (instance :: <tree-control>)
  make-test-pane(<tree-control>,
		 children-generator: always(#[]))
end method make-test-instance;

define duim-gadgets class-test <gadget> ()
  //---*** Fill this in...
end class-test <gadget>;

define duim-gadgets class-test <action-gadget> ()
  //---*** Fill this in...
end class-test <action-gadget>;

define duim-gadgets class-test <value-gadget> ()
  //---*** Fill this in...
end class-test <value-gadget>;

define duim-gadgets class-test <value-range-gadget> ()
  //---*** Fill this in...
end class-test <value-range-gadget>;

define duim-gadgets class-test <collection-gadget> ()
  //---*** Fill this in...
end class-test <collection-gadget>;

define duim-gadgets class-test <text-gadget> ()
  //---*** Fill this in...
end class-test <text-gadget>;

define duim-gadgets class-test <label> ()
  test-gadget-label(<label>);
end class-test <label>;

define duim-gadgets class-test <password-field> ()
  test-text-gadget-text(<password-field>);
  test-text-field-values(<password-field>);
end class-test <password-field>;

define duim-gadgets class-test <slider> ()
  test-range-pane-values(<slider>);
end class-test <slider>;

define duim-gadgets class-test <text-editor> ()
  test-text-gadget-text(<text-editor>);
  test-text-field-values(<text-editor>);
end class-test <text-editor>;

define duim-gadgets class-test <text-field> ()
  test-text-gadget-text(<text-field>);
  test-text-field-values(<text-field>);
end class-test <text-field>;

define duim-gadgets class-test <button-box> ()
  test-collection-gadget-value(<button-box>);
end class-test <button-box>;

define duim-gadgets class-test <button> ()
  //---*** Fill this in...
end class-test <button>;

define duim-gadgets class-test <check-box> ()
  test-gadget-box-pane-buttons(<check-box>);
  let cbp = make-collection-gadget(<check-box>, selection: #(0, 2));
  verify-gadget-box-pane-button-selection(cbp);
  let button1 = make-test-pane(<check-button>, id: #"one");
  let button2 = make-test-pane(<check-button>, id: #"two");
  let button3 = make-test-pane(<check-button>, id: #"three");
  let sub-layout
    = make-test-pane(<row-layout>, children: vector(button2, button3));
  let layout
    = make-test-pane(<column-layout>, children: vector(button1, sub-layout));
  let cbp = make-test-pane(<check-box>, child: layout);
  check-equal("arbitrary layout <check-box> items",
	      gadget-items(cbp),
	      #[#"one", #"two", #"three"]);
  check-equal("arbitrary layout <check-box> initial value",
              gadget-value(cbp),
	      #[]);
  check-equal("arbitrary layout <check-box> set value",
	      begin
		gadget-value(cbp) := #[#"two", #"three"];
		gadget-value(cbp)
	      end,
	      #[#"two", #"three"]);
end class-test <check-box>;

define duim-gadgets class-test <check-button> ()
  test-gadget-label(<check-button>);
  test-button-values(<check-button>);
end class-test <check-button>;

define duim-gadgets class-test <check-menu-button> ()
  test-gadget-label(<check-menu-button>);
  test-button-values(<check-menu-button>);
end class-test <check-menu-button>;

define duim-gadgets class-test <check-menu-box> ()
  test-gadget-box-pane-buttons(<check-menu-box>);
  let cbp = make-collection-gadget(<check-menu-box>, selection: #(0, 2));
  verify-gadget-box-pane-button-selection(cbp);
end class-test <check-menu-box>;

define duim-gadgets class-test <list-box> ()
  test-collection-gadget-value(<list-box>);
end class-test <list-box>;

define duim-gadgets class-test <menu-bar> ()
  //---*** Fill this in...
end class-test <menu-bar>;

define duim-gadgets class-test <menu-button> ()
  //---*** Fill this in...
end class-test <menu-button>;

define duim-gadgets class-test <menu-box> ()
  test-collection-gadget-value(<menu-box>);
end class-test <menu-box>;

define duim-gadgets class-test <menu> ()
  test-gadget-label(<menu>);
  test-no-value-gadget-values(<menu>);
end class-test <menu>;

define duim-gadgets class-test <option-box> ()
  test-single-selection-collection-gadget-values(<option-box>);
end class-test <option-box>;

define duim-gadgets class-test <combo-box> ()
  test-text-gadget-text(<combo-box>);
  test-text-field-values(<combo-box>);
end class-test <combo-box>;

define duim-gadgets class-test <push-box> ()
  test-gadget-box-pane-buttons(<push-box>);
end class-test <push-box>;

define duim-gadgets class-test <push-button> ()
  test-gadget-label(<push-button>);
  test-button-values(<push-button>);
end class-test <push-button>;

define duim-gadgets class-test <push-menu-button> ()
  test-gadget-label(<push-menu-button>);
  test-button-values(<push-menu-button>);
end class-test <push-menu-button>;

define duim-gadgets class-test <push-menu-box> ()
  test-gadget-box-pane-buttons(<push-menu-box>);
end class-test <push-menu-box>;

define duim-gadgets class-test <radio-box> ()
  let rbp = make-collection-gadget(<radio-box>);
  verify-gadget-box-pane-button-selection(rbp);
  test-gadget-box-pane-buttons(<radio-box>);
  let button1 = make-test-pane(<radio-button>, id: #"one");
  let button2 = make-test-pane(<radio-button>, id: #"two");
  let button3 = make-test-pane(<radio-button>, id: #"three");
  let sub-layout
    = make-test-pane(<row-layout>, children: vector(button2, button3));
  let layout
    = make-test-pane(<column-layout>, children: vector(button1, sub-layout));
  let rbp = make-test-pane(<radio-box>, child: layout);
  check-equal("arbitrary layout <radio-box> items",
	      gadget-items(rbp),
	      #[#"one", #"two", #"three"]);
  check-equal("arbitrary layout <radio-box> initial value",
              gadget-value(rbp),
	      #"one");
  check-equal("arbitrary layout <radio-box> set value",
	      begin
		gadget-value(rbp) := #"two";
		gadget-value(rbp)
	      end,
	      #"two");
end class-test <radio-box>;

define duim-gadgets class-test <radio-button> ()
  test-gadget-label(<radio-button>);
  test-button-values(<radio-button>);
end class-test <radio-button>;

define duim-gadgets class-test <radio-menu-button> ()
  test-gadget-label(<radio-menu-button>);
  test-button-values(<radio-menu-button>);
end class-test <radio-menu-button>;

define duim-gadgets class-test <radio-menu-box> ()
  let rbp = make-collection-gadget(<radio-menu-box>);
  verify-gadget-box-pane-button-selection(rbp);
  test-gadget-box-pane-buttons(<radio-menu-box>);
end class-test <radio-menu-box>;

define duim-gadgets class-test <spin-box> ()
  test-single-selection-collection-gadget-values(<spin-box>);
end class-test <spin-box>;

define duim-gadgets class-test <status-bar> ()
  test-gadget-label(<status-bar>);
  test-range-pane-values(<status-bar>, 
                         make-function: method (class, #rest args)
                                          apply(make-test-pane, class,
                                                progress-bar?: #t,
                                                args)
                                        end,
                         default: #f);
end class-test <status-bar>;

define duim-gadgets class-test <tool-bar> ()
  test-no-value-gadget-values(<tool-bar>);
end class-test <tool-bar>;

define duim-gadgets class-test <scroll-bar> ()
  test-slug-gadget-values(<scroll-bar>);
end class-test <scroll-bar>;

define duim-gadgets class-test <border> ()
  test-border-pane-layout(<border>);
end class-test <border>;

define duim-gadgets class-test <group-box> ()
  //---*** We don't really want to test its layout as such, since we
  //---*** don't want to assume a particular look. More, we want to
  //---*** make sure that the child is okay.
  // test-border-pane-layout(<group-box>);
end class-test <group-box>;

define duim-gadgets class-test <separator> ()
  test-no-value-gadget-values(<separator>);
end class-test <separator>;

define duim-gadgets class-test <spacing> ()
  test-border-pane-layout(<spacing>);
end class-test <spacing>;

define duim-gadgets class-test <splitter> ()
  //---*** Fill this in...
end class-test <splitter>;

define duim-gadgets class-test <tab-control> ()
  let button-1 = make-test-pane(<button>, label: "One");
  let button-2 = make-test-pane(<button>, label: "Two");
  let tab-control
    = make-test-pane(<tab-control>, pages: vector(button-1, button-2));
  let frame = make-test-frame(<test-frame>, layout: tab-control);
  ignore(frame);
  check-equal("Tab control default visible child",
	      tab-control-current-page(tab-control),
	      button-1);
  check-equal("Tab control default value",
	      gadget-value(tab-control), "One");
  tab-control-current-page(tab-control) := button-2;
  check-equal("Tab control new visible child",
	      tab-control-current-page(tab-control),
	      button-2);
  gadget-value(tab-control) := gadget-label(button-1);
  check-equal("Tab control new value",
	      gadget-value(tab-control), gadget-label(button-1));
  let button-3 = make-test-pane(<button>, label: "Three");
  tab-control-pages(tab-control) := vector(button-1, button-2, button-3);
  check-equal("Tab control keeps visible child after contents change",
	      tab-control-current-page(tab-control),
	      button-1);
  tab-control-current-page(tab-control) := button-3;
  check-equal("Tab control additional visible child",
	      tab-control-current-page(tab-control),
	      button-3);
  tab-control-pages(tab-control) := #[];
  check-equal("Empty tab control",
	      tab-control-pages(tab-control), #[]);
  check-equal("Empty tab control has no visible child",
	      tab-control-current-page(tab-control), #f);
end class-test <tab-control>;

define duim-gadget class-test <page> ()
  //---*** Fill this in...
end class-test <page>;

define duim-gadget class-test <tab-control-page> ()
  //---*** Fill this in...
end class-test <tab-control-page>;

define duim-gadgets class-test <list-control> ()
  test-collection-gadget-value(<list-control>);
end class-test <list-control>;

define duim-gadgets class-test <list-item> ()
  //---*** Fill this in...
end class-test <list-item>;

define duim-gadgets class-test <tree-control> ()
  test-tree-control-values();
end class-test <tree-control>;

define duim-gadgets class-test <tree-node> ()
  //---*** Fill this in...
end class-test <tree-node>;

define duim-gadgets class-test <table-control> ()
  test-collection-gadget-value(<table-control>);
  test-table-control-values();
end class-test <table-control>;

define duim-gadgets class-test <table-item> ()
  //---*** Fill this in...
end class-test <table-item>;

define duim-gadgets class-test <progress-bar> ()
  test-range-pane-values(<progress-bar>, default: #f);
end class-test <progress-bar>;



/// Gadget testing

define sideways method class-test-function
    (class :: subclass(<abstract-gadget>)) => (function :: <function>)
  test-gadget-class
end method class-test-function;

define open generic test-gadget-class
    (class :: subclass(<abstract-gadget>), #key, #all-keys) => ();

define method test-gadget-class
    (class :: subclass(<abstract-gadget>), #key name, instantiable?, #all-keys) => ()
  if (instantiable?)
  end
end method test-gadget-class;


/// parent tests

define method subchild? 
    (sheet :: <object>, child :: <sheet>) => (subchild? :: <boolean>)
  let parent = sheet-parent(child);
  if (parent)
    if (parent == sheet)
      #t
    else
      subchild?(sheet, parent)
    end
  end
end method subchild?;

define test parents-test ()
  let button = make-test-pane(<push-button>);
  let layout = make-test-pane(<column-layout>, children: vector(button));
  check-equal("Initial gadget parent", layout, button.sheet-parent);
  let frame = make-test-frame(<test-frame>, layout: layout);
  check-true("Parent after initializing frame layout",
	      subchild?(top-level-sheet(frame), layout));
  remove-child(layout, button);
  let check-button = make-test-pane(<check-button>);
  sheet-children(layout) := vector(button, check-button);
  check-equal("Parent after setting sheet children 1",
	      layout, button.sheet-parent);
  check-equal("Parent after setting sheet children 2",
	      layout, check-button.sheet-parent);
end test parents-test;


/// Gadget tests

define method verify-gadget-box-pane-button-selection 
    (box :: <gadget>) => ()
  let buttons = box.gadget-box-buttons;
  let items = box.gadget-items;
  let selection = box.gadget-selection;
  let failed = #f;
  if (buttons & items)
    for (index from 0 to size(items),
         button in buttons)
      if (button.gadget-value ~= member?(index, selection))
        failed := #t;
      end
    end;
  else
    failed := items
  end;
  check-equal(concatenate(gadget-class-name(box), " button selection"),
              failed, #f)
end method verify-gadget-box-pane-button-selection;

define constant $default-collection-items
  = #(#("Red", #"red"), 
      #("Green", #"green"),
      #("Blue", #"blue"));

define method make-collection-gadget
    (class :: subclass(<collection-gadget>), #rest args,
     #key items = $default-collection-items,
          label-key = first,
          value-key = second,
     #all-keys)
 => (gadget :: <collection-gadget>)
  apply(make-test-pane, class, 
        items: items,
        label-key: label-key,
        value-key: value-key,
        args)
end method make-collection-gadget;

define method make-collection-gadget
    (class :: subclass(<table-control>), #rest args,
     #key items = $default-collection-items,
          label-key = first,
          value-key = second,
     #all-keys)
 => (gadget :: <collection-gadget>)
  apply(make-test-pane, class, 
        items: items,
        label-key: label-key,
        value-key: value-key,
        headings: #("One", "Two"),
        generators: vector(identity, identity),
        args)
end method make-collection-gadget;

define method verify-gadget-box-pane-buttons 
    (test :: <string>, box :: <gadget>) => ()
  let buttons = box.gadget-box-buttons;
  let items = box.gadget-items;
  check-equal(test, buttons & size(buttons), items & size(items));
end method verify-gadget-box-pane-buttons;

define method test-gadget-box-pane-buttons 
    (class :: subclass(<gadget>)) => ()
  let box-pane = make-collection-gadget(class);
  let name = gadget-class-name(class);
  verify-gadget-box-pane-buttons(concatenate(name, " pane buttons"),
                                 box-pane);
  gadget-items(box-pane)
    := #(#("Four", 4), 
         #("Five", 5),
         #("Six", 6),
         #("Seven", 7));
  verify-gadget-box-pane-buttons(concatenate(name, " pane change buttons"),
                                 box-pane);
end method test-gadget-box-pane-buttons;

define method check-value 
    (name :: <string>, gadget :: <value-gadget>, expected-value) => ()
  check-equal(concatenate(name, " value"),
              gadget-value(gadget), expected-value)
end method check-value;

define method test-collection-gadget-value 
    (gadget-class :: subclass(<collection-gadget>)) => ()
  let class-name = gadget-class-name(gadget-class);
  let pane = make-collection-gadget(gadget-class, selection-mode: #"none");
  let name = concatenate("single selection ", class-name);
  check-value(name, pane, #f);
  test-single-selection-collection-gadget-values(gadget-class, name: name);
  
  let pane = make-collection-gadget(gadget-class, selection-mode: #"multiple");
  let name = concatenate("multiple selection ", class-name);
  check-value(concatenate(name, " default"), pane, #());
  let pane
    = make-collection-gadget(gadget-class,
			     selection-mode: #"multiple",
			     value: #(#"red", #"green"));
  check-value(concatenate(name, " initial"), pane, #(#"red", #"green"));
  gadget-value(pane) := #(#"red", #"blue");
  check-value(concatenate(name, " new"), pane, #(#"red", #"blue"));
  gadget-selection(pane) := #();
  check-value(concatenate(name, " after selection cleared"), pane, #());
  gadget-selection(pane) := #(0, 1);
  check-value(concatenate(name, " after selection changed"),
	      pane, #(#"red", #"green"));
  gadget-items(pane) := reverse($default-collection-items);
  check-value(concatenate(name, " after items reordered"),
	      pane, #(#"red", #"green"));
  gadget-items(pane) := #(#("One", 1), #("Two", 2), #("Three", 3));
  check-value(concatenate(name, " after items changed"), pane, #())
end method test-collection-gadget-value;

define method test-single-selection-collection-gadget-values
    (gadget-class :: <class>,
     #key name = gadget-class-name(gadget-class),
          value = #"green") => ()
  let pane = make-collection-gadget(gadget-class, selection-mode: #"single");
  check-value(concatenate(name, " default"), pane, #"red");
  let pane 
    = make-collection-gadget(gadget-class,
                             selection-mode: #"single",
                             value: value);
  check-value(concatenate(name, " initial"), pane, #"green");
  gadget-value(pane) := #"blue";
  check-value(concatenate(name, " new"), pane, #"blue");
  gadget-items(pane) := reverse($default-collection-items);
  check-value(concatenate(name, " after items reordered"), pane, #"blue");
  gadget-items(pane) := #(#("One", 1), #("Two", 2), #("Three", 3));
  check-value(concatenate(name, " after items changed"), pane, #f);
end method test-single-selection-collection-gadget-values;

define method test-range-pane-values
    (class :: <class>, #key default = 10, make-function = make-test-pane) => ()
  let name = gadget-class-name(class);
  let gadget = make-function(class, value-range: range(from: 10, to: 100));
  check-value(concatenate(name, " default"), gadget, default);
  let gadget 
    = make-function(class, value-range: range(from: 10, to: 100), value: 20);
  check-value(concatenate(name, " initial"), gadget, 20);
  gadget-value(gadget) := 120;
  check-value(concatenate(name, " maximized"), gadget, 100);
  gadget-value(gadget) := 0;
  check-value(concatenate(name, " minimized"), gadget, 10);
  let new-range = range(from: 200, to: 300);
  gadget-value-range(gadget) := new-range;
  check-equal(concatenate(name, " new range"),
	      gadget-value-range(gadget), new-range);
  check-value(concatenate(name, " after range change"),
	      gadget, 200);
end method test-range-pane-values;

define method test-slug-gadget-values
    (class :: <class>, 
     #key default = 10, 
          slug-size = 20,
          make-function = make-test-pane) => ()
  let name = gadget-class-name(class);
  let gadget
    = make-function(class, slug-size: slug-size, value-range: range(from: 10, to: 100));
  check-value(concatenate(name, " default"), gadget, default);
  let gadget 
    = make-function(class, 
                    slug-size: slug-size,
                    value-range: range(from: 10, to: 100), value: 20);
  check-value(concatenate(name, " initial"), gadget, 20);
  gadget-value(gadget) := 120;
  check-value(concatenate(name, " maximized"), gadget, 100 - slug-size + 1);
  gadget-value(gadget) := 0;
  check-value(concatenate(name, " minimized"), gadget, 10);
  let new-range = range(from: 200, to: 300);
  gadget-value-range(gadget) := new-range;
  check-equal(concatenate(name, " new range"),
	      gadget-value-range(gadget), new-range);
  check-value(concatenate(name, " after range change"),
	      gadget, 200);
end method test-slug-gadget-values;

define method test-button-values
    (class :: <class>) => ()
  let button = make-test-pane(class);
  let name = gadget-class-name(class);
  check-value(concatenate(name, " default"), button, #f);
  gadget-value(button) := #t;
  check-value(concatenate(name, " new"), button, #t);
  check-value(concatenate(name, " initial"), make-test-pane(class, value: #t), #t);
end method test-button-values;

define method test-text-field-values 
    (class :: <class>) => ()
  let name = gadget-class-name(class);
  let text-field = make-test-pane(class);
  check-value(concatenate(name, " default"), text-field, "");
  gadget-value(text-field) := "Hello";
  check-value(concatenate(name, " changed"), text-field, "Hello");
  check-value(concatenate(name, " initial"),
	      make-test-pane(class, value: "Initial"), "Initial");
  let text-field = make-test-pane(class, value: 0, value-type: <integer>);
  check-equal(concatenate(name, " integer initial value"),
              gadget-value(text-field), 0);
  check-equal(concatenate(name, " integer initial text"),
              gadget-text(text-field), "0");
  gadget-value(text-field) := 10;
  check-equal(concatenate(name, " changed value value"),
              gadget-value(text-field), 10);
  check-equal(concatenate(name, " changed value text"),
              gadget-text(text-field), "10");
  gadget-text(text-field) := "100";
  check-equal(concatenate(name, " changed text value"),
              gadget-value(text-field), 100);
  check-equal(concatenate(name, " changed text text"),
              gadget-text(text-field), "100");
end method test-text-field-values;

define method test-no-value-gadget-values 
    (class :: <class>) => ()
  let name = gadget-class-name(class);
  check-value(concatenate(name, " default"), make-test-pane(class), #f);
  check-value(concatenate(name, " initial"), make-test-pane(class, value: 10), #f);
end method test-no-value-gadget-values;

define method test-tree-control-children-generator 
    (x :: <integer>) => (children :: <vector>)
  let children
    = if (x < 8)
        vector(x * 2, (x * 2) + 1)
      else
        #[]
      end;
  children
end method test-tree-control-children-generator;

define method make-test-tree-control 
    (#rest args, #key depth = 2, #all-keys)
 => (tree-control :: <tree-control>)
  apply(make-test-pane, <tree-control>,
        roots: #(1),
        children-generator: test-tree-control-children-generator,
        depth: depth,
        args)
end method make-test-tree-control;

define method test-tree-control-values () => ()
  let name = gadget-class-name(<tree-control>);

  let pane-name = concatenate("no selection ", name);
  let pane = make-test-tree-control(selection-mode: #"none");
  check-value(pane-name, pane, #f);

  let pane-name = concatenate("multiple selection ", name);
  let pane = make-test-tree-control(selection-mode: #"multiple");
  check-value(concatenate(pane-name, " default"), pane, #());
  let pane
    = make-test-tree-control(selection-mode: #"multiple", value: #(2, 4));
  check-value(concatenate(name, " initial"), pane, #(2, 4));
  gadget-value(pane) := #(3, 5);
  check-value(concatenate(name, " new"), pane, #(3, 5));
  gadget-selection(pane) := #();
  check-value(concatenate(name, " after selection cleared"), pane, #());
  gadget-selection(pane) := #(0, 1);
  check-value(concatenate(name, " after selection changed"),
	      pane, #(1, 2));
end method test-tree-control-values;

define method make-test-table-control 
    (#rest args, #key, #all-keys)
 => (table-control :: <table-control>)
  apply(make-test-pane, <table-control>,
        headings: #("Identity", "+1", "+2"),
        generators: vector(identity, curry(\+, 1), curry(\+, 2)),
        args)
end method make-test-table-control;

define method test-table-control-values () => ()
  let name = gadget-class-name(<table-control>);
  let pane = make-test-table-control(items: range(from: 0, to: 10));
  check-value(concatenate(name, " default"), pane, 0);
  gadget-value(pane) := 4;
  check-value(concatenate(name, " new"), pane, 4);
  let pane = make-test-table-control(items: range(from: 0, to: 10), value: 5);
  check-value(concatenate(name, " initial"), pane, 5);
end method test-table-control-values;


/// Gadget labels

define method test-gadget-label (class :: <class>, #rest args) => ()
  let name = gadget-class-name(class);
  let pane = apply(make-test-pane, class, args);
  check-equal(concatenate(name, " default label"),
              gadget-label(pane), "");
  let pane
     = apply(make-test-pane, class, label: "Hello", args);
  check-equal(concatenate(name, " initial label"),
              gadget-label(pane), "Hello");
  gadget-label(pane) := "New label";
  check-equal(concatenate(name, " new label"),
              gadget-label(pane), "New label");
end method test-gadget-label;


/// Gadget text testing

define method test-text-gadget-text
    (class :: subclass(<text-gadget>)) => ()
  let name = gadget-class-name(class);
  let text-field = make-test-pane(class);
  check-equal(concatenate(name, " default text"), gadget-text(text-field), "");
  gadget-text(text-field) := "Hello";
  check-equal(concatenate(name, " changed text"), gadget-text(text-field), "Hello");
  check-equal(concatenate(name, " specified text"),
	      gadget-text(make-test-pane(class, text: "Initial")), "Initial")
end method test-text-gadget-text;


/// border-pane tests

define method expected-named-border-size
    (pane, name, #key width, height, thickness = 1, #all-keys)
 => (size :: <integer>)
  let child = sheet-child(pane);
  let double-thickness = thickness * 2;
  if (child)
    let size-function = expected-size-function(name);
    size-function(child, 
                  width:  width & width - double-thickness,
                  height: height & height - double-thickness)
      + double-thickness
  else
    expected-default-size(name, width: width, height: height)
  end
end method expected-named-border-size;

define method expected-border-space-allocation
    (pane, #key thickness = 1, width, height, #all-keys)
 => (space-allocation :: false-or(<sequence>))
  if (sheet-child(pane))
    vector(vector(thickness, thickness,
                  width - thickness * 2,
                  height - thickness * 2))
  end
end method expected-border-space-allocation;

define method expected-named-size
    (pane :: <border>, name, #rest args, #key width, height)
 => (size :: <integer>)
  ignore(width, height);
  apply(expected-named-border-size, pane, name, args)
end method expected-named-size;

define method expected-space-allocation 
    (pane :: <border>, #rest args, #key)
 => (space-allocation :: false-or(<sequence>))
  apply(expected-border-space-allocation, pane, args)
end method expected-space-allocation;

define method expected-named-size
    (pane :: <spacing>, name, #rest args, #key width, height)
 => (size :: <integer>)
  ignore(width, height);
  apply(expected-named-border-size, pane, name, args)
end method expected-named-size;

define method expected-space-allocation 
    (pane :: <spacing>, #rest args, #key)
 => (space-allocation :: false-or(<sequence>))
  apply(expected-border-space-allocation, pane, args)
end method expected-space-allocation;

define method test-border-pane
    (class :: <class>, name, child, #rest args,
     #key thickness = 1, #all-keys) => ()
  let border-pane = make-test-pane(class, child: child, thickness: thickness);
  let frame = make-test-frame(<test-frame>, layout: border-pane);
  let name = concatenate(name, " ", gadget-class-name(class));
  apply(check-layout-pane-layout,
        border-pane, name,
        thickness: thickness,
        args)
end method test-border-pane;

define method test-border-pane-layout (class :: <class>) => ()
  test-border-pane(class, "empty", #f, thickness: 0);
  test-border-pane(class, "thickness 0", make-test-pane(<test-push-button-pane>),
		   thickness: 0);
  test-border-pane(class, "thickness 10", make-test-pane(<test-push-button-pane>),
		   thickness: 10);
  test-border-pane(class, "non-fixed thickness 10",
		   make-test-pane(<test-list-box>),
		   thickness: 10);
  test-border-pane(class, "nested",
		   make-test-pane(<border-pane>,
                                  child: make-test-pane(<test-push-button-pane>)));
  test-border-pane(class, "non-fixed nested",
		   make-test-pane(<border-pane>, 
                                  child: make-test-pane(<test-list-box>)));
end method test-border-pane-layout;


/// Define gadgets test suite

define suite duim-gadgets-suite ()
  test parents-test;
end suite duim-gadgets-suite;
