Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Gadget box pane implementation class

// Implements the adding of buttons to a gadget box to represent the items
define abstract class <gadget-box-pane-mixin> (<collection-gadget>)
  sealed slot frame-manager :: false-or(<frame-manager>) = #f,
    init-keyword: frame-manager:;
  sealed slot gadget-box-button-class :: false-or(<class>) = #f,
    init-keyword: button-class:;
  sealed slot gadget-box-buttons :: <sequence> = #[];
end class <gadget-box-pane-mixin>;

define open generic button-class-for-gadget-box (box);

define method gadget-box-button-value 
    (box :: <gadget-box-pane-mixin>, item)
  let value-key = gadget-value-key(box);
  select (gadget-selection-mode(box))
    #"none"   => value-key(item);
    otherwise => #f;
  end
end method gadget-box-button-value;

define method make-button-for-gadget-box
    (box :: <gadget-box-pane-mixin>, item, button-class)
 => (button :: <sheet>)
  let selection-mode = gadget-selection-mode(box);
  let documentation = gadget-documentation(box);
  let label = gadget-item-label(box, item);
  let framem = frame-manager(box);
  with-frame-manager (framem)
    make-pane(button-class,
              selection-mode: selection-mode,
	      button-style: push-button-like?(box) & #"push-button",
              enabled?: gadget-enabled?(box),
              client: box,
              label: label,
	      documentation: documentation,
              value: gadget-box-button-value(box, item),
              foreground: default-foreground(box),
              background: default-background(box),
              text-style: default-text-style(box))
  end
end method make-button-for-gadget-box;

define function make-buttons-for-gadget-box 
    (box :: <gadget-box-pane-mixin>, items :: <sequence>)
 => (buttons :: <vector>)
  let button-class 
    = gadget-box-button-class(box)
      | button-class-for-gadget-box(box);
  // Make a stretchy vector of the buttons because these are going
  // to go into the children of some lucky sheet...
  let buttons
    = map-as(<stretchy-vector>,
	     method (item)
	       make-button-for-gadget-box(box, item, button-class)
	     end,
	     items);
  gadget-box-buttons(box) := buttons
end function make-buttons-for-gadget-box;

define method gadget-box-button-index 
    (box :: <gadget-box-pane-mixin>, button :: <button>)
 => (index :: false-or(<integer>))
  position(gadget-box-buttons(box), button)
end method gadget-box-button-index;

define method update-button-selections 
    (box :: <gadget-box-pane-mixin>, selection) => ()
  let buttons = gadget-box-buttons(box);
  when (buttons)
    for (index :: <integer> from 0 below size(buttons))
      let button = buttons[index];
      gadget-value(button) := (selection & member?(index, selection))
    end  
  end
end method update-button-selections;

//--- Could we make this more general via 'layout-default-spacing' or something?
define open generic button-box-spacing
    (framem :: <frame-manager>, box :: <button-box>)
 => (spacing :: <integer>);

define method button-box-spacing
    (framem :: <frame-manager>, box :: <button-box>)
 => (spacing :: <integer>)
  2
end method button-box-spacing;

define method initialize
    (box :: <gadget-box-pane-mixin>, #rest initargs,
     #key layout-class, spacing, rows, columns) => ()
  dynamic-extent(initargs);
  next-method();
  when (empty?(sheet-children(box)))
    let framem = frame-manager(box);
    with-frame-manager (framem)
      let items = gadget-items(box);
      let layout-class
        = layout-class
          | ((rows | columns) & <table-layout>)
          | gadget-pane-default-layout-class(box);
      let layout
        = if (layout-class)
            with-keywords-removed (pane-args = initargs,
				   #[layout-class:, border:, x:, y:])
              apply(make-pane,
                    layout-class,
                    spacing: spacing | button-box-spacing(framem, box),
                    parent: box,
                    pane-args)
            end
          else
            box
          end;
      let buttons = make-buttons-for-gadget-box(box, items);
      sheet-children(layout) := buttons;
      unless (gadget-selection-mode(box) = #"none")
	update-button-selections(box, gadget-selection(box))
      end
    end
  end
end method initialize;

define method gadget-box-buttons-parent
    (box :: <gadget-box-pane-mixin>) => (sheet :: <sheet>)
  sheet-child(box)
end method gadget-box-buttons-parent;

define method button-first-in-group?
    (gadget :: <button>) => (first? :: <boolean>)
  let box = button-gadget-box(gadget);
  select (box by instance?)
    <gadget-box-pane-mixin> =>
      gadget == first(gadget-box-buttons(box));
    otherwise =>
      //--- This is not a very good approximation, and maybe the first
      //--- select case above is enough on its own.
      let parent = sheet-parent(gadget);
      let children = sheet-children(parent);
      let index = position(children, gadget);
      index == 0
	| (index > 0
	     & object-class(children[index - 1]) ~== object-class(gadget));
  end
end method button-first-in-group?;

define method button-in-tool-bar?
    (gadget :: <button>) => (in-tool-bar? :: <boolean>)
  block (return)
    for (parent = sheet-parent(gadget) then sheet-parent(parent),
	 while: parent)
      when (instance?(parent, <tool-bar>))
	return(#t)
      end
    end;
    #f
  end
end method button-in-tool-bar?;

define method button-gadget-box
    (gadget :: <button>) => (box :: false-or(<gadget-box>))
  let client = gadget-client(gadget);
  instance?(client, <gadget-box>) & client
end method button-gadget-box;

define method note-gadget-items-changed
    (box :: <gadget-box-pane-mixin>) => ()
  next-method();
  let layout = gadget-box-buttons-parent(box);
  let buttons = make-buttons-for-gadget-box(box, gadget-items(box));
  //--- 'sheet-children-setter' doesn't do relayout so that it remains
  //--- inexpensive.  Is this really what we want?
  //---*** Also, won't the layout be the wrong size if box isn't mapped
  //---*** (hence we don't call 'relayout-parent')?  But we can't call it
  //---*** because nothing will have been mirrored!
  sheet-children(layout) := buttons;
  relayout-parent(layout);
  when (sheet-mapped?(layout))
    do(curry(sheet-mapped?-setter, #t), buttons)
  end
end method note-gadget-items-changed;

define method note-gadget-selection-changed
    (box :: <gadget-box-pane-mixin>) => ()
  next-method();
  update-button-selections(box, gadget-selection(box))
end method note-gadget-selection-changed;

define method update-button-enabled-states 
    (box :: <gadget-box-pane-mixin>, enabled? :: <boolean>) => ()
  let buttons = gadget-box-buttons(box);
  for (button in buttons)
    gadget-enabled?(button) := enabled?
  end
end method update-button-enabled-states;

define method note-gadget-enabled
    (client, box :: <gadget-box-pane-mixin>) => ()
  ignore(client);
  update-button-enabled-states(box, #t)
end method note-gadget-enabled;

define method note-gadget-disabled
    (client, box :: <gadget-box-pane-mixin>) => ()
  ignore(client);
  update-button-enabled-states(box, #f)
end method note-gadget-disabled;


/// Generic implementation of button-box using just buttons

define sealed class <button-box-pane-mixin> (<gadget-box-pane-mixin>)
end class <button-box-pane-mixin>;

define method initialize
    (box :: <button-box-pane-mixin>, #key child) => ()
  when (child)
    // Reverse engineer the buttons from the sheet hierarchy
    //--- One flaw is that we can't notice any new buttons that get added
    let buttons = find-button-box-buttons(box, child);
    gadget-box-buttons(box) := buttons;
    box.%items := map-as(<vector>, gadget-id, buttons)
  end;
  next-method()
end method initialize;

define method find-button-box-buttons
    (box :: <button-box-pane-mixin>, child :: <sheet>)
 => (buttons :: <sequence>)
  let button-class = button-class-for-gadget-box(box);
  let buttons = make(<stretchy-vector>);
  do-sheet-tree(method (sheet)
		  when (instance?(sheet, button-class))
                    gadget-client(sheet) := box;
		    add!(buttons, sheet)
		  end
		end,
		child);
  buttons
end method find-button-box-buttons;


/// Push box panes

define sealed class <push-box-pane> 
    (<button-box-pane-mixin>,
     <push-box>,
     <single-child-wrapping-pane>)
end class <push-box-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <push-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<push-box-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<push-box-pane>));
define sealed domain initialize (<push-box-pane>);

define sealed method button-class-for-gadget-box
    (box :: <push-box-pane>) => (class :: <class>)
  <push-button>
end method button-class-for-gadget-box;

define method do-execute-activate-callback
    (button :: <push-button>, box :: <push-box-pane>, id) => ()
  ignore(id);
  gadget-value(box) := gadget-value(button);
  execute-activate-callback(box, gadget-client(box), gadget-id(box));
  next-method()
end method do-execute-activate-callback;


/// Radio box panes

define sealed class <radio-box-pane>
    (<button-box-pane-mixin>,
     <radio-box>,
     <single-child-wrapping-pane>)
end class <radio-box-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <radio-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>));
  values(<radio-box-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<radio-box-pane>));
define sealed domain initialize (<radio-box-pane>);

define sealed method button-class-for-gadget-box
    (box :: <radio-box-pane>) => (class :: <class>)
  <radio-button>
end method button-class-for-gadget-box;

define method do-execute-value-changed-callback
    (button :: <radio-button>, box :: <radio-box-pane>, id) => ()
  ignore(id);
  gadget-selection(box, do-callback?: #t)
    := if (gadget-value(button) == #t)
	 vector(gadget-box-button-index(box, button))
       else
	 #[]
       end;
  next-method()
end method do-execute-value-changed-callback;

define method do-execute-activate-callback
    (button :: <radio-button>, box :: <radio-box-pane>, id) => ()
  ignore(id);
  execute-activate-callback(box, gadget-client(box), gadget-id(box));
  next-method()
end method do-execute-activate-callback;


/// Check box panes

define sealed class <check-box-pane>
    (<button-box-pane-mixin>,
     <check-box>,
     <single-child-wrapping-pane>)
end class <check-box-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <check-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>));
  values(<check-box-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<check-box-pane>));
define sealed domain initialize (<check-box-pane>);

define sealed method button-class-for-gadget-box
    (box :: <check-box-pane>) => (class :: <class>)
  <check-button>
end method button-class-for-gadget-box;

define method do-execute-value-changed-callback
    (button :: <check-button>, box :: <check-box-pane>, id) => ()
  ignore(id);
  let index = gadget-box-button-index(box, button);
  let new-selection
    = if (gadget-value(button) == #t)
        add-new(gadget-selection(box), index)
      else
	remove(gadget-selection(box), index)
      end;
  gadget-selection(box, do-callback?: #t) := new-selection;
  next-method()
end method do-execute-value-changed-callback;

define method do-execute-activate-callback
    (button :: <check-button>, box :: <check-box-pane>, id) => ()
  ignore(id);
  execute-activate-callback(box, gadget-client(box), gadget-id(box));
  next-method()
end method do-execute-activate-callback;
