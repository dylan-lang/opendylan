Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Menus

define open abstract class <menu-bar>
    (<no-value-gadget-mixin>,
     <gadget-bar-mixin>,
     <basic-gadget>)
end class <menu-bar>;

//--- Maybe this should just be <sheet>, but it's often convenient 
//--- to specify a frame
define constant <menu-owner> = false-or(type-union(<sheet>, <frame>));

// A menu will have three kinds of children:
//  - one of the subclasses of <menu-button>
//  - one of the subclasses of <menu-box>
//  - other <menu>'s
define open abstract class <menu>
    (<no-value-gadget-mixin>,
     <gadget-command-mixin>,
     <gadget-bar-mixin>,
     <mnemonic-mixin>,
     <updatable-gadget-mixin>,
     <labelled-gadget-mixin>,
     <basic-gadget>)
  sealed slot menu-owner :: <menu-owner> = #f,		// for pop-up menus
    init-keyword: owner:;
end class <menu>;

define method initialize
    (menu :: <menu>,
     #key tear-off? = #f, help-menu? = #f,
	  x-alignment = #"left", y-alignment = #"top")
  next-method();
  let bits = logior(if (tear-off?) %tear_off_menu else 0 end,
		    if (help-menu?) %help_menu else 0 end);
  // <labelled-gadget-mixin> also does this, but we do it again
  // here because the defaults are different for menus...
  let xa = select (x-alignment)
	     #"left"  => %x_alignment_left;
	     #"right" => %x_alignment_right;
	     #"center", #"centre" => %x_alignment_center;
	   end;
  let ya = select (y-alignment)
	     #"top"      => %y_alignment_top;
	     #"bottom"   => %y_alignment_bottom;
	     #"baseline" => %y_alignment_baseline;
	     #"center", #"centre" => %y_alignment_center;
	   end;
  gadget-flags(menu)
    := logior(logand(gadget-flags(menu),
		     lognot(logior(%tear_off_menu, %help_menu,
				   %x_alignment_mask, %y_alignment_mask))),
	      bits, xa, ya)
end method initialize;

define sealed inline method tear-off-menu?
    (menu :: <menu>) => (tear-off? :: <boolean>)
  logand(gadget-flags(menu), %tear_off_menu) = %tear_off_menu
end method tear-off-menu?;

define sealed inline method help-menu?
    (menu :: <menu>) => (help? :: <boolean>)
  logand(gadget-flags(menu), %help_menu) = %help_menu
end method help-menu?;

define sealed method top-level-sheet 
    (menu :: <menu>) => (sheet :: false-or(<sheet>))
  let owner = menu-owner(menu);
  if (owner)
    top-level-sheet(owner)
  else
    next-method()
  end
end method top-level-sheet;

define sealed method display
    (sheet :: <menu>) => (display :: false-or(<display>))
  if (menu-owner(sheet))
    display(menu-owner(sheet))
  else
    next-method()
  end
end method display;

define sealed method gadget-selection-mode
    (menu :: <menu>) => (selection-mode :: <selection-mode>)
  #"none"
end method gadget-selection-mode;

define open generic note-menu-attached 
    (frame :: <abstract-frame>, menu :: <menu>) => ();

define open generic note-menu-detached
    (frame :: <abstract-frame>, menu :: <menu>) => ();

// If this is a pop-up menu, ensure that it's mirrored before we map it.
// Then unmap it when we're done.
define method sheet-mapped?-setter 
    (mapped? == #t, menu :: <menu>, 
     #key do-repaint? = #t, clear? = do-repaint?)
 => (mapped? :: <boolean>)
  ignore(do-repaint?, clear?);
  let owner     = menu-owner(menu);
  let top-sheet = owner & top-level-sheet(owner);
  if (top-sheet)
    block ()
      unless (sheet-attached?(menu))
        //--- We should add this to the owner's top-level sheet,
        //--- but <top-level-sheet> only allows a single child
        add-child(display(top-sheet), menu);	// ensure the menu is attached
	note-menu-attached(sheet-frame(menu), menu)
      end;
      next-method()
    cleanup
      sheet-mapped?(menu) := #f
    end;
    mapped?
  else
    next-method()
  end
end method sheet-mapped?-setter;

define method note-sheet-detached
    (menu :: <menu>) => ()
  next-method();
  let owner     = menu-owner(menu);
  let top-sheet = owner & top-level-sheet(owner);
  when (top-sheet)
    note-menu-detached(sheet-frame(top-sheet), menu)
  end
end method note-sheet-detached;

define method note-menu-attached
    (frame :: <frame>, menu :: <menu>) => ()
  #f
end method note-menu-attached;

define method note-menu-detached
    (frame :: <frame>, menu :: <menu>) => ()
  #f
end method note-menu-detached;

define method display-menu
    (menu :: <menu>,
     #key x :: false-or(<integer>), y :: false-or(<integer>)) => ()
  let owner = menu-owner(menu);
  assert(owner,
	 "Cannot display a popup menu without an owner: %=", menu);
  if (x & y)
    set-sheet-position(menu, x, y)
  else
    let _port   = port(owner);
    let pointer = port-pointer(_port);
    //--- It would be much simpler if the owner was always a sheet
    let sheet
      = select (owner by instance?)
	  <frame> => top-level-sheet(owner);
	  <sheet> => owner;
	end;
    when (sheet)
      let (x, y) = pointer-position(pointer, sheet: sheet);
      set-sheet-position(menu, x, y)
    end
  end;
  sheet-mapped?(menu) := #t
end method display-menu;


/// Menu boxes

define open abstract class <menu-box>
    (<gadget-box>,
     <basic-gadget>)
end class <menu-box>;

define function menu-box-selection-mode-class
    (selection-mode :: <selection-mode>) => (class :: <class>)
  select (selection-mode)
    #"none"     => <push-menu-box>;
    #"single"   => <radio-menu-box>;
    #"multiple" => <check-menu-box>;
  end
end function menu-box-selection-mode-class;

define sealed inline method make
    (class == <menu-box>, #rest initargs,
     #key selection-mode :: <selection-mode> = #"none", #all-keys)
 => (menu-box :: <menu-box>)
  apply(make, menu-box-selection-mode-class(selection-mode), initargs)
end method make;


define open abstract class <push-menu-box>
    (<action-gadget-mixin>,
     <menu-box>,
     <basic-value-gadget>)
end class <push-menu-box>;

define sealed method gadget-selection-mode
    (menu :: <push-menu-box>) => (selection-mode :: <selection-mode>)
  #"none"
end method gadget-selection-mode;


define open abstract class <radio-menu-box> 
    (<gadget-selection-mixin>, <menu-box>)
end class <radio-menu-box>;

define sealed method gadget-selection-mode
    (menu :: <radio-menu-box>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;


define open abstract class <check-menu-box> 
    (<gadget-selection-mixin>, <menu-box>)
end class <check-menu-box>;

define sealed method gadget-selection-mode
    (menu :: <check-menu-box>) => (selection-mode :: <selection-mode>)
  #"multiple"
end method gadget-selection-mode;


/// Menu buttons

define open abstract class <menu-button>
    (<updatable-gadget-mixin>,
     <button>)
end class <menu-button>;


define open abstract class <push-menu-button> 
    (<menu-button>, <default-gadget-mixin>, <basic-value-gadget>)
end class <push-menu-button>;

define method initialize
    (button :: <push-menu-button>, #key) => ()
  next-method();
  when (gadget-command(button) & ~gadget-activate-callback(button))
    gadget-activate-callback(button) := callback-for-command(gadget-command(button))
  end
end method initialize;

define sealed method gadget-selection-mode
    (button :: <push-menu-button>) => (selection-mode :: <selection-mode>)
  #"none"
end method gadget-selection-mode;


define open abstract class <radio-menu-button>
    (<menu-button>, <basic-value-gadget>)
end class <radio-menu-button>;

define sealed method gadget-selection-mode
    (button :: <radio-menu-button>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;

// Because we don't have <action-gadget-mixin> in radio menu buttons
define sealed method activate-gadget
    (gadget :: <radio-menu-button>) => ()
  execute-value-changed-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method activate-gadget;


define open abstract class <check-menu-button>
    (<menu-button>, <basic-value-gadget>)
end class <check-menu-button>;

define sealed method gadget-selection-mode
    (button :: <check-menu-button>) => (selection-mode :: <selection-mode>)
  #"multiple"
end method gadget-selection-mode;

// Because we don't have <action-gadget-mixin> in check menu buttons
define sealed method activate-gadget
    (gadget :: <check-menu-button>) => ()
  execute-value-changed-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method activate-gadget;


define function menu-button-selection-mode-class
    (selection-mode :: <selection-mode>) => (class :: <class>)
  select (selection-mode)
    #"none"     => <push-menu-button>;
    #"single"   => <radio-menu-button>;
    #"multiple" => <check-menu-button>;
  end;
end function menu-button-selection-mode-class;

define sealed inline method make
    (class == <menu-button>, #rest initargs,
     #key selection-mode :: <selection-mode> = #"none", #all-keys)
 => (button :: <menu-button>)
  apply(make, menu-button-selection-mode-class(selection-mode), initargs)
end method make;


/// Popup menus

// Default implementation of menu chooser when handed a sequence of items.
// This is "sideways" because it is a forward reference from DUIM-Sheets.
define sideways method do-choose-from-menu
    (framem :: <frame-manager>, owner :: <sheet>, items :: <sequence>,
     #rest keys,
     #key title = #f, value,
          label-key = collection-gadget-default-label-key,
          value-key = collection-gadget-default-value-key,
          width, height, foreground, background, text-style,
	  multiple-sets? = #f,
     #all-keys)
 => (value, success? :: <boolean>)
  dynamic-extent(keys);
  ignore(value);
  let menu
    = make-menu-from-items(framem, items,
			   label-key: label-key,
			   value-key: value-key,
			   width:  width,
			   height: height,
			   foreground: foreground,
			   background: background,
			   text-style: text-style,
			   title: title,
			   owner: owner,
			   multiple-sets?: multiple-sets?);
  block ()
    apply(do-choose-from-menu, framem, owner, menu, keys)
  cleanup
    // We're done with it, so get rid of all back-end resources
    destroy-sheet(menu)
  end  
end method do-choose-from-menu;

define method make-menu-from-items
    (framem :: <frame-manager>, items :: <sequence>,
     #key owner, title = "Menu",
          label-key = collection-gadget-default-label-key,
          value-key = collection-gadget-default-value-key,
          width, height, foreground, background, text-style, multiple-sets? = #f)
 => (menu :: <menu>)
  with-frame-manager (framem)
    let menu-boxes :: <stretchy-object-vector> = make(<stretchy-vector>);
    local method make-menu-box (items)
	    add!(menu-boxes, make(<menu-box>,
				  items: items,
				  label-key: label-key,
				  value-key: value-key,
				  foreground: foreground,
				  background: background,
				  text-style: text-style))
	  end method;
    if (multiple-sets?)
      do(make-menu-box, items)
    else
      make-menu-box(items)
    end;
    make(<menu>,
	 owner: owner,
	 label: title,
	 width:  width,
	 height: height,
	 children:   menu-boxes, 
	 foreground: foreground,
	 background: background,
	 text-style: text-style)
  end
end method make-menu-from-items;


/// Menu creation macros

/*--- The problem with this is that we lose the variable 'menu'!
define macro menu
  { menu (?label:expression, #rest ?options:expression)
      ?entries:*
    end }
    => { make(<menu>, label: ?label, children: vector(?entries), ?options) }
end macro menu;

define macro menu-box
  { menu-box (?label:expression, #rest ?options:expression)
      ?entries:*
    end }
    => { make(<menu-box>, label: ?label, children: vector(?entries), ?options) }
 entries:
  { } => { }
  { item ?label:expression => ?callback:expression; ... }
    => { make(<menu-button>, label: ?label, activate-callback: ?callback), ... }
end macro menu-box;
*/
