Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Menu handling

define sealed class <capi-menu-object-mixin> (<object>)
end class <capi-menu-object-mixin>;

define method mirror-edges 
    (_port :: <capi-port>,
     sheet :: <capi-menu-object-mixin>,
     mirror)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, 100, 100)
end method mirror-edges;

define method set-mirror-edges 
    (_port :: <capi-port>, sheet :: <capi-menu-object-mixin>, mirror,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  //--- Do nothing!
  #f
end method set-mirror-edges;



/// menu handling

define sealed class <capi-menu-bar-pane>
    (<capi-menu-object-mixin>,
     <capi-sheet-mixin>,
     <menu-bar>,
     <multiple-child-composite-pane>)
end class <capi-menu-bar-pane>;

define method repaint-within-parent
    (menu-bar :: <capi-menu-bar-pane>, #key clear? = #t) => ()
  ignore(clear?);
  #f
end method repaint-within-parent;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <menu-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-menu-bar-pane>, #f)
end method class-for-make-pane;

define method do-compose-space
    (sheet :: <capi-menu-bar-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let top-sheet = top-level-sheet(sheet);
  if (top-sheet)
    let interface = sheet-mirror(top-sheet);
    let rep = ensure-representation(interface);
    let width  = width-menu-bar(rep);
    let height = height-menu-bar(rep);
    make(<space-requirement>,
	 width: width, height: height)
  else
    warn("'compose-space' called on unattached menu-bar %=", sheet);
    default-space-requirement(sheet)
  end
end method do-compose-space;


define sealed class <capi-menu-pane>
    (<capi-menu-object-mixin>,
     <capi-gadget-mixin>,
     <menu>,
     <multiple-child-composite-pane>)
end class <capi-menu-pane>;

define method do-compose-space
    (sheet :: <capi-menu-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  warn("Trying to call 'compose-space' on a menu-pane %=", sheet);
  default-space-requirement(sheet)
end method do-compose-space;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <menu>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-menu-pane>, #f)
end method class-for-make-pane;

define method mirror-popup-menu-callback
    (#rest args) => ()
  format-out("%=", args)
end method mirror-popup-menu-callback;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-menu-pane>)
  // Owner will be #f for a non-popup menu...
  let owner     = menu-owner(sheet);
  let top-sheet = owner & top-level-sheet(owner);
  let interface = top-sheet & sheet-mirror(top-sheet);
  when (interface & ~instance?(interface, <capi-top-level-mirror>))
    error("Non-interface %= as owner for popup menu %=",
          interface, sheet)
  end;
  let (label, mnemonic, index)
    = compute-mnemonic-from-label(sheet, gadget-label(sheet), remove-ampersand?: #t);
  ignore(mnemonic, index);
  make-capi-mirror(_port, sheet, <capi-menu>,
                   title: label | if (owner) #() else "Untitled" end,
                   interface: interface,
                   popup-menu-callback: mirror-popup-menu-callback,
                   menu-interface: interface)
end method do-make-mirror;

define method remove-capi-mirror
    (sub-menu :: <capi-menu>, menu :: <capi-menu>)
  menu-items(menu) := remove(menu-items(menu), sub-menu)
end method remove-capi-mirror;

define method remove-capi-mirror
    (menu :: <capi-menu>, interface :: <capi-interface>)
  interface-menu-bar-items(interface)
    := remove(interface-menu-bar-items(interface), menu)
end method remove-capi-mirror;

define method destroy-mirror
    (_port :: <capi-port>, menu :: <capi-menu-pane>, mirror) => ()
  remove-capi-mirror(mirror, element-parent(mirror));
  sheet-direct-mirror(menu) := #f
end method destroy-mirror;

define method update-capi-gadget-enabled-state 
    (gadget :: <capi-menu-pane>, state)
  let mirror = sheet-mirror(gadget);
  when (mirror)
    menu-object-enabled(mirror) := state | #()
  end
end method update-capi-gadget-enabled-state;


define open abstract class <capi-menu-button-mixin> 
    (<capi-menu-object-mixin>,
     <capi-button-mixin>)
end class <capi-menu-button-mixin>;

define sealed class <capi-menu-item-mirror> (<capi-mirror>, <capi-menu-item>)
end class <capi-menu-item-mirror>;

define method capi-mirror-value (mirror :: <capi-menu-item-mirror>)
  lisp-true?(item-selected(mirror))
end method capi-mirror-value;

define method update-capi-gadget-enabled-state 
    (gadget :: <capi-menu-button-mixin>, state)
  let mirror = sheet-mirror(gadget);
  if (mirror)
    menu-object-enabled(mirror) := state | #()
  end
end method update-capi-gadget-enabled-state;


define sealed class <capi-push-menu-button-pane>
    (<capi-menu-button-mixin>,
     <push-menu-button>,
     <leaf-pane>)
end class <capi-push-menu-button-pane>;

define method sheet-primary-callback (sheet :: <capi-push-menu-button-pane>)
  distribute-activate-callback(sheet)
end method sheet-primary-callback;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <push-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-push-menu-button-pane>, #f)
end method class-for-make-pane;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-menu-button-mixin>)
  let (text, image, mnemonic)
    = text-or-image-from-gadget-label(sheet);
  let parent = capi-mirror-parent(sheet);
  let mirror
    = make-capi-mirror(_port, sheet, <capi-menu-item-mirror>,
                       selected: gadget-value(sheet) | #(),
                       text: text,
                       callback-type: item:,
                       callback: mirror-primary-callback);
  select (parent by instance?)
    <capi-menu> =>
      menu-items(parent)
	:= concatenate(menu-items(parent), list(mirror));
    <capi-menu-component> =>
      collection-items(parent)
	:= concatenate(collection-items(parent), list(mirror));
  end;
  mirror
end method do-make-mirror;

define method destroy-mirror
    (_port :: <capi-port>, button :: <capi-menu-button-mixin>, mirror) => ()
  let parent = capi-mirror-parent(sheet-parent(button));
  select (parent by instance?)
    <capi-menu> =>
      menu-items(parent)
	:= remove!(menu-items(parent), mirror);
    <capi-menu-component> =>
      collection-items(parent)
	:= remove!(collection-items(parent), mirror);
  end;
  sheet-direct-mirror(button) := #f
end method destroy-mirror;


define sealed class <capi-radio-menu-button-pane>
    (<capi-menu-button-mixin>,
     <radio-menu-button>,
     <leaf-pane>)
end class <capi-radio-menu-button-pane>;

define method sheet-primary-callback (gadget :: <capi-radio-menu-button-pane>)
  let value = capi-new-mirror-value(sheet-direct-mirror(gadget));
  distribute-value-changed-callback(gadget, value)
end method sheet-primary-callback;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <radio-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-radio-menu-button-pane>, #f)
end method class-for-make-pane;


define sealed class <capi-check-menu-button-pane>
    (<capi-menu-button-mixin>,
     <check-menu-button>,
     <leaf-pane>)
end class <capi-check-menu-button-pane>;

define method sheet-primary-callback (gadget :: <capi-check-menu-button-pane>)
  let value = capi-new-mirror-value(sheet-direct-mirror(gadget));
  distribute-value-changed-callback(gadget, value)
end method sheet-primary-callback;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <check-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-check-menu-button-pane>, #f)
end method class-for-make-pane;


define open abstract class <capi-menu-box-mixin> (<capi-gadget-mixin>)
end class <capi-menu-box-mixin>;

define method do-compose-space
    (sheet :: <capi-menu-box-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  warn("Trying to call 'compose-space' on a menu-box %=", sheet);
  default-space-requirement(sheet)
end method do-compose-space;

define sealed class <capi-push-menu-box-pane>
    (<capi-menu-box-mixin>,
     <push-menu-box-pane>)
end class <capi-push-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <push-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-push-menu-box-pane>, #f)
end method class-for-make-pane;

define method capi-selection-mode (gadget :: <capi-menu-box-mixin>)
  select (gadget-selection-mode(gadget))
    #"multiple" => multiple-selection:;
    otherwise => next-method();
  end
end method capi-selection-mode;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-menu-box-mixin>)
  make-capi-mirror(_port, sheet, <capi-menu-component>,
                   interaction: capi-selection-mode(sheet));
end method do-make-mirror;

// Same as <capi-push-menu-box-pane>
define sealed class <capi-radio-menu-box-pane>
    (<capi-menu-box-mixin>,
     <radio-menu-box-pane>)
end class <capi-radio-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <radio-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-radio-menu-box-pane>, #f)
end method class-for-make-pane;


// Same as <capi-push-menu-box-pane>
define sealed class <capi-check-menu-box-pane>
    (<capi-menu-box-mixin>,
     <check-menu-box-pane>)
end class <capi-check-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <check-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-check-menu-box-pane>, #f)
end method class-for-make-pane;


/// Popup menus
    
define method map-mirror
    (_port :: <capi-port>, menu :: <capi-menu-pane>, mirror) => ()
  ignore(mirror);
  // That is, 'sheet-mapped?-setter' on a pop-up menu pops up the menu!
  when (menu-owner(menu))
    popup-capi-menu-pane(menu);
  end
end method map-mirror;

define method do-choose-from-menu
    (framem :: <capi-frame-manager>, owner :: <sheet>, menu :: <menu>,
     #key title, value,
          label-key, value-key,
          width, height, foreground, background, text-style, multiple-sets?)
 => (value, success? :: <boolean>)
  ignore(value, multiple-sets?, label-key, value-key, width, height);
  menu-owner(menu) := owner;
  unless (sheet-attached?(menu))
    add-child(display(owner), menu)		// ensure the menu is attached
  end;
  let selected-button = popup-capi-menu-pane(menu);
  values(selected-button & button-gadget-value(selected-button),
         selected-button & #t)
end method do-choose-from-menu;

define method popup-capi-menu-pane (menu :: <capi-menu-pane>)
  let owner  = menu-owner(menu);
  let owner  = if (frame?(owner)) top-level-sheet(owner) else owner end;
  let mirror = sheet-mirror(menu);
  let item-mirror
    = display-popup-menu(mirror, owner: sheet-mirror(owner));
  when (instance?(item-mirror, <capi-mirror>))
    mirror-sheet(item-mirror)
  end
end method popup-capi-menu-pane;

