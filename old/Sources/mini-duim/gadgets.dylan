Module:    mini-duim
Synopsis:  Mini-DUIM gadgets
Author:    Scott McKay, Andrew Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Labels

define open abstract class <label> (<basic-gadget>)
end class <label>;


/// Text fields

// A single-line text editing field
define open abstract class <text-field>
    (<value-gadget-mixin>, 
     <focus-gadget-mixin>,
     <basic-gadget>)
  slot gadget-editable? = #t,
    init-keyword: editable?:;
  slot text-field-maxiumum-size :: false-or(<integer>) = #f,
    init-keyword: maximum-size:;
end class <text-field>;

define open abstract class <password-field> (<text-field>)
end class <password-field>;


/// Buttons

define open abstract class <button> 
    (<mnemonic-mixin>,
     <basic-gadget>)
end class <button>;

//--- Stub to get the backend to work
define open abstract class <button-box>
    (<basic-gadget>)
end class <button-box>;

define constant <selection-mode> = one-of(#"none", #"single", #"multiple");

define method make
    (class == <button>, #rest initargs, #key selection-mode = #"none")
 => (button :: <button>)
  apply(make, button-selection-mode-class(selection-mode), initargs)
end method make;

define method button-selection-mode-class
    (selection-mode) => (class :: <class>)
  select (selection-mode)
    #"none"     => <push-button>;
    #"single"   => <radio-button>;
    #"multiple" => <check-button>;
  end;
end method button-selection-mode-class;

// Inside of things like menus, we want to be able to get the value of
// a button.  When the button is inside some kind of a button box, its
// value is the value of the client box.
define method button-gadget-value (button :: <button>) => (value)
  select (gadget-selection-mode(button))
    #"none"   =>
      gadget-value(button);
    #"single", #"multiple" =>
      let client = gadget-client(button);
      if (client)
        gadget-value(client)
      end
  end
end method button-gadget-value;


define open abstract class <push-button>
    (<action-gadget-mixin>, <value-gadget-mixin>, <button>)
  slot default-button? = #f,
    init-keyword: default-button?:;
end class <push-button>;

define method gadget-selection-mode
    (button :: <push-button>) => (selection-mode :: <selection-mode>)
  ignore(button);
  #"none"
end method gadget-selection-mode;


// A button in a "one-of" panel
define open abstract class <radio-button>
    (<value-gadget-mixin>, <button>)
end class <radio-button>;

define method gadget-selection-mode
    (button :: <radio-button>) => (selection-mode :: <selection-mode>)
  ignore(button);
  #"single"
end method gadget-selection-mode;


// A button in a "some-of" panel
define open abstract class <check-button>
    (<value-gadget-mixin>, <button>)
end class <check-button>;

define method gadget-selection-mode
    (button :: <check-button>) => (selection-mode :: <selection-mode>)
  ignore(button);
  #"multiple"
end method gadget-selection-mode;


/// Viewports

//--- This is a dummy implementation to support the backend
define open abstract class <viewport> (<basic-gadget>)
end class <viewport>;


/// Menus

define open abstract class <menu-bar> (<basic-gadget>)
end class <menu-bar>;

define open abstract class <menu>
    (<mnemonic-mixin>,
     <basic-gadget>)
  slot menu-owner = #f,
    init-keyword: owner:;
end class <menu>;

define open abstract class <menu-button> (<basic-gadget>)
  slot gadget-selection-mode :: <selection-mode> = #"none",
    init-keyword: selection-mode:;
end class <menu-button>;

define method make
    (class == <menu-button>, #rest initargs, #key selection-mode = #"none")
 => (button :: <menu-button>)
  apply(make, menu-button-selection-mode-class(selection-mode), initargs)
end method make;

define method menu-button-selection-mode-class
    (selection-mode) => (class :: <class>)
  select (selection-mode)
    #"none"     => <push-menu-button>;
    #"single"   => <radio-menu-button>;
    #"multiple" => <check-menu-button>;
  end;
end method menu-button-selection-mode-class;

define method top-level-sheet 
    (menu :: <menu>) => (sheet :: false-or(<sheet>))
  let owner = menu-owner(menu);
  if (owner)
    top-level-sheet(owner)
  else
    next-method()
  end
end method top-level-sheet;


define open abstract class <push-menu-button>
    (<action-gadget-mixin>, <value-gadget-mixin>, <menu-button>)
end class <push-menu-button>;

define method gadget-selection-mode
    (button :: <push-menu-button>) => (selection-mode :: <selection-mode>)
  ignore(button);
  #"none"
end method gadget-selection-mode;


// A button in a "one-of" panel
define open abstract class <radio-menu-button>
    (<value-gadget-mixin>, <menu-button>)
end class <radio-menu-button>;

define method gadget-selection-mode
    (button :: <radio-menu-button>) => (selection-mode :: <selection-mode>)
  ignore(button);
  #"single"
end method gadget-selection-mode;


// A button in a "some-of" panel
define open abstract class <check-menu-button>
    (<value-gadget-mixin>, <menu-button>)
end class <check-menu-button>;

define method gadget-selection-mode
    (button :: <check-menu-button>) => (selection-mode :: <selection-mode>)
  ignore(button);
  #"multiple"
end method gadget-selection-mode;


/// Popup menus

// This does special work when trying to map a popup menu
define method sheet-mapped?-setter
    (mapped? == #t, menu :: <menu>)
 => (mapped? :: <boolean>)
  let owner = menu-owner(menu);
  if (owner)
    block ()
      unless (sheet-grafted?(menu))
        graft-sheet(owner, menu)
      end;
      next-method()
    cleanup
      sheet-mapped?(menu) := #f;
    end
  else
    next-method()
  end;
end method sheet-mapped?-setter;


/// Menu boxes

// Stub to support the back-ends for now
define open abstract class <menu-box> (<basic-gadget>)
  slot gadget-selection-mode :: <selection-mode> = #"none",
    init-keyword: selection-mode:;
end class <menu-box>;

define sealed class <menu-box-pane>
    (<menu-box>, <multiple-child-composite-pane>)
end class <menu-box-pane>;

define method class-for-make-pane
    (framem :: <frame-manager>, class == <menu-box>,
     #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<menu-box-pane>, #f)
end method class-for-make-pane;


/// Scroll bars

// Scroll bar definition
define open abstract class <scroll-bar>
  (<oriented-gadget-mixin>,
   <range-gadget-mixin>,
   <slug-gadget-mixin>,
   <changing-value-gadget-mixin>,
   <value-gadget-mixin>,
   <basic-gadget>)
  slot %viewports = make(<stretchy-vector>);
end class <scroll-bar>;

define method line-scroll-amount
    (scroll-bar :: <scroll-bar>, orientation) => (amount :: <real>)
  let amount = #f;
  let viewports = scroll-bar.%viewports;
  case
    ~empty?(viewports) =>
      for (viewport in viewports)
	let viewport-amount = line-scroll-amount(viewport, orientation);
	amount := min(amount | viewport-amount, viewport-amount)
      end;
      amount;
    otherwise =>
      //--- Is there a better default line amount?
      gadget-value-increment(scroll-bar) * 3;
  end
end method line-scroll-amount;

define method page-scroll-amount 
    (scroll-bar :: <scroll-bar>, orientation) => (amount :: <real>)
  let amount = #f;
  for (viewport in scroll-bar.%viewports)
    let viewport-amount = page-scroll-amount(viewport, orientation);
    amount := min(amount | viewport-amount, viewport-amount)
  end;
  amount | gadget-slug-size(scroll-bar)
end method page-scroll-amount;

define method scroll-by-pixels 
    (scroll-bar :: <scroll-bar>, pixels :: <integer>) => ()
  let old-value = gadget-value(scroll-bar);
  let increment = gadget-value-increment(scroll-bar);
  let new-pixel-position = round/(old-value, increment) + pixels;
  let new-value = 
    select (increment by instance?)
      <integer> => round(new-pixel-position * increment);
      <real>    => new-pixel-position * increment;
    end;
  gadget-value(scroll-bar, do-callback?: #t) := new-value
end method scroll-by-pixels;

define method scroll-up-line (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, -gadget-line-scroll-amount(scroll-bar))
end method scroll-up-line;

define method scroll-down-line (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, gadget-line-scroll-amount(scroll-bar))
end method scroll-down-line;

define method scroll-up-page (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, -gadget-page-scroll-amount(scroll-bar))
end method scroll-up-page;

define method scroll-down-page (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, gadget-page-scroll-amount(scroll-bar))
end method scroll-down-page;

define method scroll-to-position
    (scroll-bar :: <scroll-bar>, value :: <real>) => ()
  gadget-value(scroll-bar, do-callback?: #t) := value
end method scroll-to-position;

define method scroll-to-position
    (scroll-bar :: <scroll-bar>, value == #"top") => ()
  gadget-value(scroll-bar, do-callback?: #t) := gadget-start-value(scroll-bar)
end method scroll-to-position;

define method scroll-to-position 
    (scroll-bar :: <scroll-bar>, value == #"bottom") => ()
  let size = gadget-slug-size(scroll-bar);
  gadget-value(scroll-bar, do-callback?: #t)
    := gadget-end-value(scroll-bar) - size
end method scroll-to-position;
