Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic implementation of <menu-box> using just buttons

// This is really a mixin class, but you can do 'make-pane' on it, so
// we'll go with the more appetizing name...
define open abstract class <menu-box-pane>
    (<gadget-box-pane-mixin>, 
     <abstract-sheet>)
end class <menu-box-pane>;

define method button-class-for-gadget-box
    (box :: <menu-box-pane>) => (class :: <class>)
  <menu-button>
end method button-class-for-gadget-box;

define method gadget-pane-default-layout-class
    (box :: <menu-box-pane>) => (class)
  #f
end method gadget-pane-default-layout-class;

define method gadget-box-buttons-parent
    (box :: <menu-box-pane>) => (sheet :: <sheet>)
  box
end method gadget-box-buttons-parent;

define function menu-box-pane-selection-mode-class
    (selection-mode :: <selection-mode>) => (class :: <class>)
  select (selection-mode)
    #"none"     => <push-menu-box-pane>;
    #"single"   => <radio-menu-box-pane>;
    #"multiple" => <check-menu-box-pane>;
  end
end function menu-box-pane-selection-mode-class;

define sealed inline method make 
    (class == <menu-box-pane>, #rest initargs,
     #key selection-mode :: <selection-mode> = #"none", #all-keys)
 => (component :: <menu-box-pane>)
  apply(make, menu-box-pane-selection-mode-class(selection-mode), initargs)
end method make;


// Relaying-out a menu box doesn't make any sense, so just do nothing
define method relayout-parent
    (sheet :: <menu-box-pane>, #key width, height)
 => (did-layout? :: <boolean>)
  ignore(width, height);
  #f
end method relayout-parent;


define sealed class <push-menu-box-pane>
    (<menu-box-pane>, 
     <push-menu-box>, 
     <multiple-child-composite-pane>)
end class <push-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <push-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<push-menu-box-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<push-menu-box-pane>));
define sealed domain initialize (<push-menu-box-pane>);

define method do-execute-activate-callback
    (button :: <push-menu-button>, box :: <push-menu-box-pane>, id) => ()
  ignore(id);
  gadget-value(box) := gadget-value(button);
  execute-activate-callback(box, gadget-client(box), gadget-id(box));
  next-method()
end method do-execute-activate-callback;


define sealed class <radio-menu-box-pane>
    (<menu-box-pane>,
     <radio-menu-box>,
     <multiple-child-composite-pane>)
end class <radio-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <radio-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<radio-menu-box-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<radio-menu-box-pane>));
define sealed domain initialize (<radio-menu-box-pane>);

define method do-execute-value-changed-callback
    (button :: <radio-menu-button>, box :: <radio-menu-box-pane>, id) => ()
  ignore(id);
  gadget-selection(box, do-callback?: #t)
    := if (gadget-value(button) == #t)
	 vector(gadget-box-button-index(box, button))
       else
	 #[]
       end;
  next-method()
end method do-execute-value-changed-callback;


define sealed class <check-menu-box-pane>
    (<menu-box-pane>,
     <check-menu-box>,
     <multiple-child-composite-pane>)
end class <check-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <check-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<check-menu-box-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<check-menu-box-pane>));
define sealed domain initialize (<check-menu-box-pane>);

define method do-execute-value-changed-callback
    (button :: <check-menu-button>, box :: <check-menu-box-pane>, id) => ()
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
