Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Vanilla gadgets

define open abstract class <vanilla-pane-mixin>
    (<standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <vanilla-pane-mixin>;


define method do-compose-space 
    (pane :: <vanilla-pane-mixin>, #key width, height)
 => (space-requirement :: <space-requirement>)
  //--- Compute the space requirements of all of the backend gadgets
end method do-compose-space;

define method note-gadget-enabled
    (client, gadget :: <vanilla-pane-mixin>) => ()
  //--- Enable the gadget mirror
end method note-gadget-enabled;

define method note-gadget-disabled 
    (client, gadget :: <vanilla-pane-mixin>) => ()
  //--- Enable the gadget mirror
end method note-gadget-enabled;

define method port-handles-repaint?
    (_port :: <vanilla-port>, sheet :: <vanilla-pane-mixin>) => (true? :: <boolean>)
  //--- Return #t if the port generates damage events for each
  //--- mirrored sheet, in effect, handling repaint itself (this is
  //--- the X model).  Return #f if only one damage event comes in for
  //--- the top level sheet, meaning that repainting of child sheets
  //--- must be done manually (the Mac model).
  #t
end method port-handles-repaint?;

define method default-foreground-setter
    (fg :: <ink>, pane :: <vanilla-pane-mixin>) => (foreground :: <ink>)
  next-method();
  //--- Change the foreground of the gadget
  fg
end method default-foreground-setter;

define method default-background-setter
    (bg :: <ink>, pane :: <vanilla-pane-mixin>) => (background :: <ink>)
  next-method();
  //--- Change the background of the gadget
  bg
end method default--background-setter;


define sealed class <vanilla-top-level-sheet>
    (<standard-repainting-mixin>,
     <vanilla-pane-mixin>,
     <top-level-sheet>)
end class <vanilla-top-level-sheet>;

define method class-for-make-pane
    (framem :: <vanilla-frame-manager>, class == <top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-top-level-sheet>, #f)
end method class-for-make-pane;


define sealed class <vanilla-viewport>
    (<vanilla-pane-mixin>,
     <viewport>,
     <permanent-medium-mixin>,
     <mirrored-sheet-mixin>,
     <single-child-composite-pane>)
end class <vanilla-viewport>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-viewport>, #f)
end method class-for-make-pane;


define open abstract class <vanilla-button-mixin>
    (<vanilla-pane-mixin>)
end class <vanilla-button-mixin>;

define method do-compose-space
    (pane :: <vanilla-button-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  make(<space-requirement>,
       width: 40,
       height: 15)
end method do-compose-space;

define method allocate-space
    (pane :: <vanilla-button-mixin>, width :: <integer>, height :: <integer>)
end method allocate-space;


define sealed class <vanilla-push-button-pane>
    (<vanilla-button-mixin>,
     <push-button>,
     <leaf-pane>)
end class <vanilla-push-button-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-push-button-pane>, #f)
end method class-for-make-pane;

define method handle-event 
    (pane :: <vanilla-push-button-pane>, event :: <button-release-event>) => ()
  execute-activate-callback(pane, gadget-client(pane), gadget-id(pane))
end method handle-event;


define sealed class <vanilla-radio-button-pane>
    (<vanilla-button-mixin>,
     <radio-button>,
     <leaf-pane>)
end class <vanilla-radio-button-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-radio-button-pane>, #f)
end method class-for-make-pane;

define method handle-event
    (pane :: <vanilla-radio-button-pane>, event :: <button-release-event>) => ()
  gadget-value(pane, do-callback?: #t) := ~gadget-value(pane)
end method handle-event;


define sealed class <vanilla-check-button-pane>
    (<vanilla-button-mixin>,
     <check-button>,
     <leaf-pane>)
end class <vanilla-check-button-pane>;

define method class-for-make-pane
    (framem :: <vanilla-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-check-button-pane>, #f)
end method class-for-make-pane;

define method handle-event 
    (pane :: <vanilla-check-button-pane>, event :: <button-release-event>) => ()
  gadget-value(pane, do-callback?: #t) := ~gadget-value(pane)
end method handle-event;


define sealed class <vanilla-list-box-pane> 
    (<vanilla-pane-mixin>,
     <leaf-pane>,
     <list-box>)
end class <vanilla-list-box-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-list-box-pane>, #f)
end method class-for-make-pane;


define sealed class <vanilla-menu-bar-pane>
    (<vanilla-pane-mixin>,
     <multiple-child-composite-pane>,
     <menu-bar>)
end class <vanilla-menu-bar-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <menu-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-menu-bar-pane>, #f)
end method class-for-make-pane;


define sealed class <vanilla-menu-pane>
    (<vanilla-pane-mixin>,
     <multiple-child-composite-pane>,
     <menu>)
end class <vanilla-menu-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <menu>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-menu-pane>, #f)
end method class-for-make-pane;


define sealed class <vanilla-push-menu-button-pane>
    (<vanilla-button-mixin>,
     <push-menu-button>)
end class <vanilla-push-menu-button-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <push-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-push-menu-button-pane>, #f)
end method class-for-make-pane;


define sealed class <vanilla-radio-menu-button-pane>
    (<vanilla-button-mixin>,
     <radio-menu-button>)
end class <vanilla-radio-menu-button-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <radio-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-radio-menu-button-pane>, #f)
end method class-for-make-pane;


define sealed class <vanilla-check-menu-button-pane>
    (<vanilla-button-mixin>,
     <check-menu-button>)
end class <vanilla-check-menu-button-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <check-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-check-menu-button-pane>, #f)
end method class-for-make-pane;


//--- It might be the case that this does not need to be mirrored,
//--- in which case this class and the 'class-for-make-pane' go away.
define sealed class <vanilla-push-menu-box-pane>
    (<vanilla-pane-mixin>,
     <push-menu-box-pane>)
end class <vanilla-push-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <push-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-push-menu-box-pane>, #f)
end method class-for-make-pane;


//--- Same as <vanilla-push-menu-box-pane>
define sealed class <vanilla-radio-menu-box-pane>
    (<vanilla-pane-mixin>,
     <radio-menu-box-pane>)
end class <vanilla-radio-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <radio-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-radio-menu-box-pane>, #f)
end method class-for-make-pane;


//--- Same as <vanilla-push-menu-box-pane>
define sealed class <vanilla-check-menu-box-pane>
    (<vanilla-pane-mixin>,
     <check-menu-box-pane>)
end class <vanilla-check-menu-box-pane>;

define method class-for-make-pane 
    (framem :: <vanilla-frame-manager>, class == <check-menu-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<vanilla-check-menu-box-pane>, #f)
end method class-for-make-pane;


//--- Missing
//---  <scroll-bar>
//---  <slider>
//---  <label>
//---  <text-field>
//---  <text-editor>
//---  <option-box>

