Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Labels

// Abstract label
define open abstract class <label>
    (<labelled-gadget-mixin>,
     <basic-gadget>)
  keyword accepts-focus?: = #f;
  keyword tab-stop?:      = #f;
end class <label>;

define method initialize
    (gadget :: <label>, #key multi-line? = #f) => ()
  next-method();
  let bit = if (multi-line?) %multi_line_label else 0 end;
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%multi_line_label)), bit)
end method initialize;

define sealed inline method multi-line-label?
    (gadget :: <label>) => (multi-line? :: <boolean>)
  logand(gadget-flags(gadget), %multi_line_label) = %multi_line_label
end method multi-line-label?;

// A <label> is always enabled
define sealed method gadget-enabled?
    (label :: <label>) => (enabled? :: <boolean>)
  #t
end method gadget-enabled?;

define sealed method gadget-enabled?-setter
    (enabled? :: <boolean>, label :: <label>) => (enabled? :: <boolean>)
  enabled?
end method gadget-enabled?-setter;

define macro labelling
  { labelling (?label:expression)
      ?contents:body
    end }
    => { let _contents = ?contents;	// contents is a single expression
	 horizontally (x-spacing: 2, y-alignment: #"center")
           make(<label>, label: ?label);
           _contents
         end }
  { labelling (?label:expression, #rest ?options:expression)
      ?contents:body
    end }
    => { let _contents = ?contents;	// contents is a single expression
	 horizontally (?options, x-spacing: 2, y-alignment: #"center")
           make(<label>, label: ?label);
           _contents
         end }
end macro labelling;

// Everyone always spells this "wrong", so be forgiving
define macro labeling
  { labeling (?label:expression)
      ?contents:*
    end }
    => { labelling (?label)
           ?contents
         end }
  { labeling (?label:expression, #rest ?options:expression)
      ?contents:*
    end }
    => { labelling (?label, ?options)
           ?contents
         end }
end macro labeling;


// Default label implementation
define sealed class <label-pane> (<label>, <simple-pane>)
end class <label-pane>;

define method class-for-make-pane
    (framem :: <frame-manager>, class == <label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<label-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<label-pane>));
define sealed domain initialize (<label-pane>);

define method do-compose-space
    (pane :: <label-pane>, #key width, height)
 => (space-requirement :: <space-requirement>)
  ignore(width, height);
  let (width, height) = gadget-label-size(pane);
  make(<space-requirement>,
       width: width, height: height)
end method do-compose-space;

define method handle-repaint
    (sheet :: <label-pane>, medium :: <medium>, region :: <region>) => ()
  draw-gadget-label(sheet, medium, 0, 0)
end method handle-repaint;


/// Buttons

define open abstract class <button>
    (<accelerator-mixin>,
     <mnemonic-mixin>,
     <labelled-gadget-mixin>,
     <gadget-command-mixin>,
     <value-gadget>)
end class <button>;

define method handle-button-gadget-click
    (gadget :: <basic-gadget>, #key double-click? = #f)
 => (handled? :: <boolean>)
  select (gadget-selection-mode(gadget))
    #"none" =>
      distribute-activate-callback(gadget);
    #"single" =>
      // You can only turn single-selection buttons on by clicking
      // on them; they go off when you click on some other button
      unless (gadget-value(gadget))
	distribute-value-changed-callback(gadget, #t);
	when (double-click?)
	  distribute-activate-callback(gadget)
	end;
      end;
    #"multiple" =>
      //---*** 'gadget-value' won't be up to date if we distribute
      //---*** two of these before handling any of them
      distribute-value-changed-callback(gadget, ~gadget-value(gadget));
      when (double-click?)
	distribute-activate-callback(gadget)
      end;
  end;
  #t
end method handle-button-gadget-click;

define function button-selection-mode-class
    (selection-mode :: <selection-mode>) => (class :: <class>)
  select (selection-mode)
    #"none"     => <push-button>;
    #"single"   => <radio-button>;
    #"multiple" => <check-button>;
  end
end function button-selection-mode-class;

define sealed inline method make
    (class == <button>, #rest initargs,
     #key selection-mode :: <selection-mode> = #"none", #all-keys)
 => (button :: <button>)
  apply(make, button-selection-mode-class(selection-mode), initargs)
end method make;

// Inside of things like menus, we want to be able to get the value of
// a button.  When the button is inside some kind of a button box, its
// value is the value of the client box.
define method button-gadget-value (button :: <button>) => (value)
  select (gadget-selection-mode(button))
    #"none" =>
      gadget-value(button);
    #"single", #"multiple" =>
      let client = gadget-client(button);
      when (client)
        gadget-value(client)
      end
  end
end method button-gadget-value;


/// Push buttons

define open abstract class <push-button>
    (<button>, <default-gadget-mixin>, <basic-value-gadget>)
end class <push-button>;

define method initialize
    (button :: <push-button>, #key) => ()
  next-method();
  when (gadget-command(button) & ~gadget-activate-callback(button))
    gadget-activate-callback(button) := callback-for-command(gadget-command(button))
  end
end method initialize;

define sealed method gadget-selection-mode
    (button :: <push-button>) => (selection-mode :: <selection-mode>)
  #"none"
end method gadget-selection-mode;


/// Radio buttons and check buttons

// A button in a "one-of" panel
define open abstract class <radio-button>
    (<button>, <action-gadget-mixin>, <basic-value-gadget>)
end class <radio-button>;

define sealed method gadget-selection-mode
    (button :: <radio-button>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;


// A button in a "some-of" panel
define open abstract class <check-button>
    (<button>, <action-gadget-mixin>, <basic-value-gadget>)
end class <check-button>;

define sealed method gadget-selection-mode
    (button :: <check-button>) => (selection-mode :: <selection-mode>)
  #"multiple"
end method gadget-selection-mode;


/// Sliders

// Sliders generate a value-changing callback as the slider is being moved,
// and a value-changed callback once the slider is released
define open abstract class <slider>
    (<oriented-gadget-mixin>,
     <range-gadget-mixin>,
     <labelled-gadget-mixin>,
     <bordered-gadget-mixin>,
     <changing-value-gadget-mixin>,
     <basic-value-gadget>)
  sealed constant slot slider-tick-marks :: false-or(<integer>) = #f,
    init-keyword: tick-marks:;
  // These ones aren't natively supported on Windows
  sealed slot slider-decimal-places :: <integer> = 0,
    init-keyword: decimal-places:;
  sealed slot slider-min-label = #f,
    init-keyword: min-label:;
  sealed slot slider-max-label = #f,
    init-keyword: max-label:;
end class <slider>;

define method initialize
    (slider :: <slider>,
     #key decimal-places = $unsupplied, show-value? = #t) => ()
  next-method();
  let bit = if (show-value?) %show_value else 0 end;
  gadget-flags(slider)
    := logior(logand(gadget-flags(slider), lognot(%show_value)), bit);
  when (unsupplied?(decimal-places))
    let slider-range = range-values(gadget-value-range(slider));
    case
      slider-range <=  1 =>
	slider-decimal-places(slider) := 2;
      slider-range <= 10 =>
	slider-decimal-places(slider) := 1;
    end
  end
end method initialize;

define sealed inline method slider-show-value?
    (slider :: <slider>) => (default? :: <boolean>)
  logand(gadget-flags(slider), %show_value) = %show_value
end method slider-show-value?;


/// Progress controls

define open abstract class <progress-bar> 
    (<oriented-gadget-mixin>, 
     <range-gadget-mixin>,
     <basic-value-gadget>)
  keyword accepts-focus?: = #f;
  keyword tab-stop?:      = #f;
end class <progress-bar>;

define method initialize (pane :: <progress-bar>, #key value-range)
  next-method();
  unless (value-range)
    gadget-value-range(pane) := range(from: 0, to: 100)
  end;
  unless (gadget-value(pane))
    gadget-value(pane) := gadget-value-range(pane)[0]
  end
end method initialize;


/// Separator panes

define open abstract class <separator> 
    (<oriented-gadget-mixin>, 
     <no-value-gadget-mixin>,
     <basic-gadget>)
  keyword accepts-focus?: = #f;
  keyword tab-stop?:      = #f;
end class <separator>;


define sealed class <separator-pane> 
    (<separator>,
     <drawing-pane>)
end class <separator-pane>;

define method class-for-make-pane
    (framem :: <frame-manager>, class == <separator>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<separator-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<separator-pane>));
define sealed domain initialize (<separator-pane>);

define sealed method do-compose-space
    (pane :: <separator-pane>, #key width, height)
 => (space-requirement :: <space-requirement>)
  select (gadget-orientation(pane))
    #"horizontal" =>
      make(<space-requirement>,
	   min-width: 1, width: width | 1, max-width: $fill,
	   height: 2);
    #"vertical" =>
      make(<space-requirement>,
	   width: 2,
	   min-height: 1, height: height | 1, max-height: $fill);
  end
end method do-compose-space;

define sealed method handle-repaint
    (pane :: <separator-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);	// not worth checking
  let (left, top, right, bottom) = box-edges(pane);
  draw-separator(pane, medium, gadget-orientation(pane), left, top, right, bottom)
end method handle-repaint;

define open generic draw-separator
    (sheet :: <abstract-sheet>, medium :: <abstract-medium>, orientation :: <gadget-orientation>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>,
     #key type :: <border-type>) => ();

define method draw-separator
    (sheet :: <sheet>, medium :: <medium>, orientation :: <gadget-orientation>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>, #key type :: <border-type> = #f) => ()
  ignore(type);
  with-drawing-options (medium, brush: default-foreground(sheet))
    select (orientation)
      #"horizontal" =>
        let bottom = top + 1;
	draw-rectangle(medium, left, top, right, bottom, filled?: #t);
      #"vertical" =>
        let right = left + 1;
	draw-rectangle(medium, left, top, right, bottom, filled?: #t);
    end
  end
end method draw-separator;


/// Pages

define open abstract class <page> (<gadget>)
end class <page>;

define open abstract class <basic-page>
    (<labelled-gadget-mixin>, 
     <page>,
     <basic-gadget>)
  sealed slot page-initial-focus :: false-or(<sheet>) = #f,
    init-keyword: input-focus:;
end class <basic-page>;

// Default method, because we allow property sheets to contain non-<page> objects
define method page-initial-focus
    (sheet :: <sheet>) => (focus :: false-or(<sheet>))
  #f
end method page-initial-focus;
