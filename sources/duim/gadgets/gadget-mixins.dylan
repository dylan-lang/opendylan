Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic gadget protocols

// By the time we instantiate a gadget, it will be a <sheet> as well...
define open abstract class <abstract-gadget> (<abstract-sheet>) end;

// "Primary" gadget classes should inherit from <gadget>
// "Mixin" gadget classes should inherit from <abstract-gadget>
define protocol-class gadget (<abstract-gadget>)
  /*
  virtual slot gadget-enabled? :: <boolean>;
  virtual slot gadget-label;
  virtual slot gadget-value;
  virtual slot gadget-id;
  virtual slot gadget-client;
  virtual slot gadget-documentation;
 */
end protocol-class gadget;

//--- If you change this method, change the one in sheets/frame-managers
define method make
    (pane-class :: subclass(<gadget>),
     #rest pane-options,
     #key port, frame-manager: framem, #all-keys)
 => (pane :: <gadget>)
  dynamic-extent(pane-options);
  let framem = framem
               | *current-frame-manager*
               | port-default-frame-manager(port | default-port())
               | error("Can't find a frame manager to use with 'make-pane'");
  let (concrete-class, concrete-options)
    = apply(class-for-make-pane, framem, pane-class, pane-options);
  // If there's a mapping from the abstract pane class to a concrete pane
  // class, then use it.  Otherwise just try to create a class named by the
  // abstract pane class.
  if (concrete-class == pane-class)
    apply(next-method, pane-class,
	  frame-manager: framem, pane-options)
  else
    //---*** Unfortunately, this recursive call to make will call
    //---*** 'class-for-make-pane' again.  How to speed this up?
    apply(make, concrete-class,
	  frame-manager: framem,
	  concrete-options | pane-options)
  end
end method make;

define constant <gadget-orientation>
    = one-of(#"horizontal", #"vertical", #"none");

define constant <selection-mode> = one-of(#"none", #"single", #"multiple");

// Currently just for Windows "push-button-like" buttons...
define constant <button-style> = one-of(#f, #"push-button");

define constant <mnemonic> = type-union(<character>, <keyboard-gesture>);

define open abstract class <gadget-state> (<object>)
end class <gadget-state>;

define protocol <<gadget-protocol>> ()
  getter gadget-enabled?
    (gadget :: <abstract-gadget>) => (enabled? :: <boolean>);
  setter gadget-enabled?-setter
    (enabled? :: <boolean>, gadget :: <abstract-gadget>) => (enabled? :: <boolean>);
  function note-gadget-disabled
    (client, gadget :: <abstract-gadget>) => ();
  function note-gadget-enabled
    (client, gadget :: <abstract-gadget>) => ();
  getter gadget-id
    (gadget :: <abstract-gadget>) => (id);
  setter gadget-id-setter
    (id, gadget :: <abstract-gadget>) => (id);
  getter gadget-client
    (gadget :: <abstract-gadget>) => (client);
  function gadget-client-setter
    (client, gadget :: <abstract-gadget>) => (client);
  getter gadget-documentation
    (gadget :: <abstract-gadget>) => (documentation);
  setter gadget-documentation-setter
    (documentation, gadget :: <abstract-gadget>) => (documentation);
  getter gadget-selection-mode 
    (gadget :: <abstract-gadget>) => (selection-mode :: <selection-mode>);
  getter gadget-default?
    (gadget :: <abstract-gadget>) => (default? :: <boolean>);
  getter gadget-default?-setter
    (default? :: <boolean>, gadget :: <abstract-gadget>)
 => (default? :: <boolean>);
  getter gadget-read-only?
    (gadget :: <abstract-gadget>) => (read-only? :: <boolean>);
  getter gadget-slug-size
    (gadget :: <abstract-gadget>) => (slug-size :: <real>);
  getter gadget-slug-size-setter
    (slug-size :: <real>, gadget :: <abstract-gadget>) => (slug-size :: <real>);
  function note-gadget-slug-size-changed
    (gadget :: <abstract-gadget>) => ();
  // Orientation and labels
  getter gadget-orientation
    (gadget :: <abstract-gadget>) => (orientation :: <gadget-orientation>);
  getter gadget-label
    (gadget :: <abstract-gadget>) => (label);
  setter gadget-label-setter
    (label, gadget :: <abstract-gadget>) => (label);
  function note-gadget-label-changed
    (gadget :: <abstract-gadget>) => ();
  function gadget-label-size 
    (gadget :: <abstract-gadget>, #key do-newlines?, do-tabs?)
 => (width :: <integer>, height :: <integer>);
  function draw-gadget-label
    (gadget :: <abstract-gadget>, medium :: <medium>, x, y,
     #key align-x, align-y, state, do-tabs?, brush, underline?) => ();
  // Accelerators and mnemonics
  getter gadget-accelerator
    (gadget :: <abstract-gadget>) => (accelerator);
  setter gadget-accelerator-setter
    (accelerator, gadget :: <abstract-gadget>) => (accelerator);
  getter defaulted-gadget-accelerator
    (framem :: <abstract-frame-manager>, gadget :: <abstract-gadget>)
 => (accelerator :: false-or(<accelerator>));
  getter gadget-mnemonic
    (gadget :: <abstract-gadget>) => (mnemonic);
  setter gadget-mnemonic-setter
    (mnemonic, gadget :: <abstract-gadget>) => (mnemonic);
  getter defaulted-gadget-mnemonic
    (framem :: <abstract-frame-manager>, gadget :: <abstract-gadget>)
 => (mnemonic :: false-or(<mnemonic>));
  function compute-mnemonic-from-label
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>), label, #key remove-ampersand?)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>));
  getter gadget-scrolling?
    (gadget :: <abstract-gadget>)
 => (horizontally? :: <boolean>, vertically? :: <boolean>);
  getter gadget-scrolling-horizontally?
    (gadget :: <abstract-gadget>) => (horizontally? :: <boolean>);
  getter gadget-scrolling-vertically?
    (gadget :: <abstract-gadget>) => (vertically? :: <boolean>);
  getter gadget-lines
    (gadget :: <abstract-gadget>) => (lines :: false-or(<integer>));
  getter gadget-columns
    (gadget :: <abstract-gadget>) => (columns :: false-or(<integer>));
  getter gadget-state
    (gadget :: <abstract-gadget>) => (state :: false-or(<gadget-state>));
  setter gadget-state-setter
    (state :: <gadget-state>, gadget :: <abstract-gadget>) => (state :: <gadget-state>);
end protocol <<gadget-protocol>>;


/// Basic gadget definition

// Note that only the concrete gadget classes have a <style-descriptor>,
// which will typically come when <basic-sheet> gets included in the CPL
define open abstract class <basic-gadget> (<gadget>)
  sealed slot gadget-id = #f,
    init-keyword: id:;
  sealed slot gadget-client = #f,
    init-keyword: client:;
  slot gadget-documentation :: false-or(<string>) = #f,
    init-keyword: documentation:,
    setter: %documentation-setter;
  // This stores mostly "static" properties that get used at mirroring time
  sealed slot gadget-flags :: <integer> = $initial-gadget-flags;
end class <basic-gadget>;


// Bits 0..5 are some basic boolean flags
define constant %gadget_enabled   :: <integer> = #o01;
define constant %gadget_read_only :: <integer> = #o02;
define constant %default_button   :: <integer> = #o04;
define constant %show_value       :: <integer> = #o10;
define constant %tear_off_menu    :: <integer> = #o20;
define constant %help_menu        :: <integer> = #o40;

// Reuse this bit, since you can't be both a label and a menu
define constant %multi_line_label :: <integer> = %tear_off_menu;

// Bits 6..8 is the x-alignment field
define constant %x_alignment_shift  :: <integer> = 6;
define constant %x_alignment_mask   :: <integer> = #o700;
define constant %x_alignment_left   :: <integer> = #o000;
define constant %x_alignment_right  :: <integer> = #o100;
define constant %x_alignment_center :: <integer> = #o200;

// Bits 9..11 is the y-alignment field
define constant %y_alignment_shift    :: <integer> = 9;
define constant %y_alignment_mask     :: <integer> = #o7000;
define constant %y_alignment_top      :: <integer> = #o0000;
define constant %y_alignment_bottom   :: <integer> = #o1000;
define constant %y_alignment_center   :: <integer> = #o2000;
define constant %y_alignment_baseline :: <integer> = #o3000;

// Bits 12..14 is the scroll bar field
define constant %scroll_bar_shift      :: <integer> = 12;
define constant %scroll_bar_mask       :: <integer> = #o70000;
define constant %scroll_bar_false      :: <integer> = #o00000;
define constant %scroll_bar_none       :: <integer> = #o10000;
define constant %scroll_bar_horizontal :: <integer> = #o20000;
define constant %scroll_bar_vertical   :: <integer> = #o30000;
define constant %scroll_bar_both       :: <integer> = #o40000;
define constant %scroll_bar_dynamic    :: <integer> = #o50000;

// Bits 15..17 is the orientation field
// define constant %orientation_shift   :: <integer> = 15;
define constant %orientation_mask       :: <integer> = #o700000;
define constant %orientation_none       :: <integer> = #o000000;
define constant %orientation_horizontal :: <integer> = #o100000;
define constant %orientation_vertical   :: <integer> = #o200000;

// Bits 18..20 are some text field flags
define constant %text_case_shift :: <integer> = 18;
define constant %text_case_mask  :: <integer> = #o3000000;
define constant %text_case_false :: <integer> = #o0000000;
define constant %text_case_lower :: <integer> = #o1000000;
define constant %text_case_upper :: <integer> = #o2000000;
define constant %auto_scroll     :: <integer> = #o4000000;

// Bits 21..23 are for gadgets that do their own borders
define constant %border_shift   :: <integer> = 21;
define constant %border_mask    :: <integer> = #o70000000;
define constant %border_default :: <integer> = #o00000000;
define constant %border_none    :: <integer> = #o10000000;
define constant %border_sunken  :: <integer> = #o20000000;
define constant %border_raised  :: <integer> = #o30000000;
define constant %border_ridge   :: <integer> = #o40000000;
define constant %border_groove  :: <integer> = #o50000000;
define constant %border_input   :: <integer> = #o60000000;
define constant %border_output  :: <integer> = #o70000000;

// Bit 24 is for "push-button-like" gadgets in tool bars
define constant %push_button_like :: <integer> = #o100000000;

// Bit 25 tells the gadget to ensure that the selection is visible
define constant %keep_selection_visible :: <integer> = #o200000000;

define constant $initial-gadget-flags :: <integer>
    = logior(%gadget_enabled, %show_value,
	     %x_alignment_center, %y_alignment_top,
	     %scroll_bar_both, %orientation_horizontal,
	     %border_default, %keep_selection_visible);

define method initialize
    (gadget :: <basic-gadget>,
     #key enabled? = #t, read-only? = #f, button-style :: <button-style> = #f)
  next-method();
  let bits = logior(if (enabled?)   %gadget_enabled   else 0 end,
		    if (read-only?) %gadget_read_only else 0 end,
		    if (button-style == #"push-button") %push_button_like else 0 end);
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget),
		     lognot(logior(%gadget_enabled, %gadget_read_only, %push_button_like))),
	      bits)
end method initialize;


define method gadget-state
    (gadget :: <basic-gadget>) => (state :: false-or(<gadget-state>))
  #f
end method gadget-state;

define method gadget-state-setter
    (state :: <gadget-state>, gadget :: <basic-gadget>) => (state :: <gadget-state>)
  state
end method gadget-state-setter;


/// Basic gadget methods

// Make gadgets have no label by default
define method gadget-label (gadget :: <gadget>) => (label)
  #f
end method gadget-label;

define method gadget-documentation-setter
    (documentation, gadget :: <basic-gadget>) => (documentation :: false-or(<string>))
  gadget.%documentation := documentation
end method gadget-documentation-setter;

define sealed inline method gadget-read-only?
    (gadget :: <basic-gadget>) => (read-only? :: <boolean>)
  logand(gadget-flags(gadget), %gadget_read_only) = %gadget_read_only
end method gadget-read-only?;

define sealed inline method push-button-like?
    (gadget :: <basic-gadget>) => (push-button-like? :: <boolean>)
  logand(gadget-flags(gadget), %push_button_like) = %push_button_like
end method push-button-like?;

define method update-gadget (gadget :: <gadget>) => ()
  #f
end method update-gadget;


/// Callbacks

// Could be 'false-or(<class>, <command>, <function>, <list>)', but it's not worth it
define constant <callback-type>   = <object>;
define constant <callback-client> = type-union(<abstract-gadget>, <abstract-frame>);

define open generic execute-callback
    (client :: <callback-client>, callback :: <callback-type>, #rest args) => ();

define method execute-callback
    (client :: <callback-client>, function :: <function>, #rest args) => ()
  dynamic-extent(args);
  apply(function, args)
end method execute-callback;

// A little bit of Lisp for you...
define method execute-callback
    (client :: <callback-client>, function :: <list>, #rest args) => ()
  dynamic-extent(args);
  apply(head(function), concatenate(args, tail(function)))
end method execute-callback;


/// Command callbacks

define method callback-for-command 
    (command :: <function>) => (callback :: <function>, command)
  values(method (sheet)
	   command(sheet-frame(sheet))
	 end method,
	 command)
end method callback-for-command;

define method callback-for-command
    (command :: <command>) => (callback :: <function>, command)
  values(method (sheet)
	   //--- This could copy the command and plug in the new server and client...
	   execute-command(command)
	 end method,
	 command)
end method callback-for-command;

define method callback-for-command
    (command-type :: subclass(<command>)) => (callback :: <function>, command)
  values(method (sheet)
	   execute-command-type(command-type, server: sheet-frame(sheet), client: sheet)
	 end method,
	 command-type)
end method callback-for-command;

define method callback-for-command
    (command-type :: <list>) => (callback :: <function>, command)
  values(method (sheet)
	   execute-command-type(command-type, server: sheet-frame(sheet), client: sheet)
	 end method,
	 head(command-type))
end method callback-for-command;


/// Enabling and disabling of gadgets

define sealed inline method gadget-enabled?
    (gadget :: <basic-gadget>) => (enabled? :: <boolean>)
  logand(gadget-flags(gadget), %gadget_enabled) = %gadget_enabled
end method gadget-enabled?;

define sealed method gadget-enabled?-setter
    (enabled? :: <boolean>, gadget :: <basic-gadget>)
 => (enabled? :: <boolean>)
  let bit = if (enabled?) %gadget_enabled else 0 end;
  when (logand(gadget-flags(gadget), %gadget_enabled) ~= bit)
    gadget-flags(gadget)
      := logior(logand(gadget-flags(gadget), lognot(%gadget_enabled)), bit);
    if (enabled?)
      note-gadget-enabled(gadget-client(gadget), gadget)
    else
      note-gadget-disabled(gadget-client(gadget), gadget)
    end
  end;
  enabled?
end method gadget-enabled?-setter;

define method note-gadget-enabled (client, gadget :: <gadget>) => ()
  ignore(client);
  #f
end method note-gadget-enabled;

define method note-gadget-disabled (client, gadget :: <gadget>) => ()
  ignore(client);
  #f
end method note-gadget-disabled;


/// Value gadgets

define open abstract class <value-gadget> (<gadget>)
end class <value-gadget>;

define open abstract class <value-range-gadget> (<value-gadget>)
end class <value-range-gadget>;

define protocol <<value-gadget-protocol>> (<<gadget-protocol>>)
  getter gadget-value
    (gadget :: <value-gadget>) => (value);
  setter gadget-value-setter
    (value, gadget :: <value-gadget>, #key do-callback?) => (value);
  function do-gadget-value-setter
    (gadget :: <value-gadget>, normalized-value) => ();
  getter gadget-value-type
    (gadget :: <value-gadget>) => (type :: <type>);
  getter gadget-label-key
    (gadget :: <value-gadget>) => (label-key :: <function>);
  getter gadget-value-key
    (gadget :: <value-gadget>) => (value-key :: <function>);
  function normalize-gadget-value
    (gadget :: <value-gadget>, value) => (value);
  function note-gadget-value-changed
    (gadget :: <value-gadget>) => ();
  function execute-value-changed-callback
    (gadget :: <value-gadget>, client, id) => ();
  function do-execute-value-changed-callback
    (gadget :: <value-gadget>, client, id) => ();
  getter gadget-value-changed-callback
    (gadget :: <value-gadget>) => (callback :: <callback-type>);
  setter gadget-value-changed-callback-setter
    (callback :: <callback-type>, gadget :: <value-gadget>)
 => (callback :: <callback-type>);
  getter gadget-value-range
    (gadget :: <value-range-gadget>) => (value-range :: <range>);
  getter gadget-value-range-setter 
    (value-range :: <range>, gadget :: <value-range-gadget>)
 => (value-range :: <range>);
  function note-gadget-value-range-changed
    (gadget :: <value-range-gadget>) => ();
  getter gadget-start-value
    (gadget :: <value-range-gadget>) => (start-value :: <real>);
  getter gadget-end-value
    (gadget :: <value-range-gadget>) => (start-value :: <real>);
  getter gadget-value-increment
    (gadget :: <value-range-gadget>) => (increment :: <real>);
  // This one is a little odd, but it is exported...
  getter button-gadget-value (button :: <button>) => (value)
end protocol <<value-gadget-protocol>>;


// This class can be used for things that act like value gadgets, but whose
// value is computed on the fly
define open abstract class <value-gadget-mixin> (<value-gadget>)
  sealed slot gadget-value-changed-callback :: <callback-type> = #f,
    init-keyword: value-changed-callback:;
end class <value-gadget-mixin>;

define method initialize (gadget :: <value-gadget-mixin>, #key value)
  next-method();
  // Ensure that gadget value normalization takes place
  gadget-value(gadget) := value | gadget-value(gadget)
end method initialize;

define method normalize-gadget-value
    (gadget :: <value-gadget>, value) => (value)
  value
end method normalize-gadget-value;


// By default, we don't call the value-changed callback
define method gadget-value-setter
    (value, gadget :: <value-gadget-mixin>, #key do-callback? = #f)
 => (value)
  let value = normalize-gadget-value(gadget, value);
  unless (value = gadget-value(gadget))
    do-gadget-value-setter(gadget, value);
    when (do-callback?)
      execute-value-changed-callback(gadget, gadget-client(gadget), gadget-id(gadget))
    end;
    note-gadget-value-changed(gadget)
  end;
  value
end method gadget-value-setter;

define method note-gadget-value-changed
    (gadget :: <value-gadget>) => ()
  #f
end method note-gadget-value-changed;

// The value-changed callback gets invoked when the value is "definitively"
// changed, such as clicking on a radio button in a radio box or releasing
// the mouse button after dragging a slider
define sealed method execute-value-changed-callback
    (gadget :: <value-gadget-mixin>, client, id) => ()
  ignore(client, id);
  let callback = gadget-value-changed-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget)
  else
    do-execute-value-changed-callback(gadget, client, id)
  end
end method execute-value-changed-callback;

define method do-execute-value-changed-callback
    (gadget :: <value-gadget-mixin>, client, id) => ()
  ignore(client, id);
  #f
end method do-execute-value-changed-callback;

// By default, valued gadgets have unconstrained type
define method gadget-value-type
    (gadget :: <value-gadget>) => (type :: <type>)
  <object>
end method gadget-value-type;

// By default, it is an error to set a gadget's value... if it is legal,
// then there will be another method below it.
define method gadget-value-setter
    (value, gadget :: <value-gadget>, #key do-callback? = #f)
 => (value)
  ignore(do-callback?);
  error("Setting value for %= which is a class of gadget with no value: %=",
	gadget, value)
end method gadget-value-setter;


define open abstract class <no-value-gadget-mixin> (<value-gadget>)
end class <no-value-gadget-mixin>;

define sealed method gadget-value
    (gadget :: <no-value-gadget-mixin>) => (value)
  #f
end method gadget-value;


define open abstract class <basic-value-gadget>
    (<value-gadget-mixin>, 
     <basic-gadget>)
  slot gadget-value = #f,
    init-keyword: value:,
    setter: %value-setter;
end class <basic-value-gadget>;

define sealed method do-gadget-value-setter
    (gadget :: <basic-value-gadget>, value) => ()
  gadget.%value := value
end method do-gadget-value-setter;


/// Value gadget state

define sealed class <value-gadget-state> (<gadget-state>)
  sealed constant slot %state-value,
    required-init-keyword: value:;
end class <value-gadget-state>;

define sealed domain make (singleton(<value-gadget-state>));
define sealed domain initialize (<value-gadget-state>);

define method gadget-state
    (gadget :: <value-gadget>) => (state :: <value-gadget-state>)
  make(<value-gadget-state>,
       value: gadget-value(gadget))
end method gadget-state;

define method gadget-state-setter
    (state :: <value-gadget-state>, gadget :: <value-gadget>)
 => (state :: <value-gadget-state>)
  gadget-value(gadget) := state.%state-value;
  state
end method gadget-state-setter;


/// Value-changing (formerly "dragging") gadgets

define protocol <<changing-value-gadget-protocol>> ()
  getter gadget-value-changing-callback
    (gadget :: <value-gadget>) => (callback :: <callback-type>);
  getter gadget-value-changing-callback-setter
    (callback :: <callback-type>, gadget :: <value-gadget>)
 => (callback :: <callback-type>);
  function execute-value-changing-callback
    (gadget :: <value-gadget>, client, id) => ();
  function do-execute-value-changing-callback
    (gadget :: <value-gadget>, client, id) => ();
end protocol <<changing-value-gadget-protocol>>;

define open abstract class <changing-value-gadget-mixin> (<value-gadget>)
  sealed slot gadget-value-changing-callback :: <callback-type> = #f,
    init-keyword: value-changing-callback:;
end class <changing-value-gadget-mixin>;

// The value-changing callback gets invoked when the value is in the
// process of changing, such as dragging a slider before releasing
// the mouse button or "casual" typing to a text field
define sealed method execute-value-changing-callback 
    (gadget :: <changing-value-gadget-mixin>, client, id) => ()
  ignore(client, id);
  let callback = gadget-value-changing-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget)
  else
    do-execute-value-changing-callback(gadget, client, id)
  end
end method execute-value-changing-callback;

define method do-execute-value-changing-callback 
    (gadget :: <changing-value-gadget-mixin>, client, id) => ()
  ignore(client, id);
  #f
end method do-execute-value-changing-callback;


/// Action gadget mixin

define open abstract class <action-gadget> (<gadget>)
end class <action-gadget>;

define protocol <<action-gadget-protocol>> (<<gadget-protocol>>)
  function execute-activate-callback
    (gadget :: <action-gadget>, client, id) => ();
  function do-execute-activate-callback
    (gadget :: <action-gadget>, client, id) => ();
  getter gadget-activate-callback
    (gadget :: <action-gadget>) => (callback :: <callback-type>);
  setter gadget-activate-callback-setter
    (callback :: <callback-type>, gadget :: <action-gadget>)
 => (callback :: <callback-type>);
end protocol <<action-gadget-protocol>>;

define open abstract class <action-gadget-mixin> (<action-gadget>)
  sealed slot gadget-activate-callback :: <callback-type> = #f,
    init-keyword: activate-callback:;
end class <action-gadget-mixin>;

define sealed method activate-gadget
    (gadget :: <action-gadget-mixin>) => ()
  execute-activate-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method activate-gadget;

define sealed method execute-activate-callback
    (gadget :: <action-gadget-mixin>, client, id) => ()
  ignore(client, id);
  let callback = gadget-activate-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget)
  else
    do-execute-activate-callback(gadget, client, id)
  end
end method execute-activate-callback;

define method do-execute-activate-callback
    (gadget :: <action-gadget-mixin>, client, id) => ()
  ignore(client, id);
  #f
end method do-execute-activate-callback;


define open abstract class <basic-action-gadget>
    (<action-gadget-mixin>, <basic-gadget>)
end class <basic-action-gadget>;


/// "Updatable" gadgets

define protocol <<updatable-gadget-protocol>> (<<gadget-protocol>>)
  function update-gadget
    (gadget :: <abstract-gadget>) => ();
  function execute-update-callback
    (gadget :: <abstract-gadget>, client, id) => ();
  function do-execute-update-callback
    (gadget :: <abstract-gadget>, client, id) => ();
  getter gadget-update-callback
    (gadget :: <abstract-gadget>) => (callback :: <callback-type>);
  setter gadget-update-callback-setter
    (callback :: <callback-type>, gadget :: <abstract-gadget>)
 => (callback :: <callback-type>);
end protocol <<updatable-gadget-protocol>>;

// This class provides an update callback that should cause the gadget to
// update itself based on the current environment.  Useful for popup menus
// to set their selection correctly, or whatever.
define open abstract class <updatable-gadget-mixin> (<abstract-gadget>)
  sealed slot gadget-update-callback :: <callback-type> = #f,
    init-keyword: update-callback:;
end class <updatable-gadget-mixin>;

define sealed method execute-update-callback
    (gadget :: <updatable-gadget-mixin>, client, id) => ()
  ignore(client, id);
  let callback = gadget-update-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget)
  else
    do-execute-update-callback(gadget, client, id)
  end
end method execute-update-callback;

define method do-execute-update-callback
    (gadget :: <updatable-gadget-mixin>, client, id) => ()
  ignore(client, id);
  #f
end method do-execute-update-callback;


/// Key-press gadget mixins

define protocol <<key-press-gadget-protocol>> (<<gadget-protocol>>)
  function execute-key-press-callback
    (gadget :: <abstract-gadget>, client, id, keysym,
     #rest args, #key, #all-keys) => ();
  function do-execute-key-press-callback
    (gadget :: <abstract-gadget>, client, id, keysym,
     #rest args, #key, #all-keys) => ();
  getter gadget-key-press-callback
    (gadget :: <abstract-gadget>) => (callback :: <callback-type>);
  setter gadget-key-press-callback-setter
    (callback :: <callback-type>, gadget :: <abstract-gadget>)
 => (callback :: <callback-type>);
end protocol <<key-press-gadget-protocol>>;

define open abstract class <key-press-gadget-mixin> (<abstract-gadget>)
  sealed slot gadget-key-press-callback :: <callback-type> = #f,
    init-keyword: key-press-callback:;
end class <key-press-gadget-mixin>;

// GADGET is the gadget that got key press, and KEYSYM is the keysym
// for the key that got pressed.
define sealed method execute-key-press-callback
    (gadget :: <key-press-gadget-mixin>, client, id, keysym,
     #rest args, #key, #all-keys) => ()
  ignore(client, id);
  let callback = gadget-key-press-callback(gadget);
  if (callback)
    apply(execute-callback, gadget, callback, gadget, keysym, args)
  else
    apply(do-execute-key-press-callback, gadget, client, id, keysym, args)
  end
end method execute-key-press-callback;

define method do-execute-key-press-callback
    (gadget :: <key-press-gadget-mixin>, client, id, keysym,
     #rest args, #key, #all-keys) => ()
  ignore(client, id, keysym, args);
  #f
end method do-execute-key-press-callback;


/// Popup menu gadget mixins

define protocol <<popup-menu-gadget-protocol>> (<<gadget-protocol>>)
  function execute-popup-menu-callback
    (gadget :: <abstract-gadget>, client, id, target,
     #rest args, #key, #all-keys) => ();
  function do-execute-popup-menu-callback
    (gadget :: <abstract-gadget>, client, id, target,
     #rest args, #key, #all-keys) => ();
  getter gadget-popup-menu-callback
    (gadget :: <abstract-gadget>) => (callback :: <callback-type>);
  setter gadget-popup-menu-callback-setter
    (callback :: <callback-type>, gadget :: <abstract-gadget>)
 => (callback :: <callback-type>);
end protocol <<popup-menu-gadget-protocol>>;

define open abstract class <popup-menu-gadget-mixin> (<abstract-gadget>)
  sealed slot gadget-popup-menu-callback :: <callback-type> = #f,
    init-keyword: popup-menu-callback:;
end class <popup-menu-gadget-mixin>;

// GADGET is the recipient of the callback, and TARGET is the gadget
// that actually got the event.  For example, clicking on a node in a
// tree or list control sends the callback to the control, but the
// target is the node
define sealed method execute-popup-menu-callback
    (gadget :: <popup-menu-gadget-mixin>, client, id, target,
     #rest args, #key, #all-keys) => ()
  ignore(client, id);
  let callback = gadget-popup-menu-callback(gadget);
  if (callback)
    apply(execute-callback, gadget, callback, gadget, target, args)
  else
    apply(do-execute-popup-menu-callback, gadget, client, id, target, args)
  end
end method execute-popup-menu-callback;

define method do-execute-popup-menu-callback
    (gadget :: <popup-menu-gadget-mixin>, client, id, target,
     #rest args, #key, #all-keys) => ()
  ignore(client, id, target, args);
  #f
end method do-execute-popup-menu-callback;


/// Oriented gadgets

define open abstract class <oriented-gadget-mixin> (<abstract-gadget>)
end class <oriented-gadget-mixin>;

define method initialize
    (gadget :: <oriented-gadget-mixin>, #key orientation = #"horizontal")
  next-method();
  let o = select (orientation)
	    #"horizontal" => %orientation_horizontal;
	    #"vertical"   => %orientation_vertical;
	    #"none"       => %orientation_none;
	   end;
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%orientation_mask)), o)
end method initialize;

define sealed inline method gadget-orientation
    (gadget :: <oriented-gadget-mixin>)
 => (orientation :: <gadget-orientation>)
  select (logand(gadget-flags(gadget), %orientation_mask))
    %orientation_horizontal => #"horizontal";
    %orientation_vertical   => #"vertical";
    %orientation_none       => #"none";
  end
end method gadget-orientation;

define method gadget-pane-default-layout-class
    (gadget :: <oriented-gadget-mixin>) => (class :: <class>)
  select (logand(gadget-flags(gadget), %orientation_mask))
    %orientation_horizontal => <row-layout>;
    %orientation_vertical   => <column-layout>;
    %orientation_none       => <pinboard-layout>;
  end
end method gadget-pane-default-layout-class;


/// Labelled gadgets

define open abstract class <labelled-gadget> (<abstract-gadget>)
end class <labelled-gadget>;

define open abstract class <labelled-gadget-mixin> (<labelled-gadget>)
  sealed slot gadget-label = "",
    init-keyword: label:,
    setter: %label-setter;
end class <labelled-gadget-mixin>;

define method initialize
    (gadget :: <labelled-gadget-mixin>,
     #key label, pressed-label, armed-label, disabled-label,
          x-alignment = #"center", y-alignment = #"top") => ()
  next-method();
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
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget),
		     lognot(logior(%x_alignment_mask, %y_alignment_mask))),
	      logior(xa, ya));
  when (pressed-label | armed-label | disabled-label)
    gadget.%label := make(<image-label>,
			  normal-label:   label,
			  pressed-label:  pressed-label  | label,
			  armed-label:    armed-label    | label,
			  disabled-label: disabled-label | label)
  end
end method initialize;

define sealed method gadget-label-setter 
    (label, gadget :: <labelled-gadget-mixin>) => (label)
  gadget.%label := label;
  note-gadget-label-changed(gadget);
  label
end method gadget-label-setter;

define method note-gadget-label-changed (gadget :: <gadget>) => ()
  #f
end method note-gadget-label-changed;

// Must be in the same order as the x alignment constants
define constant $x-alignments :: <simple-object-vector>
    = #[#"left", #"right", #"center"];

define sealed inline method gadget-x-alignment
    (gadget :: <labelled-gadget-mixin>) => (alignment :: <x-alignment>)
  let index = ash(logand(gadget-flags(gadget), %x_alignment_mask),
		  -%x_alignment_shift);
  $x-alignments[index]
end method gadget-x-alignment;

// Must be in the same order as the y alignment constants
define constant $y-alignments :: <simple-object-vector>
    = #[#"top", #"bottom", #"center", #"baseline"];

define sealed inline method gadget-y-alignment
    (gadget :: <labelled-gadget-mixin>) => (alignment :: <y-alignment>)
  let index = ash(logand(gadget-flags(gadget), %y_alignment_mask),
		  -%y_alignment_shift);
  $y-alignments[index]
end method gadget-y-alignment;

define method gadget-label-size
    (gadget :: <labelled-gadget-mixin>,
     #key do-newlines? = #f, do-tabs? = #t)
 => (width :: <integer>, height :: <integer>)
  let label = gadget-label(gadget) | "";
  select (label by instance?)
    <string> =>
      let _port = port(gadget);
      let text-style = get-default-text-style(_port, gadget);
      // Use the computed width of the label, but the height of the font
      // Add a few pixels in each direction to keep the label from being squeezed
      values(ceiling(text-size(_port, label,
			       text-style: text-style,
			       do-newlines?: do-newlines?, do-tabs?: do-tabs?)) + 2,
	     ceiling(font-height(text-style, _port)) + 2);
    <image> =>
      values(image-width(label), image-height(label));
    <compound-label> =>
      compound-label-size(gadget, label);
    <image-label> =>
      values(image-width(label.%normal-label), image-height(label.%normal-label));
  end
end method gadget-label-size;

define method draw-gadget-label
    (gadget :: <labelled-gadget-mixin>, medium :: <medium>, x, y,
     #key align-x = #"left", align-y = #"top", state = #"normal",
	  do-tabs? = #t, brush, underline? = #f) => ()
  let label = gadget-label(gadget);
  select (label by instance?)
    <string> =>
      with-drawing-options (medium,
			    brush: brush | default-foreground(gadget),
			    text-style: default-text-style(gadget))
	draw-text(medium, label, x, y,
		  align-x: align-x, align-y: align-y,
		  do-tabs?: do-tabs?);
	when (underline?)
	  let _port = port(gadget);
	  let text-style = get-default-text-style(_port, gadget);
	  let (width, height)
	    = text-size(_port, label,
			text-style: text-style,
			do-newlines?: #f, do-tabs?: do-tabs?);
	  let descent = font-descent(text-style, _port) - 1;
	  //---*** This needs to handle other alignments!
	  draw-line(medium, x, y + height - descent, x + width, y + height - descent)
	end
      end;
    <image> =>
      let width  = image-width(label);
      let height = image-height(label);
      select (align-x)
        #"left" => #f;
        #"right" => dec!(x, width);
        #"center", #"centre" => dec!(x, truncate/(width, 2))
      end;
      select (align-y)
        #"top", #"baseline" => #f;
        #"bottom" => dec!(x, height);
        #"center", #"centre" => dec!(x, truncate/(height, 2))
      end;
      draw-image(medium, label, x, y);
    <compound-label> =>
      draw-compound-label(gadget, label, medium, x, y, state: state, brush: brush);
    <image-label> =>
      draw-image-label(medium, label, x, y, state: state);
    singleton(#f) =>
      #f;
  end
end method draw-gadget-label;


/// Compound labels

define sealed class <compound-label> (<object>)
  sealed constant slot %text :: <string>,
    required-init-keyword: text:;
  sealed constant slot %image :: <image>,
    required-init-keyword: image:;
  sealed constant slot %orientation :: one-of(#"horizontal", #"vertical") = #"horizontal",
    init-keyword: orientation:;
end class <compound-label>;

define sealed domain make (singleton(<compound-label>));
define sealed domain initialize (<compound-label>);

define sealed method compound-label-size
    (gadget :: <labelled-gadget-mixin>, label :: <compound-label>)
 => (width :: <integer>, height :: <integer>)
  let text  = label.%text;
  let image = label.%image;
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let (tw, th) = text-size(_port, text, text-style: text-style);
  let (iw, ih) = values(image-width(image), image-height(image));
  if (label.%orientation == #"horizontal")
    // Add a little slop so that label doesn't get too squeezed
    values(ceiling(iw + tw) + 2 + 1,
	   ceiling(max(font-height(text-style, _port), ih)) + 1)
  else
    values(ceiling(max(iw, tw) + 2),
	   ceiling(ih + font-height(text-style, _port)) + 2 + 1)
  end
end method compound-label-size;

define sealed method draw-compound-label
    (gadget :: <labelled-gadget-mixin>, label :: <compound-label>, medium :: <medium>, x, y,
     #key state = #"normal", brush) => ()
  let text  = label.%text;
  let image = label.%image;
  let (iw, ih) = values(image-width(image), image-height(image));
  draw-image(medium, image, x, y);
  with-drawing-options (medium,
			brush: brush | default-foreground(gadget),
			text-style: default-text-style(gadget))
    if (label.%orientation == #"horizontal")
      draw-text(medium, text, x + iw + 2, y + floor/(ih, 2),
		align-x: #"left", align-y: #"center")
    else
      draw-text(medium, text, x + floor/(iw, 2) + 2, y + ih + 2,
		align-x: #"center", align-y: #"top")
    end
  end
end method draw-compound-label;


/// Image labels

define sealed class <image-label> (<object>)
  sealed constant slot %normal-label,
    required-init-keyword: normal-label:;
  sealed constant slot %pressed-label = #f,
    init-keyword: pressed-label:;
  sealed constant slot %armed-label = #f,
    init-keyword: armed-label:;
  sealed constant slot %disabled-label = #f,
    init-keyword: disabled-label:;
end class <image-label>;

define sealed domain make (singleton(<image-label>));
define sealed domain initialize (<image-label>);

define method draw-image-label (medium, label :: <image-label>, x, y,
				#key state = #"normal") => ()
  let image = select (state)
		#"normal"   => label.%normal-label;
		#"pressed"  => label.%pressed-label;
		#"armed"    => label.%armed-label;
		#"disabled" => label.%disabled-label;
	      end;
  draw-image(medium, image, x, y)
end method draw-image-label;


/// Mixins for accelerators and mnemonics

define constant <accelerator> = type-union(<character>, <keyboard-gesture>);

define open abstract class <accelerator-mixin> (<abstract-gadget>)
  sealed slot gadget-accelerator = $unsupplied,
    init-keyword: accelerator:;
end class <accelerator-mixin>;

define method defaulted-gadget-accelerator
    (framem :: <frame-manager>, gadget :: <accelerator-mixin>)
 => (accelerator :: false-or(<accelerator>))
  supplied?(gadget-accelerator(gadget)) & gadget-accelerator(gadget)
end method defaulted-gadget-accelerator;


define open abstract class <mnemonic-mixin> (<abstract-gadget>)
  sealed slot gadget-mnemonic = $unsupplied,
    init-keyword: mnemonic:;
end class <mnemonic-mixin>;

define method defaulted-gadget-mnemonic
    (framem :: <frame-manager>, gadget :: <mnemonic-mixin>)
 => (mnemonic :: false-or(<mnemonic>))
  supplied?(gadget-mnemonic(gadget)) & gadget-mnemonic(gadget)
end method defaulted-gadget-mnemonic;


// This just gets called by the backend to compute its mnemonic
// The index is the position at which the mnemonic character is in the [new] label
define method compute-mnemonic-from-label
    (sheet :: <sheet>, label, #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  select (label by instance?)
    <string> =>
      let ampersand = position(label, '&');
      if (ampersand & (ampersand < size(label) - 1))
	if (remove-ampersand?)
	  values(remove(label, '&', count: 1), label[ampersand + 1], ampersand)
	else
	  values(label, label[ampersand + 1], ampersand + 1)
	end
      else
	values(label, #f, #f)
      end;
    otherwise =>
      values(label, #f, #f);
  end
end method compute-mnemonic-from-label;

// An explicitly supplied mnemonic wins out over "&", and
// effectively forces the "&" out of the label if there is one
define method compute-mnemonic-from-label
    (sheet :: <mnemonic-mixin>, label, #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  ignore(label, remove-ampersand?);
  let explicit-mnemonic
    = supplied?(gadget-mnemonic(sheet)) & gadget-mnemonic(sheet);
  let remove-ampersand?
    = (explicit-mnemonic & #t) | remove-ampersand?;
  let (label, mnemonic, index)
    = next-method(sheet, label, remove-ampersand?: remove-ampersand?);
  values(label, explicit-mnemonic | mnemonic, ~explicit-mnemonic & index)
end method compute-mnemonic-from-label;


/// Command table support

// Some things here get used in command table menus.  We need a back-pointer
// from the gadget to the command so that we can enabled/disable the gadget
// when, for example, a command gets enabled/disabled.  Note that the command
// can actually be a <class>, <command>, <function>, or <command-table>.
define open abstract class <gadget-command-mixin> (<abstract-gadget>)
  sealed slot gadget-command = #f,
    init-keyword: command:;
end class <gadget-command-mixin>;

define method gadget-command (gadget :: <gadget>) => (command)
  #f
end method gadget-command;


/// Range gadgets

// In effect, the default is a continuous slider ranging from 0 to 1
//--- Maybe 0 to 100 by 1 would be more generally useful...
define constant $default-value-range = range(from: 0.0, to: 1.0, by: 0.001);

//--- We need a general way of changing the range and the value together
define open abstract class <range-gadget-mixin> (<value-range-gadget>)
  sealed slot gadget-value-range :: <range> = $default-value-range,
    init-keyword: value-range:,
    setter: %value-range-setter;
end class <range-gadget-mixin>;

//--- Can we compute this from the underlying range object?
define method gadget-value-type
    (gadget :: <range-gadget-mixin>) => (type :: <type>)
  <real>
end method gadget-value-type;

define sealed method gadget-value-range-setter
    (range :: <range>, gadget :: <range-gadget-mixin>) => (range :: <range>)
  unless (range = gadget-value-range(gadget))
    gadget.%value-range := range;
    note-gadget-value-range-changed(gadget)
  end;
  range
end method gadget-value-range-setter;

define method note-gadget-value-range-changed 
    (gadget :: <value-range-gadget>) => ()
  let value = gadget-value(gadget);
  let normalized-value = normalize-gadget-value(gadget, value);
  when (value ~= normalized-value)
    gadget-value(gadget) := normalized-value
  end
end method note-gadget-value-range-changed;

define method initialize (gadget :: <range-gadget-mixin>, #key) => ()
  next-method();
  let range-size = size(gadget-value-range(gadget));
  assert(range-size & range-size > 0,
	 "Unbounded or empty value range for %=", gadget)
end method initialize;

define sealed method gadget-start-value 
    (gadget :: <range-gadget-mixin>) => (start-value :: <real>)
  let range = gadget-value-range(gadget);
  range[0]
end method gadget-start-value;

define sealed method gadget-end-value 
    (gadget :: <range-gadget-mixin>) => (end-value :: <real>)
  let range = gadget-value-range(gadget);
  range[size(range) - 1]
end method gadget-end-value;

define sealed method gadget-value-increment 
    (gadget :: <range-gadget-mixin>) => (increment :: <real>)
  let range = gadget-value-range(gadget);
  if (size(range) <= 1) 0 else range[1] - range[0] end
end method gadget-value-increment;

define method normalize-gadget-value
    (gadget :: <range-gadget-mixin>, value == #f) => (value :: <real>)
  gadget-start-value(gadget)
end method normalize-gadget-value;

define method normalize-gadget-value 
    (gadget :: <range-gadget-mixin>, value :: <number>) => (value :: <real>)
  let range-start = gadget-start-value(gadget);
  let range-end = gadget-end-value(gadget);
  let min-value = min(range-start, range-end);
  let max-value = max(range-start, range-end);
  let minimized-value = max(value, min-value);
  let maximized-value = min(minimized-value, max-value);
  maximized-value
end method normalize-gadget-value;

//--- This is unfortunate, but we don't really want to pass the
//--- orientation through at this point.  Any better ideas?
//--- There should maybe be a user-specifiable way to set this.
define method gadget-line-scroll-amount
    (gadget :: <range-gadget-mixin>) => (amount :: <integer>)
  line-scroll-amount(gadget, gadget-orientation(gadget))
end method gadget-line-scroll-amount;

//--- There should maybe be a user-specifiable way to set this
define method gadget-page-scroll-amount
    (gadget :: <range-gadget-mixin>) => (amount :: <integer>)
  page-scroll-amount(gadget, gadget-orientation(gadget))
end method gadget-page-scroll-amount;

// Why doesn't Dylan have this, I wonder?
define function range-values
    (range :: <range>)
 => (value-range :: <real>, start-value :: <real>, end-value :: <real>, increment :: <real>)
  let start-value = range[0];
  let end-value   = range[size(range) - 1];
  let value-range = abs(start-value - end-value);
  let increment   = if (size(range) <= 1) 1 else range[1] - start-value end;
  values(value-range, start-value, end-value, increment)
end function range-values;


/// Range gadget state

define sealed class <range-gadget-state> (<value-gadget-state>)
  sealed constant slot %state-range :: <range>,
    required-init-keyword: value-range:;
end class <range-gadget-state>;

define sealed domain make (singleton(<range-gadget-state>));
define sealed domain initialize (<range-gadget-state>);

define method gadget-state
    (gadget :: <range-gadget-mixin>) => (state :: <range-gadget-state>)
  make(<range-gadget-state>,
       value: gadget-value(gadget),
       value-range: gadget-value-range(gadget))
end method gadget-state;

define method gadget-state-setter
    (state :: <range-gadget-state>, gadget :: <range-gadget-mixin>)
 => (state :: <range-gadget-state>)
  gadget-value-range(gadget) := state.%state-range;
  next-method()
end method gadget-state-setter;


/// Slug gadget mixin

define open abstract class <slug-gadget-mixin> (<range-gadget-mixin>)
  // Note that the slug size really is a _size_.  That is, if a scroll bar
  // ranges from 0 to 10 and you want the slug to take the full range, the
  // size needs to be 11 (that is, it's the size of the value range, not
  // its maximum).
  //--- This isn't a very good default for scroll bars
  sealed slot gadget-slug-size :: <real> = 1,
    init-keyword: slug-size:,
    setter: %slug-size-setter;
end class <slug-gadget-mixin>;

define sealed method gadget-slug-size-setter
    (slug-size :: <real>, gadget :: <slug-gadget-mixin>) => (slug-size :: <real>)
  unless (slug-size = gadget-slug-size(gadget))
    gadget.%slug-size := slug-size;
    note-gadget-slug-size-changed(gadget)
  end;
  slug-size
end method gadget-slug-size-setter;

define method note-gadget-slug-size-changed
    (gadget :: <slug-gadget-mixin>) => ()
  #f
end method note-gadget-slug-size-changed;

// The end value of a slug gadget is the end of the range - the slug size + 1
define sealed method gadget-end-value 
    (gadget :: <slug-gadget-mixin>) => (end-value :: <real>)
  let end-value = next-method();
  end-value - gadget-slug-size(gadget) + 1
end method gadget-end-value;


/// Slug gadget state

define sealed class <slug-gadget-state> (<value-gadget-state>)
  sealed constant slot %state-slug-size :: <real>,
    required-init-keyword: slug-size:;
end class <slug-gadget-state>;

define sealed domain make (singleton(<slug-gadget-state>));
define sealed domain initialize (<slug-gadget-state>);

define method gadget-state
    (gadget :: <slug-gadget-mixin>) => (state :: <slug-gadget-state>)
  make(<slug-gadget-state>,
       value: gadget-value(gadget),
       value-range: gadget-value-range(gadget),
       slug-size: gadget-slug-size(gadget))
end method gadget-state;

define method gadget-state-setter
    (state :: <slug-gadget-state>, gadget :: <slug-gadget-mixin>)
 => (state :: <slug-gadget-state>)
  gadget-slug-size(gadget) := state.%state-slug-size;
  next-method()
end method gadget-state-setter;


/// Scrollable gadgets

// Mix-in for things that might implement their own scrolling
// Valid values for 'scroll-bars' field are:
//    #f, #"none", #"horizontal", #"vertical", #"both", or #"dynamic"
define open abstract class <scrolling-gadget-mixin> (<abstract-gadget>)
end class <scrolling-gadget-mixin>;

define constant <scroll-bar-type>
    = one-of(#f, #t, #"none", #"horizontal", #"vertical", #"both", #"dynamic");

define method initialize
    (gadget :: <scrolling-gadget-mixin>, #key scroll-bars = #"both")
  next-method();
  gadget-scroll-bars(gadget) := scroll-bars
end method initialize;

define constant $scroll-bars :: <simple-object-vector>
    = #[#f, #"none", #"horizontal", #"vertical", #"both", #"dynamic"];

define sealed inline method gadget-scroll-bars
    (gadget :: <scrolling-gadget-mixin>) => (scroll-bars :: <scroll-bar-type>)
  let index = ash(logand(gadget-flags(gadget), %scroll_bar_mask),
		  -%scroll_bar_shift);
  $scroll-bars[index]
end method gadget-scroll-bars;

define sealed method gadget-scroll-bars-setter
    (scroll-bars :: <scroll-bar-type>, gadget :: <scrolling-gadget-mixin>)
 => (scroll-bars :: <scroll-bar-type>)
  let sb = select (scroll-bars)
	     #f            => %scroll_bar_false;
	     #"none"       => %scroll_bar_none;
	     #"horizontal" => %scroll_bar_horizontal;
	     #"vertical"   => %scroll_bar_vertical;
	     #"both"       => %scroll_bar_both;
	     #"dynamic"    => %scroll_bar_dynamic;
	   end;
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%scroll_bar_mask)), sb);
  scroll-bars
end method gadget-scroll-bars-setter;

define sealed method gadget-scrolling?
    (gadget :: <scrolling-gadget-mixin>)
 => (horizontally? :: <boolean>, vertically? :: <boolean>)
  let sb = logand(gadget-flags(gadget), %scroll_bar_mask);
  values(  sb = %scroll_bar_horizontal
	 | sb = %scroll_bar_both
	 | sb = %scroll_bar_dynamic,
	   sb = %scroll_bar_vertical
	 | sb = %scroll_bar_both
	 | sb = %scroll_bar_dynamic)
end method gadget-scrolling?;

define sealed method gadget-scrolling-horizontally?
    (gadget :: <scrolling-gadget-mixin>) => (horizontally? :: <boolean>)
  let (horizontally?, vertically?) = gadget-scrolling?(gadget);
  ignore(vertically?);
  horizontally?
end method gadget-scrolling-horizontally?;

define sealed method gadget-scrolling-vertically?
    (gadget :: <scrolling-gadget-mixin>) => (vertically? :: <boolean>)
  let (horizontally?, vertically?) = gadget-scrolling?(gadget);
  ignore(horizontally?);
  vertically?
end method gadget-scrolling-vertically?;


/// Gadgets with borders

// Mix-in for things that might implement their own borders
define open abstract class <bordered-gadget-mixin> (<abstract-gadget>)
end class <bordered-gadget-mixin>;

define constant <border-type>
    = one-of(#f, #"default",		// use the default
	     #"none", #"flat",		// no border
	     #"sunken", #"raised", #"ridge", #"groove",
	     #"input", #"output");	// "logical" borders

define method initialize
    (gadget :: <bordered-gadget-mixin>, #key border-type: borders = #f)
  next-method();
  border-type(gadget) := borders
end method initialize;

define constant $borders :: <simple-object-vector>
    = #[#f, #"none", #"sunken", #"raised", #"ridge", #"groove", #"input", #"output"];

define sealed inline method border-type
    (gadget :: <bordered-gadget-mixin>) => (borders :: <border-type>)
  let index = ash(logand(gadget-flags(gadget), %border_mask),
		  -%border_shift);
  $borders[index]
end method border-type;

define sealed method border-type-setter
    (borders :: <border-type>, gadget :: <bordered-gadget-mixin>)
 => (borders :: <border-type>)
  let bt = select (borders)
	     #f         => %border_default;	// #f == #"default"
	     #"default" => %border_default;	// #"default" == #f
	     #"none"    => %border_none;	// #"none" == #"flat"
	     #"flat"    => %border_none;	// #"flat" == #"none"
	     #"sunken"  => %border_sunken;
	     #"raised"  => %border_raised;
	     #"ridge"   => %border_ridge;
	     #"groove"  => %border_groove;
	     #"input"   => %border_input;
	     #"output"  => %border_output;
	   end;
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%border_mask)), bt);
  borders
end method border-type-setter;


/// Default button

define open abstract class <default-gadget-mixin> (<action-gadget-mixin>)
end class <default-gadget-mixin>;

define method initialize
    (gadget :: <default-gadget-mixin>, #key default? = #f) => ()
  next-method();
  let bit = if (default?) %default_button else 0 end;
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%default_button)), bit)
end method initialize;

define sealed inline method gadget-default?
    (gadget :: <default-gadget-mixin>) => (default? :: <boolean>)
  logand(gadget-flags(gadget), %default_button) = %default_button
end method gadget-default?;

define method gadget-default?-setter
    (default? :: <boolean>, gadget :: <default-gadget-mixin>)
 => (default? :: <boolean>)
  let bit = if (default?) %default_button else 0 end;
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%default_button)), bit);
  default?
end method gadget-default?-setter;


/// Scrolling sheets

define open abstract class <scrolling-sheet-mixin> (<abstract-sheet>)
  // The setters for these are in gadgets/scroll-bars.dylan
  sealed slot sheet-horizontal-scroll-bar :: false-or(<scroll-bar>) = #f,
    setter: %horizontal-scroll-bar-setter,
    init-keyword: horizontal-scroll-bar:;
  sealed slot sheet-vertical-scroll-bar :: false-or(<scroll-bar>) = #f,
    setter: %vertical-scroll-bar-setter,
    init-keyword: vertical-scroll-bar:;
end class <scrolling-sheet-mixin>;

define method initialize 
    (sheet :: <scrolling-sheet-mixin>,
     #key horizontal-scroll-bar: horizontal-bar,
          vertical-scroll-bar:   vertical-bar) => ()
  next-method();
  when (horizontal-bar)
    attach-scroll-bar(sheet, horizontal-bar)
  end;
  when (vertical-bar) 
    attach-scroll-bar(sheet, vertical-bar)
  end;
end method initialize;


/// Scrolling Protocols

define protocol <<sheet-scrolling-protocol>> ()
  function line-scroll-amount
    (sheet :: <abstract-sheet>, orientation :: <gadget-orientation>)
 => (amount :: false-or(<integer>));
  function page-scroll-amount
    (sheet :: <abstract-sheet>, orientation :: <gadget-orientation>)
 => (amount :: false-or(<integer>));
  function horizontal-line-scroll-amount
    (sheet :: <abstract-sheet>) => (amount :: <integer>);
  function vertical-line-scroll-amount
    (sheet :: <abstract-sheet>) => (amount :: <integer>);
  function sheet-scroll-range
    (sheet :: <abstract-sheet>)
 => (left :: <real>, top :: <real>, right :: <real>, bottom :: <real>);
  function sheet-visible-range
    (sheet :: <abstract-sheet>)
 => (left :: <real>, top :: <real>, right :: <real>, bottom :: <real>);
  function set-sheet-visible-range
    (sheet :: <abstract-sheet>,
     left :: <real>, top :: <real>, right :: <real>, bottom :: <real>) => ();
  // User-level scrolling functionality
  function scroll-position
    (sheet :: <abstract-sheet>) => (x :: <integer>, y :: <integer>);
  function set-scroll-position
    (sheet :: <abstract-sheet>, x :: <integer>, y :: <integer>) => ();
  // For back-end tinkering...
  function gadget-supplies-scroll-bars?
    (framem :: <frame-manager>, gadget :: false-or(<gadget>),
     #key, #all-keys)
 => (true? :: <boolean>);
end protocol <<sheet-scrolling-protocol>>;

define method line-scroll-amount
    (sheet :: <scrolling-sheet-mixin>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  select (orientation)
    #"horizontal" => horizontal-line-scroll-amount(sheet);
    #"vertical"   => vertical-line-scroll-amount(sheet);
  end
end method line-scroll-amount;

define method line-scroll-amount
    (sheet :: <sheet>, orientation :: <gadget-orientation>)
 => (amount :: false-or(<integer>))
  #f
end method line-scroll-amount;

// Page scroll scroll amount is the viewport size minus one line
//--- Seems reasonable, but maybe this is a frame manager decision
define method page-scroll-amount
    (sheet :: <scrolling-sheet-mixin>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  let (left, top, right, bottom) = sheet-visible-range(sheet);
  let line-amount = line-scroll-amount(sheet, orientation);
  select (orientation)
    #"horizontal" => right  - left - line-amount;
    #"vertical"   => bottom - top  - line-amount;
  end
end method page-scroll-amount;

define method page-scroll-amount
    (sheet :: <sheet>, orientation :: <gadget-orientation>)
 => (amount :: false-or(<integer>))
  #f
end method page-scroll-amount;

define method horizontal-line-scroll-amount
    (sheet :: <sheet>) => (amount :: <integer>)
  with-sheet-medium (medium = sheet)
    let text-style = medium-merged-text-style(medium);
    floor(font-width(text-style, port(medium)))
  end
end method horizontal-line-scroll-amount;

define method vertical-line-scroll-amount
    (sheet :: <sheet>) => (amount :: <integer>)
  with-sheet-medium (medium = sheet)
    let text-style = medium-merged-text-style(medium);
    floor(font-height(text-style, port(medium)))
  end
end method vertical-line-scroll-amount;

define method gadget-supplies-scroll-bars?
    (framem :: <frame-manager>, gadget :: false-or(<gadget>), #key)
 => (true? :: <boolean>)
  #f
end method gadget-supplies-scroll-bars?;


/// Updating scrollbars

define thread variable *inhibit-updating-scroll-bars?* = #f;
define thread variable *inhibit-updating-scroll-bars-viewports* = #f;

// Inhibit updating any scroll bars for the duration of the body
define macro inhibit-updating-scroll-bars
  { inhibit-updating-scroll-bars ?:body end }
    => { dynamic-bind (*inhibit-updating-scroll-bars-viewports* = #())
	   block ()
	     dynamic-bind (*inhibit-updating-scroll-bars?* = #t)
               ?body
             end
	   cleanup
	     for (_viewport in *inhibit-updating-scroll-bars-viewports*)
	       when (_viewport)
		 update-scroll-bars(_viewport)
	       end
	     end
	   end
	 end }
end macro inhibit-updating-scroll-bars;

define method update-scroll-bars
    (sheet :: <scrolling-sheet-mixin>) => ()
  if (*inhibit-updating-scroll-bars?*)
    // Who cares that this is not the most efficient thing in the world...
    unless (member?(sheet, *inhibit-updating-scroll-bars-viewports*))
      push!(*inhibit-updating-scroll-bars-viewports*, sheet)
    end
  else
    update-dynamic-scroll-bars(sheet);
    let horizontal-bar = sheet-horizontal-scroll-bar(sheet);
    let vertical-bar   = sheet-vertical-scroll-bar(sheet);
    let (left, top, right, bottom)     = sheet-scroll-range(sheet);
    let (vleft, vtop, vright, vbottom) = sheet-visible-range(sheet);
    when (vertical-bar)
      update-scroll-bar(vertical-bar, top, bottom, vtop, vbottom)
    end;
    when (horizontal-bar)
      update-scroll-bar(horizontal-bar, left, right, vleft, vright)
    end
  end
end method update-scroll-bars;


/// Dynamic scroll bars

define method update-dynamic-scroll-bars
    (sheet :: <scrolling-sheet-mixin>, #key relayout? = #f) => ()
  let (changed?, hscroll-bar, hscroll-enabled?, vscroll-bar, vscroll-enabled?)
    = compute-dynamic-scroll-bar-values(sheet);
  when (changed?)
    //--- Really the back-end should take care of graying out (or hiding)
    //--- scroll bars when everything in the viewport is visible
    when (hscroll-bar)
      gadget-enabled?(hscroll-bar) := hscroll-enabled?
    end;
    when (vscroll-bar)
      gadget-enabled?(vscroll-bar) := vscroll-enabled?
    end;
    when (relayout?)
      let scroller = sheet-scroller(sheet);
      when (scroller)
        relayout-children(scroller)
      end
    end
  end
end method update-dynamic-scroll-bars;

define method compute-dynamic-scroll-bar-values
    (sheet :: <scrolling-sheet-mixin>)
 => (changed? :: <boolean>,
     horizontal-bar, hscroll-enabled? :: <boolean>,
     vertical-bar,   vscroll-enabled? :: <boolean>)
  let scroller = sheet-scroller(sheet);
  if (scroller & gadget-scroll-bars(scroller) == #"dynamic")
    let horizontal-bar = sheet-horizontal-scroll-bar(sheet);
    let vertical-bar   = sheet-vertical-scroll-bar(sheet);
    let (left, top, right, bottom)     = sheet-scroll-range(sheet);
    let (vleft, vtop, vright, vbottom) = sheet-visible-range(sheet);
    let oh-enabled? = gadget-enabled?(horizontal-bar);
    let ov-enabled? = gadget-enabled?(vertical-bar);
    let nh-enabled? = left ~= vleft | right ~= vright;
    let nv-enabled? = top ~= vtop | bottom ~= vbottom;
    values(~(oh-enabled? == nh-enabled?  & ov-enabled? == nv-enabled?),
	   ~(oh-enabled? == nh-enabled?) & horizontal-bar, nh-enabled?,
	   ~(ov-enabled? == nv-enabled?) & vertical-bar,   nv-enabled?)
  else
    values(#f, #f, #f, #f, #f)
  end
end method compute-dynamic-scroll-bar-values;

define method sheet-scroller
    (sheet :: <scrolling-sheet-mixin>) => (scroller :: false-or(<sheet>))
  let layout = sheet-parent(sheet);
  let scroller = layout & sheet-parent(layout);
  instance?(scroller, <scroller>) & scroller
end method sheet-scroller;


/// Viewport fenceposts... don't ask

define generic viewport-fencepost? (object) => (true? :: <boolean>);

define method viewport-fencepost? (sheet) => (true? :: <boolean>)
  #f
end method viewport-fencepost?;


/// Callbacks on gadgets generate these events

define open abstract class <gadget-event> (<basic-event>)
  sealed constant slot event-gadget :: false-or(<gadget>),
    required-init-keyword: gadget:;
end class <gadget-event>;

define sealed method event-client
    (event :: <gadget-event>) => (gadget :: false-or(<gadget>))
  event-gadget(event)
end method event-client;


define sealed class <activate-gadget-event> (<gadget-event>)
end class <activate-gadget-event>;

define sealed domain make (singleton(<activate-gadget-event>));
define sealed domain initialize (<activate-gadget-event>);

define sealed method handle-event
    (gadget :: <action-gadget-mixin>, event :: <activate-gadget-event>) => ()
  execute-activate-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define function distribute-activate-callback
    (gadget :: <action-gadget-mixin>) => ()
  distribute-event(port(gadget),
		   make(<activate-gadget-event>,
			gadget: gadget))
end function distribute-activate-callback;


define sealed class <value-changed-gadget-event> (<gadget-event>)
  sealed constant slot event-value,
    required-init-keyword: value:;
end class <value-changed-gadget-event>;

define sealed domain make (singleton(<value-changed-gadget-event>));
define sealed domain initialize (<value-changed-gadget-event>);

define sealed method handle-event
    (gadget :: <value-gadget-mixin>, event :: <value-changed-gadget-event>) => ()
  gadget-value(gadget, do-callback?: #t) := event-value(event)
end method handle-event;

define function distribute-value-changed-callback
    (gadget :: <value-gadget-mixin>, value) => ()
  distribute-event(port(gadget),
		   make(<value-changed-gadget-event>,
			gadget: gadget,
			value: value))
end function distribute-value-changed-callback;


define sealed class <value-changing-gadget-event> (<gadget-event>)
  sealed constant slot event-value,
    required-init-keyword: value:;
end class <value-changing-gadget-event>;

define sealed domain make (singleton(<value-changing-gadget-event>));
define sealed domain initialize (<value-changing-gadget-event>);

define sealed method handle-event
    (gadget :: <changing-value-gadget-mixin>, event :: <value-changing-gadget-event>) => ()
  gadget-value(gadget) := event-value(event);
  execute-value-changing-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define function distribute-value-changing-callback
    (gadget :: <changing-value-gadget-mixin>, value) => ()
  distribute-event(port(gadget),
		   make(<value-changing-gadget-event>,
			gadget: gadget,
			value: value))
end function distribute-value-changing-callback;


define sealed class <key-press-gadget-event> 
    (<gadget-event>)
  sealed constant slot event-key-name = #f,
    init-keyword: key-name:;
end class <key-press-gadget-event>;

define sealed domain make (singleton(<key-press-gadget-event>));
define sealed domain initialize (<key-press-gadget-event>);

define sealed method handle-event
    (gadget :: <key-press-gadget-mixin>, event :: <key-press-gadget-event>) => ()
  execute-key-press-callback
    (gadget, gadget-client(gadget), gadget-id(gadget), event-key-name(event))
end method handle-event;

define function distribute-key-press-callback
    (gadget :: <key-press-gadget-mixin>, keysym) => ()
  distribute-event(port(gadget), 
		   make(<key-press-gadget-event>,
			gadget: gadget,
			key-name: keysym))
end function distribute-key-press-callback;


define sealed class <popup-menu-gadget-event> 
    (<gadget-event>)
  sealed constant slot event-target,
    required-init-keyword: target:;
  sealed constant slot event-x :: false-or(<integer>) = #f,
    init-keyword: x:;
  sealed constant slot event-y :: false-or(<integer>) = #f,
    init-keyword: y:;
end class <popup-menu-gadget-event>;

define sealed domain make (singleton(<popup-menu-gadget-event>));
define sealed domain initialize (<popup-menu-gadget-event>);

define sealed method handle-event
    (gadget :: <popup-menu-gadget-mixin>, event :: <popup-menu-gadget-event>) => ()
  execute-popup-menu-callback
    (gadget, gadget-client(gadget), gadget-id(gadget), event-target(event),
     x: event-x(event), y: event-y(event))
end method handle-event;

define function distribute-popup-menu-callback
    (gadget :: <popup-menu-gadget-mixin>, target, #key x, y) => ()
  distribute-event(port(gadget), 
		   make(<popup-menu-gadget-event>,
			gadget: gadget,
			target: target,
			x: x, y: y))
end function distribute-popup-menu-callback;


define sealed class <update-gadget-event> (<gadget-event>)
end class <update-gadget-event>;

define sealed domain make (singleton(<update-gadget-event>));
define sealed domain initialize (<update-gadget-event>);

define sealed method handle-event
    (gadget :: <updatable-gadget-mixin>, event :: <update-gadget-event>) => ()
  execute-update-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define function distribute-update-callback
    (gadget :: <updatable-gadget-mixin>) => ()
  distribute-event(port(gadget), 
		   make(<update-gadget-event>,
			gadget: gadget))
end function distribute-update-callback;
