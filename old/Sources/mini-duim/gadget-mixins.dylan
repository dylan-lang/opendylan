Module:    mini-duim
Synopsis:  Mini-DUIM gadget mix-in classes
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Basic gadgets

define open abstract class <abstract-gadget> (<object>)
end class <abstract-gadget>;

define open abstract class <gadget> (<abstract-gadget>)
end class <gadget>;

define method make
    (pane-class :: subclass(<gadget>),
     #rest pane-options, #key frame-manager: framem) => (pane)
  let framem = framem | find-frame-manager();
  let (concrete-class, concrete-options)
    = apply(class-for-make-pane, framem, pane-class, pane-options);
  // If there's a mapping from the abstract pane class to a concrete pane
  // class, then use it.  Otherwise just try to create a class named by the
  // abstract pane class.
  if (concrete-class == pane-class)
    apply(next-method, pane-class,
	  frame-manager: framem, pane-options)
  else
    apply(make, concrete-class,
	  frame-manager: framem,
	  concrete-options | pane-options)
  end
end method make;

define open abstract class <basic-gadget> (<gadget>, <basic-sheet>)
  slot gadget-id = #f,
    init-keyword: id:;
  slot gadget-client = #f,
    init-keyword: client:;
  slot %enabled? :: <boolean> = #t,
    init-keyword: enabled?:;
  slot %label = "Unnamed",
    init-keyword: label:;
end class <basic-gadget>;

define method gadget-enabled?
    (gadget :: <basic-gadget>) => (true? :: <boolean>)
  gadget.%enabled?
end method gadget-enabled?;

define method gadget-enabled?-setter
    (enabled?, gadget :: <basic-gadget>) => (enabled?)
  unless (gadget.%enabled? == enabled?)
    gadget.%enabled? := enabled? & #t;
    if (enabled?)
      note-gadget-enabled(gadget-client(gadget), gadget)
    else
      note-gadget-disabled(gadget-client(gadget), gadget)
    end
  end
end method gadget-enabled?-setter;

define method gadget-label (gadget :: <basic-gadget>) => (label)
  gadget.%label
end method gadget-label;

define method gadget-label-setter 
    (label, gadget :: <basic-gadget>) => (label)
  gadget.%label := label;
  note-gadget-label-changed(gadget);
  label
end method gadget-label-setter;

define method gadget-label-size
    (framem :: <frame-manager>, gadget :: <basic-gadget>)
 => (width :: <integer>, height :: <integer>)
/*
  let label = gadget-label(gadget);
  select (label by instance?)
    <string> =>
      with-sheet-medium (medium = gadget)
        let (width, height) = text-size(medium, label, do-newlines?: #t);
        // Add a little slop so that label doesn't get too squeezed
        values(width + 2, height + font-descent(text-style, medium))
      end;
    <image> =>
      values(image-width(label), image-height(label));
    <image-label> =>
      values(image-width(label.%normal-label), image-height(label.%normal-label));
    singleton(#f) =>
      values(0, 0);
end
*/
  values(100, 40)
end method gadget-label-size;

define method note-gadget-label-changed
    (gadget :: <basic-gadget>) => ()
  ignore(gadget);
  #f
end method note-gadget-label-changed;


define method execute-callback
    (gadget :: <basic-gadget>, function :: <function>, #rest args) => ()
  ignore(gadget);
  apply(function, args)
end method execute-callback;


/// Action gadgets

define open abstract class <action-gadget-mixin> (<gadget>)
  slot gadget-activate-callback :: false-or(<function>) = #f,
    init-keyword: activate-callback:;
end class <action-gadget-mixin>;

define method execute-activate-callback
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
  ignore(gadget, client, id);
  #f
end method do-execute-activate-callback;


/// Value gadgets

// This class can be used for things that act like value gadgets, but whose
// value is computed on the fly
define open abstract class <value-gadget-mixin> (<gadget>)
  slot %value = #f,
    init-keyword: value:;
  slot gadget-value-changed-callback :: false-or(<function>) = #f,
    init-keyword: value-changed-callback:;
end class <value-gadget-mixin>;

define method initialize (gadget :: <value-gadget-mixin>, #key value)
  next-method();
  // Ensure that gadget value normalization takes place
  gadget-value(gadget) := value | gadget-value(gadget)
end method initialize;

define method normalize-gadget-value (gadget :: <basic-gadget>, value)
  ignore(gadget);
  value
end method normalize-gadget-value;

define method gadget-value (gadget :: <value-gadget-mixin>) => (value)
  gadget.%value
end method gadget-value;

// By default, we don't call the value-changed callback
define method gadget-value-setter
    (value, gadget :: <value-gadget-mixin>, #key do-callback? = #f) => (value)
  let old-value = gadget-value(gadget);
  let new-value = normalize-gadget-value(gadget, value);
  unless (new-value = old-value)
    do-gadget-value-setter(gadget, new-value);
    if (do-callback?)
      execute-value-changed-callback
        (gadget, gadget-client(gadget), gadget-id(gadget), gadget-value(gadget))
    end;
    note-gadget-value-changed(gadget, old-value)
  end;
  value
end method gadget-value-setter;

define method do-gadget-value-setter
    (gadget :: <value-gadget-mixin>, value) => ()
  gadget.%value := value
end method do-gadget-value-setter;

define method note-gadget-value-changed
    (gadget :: <basic-gadget>, old-value) => ()
  ignore(gadget, old-value);
  #f
end method note-gadget-value-changed;

define method execute-value-changed-callback
    (gadget :: <value-gadget-mixin>, client, id, value) => ()
  do-gadget-value-setter(gadget, value);
  let callback = gadget-value-changed-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget, value)
  else
    do-execute-value-changed-callback(gadget, client, id, value)
  end
end method execute-value-changed-callback;

define method do-execute-value-changed-callback
    (gadget :: <value-gadget-mixin>, client, id, value) => ()
  ignore(gadget, client, id, value);
  #f
end method do-execute-value-changed-callback;

define method handle-button-gadget-click
    (gadget :: <basic-gadget>) => (handled? :: <boolean>)
  select (gadget-selection-mode(gadget))
    #"none" =>
      distribute-activate-callback(gadget);
    #"single" =>
      unless (gadget-value(gadget))
	distribute-value-changed-callback(gadget, #t)
      end;
    #"multiple" =>
      //---*** 'gadget-value' won't be up to date if we distribute
      //---*** two of these before handling any of them
      distribute-value-changed-callback(gadget, ~gadget-value(gadget))
  end;
  #t
end method handle-button-gadget-click;


/// Oriented gadgets

define open abstract class <oriented-gadget-mixin> (<gadget>)
  slot gadget-orientation = #"horizontal",
    init-keyword: orientation:;
end class <oriented-gadget-mixin>;

//--- Hack to get around VM tether bug which can't put keywords into slots
define method initialize 
    (gadget :: <oriented-gadget-mixin>, #key orientation = #"horizontal") => ()
  next-method();
  gadget-orientation(gadget) := orientation
end method initialize;


/// Range gadgets

//--- We might want a way of changing the range and the value together.
define open abstract class <range-gadget-mixin> (<gadget>)
  // In effect, the default is a continuous slider ranging from 0 to 1
  slot %value-range :: <range> = range(from: 0.0, to: 1.0, by: 0.001),
    init-keyword: value-range:;
end class <range-gadget-mixin>;

define method gadget-value-range
    (gadget :: <range-gadget-mixin>) => (range :: <range>)
  gadget.%value-range
end method gadget-value-range;

define method gadget-value-range-setter
    (range :: <range>, gadget :: <range-gadget-mixin>)
 => (range :: <range>)
  let old-range = gadget.%value-range;
  unless (range = old-range)
    gadget.%value-range := range;
    note-gadget-value-range-changed(gadget, old-range)
  end;
  range
end method gadget-value-range-setter;

define method note-gadget-value-range-changed 
    (gadget :: <range-gadget-mixin>, old-range) => ()
  ignore(old-range);
  let value = gadget-value(gadget);
  let normalized-value = normalize-gadget-value(gadget, value);
  if (value ~= normalized-value)
    gadget-value(gadget) := normalized-value
  end
end method note-gadget-value-range-changed;

define method initialize (gadget :: <range-gadget-mixin>, #key)
  next-method();
  assert(size(gadget-value-range(gadget)),
	 "Unbounded value range for %=", gadget)
end method initialize;

define method gadget-start-value 
    (gadget :: <range-gadget-mixin>) => (start-value :: <real>)
  let range = gadget-value-range(gadget);
  range[0]
end method gadget-start-value;

define method gadget-end-value 
    (gadget :: <range-gadget-mixin>) => (end-value :: <real>)
  let range = gadget-value-range(gadget);
  range[size(range) - 1]
end method gadget-end-value;

define method gadget-value-increment 
    (gadget :: <range-gadget-mixin>) => (increment :: <real>)
  let range = gadget-value-range(gadget);
  range[1] - range[0]
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
define method gadget-line-scroll-amount (gadget :: <range-gadget-mixin>)
  line-scroll-amount(gadget, gadget-orientation(gadget))
end method gadget-line-scroll-amount;

//--- There should maybe be a user-specifiable way to set this.
define method gadget-page-scroll-amount (gadget :: <range-gadget-mixin>)
  page-scroll-amount(gadget, gadget-orientation(gadget))
end method gadget-page-scroll-amount;


/// Scrollable gadgets

// Mix-in for things that might implement their own scrolling
// Valid values for 'scroll-bars' field are:
//    #f, #"none", #"horizontal", #"vertical", #"both", or #"dynamic"
define open abstract class <scrolling-gadget-mixin> (<gadget>)
  slot gadget-scroll-bars = #"both",
    setter: %scroll-bars;
end class <scrolling-gadget-mixin>;

define method gadget-scrolling-horizontally?
    (gadget :: <scrolling-gadget-mixin>) => (horizontally? :: <boolean>)
  let scroll-bars = gadget-scroll-bars(gadget);
  scroll-bars = #"horizontal" 
    | scroll-bars = #"both"
    | scroll-bars = #"dynamic"
end method gadget-scrolling-horizontally?;

define method gadget-scrolling-vertically?
    (gadget :: <scrolling-gadget-mixin>) => (vertically? :: <boolean>)
  let scroll-bars = gadget-scroll-bars(gadget);
  scroll-bars = #"vertical"
    | scroll-bars = #"both"
    | scroll-bars = #"dynamic"
end method gadget-scrolling-vertically?;


/// Slug gadget mixin

define open abstract class <slug-gadget-mixin> (<gadget>)
  //--- 1 isn't a very good default for scroll bars
  slot %slug-size :: <real> = 1,
    init-keyword: slug-size:;
end class <slug-gadget-mixin>;

define method gadget-slug-size 
    (gadget :: <slug-gadget-mixin>) => (size :: <real>)
  gadget.%slug-size
end method gadget-slug-size;

define method gadget-slug-size-setter
    (size :: <real>, gadget :: <slug-gadget-mixin>) => (size :: <real>)
  gadget.%slug-size := size;
  note-gadget-slug-size-changed(gadget);
  size
end method gadget-slug-size;

define method note-gadget-slug-size-changed
    (gadget :: <slug-gadget-mixin>) => ()
  #f
end method note-gadget-slug-size-changed;


/// Value-changing (formerly "dragging") gadgets

define open abstract class <changing-value-gadget-mixin> (<gadget>)
  slot gadget-value-changing-callback :: false-or(<function>) = #f,
    init-keyword: value-changing-callback:;
end class <changing-value-gadget-mixin>;

define method execute-value-changing-callback 
    (gadget :: <changing-value-gadget-mixin>, client, id, value) => ()
  ignore(client, id);
  let callback = gadget-value-changing-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget, value)
  else
    do-execute-value-changing-callback(gadget, client, id, value)
  end
end method execute-value-changing-callback;

define method do-execute-value-changing-callback 
    (gadget :: <changing-value-gadget-mixin>, client, id, value) => ()
  ignore(client, id, value);
  #f
end method do-execute-value-changing-callback;


/// Focus gadget

define open abstract class <focus-gadget-mixin> (<gadget>)
  slot gadget-focus-out-callback :: false-or(<function>) = #f,
    init-keyword: focus-out-callback:;
  slot gadget-focus-in-callback :: false-or(<function>) = #f,
    init-keyword: focus-in-callback:;
end class <focus-gadget-mixin>;

define method execute-focus-in-callback
    (gadget :: <focus-gadget-mixin>, client, id) => ()
  ignore(client, id);
  let callback = gadget-focus-in-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget)
  else
    do-execute-focus-in-callback(gadget, client, id)
  end
end method execute-focus-in-callback;

define method do-execute-focus-in-callback
    (gadget :: <focus-gadget-mixin>, client, id) => ()
  ignore(client, id);
  #f
end method do-execute-focus-in-callback;


define method execute-focus-out-callback
    (gadget :: <focus-gadget-mixin>, client, id) => ()
  ignore(client, id);
  let callback = gadget-focus-out-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget)
  else
    do-execute-focus-out-callback(gadget, client, id)
  end
end method execute-focus-out-callback;

define method do-execute-focus-out-callback
    (gadget :: <focus-gadget-mixin>, client, id) => ()
  ignore(client, id);
  #f
end method do-execute-focus-out-callback;


/// Mixins for accelerators and mnemonics

define open abstract class <accelerator-mixin> (<gadget>)
  slot gadget-accelerator = #f, init-keyword: accelerator:;
end class <accelerator-mixin>;


define open abstract class <mnemonic-mixin> (<gadget>)
  slot gadget-mnemonic = #f, init-keyword: mnemonic:;
end class <mnemonic-mixin>;

// This just gets called by the backend to compute its mnemonic
define method compute-mnemonic-from-label
    (sheet :: <sheet>, label)
 => (label, mnemonic :: false-or(<character>))
  select (label by instance?)
    <string> =>
      let ampersand = find-key(label, curry(\=, '&'));
      if (ampersand & (ampersand < size(label) - 1))
	values(remove(label, '&', count: 1), label[ampersand + 1])
      else
	values(label, #f)
      end;
    otherwise =>
      values(label, #f);
  end
end method compute-mnemonic-from-label;

// An explicitly supplied mnemonic wins out over "&"...
define method compute-mnemonic-from-label
    (sheet :: <mnemonic-mixin>, label) => (label, mnemonic)
  ignore(label);
  let (label, mnemonic) = next-method();
  values(label, gadget-mnemonic(sheet) | mnemonic)
end method compute-mnemonic-from-label;


/// Callbacks on gadgets generate these events

define open abstract class <gadget-event> (<event>)
  slot event-gadget :: <gadget>,
    required-init-keyword: gadget:;
end class <gadget-event>;

define method event-client
    (event :: <gadget-event>) => (gadget :: <gadget>)
  event-gadget(event)
end method event-client;

define sealed class <activate-gadget-event> (<gadget-event>)
end class <activate-gadget-event>;

define method handle-event
    (gadget :: <action-gadget-mixin>, event :: <activate-gadget-event>) => ()
  execute-activate-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define method distribute-activate-callback
    (gadget :: <action-gadget-mixin>) => ()
  distribute-event(port(gadget),
		   make(<activate-gadget-event>,
			gadget: gadget))
end method distribute-activate-callback;


define sealed class <value-changed-gadget-event> (<gadget-event>)
  slot event-value = #f, init-keyword: value:;
end class <value-changed-gadget-event>;

define method handle-event
    (gadget :: <value-gadget-mixin>, event :: <value-changed-gadget-event>) => ()
  execute-value-changed-callback
    (gadget, gadget-client(gadget), gadget-id(gadget), event-value(event))
end method handle-event;

define method distribute-value-changed-callback
    (gadget :: <value-gadget-mixin>, value) => ()
  distribute-event(port(gadget),
		   make(<value-changed-gadget-event>,
			gadget: gadget,
			value: value))
end method distribute-value-changed-callback;


define sealed class <value-changing-gadget-event> (<gadget-event>)
  slot event-value = #f, init-keyword: value:;
end class <value-changing-gadget-event>;

define method handle-event
    (gadget :: <changing-value-gadget-mixin>, event :: <value-changing-gadget-event>) => ()
  execute-value-changing-callback
    (gadget, gadget-client(gadget), gadget-id(gadget), event-value(event))
end method handle-event;

define method distribute-value-changing-callback
    (gadget :: <changing-value-gadget-mixin>, value) => ()
  distribute-event(port(gadget),
		   make(<value-changing-gadget-event>,
			gadget: gadget,
			value: value))
end method distribute-value-changing-callback;


define sealed class <focus-in-gadget-event> (<gadget-event>)
end class <focus-in-gadget-event>;

define method handle-event
    (gadget :: <focus-gadget-mixin>, event :: <focus-in-gadget-event>) => ()
  execute-focus-in-callback
    (gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define method distribute-focus-in-callback
    (gadget :: <focus-gadget-mixin>) => ()
  distribute-event(port(gadget),
		   make(<focus-in-gadget-event>,
			gadget: gadget))
end method distribute-focus-in-callback;

define method distribute-focus-in-callback
    (sheet :: <sheet>) => ()
  #f
end method distribute-focus-in-callback;


define sealed class <focus-out-gadget-event> (<gadget-event>)
end class <focus-out-gadget-event>;

define method handle-event
    (gadget :: <focus-gadget-mixin>, event :: <focus-out-gadget-event>) => ()
  execute-focus-out-callback
    (gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define method distribute-focus-out-callback
    (gadget :: <focus-gadget-mixin>) => ()
  distribute-event(port(gadget),
		   make(<focus-out-gadget-event>,
			gadget: gadget))
end method distribute-focus-out-callback;

define method distribute-focus-out-callback
    (sheet :: <sheet>) => ()
  #f
end method distribute-focus-out-callback;

