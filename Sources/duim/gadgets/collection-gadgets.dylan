Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

//---*** These should really be computed by the backend
define constant $progress-bar-best-width    :: <integer> = 120;
define constant $status-bar-label-min-width :: <integer> = 150;


/// Collection gadgets

define constant <selection-specifier> = type-union(one-of(#"all"), <sequence>);

define open abstract class <collection-gadget> (<value-gadget>)
end class <collection-gadget>;

define protocol <<collection-gadget-protocol>> ()
  getter gadget-items
    (gadget :: <collection-gadget>) => (items :: <sequence>);
  getter gadget-items-setter 
    (items :: <sequence>, gadget :: <collection-gadget>) => (items :: <sequence>);
  function note-gadget-items-changed
    (gadget :: <collection-gadget>) => ();
  getter gadget-test
    (gadget :: <collection-gadget>) => (test :: <function>);
  getter gadget-selection
    (gadget :: <collection-gadget>) => (selection :: <sequence>);
  getter gadget-selection-setter 
    (selection-specifier :: <selection-specifier>, gadget :: <collection-gadget>,
     #key do-callback?)
 => (selection :: <sequence>);
  function note-gadget-selection-changed
    (gadget :: <collection-gadget>) => ();
end protocol <<collection-gadget-protocol>>;

// Mix-in for gadgets that operate upon collections (e.g. button-boxes)
define open abstract class <collection-gadget-mixin> 
    (<value-gadget-mixin>,
     <collection-gadget>)
  slot gadget-items :: <sequence> = #[],
    setter: %items-setter,
    init-keyword: items:;
  sealed slot gadget-label-key :: <function> = collection-gadget-default-label-key,
    init-keyword: label-key:;
  sealed slot gadget-value-key :: <function> = collection-gadget-default-value-key,
    init-keyword: value-key:;
  sealed slot gadget-test :: <function> = \==,
    init-keyword: test:;
end class <collection-gadget-mixin>;

define method initialize
    (gadget :: <collection-gadget-mixin>, #key name-key) => ()
  //---*** Hack to remove when name-key has really gone
  when (name-key)
    gadget-label-key(gadget) := name-key
  end;
  next-method()
end method initialize;

define method gadget-items-setter
    (items :: <sequence>, gadget :: <collection-gadget-mixin>)
 => (items :: <sequence>)
  unless (items = gadget-items(gadget))
    gadget.%items := items;
    note-gadget-items-changed(gadget)
  end;
  items
end method gadget-items-setter;

define method note-gadget-items-changed
    (gadget :: <collection-gadget>) => ()
  #f
end method note-gadget-items-changed;

define method gadget-item-value
    (gadget :: <collection-gadget-mixin>, item) => (value)
  let value-key = gadget-value-key(gadget);
  value-key(item)
end method gadget-item-value;

define method gadget-item-label
    (gadget :: <collection-gadget-mixin>, item)
 => (label)
  let label-key = gadget-label-key(gadget);
  label-key(item)
end method gadget-item-label;

define method collection-gadget-item-label
    (gadget :: <collection-gadget-mixin>, item)
 => (label :: <string>)
  let label = gadget-label-key(gadget)(item);
  assert(instance?(label, <string>),
	 "'gadget-label-key' returned non-string %= for gadget %=", label, gadget);
  let ampersand = position(label, '&');
  if (ampersand & (ampersand < size(label) - 1))
    remove(label, '&', count: 1)
  else
    label
  end
end method collection-gadget-item-label;

define macro with-preserved-selection
  { with-preserved-selection (?gadget:expression)
      ?body:body
    end }
 => { let gadget = ?gadget;
      let value = gadget-value(gadget);
      // Clear the selection, since it may no longer be valid
      gadget-selection(gadget) := #[];
      ?body;
      when (value)
        gadget-value(gadget) := value
      end }
end macro with-preserved-selection;

define method update-gadget
    (gadget :: <collection-gadget-mixin>) => ()
  with-preserved-selection (gadget)
    note-gadget-items-changed(gadget)
  end
end method update-gadget;

//--- Can we do anything better than this?
define method gadget-value-type
    (gadget :: <collection-gadget-mixin>) => (type :: <type>)
  select (gadget-selection-mode(gadget))
    #"multiple" => <sequence>;
    otherwise   => <object>;
  end
end method gadget-value-type;


// Mix-in to allow a selection of zero or more items
// Note: This must come before <collection-gadget-mixin> in any CPL,
// as it overrides some of the behavior.
define open abstract class <gadget-selection-mixin> (<collection-gadget>)
  sealed slot gadget-selection :: <sequence> = #[],
    init-keyword: selection:,
    setter: %selection-setter;
end class <gadget-selection-mixin>;

define method initialize 
    (gadget :: <gadget-selection-mixin>,
     #key keep-selection-visible? = #t, value = $unsupplied) => ()
  next-method();
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%keep_selection_visible)),
	      if (keep-selection-visible?) %keep_selection_visible else 0 end);
  if (unsupplied?(value))
    let selection = gadget-selection(gadget);
    let items = gadget-items(gadget);
    when (empty?(selection)
	  & items & size(items) > 0
	  & gadget-selection-mode(gadget) = #"single")
      gadget-selection(gadget) := #[0]
    end
  else
    gadget-selection(gadget) := gadget-selection-for-value(gadget, value)
  end
end method initialize;

define sealed method gadget-selection-setter
    (selection :: <sequence>, gadget :: <gadget-selection-mixin>,
     #key do-callback? = #f)
 => (selection :: <sequence>)
  unless (selection = gadget-selection(gadget))
    let items-size = size(gadget-items(gadget));
    for (key in selection)
      assert(instance?(key, <integer>) & key >= 0 & key < items-size,
	     "Invalid key %= in selection for %=",
	     key, gadget)
    end;
    gadget.%selection := selection;
    when (do-callback?)
      execute-value-changed-callback
	(gadget, gadget-client(gadget), gadget-id(gadget))
    end;
    note-gadget-value-changed(gadget);
    note-gadget-selection-changed(gadget)
  end;
  selection
end method gadget-selection-setter;

define sealed method gadget-selection-setter
    (selection == #"all", gadget :: <gadget-selection-mixin>, 
     #key do-callback? = #f)
 => (new-selection :: <sequence>)
  assert(gadget-selection-mode(gadget) == #"multiple",
         "Cannot set selection of non-multiple selection gadget to %=: %=",
         selection, gadget);
  let new-selection = range(from: 0, below: size(gadget-items(gadget)));
  gadget-selection-setter(new-selection, gadget, do-callback?: do-callback?)
end method gadget-selection-setter;

define method gadget-items-setter
    (items :: <sequence>, gadget :: <gadget-selection-mixin>)
 => (items :: <sequence>)
  with-preserved-selection (gadget)
    next-method()
  end;
  items
end method gadget-items-setter;

define method note-gadget-selection-changed
    (gadget :: <gadget-selection-mixin>) => ()
  #f
end method note-gadget-selection-changed;

define method gadget-value (gadget :: <gadget-selection-mixin>) => (value)
  let items = gadget-items(gadget);
  let selection = gadget-selection(gadget);
  when (selection)
    select (gadget-selection-mode(gadget))
      #"single" =>
	unless (empty?(selection))
	  gadget-item-value(gadget, items[selection[0]])
	end;
      #"multiple" =>
	map-as(<simple-vector>,
	       method (index :: <integer>)
		 gadget-item-value(gadget, items[index])
	       end,
	       selection);
      #"none" =>
	#f;
    end
  end
end method gadget-value;

define sealed method gadget-value-index
    (gadget :: <gadget-selection-mixin>, value)
 => (index :: false-or(<integer>))
  let test = gadget-test(gadget);
  find-key(gadget-items(gadget),
           method (item)
             test(gadget-item-value(gadget, item), value)
           end)
end method gadget-value-index;

define sealed method gadget-selection-for-value 
    (gadget :: <gadget-selection-mixin>, value) => (selection :: <sequence>)
  select (gadget-selection-mode(gadget))
    #"single" =>
      let index = gadget-value-index(gadget, value);
      if (index) vector(index) else #[] end;
    #"multiple" =>
      error("Non-sequence %= supplied as value for multiple selection gadget %=",
	    value, gadget);
  end
end method gadget-selection-for-value;
  
define sealed method gadget-selection-for-value
    (gadget :: <gadget-selection-mixin>, value :: <collection>)
 => (selection :: <sequence>)
  select (gadget-selection-mode(gadget))
    #"single" =>
      next-method();
    #"multiple" =>
      let indices
	= map-as(<simple-vector>,
		 method (subvalue)
		   gadget-value-index(gadget, subvalue)
		 end,
		 value);
      remove!(indices, #f)
  end
end method gadget-selection-for-value;

define sealed method do-gadget-value-setter 
    (gadget :: <gadget-selection-mixin>, value) => ()
  unless (value = gadget-value(gadget))
    let selection = gadget-selection-for-value(gadget, value);
    gadget.%selection := selection;
    note-gadget-selection-changed(gadget)
  end
end method do-gadget-value-setter;
  
// Returns #t if the item is selected in the collection gadget
define sealed method gadget-item-selected?
    (gadget :: <collection-gadget-mixin>, item) => (true? :: <boolean>)
  let value = gadget-value(gadget);
  let test = gadget-test(gadget);
  let mode = gadget-selection-mode(gadget);
  select (mode)
    #"single"   => test(gadget-item-value(gadget, item), value);
    #"multiple" => member?(gadget-item-value(gadget, item), value, test: test);
    #"none"     => #f;
  end
end method gadget-item-selected?;

define sealed inline method gadget-keep-selection-visible?
    (gadget :: <collection-gadget-mixin>) => (true? :: <boolean>)
  logand(gadget-flags(gadget), %keep_selection_visible) = %keep_selection_visible
end method gadget-keep-selection-visible?;


/// Collection gadget state

define sealed class <collection-gadget-state> (<value-gadget-state>)
  sealed constant slot %state-items :: <sequence>,
    required-init-keyword: items:;
end class <collection-gadget-state>;

define sealed domain make (singleton(<collection-gadget-state>));
define sealed domain initialize (<collection-gadget-state>);

define method gadget-state
    (gadget :: <collection-gadget>) => (state :: <collection-gadget-state>)
  make(<collection-gadget-state>,
       value: gadget-value(gadget),
       items: gadget-items(gadget))
end method gadget-state;

define method gadget-state-setter
    (state :: <collection-gadget-state>, gadget :: <collection-gadget>)
 => (state :: <collection-gadget-state>)
  gadget-items(gadget) := state.%state-items;
  next-method()
end method gadget-state-setter;


// The base class for hairy controls like list and tree controls, etc
define open abstract class <basic-choice-gadget> 
    (<gadget-selection-mixin>,
     <collection-gadget-mixin>,
     <basic-gadget>)
  sealed slot gadget-selection-mode :: <selection-mode> = #"single",
    init-keyword: selection-mode:;
end class <basic-choice-gadget>;


/// List boxes

// This is both an action gadget and a value gadget. Changing the
// selection invokes the value-changed-callback, and double clicking
// invokes the activate-callback.
// When 'read-only?: #t', this is like a Windows list box.
// When 'read-only?: #f', this is like a Windows combo box, if one existed.
define open abstract class <list-box>
    (<bordered-gadget-mixin>,
     <scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <basic-choice-gadget>)
  sealed constant slot gadget-lines :: false-or(<integer>) = #f,
    init-keyword: lines:;
  keyword read-only?: = #t;
end class <list-box>;


/// Option boxes

// An option box is like a list box, except that it only displays a single
// selection, and you pull it down to make a different selection.
// When 'read-only?: #t', this is like a Windows drop-down list box.
// When 'read-only?: #f', this is like a Windows drop-down combo box.
define open abstract class <option-box>
    (<bordered-gadget-mixin>,
     <scrolling-gadget-mixin>,
     // No <action-gadget-mixin> because you can't double click!
     <basic-choice-gadget>)
  keyword read-only?: = #t;
end class <option-box>;

define sealed method gadget-selection-mode 
    (pane :: <option-box>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;


/// Combo boxes

// As it turns out, in real life we don't actually model 'read-only?: #f'
// list and option boxes as combo boxes, because we need all the text gadget
// protocols as well.  So we have <combo-box>, too...
define open abstract class <combo-box>
    (<bordered-gadget-mixin>,
     <scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <changing-value-gadget-mixin>,
     <text-gadget>,
     <basic-choice-gadget>)
  // Same deal as <text-field>...
  sealed slot gadget-text-buffer :: <string> = "",
    init-keyword: text:;
  constant slot gadget-value-type :: <type> = <string>,
    init-keyword: value-type:;
  sealed slot text-field-maximum-size :: false-or(<integer>) = #f,
    init-keyword: maximum-size:;
  keyword read-only?: = #f;
end class <combo-box>;

define sealed method gadget-selection-mode 
    (pane :: <combo-box>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;

define method gadget-value
    (gadget :: <combo-box>) => (value)
  gadget-text-parser(gadget-value-type(gadget), gadget-text(gadget))
end method gadget-value;

// Back-ends where 'gadget-text' and 'gadget-text-buffer' use different
// representations should specialize this method
define method do-gadget-value-setter
    (gadget :: <combo-box>, value) => ()
  let text = gadget-value-printer(gadget-value-type(gadget), value);
  unless (text = gadget-text(gadget))
    gadget-text-buffer(gadget) := text;
    note-gadget-text-changed(gadget)
  end
end method do-gadget-value-setter;


/// Spin boxes

// You can get "integer spin boxes" by specifying 'items: range(...)',
// or <option-pane>-like spin boxes by specifying a set of items
define open abstract class <spin-box>
    (<bordered-gadget-mixin>,
     <gadget-selection-mixin>,
     <collection-gadget-mixin>,
     <basic-gadget>)
end class <spin-box>;

define sealed method gadget-selection-mode 
    (pane :: <spin-box>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;

define method gadget-label
    (pane :: <spin-box>) => (label :: <string>)
  let selection = gadget-selection(pane);
  case
    empty?(selection) =>
      "";
    otherwise =>
      let item = gadget-items(pane)[selection[0]];
      gadget-label-key(pane)(item)
  end
end method gadget-label;


/// Gadget Boxes

// A gadget box is a value gadget whose value is given by a selection
// of zero of more buttons contained with the box.  For example, radio
// and check boxes.
define open abstract class <gadget-box>
    (<oriented-gadget-mixin>,
     <gadget-command-mixin>,
     <collection-gadget-mixin>,
     <updatable-gadget-mixin>)
end class <gadget-box>;


/// Tool bars

define abstract class <gadget-bar-mixin>
    (<updatable-gadget-mixin>)
end class <gadget-bar-mixin>;

define open abstract class <tool-bar>
    (<no-value-gadget-mixin>,
     <gadget-bar-mixin>,
     <basic-gadget>)
end class <tool-bar>;

define sealed class <tool-bar-pane>
    (<tool-bar>, <single-child-wrapping-pane>)
end class <tool-bar-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <tool-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<tool-bar-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<tool-bar-pane>));
define sealed domain initialize (<tool-bar-pane>);


/// Status bars

define open abstract class <status-bar>
    (<value-gadget-mixin>,
     <range-gadget-mixin>,
     <bordered-gadget-mixin>,
     <labelled-gadget-mixin>,
     <gadget-bar-mixin>,
     <basic-gadget>)
  sealed slot status-bar-label-pane :: false-or(<labelled-gadget-mixin>) = #f,
    init-keyword: label-pane:;
  sealed slot status-bar-progress-bar :: false-or(<progress-bar>) = #f,
    init-keyword: progress-bar:;
end class <status-bar>;

define method initialize 
    (pane :: <status-bar>,
     #key frame-manager: framem, label, value, value-range, progress-bar?)
 => ()
  next-method();
  when (empty?(sheet-children(pane)))
    with-frame-manager (framem)
      let label-pane
	= make(<label>, label: label | "", 
	       min-width: $status-bar-label-min-width, max-width: $fill);
      status-bar-label-pane(pane) := label-pane;
      let children
	= if (progress-bar? | (value | value-range))
	    let best-width  = $progress-bar-best-width;
	    let value-range = value-range | range(from: 0, to: 100);
            let progress-bar
              = make(<progress-bar>, 
		     value-range: value-range,
		     value:       value | value-range[0],
		     withdrawn?:  ~value,
		     min-width: best-width, max-width: best-width);
            status-bar-progress-bar(pane) := progress-bar;
            vector(label-pane, progress-bar)
	  else
	    vector(label-pane)
	  end;
      sheet-children(pane) := children
    end
  end;
  gadget-label(pane) := label
end method initialize;

// The idea is that a status bar can hold at least a label pane
// and a progress control...
define open generic status-bar-label-pane
    (status-bar :: <status-bar>)
 => (label-pane :: false-or(<labelled-gadget-mixin>));
define open generic status-bar-progress-bar
    (status-bar :: <status-bar>)
 => (progress-bar :: false-or(<progress-bar>));

// The label of a status bar is the label of the label pane in the
// status bar, if it has one.  Setting the label updates the label pane.
define method gadget-label 
    (status-bar :: <status-bar>) => (message :: false-or(<string>))
  let label-pane = status-bar-label-pane(status-bar);
  label-pane & gadget-label(label-pane)
end method gadget-label;

define method gadget-label-setter
    (message :: <string>, status-bar :: <status-bar>) => (message :: <string>)
  let label-pane = status-bar-label-pane(status-bar);
  label-pane & (gadget-label(label-pane) := message);
  message
end method gadget-label-setter;

// The value of a status bar returns the value of the progress
// control in the status bar, if it has one.  Setting the value
// updates the progress control.
define method gadget-value
    (status-bar :: <status-bar>) => (value :: false-or(<real>))
  let progress-bar = status-bar-progress-bar(status-bar);
  progress-bar
    & ~sheet-withdrawn?(progress-bar)
    & gadget-value(progress-bar)
end method gadget-value;

// Allow #f meaning don't display the progress bar if possible
define method normalize-gadget-value
    (status-bar :: <status-bar>, value :: false-or(<real>))
 => (value :: false-or(<real>))
  let progress-bar = status-bar-progress-bar(status-bar);
  if (progress-bar)
    value & normalize-gadget-value(progress-bar, value)
  else
    next-method()
  end
end method normalize-gadget-value;

define method do-gadget-value-setter
    (status-bar :: <status-bar>, value :: false-or(<real>)) => ()
  let progress-bar = status-bar-progress-bar(status-bar);
  when (progress-bar)
    if (value)
      sheet-withdrawn?(progress-bar) := #f;
      gadget-value(progress-bar) := value
    else
      sheet-withdrawn?(progress-bar) := #t
    end;
    if (sheet-mapped?(status-bar))
      relayout-children(status-bar);
      sheet-mapped?(progress-bar) := true?(value)
    end
  end
end method do-gadget-value-setter;

define method gadget-value-range-setter 
    (range :: <range>, status-bar :: <status-bar>)
 => (range :: <range>)
  next-method();
  let progress-bar = status-bar-progress-bar(status-bar);
  when (progress-bar)
    gadget-value-range(progress-bar) := range
  end;
  range
end method gadget-value-range-setter;


/// Status bar pane

define sealed class <status-bar-pane>
    (<status-bar>, <row-layout>)
  inherited slot layout-x-spacing = 4;
  inherited slot layout-y-alignment = #"center";
end class <status-bar-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <status-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<status-bar-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<status-bar-pane>));
define sealed domain initialize (<status-bar-pane>);


/// Button box [exclusive-choice] .. [inclusive-choice]

define open abstract class <button-box>
    (<gadget-box>, <basic-gadget>)
end class <button-box>;

define function button-box-selection-mode-class
    (selection-mode :: <selection-mode>) => (class :: <class>)
  select (selection-mode)
    #"none"     => <push-box>;
    #"single"   => <radio-box>;
    #"multiple" => <check-box>;
  end;
end function button-box-selection-mode-class;

define sealed inline method make
    (class == <button-box>, #rest initargs, 
     #key selection-mode :: <selection-mode> = #"none", #all-keys)
 => (button :: <button-box>)
  apply(make, button-box-selection-mode-class(selection-mode), initargs)
end method make;


/// Push box

define open abstract class <push-box> 
    (<action-gadget-mixin>,
     <button-box>,
     <basic-value-gadget>)
end class <push-box>;

define sealed method gadget-selection-mode 
    (box :: <push-box>) => (selection-mode :: <selection-mode>)
  #"none"
end method gadget-selection-mode;


/// Radio boxes

// Radio box [exclusive-choice] .. [inclusive-choice]
define open abstract class <radio-box>
    (<gadget-selection-mixin>,
     <action-gadget-mixin>,
     <button-box>)
end class <radio-box>;

define sealed method gadget-selection-mode
    (box :: <radio-box>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;


/// Check-box

define open abstract class <check-box>
    (<gadget-selection-mixin>,
     <action-gadget-mixin>,
     <button-box>)
end class <check-box>;

define sealed method gadget-selection-mode
    (box :: <check-box>) => (selection-mode :: <selection-mode>)
  #"multiple"
end method gadget-selection-mode;


/// Some useful default methods

define method collection-gadget-default-label-key
    (object) => (name :: <string>)
  format-to-string("%s", object)
end method collection-gadget-default-label-key;

define method collection-gadget-default-label-key
    (string :: <string>) => (name :: <string>)
  string
end method collection-gadget-default-label-key;

define method collection-gadget-default-label-key 
    (n :: <integer>) => (name :: <string>)
  integer-to-string(n)
end method collection-gadget-default-label-key;


define method collection-gadget-default-value-key
    (object) => (value)
  object
end method collection-gadget-default-value-key;


/// Event handling

define sealed class <selection-changed-gadget-event> (<gadget-event>)
  sealed constant slot event-selection :: <sequence>,
    required-init-keyword: selection:;
end class <selection-changed-gadget-event>;

define sealed domain make (singleton(<selection-changed-gadget-event>));
define sealed domain initialize (<selection-changed-gadget-event>);

define method handle-event
    (gadget :: <gadget-selection-mixin>,
     event :: <selection-changed-gadget-event>) => ()
  gadget-selection(gadget, do-callback?: #t) := event-selection(event);
end method handle-event;

define function distribute-selection-changed-callback
    (gadget :: <gadget-selection-mixin>, selection :: <sequence>) => ()
  distribute-event(port(gadget),
		   make(<selection-changed-gadget-event>,
                        selection: selection,
			gadget: gadget))
end function distribute-selection-changed-callback;
