Module:    mini-duim
Synopsis:  Mini-DUIM collection gadgets
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Mix-in for gadgets that operate upon collections (e.g. button-boxes)
define open abstract class <collection-gadget-mixin> (<value-gadget-mixin>)
  slot gadget-items :: <sequence> = make(<stretchy-vector>),
    init-keyword: items:,
    setter: %items-setter;
  slot gadget-name-key :: <function> = collection-gadget-default-name-key,
    init-keyword: name-key:;
  slot gadget-value-key :: <function> = collection-gadget-default-value-key,
    init-keyword: value-key:;
  slot gadget-test :: <function> = \==,
    init-keyword: test:;
end class <collection-gadget-mixin>;

define method gadget-items-setter
    (items :: <sequence>, gadget :: <collection-gadget-mixin>)
 => (items :: <sequence>)
  let old-items = gadget-items(gadget);
  unless (items = old-items)
    gadget.%items := items;
    note-gadget-items-changed(gadget, old-items)
  end;
  items
end method gadget-items-setter;

define method note-gadget-items-changed
    (gadget :: <gadget>, old-items) => ()
  ignore(old-items);
  #f
end method note-gadget-items-changed;

define method gadget-item-value
    (gadget :: <collection-gadget-mixin>, item) => (value)
  let value-key = gadget-value-key(gadget);
  value-key(item);
end method gadget-item-value;

define method gadget-item-name
    (gadget :: <collection-gadget-mixin>, item) => (name :: <string>)
  let name-key = gadget-name-key(gadget);
  name-key(item);
end method gadget-item-name;


// Mix-in to allow a selection of zero or more items
// Note: This must come after <collection-gadget-mixin> as it overrides
// some of the behavior.
define open abstract class <gadget-selection-mixin> (<gadget>)
  slot gadget-selection :: <sequence> = #[],
    init-keyword: selection:,
    setter: %selection-setter;
end class <gadget-selection-mixin>;

define method initialize 
    (gadget :: <gadget-selection-mixin>, #key value = $unsupplied) => ()
  next-method();
  if (unsupplied?(value))
    let selection = gadget-selection(gadget);
    let items = gadget-items(gadget);
    if (empty?(selection)
	& items
	& size(items) > 0
	& gadget-selection-mode(gadget) = #"single")
      gadget-selection(gadget) := #[0]
    end
  else
    gadget-selection(gadget) := gadget-selection-for-value(gadget, value)
  end
end method initialize;

define method gadget-selection-setter
    (selection :: <sequence>, gadget :: <gadget-selection-mixin>,
     #key do-callback? = #f)
 => (selection :: <sequence>)
  let old-value = gadget-value(gadget);
  let old-selection = gadget-selection(gadget);
  unless (selection = old-selection)
    gadget.%selection := selection;
    if (do-callback?)
      execute-value-changed-callback
        (gadget, gadget-client(gadget), gadget-id(gadget),
	 gadget-value(gadget))
    end;
    note-gadget-value-changed(gadget, old-value);
    note-gadget-selection-changed(gadget, old-selection)
  end;
  selection
end method gadget-selection-setter;

define method gadget-selection-setter
    (selection == #"all", gadget :: <gadget-selection-mixin>, 
     #key do-callback? = #f)
 => (new-selection :: <sequence>)
  assert(gadget-selection-mode(gadget) == #"multiple",
         "Cannot set selection of non-multiple selection gadget %= to %=",
         gadget, selection);
  let new-selection = range(from: 0, below: size(gadget-items(gadget)));
  gadget-selection-setter(new-selection, gadget, do-callback?: do-callback?)
end method gadget-selection-setter;

define method gadget-items-setter
    (items :: <sequence>, gadget :: <gadget-selection-mixin>)
 => (items :: <sequence>)
  let old-value = gadget-value(gadget);
  gadget-selection(gadget) := #[];
  next-method();
  if (old-value)
    gadget-value(gadget) := old-value
  end;
  items
end method gadget-items-setter;

define method note-gadget-selection-changed
    (gadget :: <gadget>, old-selection) => ()
  ignore(old-selection);
  #f
end method note-gadget-selection-changed;

define method gadget-value (gadget :: <gadget-selection-mixin>) => (value)
  let items = gadget-items(gadget);
  let selection = gadget-selection(gadget);
  if (selection)
    select (gadget-selection-mode(gadget))
      #"single" =>
	unless (empty?(selection))
	  gadget-item-value(gadget, items[selection[0]])
	end;
      #"multiple" =>
	map-as(<vector>,
	       method (index)
		 gadget-item-value(gadget, items[index])
	       end,
	       selection);
      #"none" =>
	#f;
    end
  end
end method gadget-value;

define method gadget-value-index
    (gadget :: <gadget-selection-mixin>, value) => (index :: <integer>)
  let test = gadget-test(gadget);
  find-key(gadget-items(gadget),
           method (item)
             test(gadget-item-value(gadget, item), value)
           end)
end method gadget-value-index;

define method gadget-selection-for-value 
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
  
define method gadget-selection-for-value
    (gadget :: <gadget-selection-mixin>, value :: <collection>)
 => (selection :: <sequence>)
  select (gadget-selection-mode(gadget))
    #"single" =>
      next-method();
    #"multiple" =>
      let indices
	= map-as(<vector>,
		 method (subvalue)
		   gadget-value-index(gadget, subvalue)
		 end,
		 value);
      remove!(indices, #f)
  end
end method gadget-selection-for-value;

define method do-gadget-value-setter 
    (gadget :: <gadget-selection-mixin>, value) => ()
  unless (value = gadget-value(gadget))
    let old-selection = gadget-selection(gadget);
    let selection = gadget-selection-for-value(gadget, value);
    gadget.%selection := selection;
    note-gadget-selection-changed(gadget, old-selection)
  end;
end method do-gadget-value-setter;
  
// Returns #t if the item is selected in the collection gadget
define method gadget-item-selected?
    (gadget :: <gadget>, item) => (true? :: <boolean>)
  let value = gadget-value(gadget);
  let test = gadget-test(gadget);
  let mode = gadget-selection-mode(gadget);
  select (mode)
    #"single"   => test(gadget-item-value(gadget, item), value);
    #"multiple" => member?(gadget-item-value(gadget, item), value, test: test);
    #"none"     => #f;
  end
end method gadget-item-selected?;


// The base class for things like radio and check boxes
define open abstract class <choice-gadget> 
    (<gadget-selection-mixin>,
     <collection-gadget-mixin>,
     <basic-gadget>)
  slot gadget-selection-mode :: <selection-mode> = #"single",
    init-keyword: selection-mode:;
end class <choice-gadget>;


/// List boxes

// This is both an action gadget and a value gadget. Changing the
// selection invokes the value-changed-callback, and double clicking
// invokes the activate-callback.
// When 'editable?' is #f, this is like a Windows list box.
// When 'editable?' is #t, this is like a Windows combo box.
define open abstract class <list-box>
    (<scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <choice-gadget>)
  slot gadget-editable? = #t,
    init-keyword: editable?:;
end class <list-box>;


/// Option boxes

// An option box is like a list box, except that it only displays a single
// selection, and you pull it down to make a different selection.
// When 'editable?' is #f, this is like a Windows drop-down list box.
// When 'editable?' is #t, this is like a Windows drop-down combo box.
define open abstract class <option-box>
    (<scrolling-gadget-mixin>,
     <gadget-selection-mixin>,
     <collection-gadget-mixin>,
     <basic-gadget>)
  slot gadget-editable? = #t,
    init-keyword: editable?:;
end class <option-box>;

define method gadget-selection-mode 
    (pane :: <option-box>) => (selection-mode :: <selection-mode>)
  #"single"
end method gadget-selection-mode;


/// Some useful default methods

define method collection-gadget-default-name-key
    (string :: <string>) => (name :: <string>)
  string
end method collection-gadget-default-name-key;

define method collection-gadget-default-name-key 
    (sequence :: <sequence>) => (name :: <string>)
  let string = sequence[0];
  assert(instance?(string, <string>),
         "The default name-key function expects a sequence to contain a name in the 0th position");
  string
end method collection-gadget-default-name-key;

// Treat integers specially since they are such a common case.
define method collection-gadget-default-name-key 
    (n :: <integer>) => (name :: <byte-string>)
  // Do the printing ourselves to avoid pulling in 'format'
  let digit-chars = "0123456789";
  local method repeat (arg, digits)
	  let (quotient, remainder) = floor/(arg, 10);
	  let digits = pair(digit-chars[remainder], digits);
	  if (zero?(quotient)) digits else repeat(quotient, digits) end;
	end method repeat;
  as(<byte-string>,
     if (negative?(n)) pair('-', repeat(-n, #())) else repeat(n, #()) end);
end method collection-gadget-default-name-key;


define method collection-gadget-default-value-key (object) => (value)
  object
end method collection-gadget-default-value-key;

define method collection-gadget-default-value-key
    (sequence :: <sequence>) => (value)
  if (size(sequence) = 2 & instance?(sequence[0], <string>))
    sequence[1]
  else
    next-method()
  end
end method collection-gadget-default-value-key;

