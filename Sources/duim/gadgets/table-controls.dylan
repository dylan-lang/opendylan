Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Table controls

define constant <table-control-view>
    = one-of(#"table", #"list", #"small-icon", #"large-icon");

// The items in a table control are layed out as a sets of rows,
// and the rows can have multiple columns
// Note that items are not modelled as sheets!
define open abstract class <table-control>
    (<bordered-gadget-mixin>,
     <scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <key-press-gadget-mixin>,
     <popup-menu-gadget-mixin>,
     <basic-choice-gadget>)
  sealed constant slot gadget-lines :: false-or(<integer>) = #f,
    init-keyword: lines:;
  sealed slot table-control-view :: <table-control-view> = #"table",
    init-keyword: view:,
    setter: %table-control-view-setter;
  // Takes an object, produces two icons -- a small and a large icon
  sealed constant slot table-control-icon-function :: false-or(<function>) = #f,
    init-keyword: icon-function:;
  // Describes each column in the table
  sealed slot table-control-columns :: <sequence> = make(<stretchy-vector>),
    init-keyword: columns:;
end class <table-control>;

// 'columns' is a vector of <table-column> objects...
// ...or your must supply 'headings' and 'generators'
define method initialize
    (pane :: <table-control>,
     #rest initargs,
     #key items = #[], columns,
     headings, generators, widths, alignments, callbacks) => ()
  // Ensure the items are in a stretchy vector so we can use 'add!' and 'remove!'
  apply(next-method, pane, items: as(<stretchy-vector>, items), initargs);
  assert(columns | (headings & generators),
	 "You must supply either columns, or headings and generators");
  unless (columns)
    assert(size(headings) = size(generators),
	   "There must be as many generators as there are headings");
    when (widths)
      assert(size(headings) = size(widths),
	     "There must be as many widths as there are headings")
    end;
    when (alignments)
      assert(size(headings) = size(alignments),
	     "There must be as many alignments as there are headings")
    end;
    when (callbacks)
      assert(size(headings) = size(callbacks),
	     "There must be as many callbacks as there are headings")
    end;
    let columns = make(<stretchy-vector>);
    for (i :: <integer> from 0 below size(headings))
      let heading   = headings[i];
      let generator = generators[i];
      let width     = (widths & widths[i]) | 100;
      let alignment = (alignments & alignments[i]) | #"left";
      let callback  = (callbacks & callbacks[i]) | #f;
      add!(columns, make(<table-column>,
			 heading:   heading,
			 width:     width,
			 alignment: alignment,
			 generator: generator,
			 callback:  callback))
    end;
    table-control-columns(pane) := columns
  end
end method initialize;

define method gadget-items-setter
    (items :: <sequence>, gadget :: <table-control>)
 => (items :: <sequence>)
  // Ensure the items are in a stretchy vector so we can use 'add!' and 'remove!'
  next-method(as(<stretchy-vector>, items), gadget)
end method gadget-items-setter;


define sealed class <table-column> (<object>)
  constant slot table-column-heading :: <string>,
    required-init-keyword: heading:;
  constant slot table-column-width :: <integer> = 100,
    init-keyword: width:;
  constant slot table-column-alignment :: <x-alignment> = #"left",
    init-keyword: alignment:;
  constant slot table-column-generator :: <function>,
    required-init-keyword: generator:;
  constant slot table-column-callback :: false-or(<function>) = #f,
    init-keyword: callback:;
end class <table-column>;

define sealed domain make (singleton(<table-column>));
define sealed domain initialize (<table-column>);

define open abstract class <table-item> (<item>)
  sealed constant slot item-object = #f,
    init-keyword: object:;
end class <table-item>;

define protocol <<table-control>> (<<list-control>>)
  function add-column
    (control :: <table-control>, column :: <table-column>, index :: <integer>) => ();
  function remove-column
    (control :: <table-control>, index :: <integer>) => ();
  function do-add-column
    (control :: <table-control>, column :: <table-column>, index :: <integer>) => ();
  function do-remove-column
    (control :: <table-control>, index :: <integer>) => ();
  // Views, etc
  getter table-control-view
    (table-control :: <table-control>) => (view :: <table-control-view>);
  setter table-control-view-setter
    (view :: <table-control-view>, table-control :: <table-control>)
 => (view :: <table-control-view>);
end protocol <<table-control>>;


define method table-control-view-setter
    (view :: <table-control-view>, pane :: <table-control>)
 => (view :: <table-control-view>)
  pane.%table-control-view := view
end method table-control-view-setter;

define sealed method make-item
    (pane :: <table-control>, object, #rest initargs, #key)
 => (table-item :: <table-item>)
  apply(do-make-item, pane, <table-item>, object: object, initargs)
end method make-item;

define sealed method find-item
    (pane :: <table-control>, object, #key)
 => (node :: false-or(<table-item>))
  do-find-item(pane, object)
end method find-item;

// AFTER indicates which item to place the new item after
define sealed method add-item
    (pane :: <table-control>, item :: <table-item>, #key after) => ()
  // Update the set of items, bypassing 'note-gadget-items-changed'
  pane.%items
    := add!(as(<stretchy-vector>, gadget-items(pane)), item-object(item));
  // Update the selection by incrementing the indices of anything
  // after the newly added item
  when (after)
    let index = position(gadget-items(pane), item-object(after));
    when (index)
      let selection = gadget-selection(pane);
      for (i :: <integer> from 0 below size(selection))
	when (selection[i] > index)
	  selection[i] := selection[i] + 1
	end
      end;
      pane.%selection := selection
    end
  end;
  do-add-item(pane, item, after: after)
end method add-item;

define sealed method remove-item
    (pane :: <table-control>, item :: <table-item>) => ()
  pane.%items
    := remove!(as(<stretchy-vector>, gadget-items(pane)), item-object(item));
  // Update the selection by decrementing the indices of anything
  // after the deleted item
  let index = position(gadget-items(pane), item-object(item));
  when (index)
    let selection = remove(gadget-selection(pane), index);
    for (i :: <integer> from 0 below size(selection))
      when (selection[i] > index)
	selection[i] := selection[i] - 1
      end
    end;
    pane.%selection := selection
  end;
  do-remove-item(pane, item)  
end method remove-item;

define sealed method add-column
    (pane :: <table-control>, column :: <table-column>, index :: <integer>) => ()
  insert-at!(table-control-columns(pane), column, index);
  do-add-column(pane, column, index)
end method add-column;

define sealed method remove-column
    (pane :: <table-control>, index :: <integer>) => ()
  do-remove-column(pane, index);
  remove-at!(table-control-columns(pane), index)
end method remove-column;


/// Column-click callbacks

define sealed class <column-click-gadget-event> (<gadget-event>)
  sealed constant slot event-column :: <table-column>,
    required-init-keyword: column:;
end class <column-click-gadget-event>;

define sealed domain make (singleton(<column-click-gadget-event>));
define sealed domain initialize (<column-click-gadget-event>);

define sealed method handle-event
    (gadget :: <table-control>, event :: <column-click-gadget-event>) => ()
  let function = table-column-callback(event-column(event));
  //--- Perhaps this should pass both the gadget and the column
  when (function)
    function(gadget)
  end
end method handle-event;

define function distribute-column-click-callback
    (gadget :: <table-control>, column :: <table-column>) => ()
  distribute-event(port(gadget),
		   make(<column-click-gadget-event>,
			gadget: gadget,
			column: column))
end function distribute-column-click-callback;
