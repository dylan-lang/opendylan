Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// List controls

define constant <list-control-view>
    = one-of(#"list", #"small-icon", #"large-icon");

// The items in a list control are layed out in one or more columns
// Note that items are not modelled as sheets!
define open abstract class <list-control>
    (<bordered-gadget-mixin>,
     <scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <key-press-gadget-mixin>,
     <popup-menu-gadget-mixin>,
     <basic-choice-gadget>)
  sealed constant slot gadget-lines :: false-or(<integer>) = #f,
    init-keyword: lines:;
  sealed slot list-control-view :: <list-control-view> = #"list",
    init-keyword: view:,
    setter: %list-control-view-setter;
  // Takes an object, produces two icons -- a small and a large icon
  sealed constant slot list-control-icon-function :: false-or(<function>) = #f,
    init-keyword: icon-function:;
end class <list-control>;

define method initialize
    (pane :: <list-control>,
     #rest initargs, #key items = #[]) => ()
  // Ensure the items are in a stretchy vector so we can use 'add!' and 'remove!'
  apply(next-method, pane, items: as(<stretchy-vector>, items), initargs)
end method initialize;

define method gadget-items-setter
    (items :: <sequence>, gadget :: <list-control>)
 => (items :: <sequence>)
  // Ensure the items are in a stretchy vector so we can use 'add!' and 'remove!'
  next-method(as(<stretchy-vector>, items), gadget)
end method gadget-items-setter;


// The superclass of list and table items and tree nodes...
define open abstract class <item> (<object>)
end class <item>;

define protocol <<item>> ()
  getter item-object
    (item :: <item>) => (object);
  getter item-label
    (item :: <item>) => (label :: false-or(<string>));
  setter item-label-setter
    (label :: false-or(<string>), item :: <item>) => (label :: false-or(<string>));
  getter item-icon
    (item :: <item>) => (icon :: false-or(<image>));
  setter item-icon-setter
    (icon :: false-or(<image>), item :: <item>) => (icon :: false-or(<image>));
end protocol <<item>>;

define open abstract class <list-item> (<item>)
  sealed constant slot item-object = #f,
    init-keyword: object:;
end class <list-item>;

// This protocol is shared by list controls and table controls
define protocol <<list-control>> ()
  function make-item (control, object, #key, #all-keys) => (item);
  function find-item (control, object, #key, #all-keys) => (item);
  function add-item  (control, item, #key after) => ();
  function remove-item (control, item) => ();
  // Back-end function to make and find an item
  function do-make-item (control, item-class, #key, #all-keys) => (item);
  function do-find-item (control, object, #key, #all-keys) => (item);
  // The concrete implementation should add the list item to the
  // list control, and then re-layout and redisplay the list control
  function do-add-item (control, item, #key after) => ();
  // The concrete implementation should remove the list item from the
  // list control, and then re-layout and redisplay the list control
  function do-remove-item (control, item) => ();
  // Views, etc
  getter list-control-view
    (list-control :: <list-control>) => (view :: <list-control-view>);
  setter list-control-view-setter
    (view :: <list-control-view>, list-control :: <list-control>)
 => (view :: <list-control-view>);
end protocol <<list-control>>;


define method list-control-view-setter
    (view :: <list-control-view>, pane :: <list-control>)
 => (view :: <list-control-view>)
  pane.%list-control-view := view
end method list-control-view-setter;

define sealed method make-item
    (pane :: <list-control>, object, #rest initargs, #key)
 => (list-item :: <list-item>)
  apply(do-make-item, pane, <list-item>, object: object, initargs)
end method make-item;

define sealed method find-item
    (pane :: <list-control>, object, #key)
 => (node :: false-or(<list-item>))
  do-find-item(pane, object)
end method find-item;

// AFTER indicates which item to place the new item after
define sealed method add-item
    (pane :: <list-control>, item :: <list-item>, #key after) => ()
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
    (pane :: <list-control>, item :: <list-item>) => ()
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
