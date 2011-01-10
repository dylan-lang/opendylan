Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Selection

define open generic frame-selection
    (frame :: <frame>) => (selection);

define open generic frame-selection-empty?
    (frame :: <frame>) => (empty? :: <boolean>);

define open generic frame-selected-text
    (frame :: <frame>) => (text :: false-or(<string>));

define open generic note-frame-selection-updated
    (frame :: <frame>) => ();

define open generic frame-sheet-with-selection
    (frame :: <frame>) => (sheet :: false-or(<abstract-sheet>));

define open generic frame-sheet-selection
    (frame :: <frame>, sheet :: <abstract-sheet>) => (selection);

define open generic frame-sheet-selection-empty?
    (frame :: <frame>, sheet :: <abstract-sheet>) => (empty? :: <boolean>);

define open generic frame-sheet-selected-text
    (frame :: <frame>, sheet :: <abstract-sheet>) => (text :: false-or(<string>));


/// <frame-selection-mixin>

define open abstract class <frame-selection-mixin> (<frame-input-focus-mixin>)
end class <frame-selection-mixin>;

define method initialize (frame :: <frame-selection-mixin>, #key) => ()
  next-method();
  note-frame-selection-updated(frame)
end method initialize;

define method note-frame-input-focus-changed
    (frame :: <frame-selection-mixin>) => ()
  next-method();
  note-frame-selection-updated(frame)
end method note-frame-input-focus-changed;

define method note-frame-selection-updated
    (frame :: <frame>) => ()
  #f
end method note-frame-selection-updated;


/// Selection commands

define open abstract class <frame-selection-command> (<frame-focus-command>)
end class <frame-selection-command>;

define class <frame-select-all-command> (<frame-focus-command>)
end class <frame-select-all-command>;

define class <frame-deselect-all-command> (<frame-selection-command>)
end class <frame-deselect-all-command>;


/// Collection gadget selections

define method execute-command-for-focus
    (gadget :: <collection-gadget>, command :: <frame-select-all-command>) => ()
  let frame = sheet-frame(gadget);
  when (gadget-selection-mode(gadget) = #"multiple")
    gadget-selection(gadget) := #"all";
    note-frame-selection-updated(frame)
  end
end method execute-command-for-focus;

define method execute-command-for-focus
    (gadget :: <collection-gadget>, command :: <frame-deselect-all-command>) => ()
  let frame = sheet-frame(gadget);
  when (gadget-selection-mode(gadget) ~= #"none")
    gadget-selection(gadget) := #[];
    note-frame-selection-updated(frame)
  end
end method execute-command-for-focus;

define method command-available-for-focus?
    (gadget :: <collection-gadget>, command == <frame-select-all-command>)
 => (available? :: <boolean>)
  gadget-selection-mode(gadget) == #"multiple"
    & size(gadget-selection(gadget)) ~= size(gadget-items(gadget))
end method command-available-for-focus?;

define method command-available-for-focus?
    (gadget :: <collection-gadget>, command == <frame-deselect-all-command>)
 => (available? :: <boolean>)
  gadget-selection-mode(gadget) == #"multiple"
    & ~empty?(gadget-selection(gadget))
end method command-available-for-focus?;


/// Text gadget selections

define method execute-command-for-focus
    (gadget :: <text-gadget>, command :: <frame-select-all-command>) => ()
  text-selection(gadget) := #t
end method execute-command-for-focus;

define method execute-command-for-focus
    (gadget :: <text-gadget>, command :: <frame-deselect-all-command>) => ()
  text-selection(gadget) := #f
end method execute-command-for-focus;

define method command-available-for-focus?
    (gadget :: <text-gadget>, command == <frame-select-all-command>)
 => (available? :: <boolean>)
  //--- There must be a more efficient way to do this!
  selected-text(gadget) ~= gadget-text(gadget)
end method command-available-for-focus?;

define method command-available-for-focus?
    (gadget :: <text-gadget>, command == <frame-deselect-all-command>)
 => (available? :: <boolean>)
  false?(selected-text(gadget))
end method command-available-for-focus?;


/// Default implementations

define method frame-selection
    (frame :: <frame>) => (selection)
  let sheet = frame-sheet-with-selection(frame);
  sheet & frame-sheet-selection(frame, sheet)
end method frame-selection;

define method frame-selection-empty?
    (frame :: <frame>) => (empty? :: <boolean>)
  let sheet = frame-sheet-with-selection(frame);
  ~sheet | frame-sheet-selection-empty?(frame, sheet)
end method frame-selection-empty?;

define method frame-selected-text
    (frame :: <frame>) => (text :: false-or(<string>))
  let sheet = frame-sheet-with-selection(frame);
  (sheet & frame-sheet-selected-text(frame, sheet))
    | begin
        let selection = frame-selection(frame);
        instance?(selection, <string>) & selection
      end
end method frame-selected-text;

//---*** This seems a bit dubious, in that it ignores
//---*** non-collection gadgets. Maybe the name should just
//---*** be changed to reflect this.
define method frame-selected-objects
    (frame :: <frame>) => (objects :: <sequence>);
  let gadget = frame-primary-collection-gadget(frame);
  if (gadget)
    select (gadget-selection-mode(gadget))
      #"multiple" => gadget-value(gadget);
      #"single"   =>
        if (empty?(gadget-selection(gadget)))
          #[]
        else
          vector(gadget-value(gadget))
        end;
      otherwise =>
        #[]
    end
  else
    #[]
  end
end method frame-selected-objects;

define method frame-sheet-with-selection
    (frame :: <frame>) => (sheet :: false-or(<sheet>))
  #f
end method frame-sheet-with-selection;

define method frame-primary-collection-gadget
    (frame :: <frame>) => (gadget :: false-or(<collection-gadget>))
  let sheet = frame-sheet-with-selection(frame);
  instance?(sheet, <collection-gadget>) & sheet
end method frame-primary-collection-gadget;

define method frame-sheet-selection
    (frame :: <frame>, sheet :: <sheet>) => (selection)
  #f
end method frame-sheet-selection;

define method frame-sheet-selection
    (frame :: <frame>, gadget :: <collection-gadget>)
 => (selection)
  if (gadget-selection-mode(gadget) == #"multiple")
    //---*** DUIM should really do this for us, plus it should never
    //---*** return duplicates but unfortunately it has been known to.
    let items = gadget-items(gadget);
    let selection = sort!(remove-duplicates(gadget-selection(gadget)));
    map-as(<simple-vector>,
	   method (index :: <integer>)
	     gadget-item-value(gadget, items[index])
	   end,
	   selection)
  else
    gadget-value(gadget)
  end
end method frame-sheet-selection;

define method frame-sheet-selection
    (frame :: <frame>, gadget :: <value-gadget>)
 => (selection)
  gadget-value(gadget)
end method frame-sheet-selection;

define method frame-sheet-selection-empty?
    (frame :: <frame>, sheet :: <sheet>) => (empty? :: <boolean>)
  #t
end method frame-sheet-selection-empty?;

define method frame-sheet-selection-empty?
    (frame :: <frame>, gadget :: <collection-gadget>) => (empty? :: <boolean>)
  empty?(gadget-selection(gadget))
end method frame-sheet-selection-empty?;

define method frame-sheet-selection-empty?
    (frame :: <frame>, gadget :: <text-gadget>) => (empty? :: <boolean>)
  text-selection(gadget) == #f
end method frame-sheet-selection-empty?;

define method frame-sheet-selected-text
    (frame :: <frame>, sheet :: <sheet>)
 => (text :: false-or(<string>))
  #f
end method frame-sheet-selected-text;

define method frame-sheet-selected-text
    (frame :: <frame>, gadget :: <collection-gadget-mixin>)
 => (text :: false-or(<string>))
  let selection = gadget-selection(gadget);
  let items = gadget-items(gadget);
  select (size(selection))
    0 =>
      #f;
    1 =>
      let index = selection[0];
      gadget-item-label(gadget, items[index]);
    otherwise =>
      let stream = make(<string-stream>, direction: #"output");
      for (index in sort(selection))
	let label = gadget-item-label(gadget, items[index]);
	format(stream, "%s\n", label);
      end;
      stream-contents(stream);
  end
end method frame-sheet-selected-text;

define method frame-sheet-selected-text
    (frame :: <frame>, gadget :: <text-gadget>)
 => (text :: false-or(<string>))
  // ---*** hughg, 1997/12/18: This is a hack because currently some DUIM
  // <text-gadgets> return #f for the selected text.  If you want to
  // get some of the text, better get all than none.
  selected-text(gadget) | gadget-text(gadget)
end method frame-sheet-selected-text;


/// Selection command table

define command-table *selection-command-table* (*global-command-table*)
  menu-item "Select All"   = <frame-select-all-command>,
    documentation: "Selects all items or information.";
//--- cpage: 1997.11.09 Remove Deselect All. I've left all the code in place
//           to minimize the number of changes, and in case we decide to use
//           it again. If not, remove it at some point.
/*
  menu-item "Deselect All" = <frame-deselect-all-command>,
    documentation: "Deselects selected items.";
*/
end command-table *selection-command-table*;
