Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Clipboard

define open abstract class <frame-clipboard-mixin> (<frame-selection-mixin>)
end class <frame-clipboard-mixin>;

define open generic cut-object (client, object) => ();
define open generic copy-object (client, object) => ();
define open generic paste-object (client, object) => ();
define open generic delete-object (client, object) => ();

define open generic cut-object? (client, object) => (cut? :: <boolean>);
define open generic copy-object? (client, object) => (copy? :: <boolean>);
define open generic paste-object? (client, object) => (paste? :: <boolean>);
define open generic delete-object? (client, object) => (delete? :: <boolean>);


/// Clipboard commands

define class <frame-cut-command> (<frame-selection-command>)
end class <frame-cut-command>;

define class <frame-copy-command> (<frame-selection-command>)
end class <frame-copy-command>;

define class <frame-paste-command> (<frame-selection-command>)
end class <frame-paste-command>;

define class <frame-delete-command> (<frame-selection-command>)
end class <frame-delete-command>;


/// Default implementations

define method cut-object?
    (sheet :: <sheet>, object) => (cut? :: <boolean>)
  cut-object?(sheet-frame(sheet), object)
end method cut-object?;

define method cut-object?
    (frame :: <frame-clipboard-mixin>, object) => (cut? :: <boolean>)
  #f
end method cut-object?;

define method cut-object
    (sheet :: <sheet>, object) => ()
  cut-object(sheet-frame(sheet), object)
end method cut-object;

define method cut-object
    (frame :: <frame-clipboard-mixin>, object) => ()
  #f
end method cut-object;


define method copy-object?
    (sheet :: <sheet>, object) => (copy? :: <boolean>)
  copy-object?(sheet-frame(sheet), object)
end method copy-object?;

define method copy-object?
    (frame :: <frame-clipboard-mixin>, object) => (copy? :: <boolean>)
  #f
end method copy-object?;

define method copy-object
    (sheet :: <sheet>, object) => ()
  copy-object(sheet-frame(sheet), object)
end method copy-object;

define method copy-object
    (frame :: <frame-clipboard-mixin>, object) => ()
  #f
end method copy-object;


define method paste-object?
    (sheet :: <sheet>, object) => (paste? :: <boolean>)
  paste-object?(sheet-frame(sheet), object)
end method paste-object?;

define method paste-object?
    (frame :: <frame-clipboard-mixin>, object) => (paste? :: <boolean>)
  #f
end method paste-object?;

define method paste-object
    (sheet :: <sheet>, object) => ()
  paste-object(sheet-frame(sheet), object)
end method paste-object;

define method paste-object
    (frame :: <frame-clipboard-mixin>, object) => ()
  #f
end method paste-object;


define method delete-object?
    (sheet :: <sheet>, object) => (delete? :: <boolean>)
  delete-object?(sheet-frame(sheet), object)
end method delete-object?;

define method delete-object?
    (frame :: <frame-clipboard-mixin>, object) => (delete? :: <boolean>)
  #f
end method delete-object?;

define method delete-object
    (sheet :: <sheet>, object) => ()
  delete-object(sheet-frame(sheet), object)
end method delete-object;

define method delete-object
    (frame :: <frame-clipboard-mixin>, object) => ()
  #f
end method delete-object;


/// Implementation for text gadgets

define method cut-object?
    (gadget :: <text-gadget>, object) => (cut? :: <boolean>)
  delete-object?(gadget, object)
    & copy-object?(gadget, object)
end method cut-object?;

define method cut-object
    (gadget :: <text-gadget>, object) => ()
  if (cut-object?(gadget, object))
    copy-object(gadget, object);
    delete-object(gadget, object)
  else
    clipboard-function-disabled-message(gadget, #"cut")
  end
end method cut-object;


define method copy-object?
    (gadget :: <text-gadget>, object) => (copy? :: <boolean>)
  true?(text-selection(gadget))
end method copy-object?;

define method copy-object
    (gadget :: <text-gadget>, object) => ()
  if (copy-object?(gadget, object))
    let string = selected-text(gadget);
    when (string)
      with-clipboard (clipboard = top-level-sheet(gadget))
	add-clipboard-data(clipboard, string)
      end
    end
  else
    clipboard-function-disabled-message(gadget, #"copy")
  end
end method copy-object;


define method paste-object?
    (gadget :: <text-gadget>, object) => (paste? :: <boolean>)
  ~gadget-read-only?(gadget)
    & gadget-enabled?(gadget)
    & with-clipboard (clipboard = top-level-sheet(gadget))
	if (clipboard)
	  clipboard-data-available?(<string>, clipboard)
	end
      end
end method paste-object?;

define method paste-object
    (gadget :: <text-gadget>, object) => ()
  if (paste-object?(gadget, object))
    with-clipboard (clipboard = top-level-sheet(gadget))
      if (clipboard)
	let clipboard-text = get-clipboard-data-as(<string>, clipboard);
	if (clipboard-text)
	  let selection = text-selection(gadget);
	  let (_start, _end)
	    = if (selection)
		values(selection.text-range-start, selection.text-range-end)
	      else
		let position = text-caret-position(gadget);
		values(position, position)
	      end;
	  let old-text = gadget-text(gadget);
	  let new-text
	    = concatenate-as
	        (<string>,
		 copy-sequence(old-text, end: _start),
		 clipboard-text,
		 copy-sequence(old-text, start: _end));
	  gadget-text(gadget) := new-text
	end
      end
    end
  else
    clipboard-function-disabled-message(gadget, #"paste")
  end
end method paste-object;


define method delete-object?
    (gadget :: <text-gadget>, object) => (delete? :: <boolean>)
  ~gadget-read-only?(gadget)
    & gadget-enabled?(gadget)
    & true?(text-selection(gadget))
end method delete-object?;

define method delete-object
    (gadget :: <text-gadget>, object) => ()
  let selection = text-selection(gadget);
  let pos = gadget.text-caret-position;
  let old-text = gadget-text(gadget);
  let (_start, _end, new-pos)
    = if (selection)
	let _start = selection.text-range-start;
	let _end   = selection.text-range-end;
	values(_start, _end,
	       case
		 pos < _start =>
		   pos;
		 pos >= _end =>
		   pos - (_end - _start);
		 otherwise =>
		   _start;
	       end)
      else
	if (pos < old-text.size)
	  values(pos, pos + 1, pos)
	else
	  values(#f, #f)
	end
      end;
  if (delete-object?(gadget, object))
    if (_start & _end)
      let prefix = copy-sequence(old-text, end: _start);
      let suffix = copy-sequence(old-text, start: _end);
      let new-text = concatenate-as(<string>, prefix, suffix);
      gadget-text(gadget) := new-text;
      text-caret-position(gadget) := new-pos
    else
      beep(sheet-frame(gadget))
    end
  else
    clipboard-function-disabled-message(gadget, #"delete");
  end
end method delete-object;


define function clipboard-function-disabled-message
    (gadget :: <text-gadget>, type :: <symbol>) => ()
  let read-only-modification? 
    = type ~== #"copy" 
        & (gadget-read-only?(gadget) | ~gadget-enabled?(gadget));
  let message
    = case
	read-only-modification? =>
	  format-to-string("Cannot %s as this gadget is read-only.", type);
	type == #"paste" =>
	  "Cannot paste into this gadget.";
	otherwise =>
	  format-to-string("Cannot %s from this gadget.", type);
      end;
  notify-user(message, 
	      title: release-product-name(),
	      style: #"error",
	      owner: sheet-frame(gadget))
end function clipboard-function-disabled-message;


/// Clipboard command table

define variable $cut-bitmap   :: <label-type> = "X";
define variable $copy-bitmap  :: <label-type> = "C";
define variable $paste-bitmap :: <label-type> = "P";

define constant $cut-doc    = "Removes the current selection and copies it onto"
                              " the Clipboard.";
define constant $copy-doc   = "Copies the current selection to the Clipboard.";
define constant $paste-doc  = "Inserts the items you have copied or cut into"
                              " the selected location.";
define constant $delete-doc = "Deletes the selected items.";

define command-table *clipboard-command-table* (*global-command-table*)
  menu-item "Cut"    = <frame-cut-command>,
    documentation: $cut-doc;
  menu-item "Copy"   = <frame-copy-command>,
    documentation: $copy-doc;
  menu-item "Paste"  = <frame-paste-command>,
    documentation: $paste-doc;
  menu-item "Delete" = <frame-delete-command>,
    documentation: $delete-doc;
end command-table *clipboard-command-table*;

define method make-clipboard-tool-bar-buttons
    (frame :: <frame-clipboard-mixin>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $cut-bitmap,
	      documentation: "Cut",
              command: <frame-cut-command>),
         make(<button>,
              label: $copy-bitmap,
	      documentation: "Copy",
              command: <frame-copy-command>),
         make(<button>,
              label: $paste-bitmap,
	      documentation: "Paste",
              command: <frame-paste-command>))
end method make-clipboard-tool-bar-buttons;
