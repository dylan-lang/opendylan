Module:       gtk-duim
Synopsis:     GTK basic clipboard implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK clipboard handling

define class <gtk-clipboard> (<clipboard>)
  sealed slot clipboard-sheet :: <sheet>,
    required-init-keyword: sheet:;
  sealed slot clipboard-cleared? :: <boolean> = #f;
  sealed slot clipboard-gtk-clipboard :: <GtkClipboard>,
    required-init-keyword: clipboard:;
end class <gtk-clipboard>;

define variable *clipboard* :: false-or(<gtk-clipboard>) = #f;

define sealed method open-clipboard
    (port :: <gtk-port>, sheet :: <sheet>)
 => (clipboard :: false-or(<gtk-clipboard>))
  let top-sheet = top-level-sheet(sheet);
  when (top-sheet)
    let clipboard = *clipboard*;
    if (clipboard)
      clipboard-sheet(clipboard) := top-sheet;
      clipboard
    else
      *clipboard* := make(<gtk-clipboard>,
                          sheet: top-sheet,
                          clipboard: gtk-clipboard-get(gdk-atom-intern("CLIPBOARD", #f)));
    end
  end
end method open-clipboard;

// Not needed in Gtk+
define sealed method close-clipboard
    (port :: <gtk-port>, clipboard :: <gtk-clipboard>) => ()
  clipboard-cleared?(clipboard) := #f
end method close-clipboard;

define sealed method clipboard-owner
    (clipboard :: <gtk-clipboard>)
 => (owner :: false-or(<sheet>))
  //---*** GET THE OWNER, e.g. GetClipboardOwner
  /*
  let mirror = window-mirror(owner);
  mirror & mirror-sheet(mirror)
  */
end method clipboard-owner;

define sealed method add-clipboard-data-as
    (type :: subclass(<string>), clipboard :: <gtk-clipboard>, data :: <string>)
 => (success? :: <boolean>)
  let buffer = string-to-clipboard-buffer(data);
  when (buffer)
    maybe-clear-clipboard(clipboard);
    gtk-clipboard-set-text(*clipboard*.clipboard-gtk-clipboard, data, size(data));
    duim-debug-message("Set clipboard to '%s'", data);
    #t
  end
end method add-clipboard-data-as;

define sealed method maybe-clear-clipboard
    (clipboard :: <gtk-clipboard>) => ()
  unless (clipboard-cleared?(clipboard))
    clear-clipboard(clipboard)
  end
end method maybe-clear-clipboard;

define sealed method clear-clipboard
    (clipboard :: <gtk-clipboard>) => ()
  next-method();
  gtk-clipboard-clear(*clipboard*.clipboard-gtk-clipboard);
  clipboard-cleared?(clipboard) := #t
end method clear-clipboard;

define sealed method clipboard-data-available?
    (class :: subclass(<string>), clipboard :: <gtk-clipboard>)
 => (available? :: <boolean>)
  ignore(class);
  clipboard-format-available?(clipboard, #"text")
end method clipboard-data-available?;

define sealed method get-clipboard-data-as
    (class :: subclass(<string>), clipboard :: <gtk-clipboard>)
 => (string :: false-or(<string>))
  ignore(class);
  when (clipboard-format-available?(clipboard, #"text"))
    let buffer = gtk-clipboard-wait-for-text(*clipboard*.clipboard-gtk-clipboard);
    clipboard-buffer-to-string(buffer)
  end
end method get-clipboard-data-as;


/// Raw clipboard handling

// _Not_ sealed, so that users can extend it
define method clipboard-format-available?
    (clipboard :: <gtk-clipboard>, format)
 => (available? :: <boolean>)
  gtk-clipboard-wait-is-text-available(*clipboard*.clipboard-gtk-clipboard);
end method clipboard-format-available?;

define macro with-clipboard-lock
  { with-clipboard-lock (?buffer:name = ?buffer-handle:expression) ?body:body end }
    => { begin
           with-gdk-lock
             let ?buffer :: <string> = ?buffer-handle;
             ?body
           end
         end }
end macro with-clipboard-lock;

define function string-to-clipboard-buffer
    (string :: <string>) => (handle :: <string>)
  convert-to-native-newlines(string);
end function string-to-clipboard-buffer;

define function clipboard-buffer-to-string
    (handle :: <string>) => (string :: <byte-string>)
  with-clipboard-lock (buffer = handle)
    let string-size = size(buffer);
    let string = make(<byte-string>, size: string-size);
    without-bounds-checks
      for (i from 0 below string-size)
        string[i] := buffer[i]
      end
    end;
    convert-from-native-newlines(string)
  end
end function clipboard-buffer-to-string;
