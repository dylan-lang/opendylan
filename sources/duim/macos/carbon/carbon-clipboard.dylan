Module:       carbon-duim
Synopsis:     Macintosh clipboard implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macintosh clipboard handling

/*---*** No clipboard for now...
define class <carbon-clipboard> (<clipboard>)
  sealed slot clipboard-sheet :: <sheet>,
    required-init-keyword: sheet:;
  sealed slot clipboard-cleared? :: <boolean> = #f;
end class <carbon-clipboard>;

define variable *clipboard* :: false-or(<carbon-clipboard>) = #f;

define sealed method open-clipboard
    (port :: <carbon-port>, sheet :: <sheet>)
 => (clipboard :: false-or(<carbon-clipboard>))
  let top-sheet = top-level-sheet(sheet);
  when (top-sheet)
    //---*** OPEN THE CLIPBOARD, e.g. Win32 OpenClipboard
    if (clipboard)
      clipboard-sheet(clipboard) := top-sheet;
      clipboard
    else
      *clipboard* := make(<carbon-clipboard>, sheet: top-sheet)
    end
  end
end method open-clipboard;

define sealed method close-clipboard
    (port :: <carbon-port>, clipboard :: <carbon-clipboard>) => ()
  //---*** CLOSE THE CLIPBOARD, e.g. Win32 CloseClipboard
  clipboard-cleared?(clipboard) := #f
end method close-clipboard;

define sealed method clipboard-owner
    (clipboard :: <carbon-clipboard>)
 => (owner :: false-or(<sheet>))
  //---*** GET THE OWNER, e.g. GetClipboardOwner
  let mirror = window-mirror(owner);
  mirror & mirror-sheet(mirror)
end method clipboard-owner;

define sealed method add-clipboard-data-as
    (type :: subclass(<string>), clipboard :: <carbon-clipboard>, data :: <string>)
 => (success? :: <boolean>)
  let buffer = string-to-clipboard-buffer(data);
  when (buffer)
    maybe-clear-clipboard(clipboard);
    //---*** SET THE DATA, e.g. SetClipboardData
    #t
  end
end method add-clipboard-data-as;

define sealed method maybe-clear-clipboard
    (clipboard :: <carbon-clipboard>) => ()
  unless (clipboard-cleared?(clipboard))
    clear-clipboard(clipboard)
  end
end method maybe-clear-clipboard;

define sealed method clear-clipboard
    (clipboard :: <carbon-clipboard>) => ()
  next-method();
  //---*** CLEAR THE CLIPBOARD, e.g. EmptyClipboard
  clipboard-cleared?(clipboard) := #t
end method clear-clipboard;

define sealed method clipboard-data-available?
    (class :: subclass(<string>), clipboard :: <carbon-clipboard>)
 => (available? :: <boolean>)
  ignore(class);
  clipboard-format-available?(clipboard, #"text")
end method clipboard-data-available?;

define sealed method get-clipboard-data-as
    (class :: subclass(<string>), clipboard :: <carbon-clipboard>)
 => (string :: false-or(<string>))
  ignore(class);
  when (clipboard-format-available?(clipboard, #"text"))
    //---*** GET THE TEXT DATA FROM THE CLIPBOARD, e.g. GetClipboardData
    clipboard-buffer-to-string(buffer)
  end
end method get-clipboard-data-as;


/// Raw clipboard handling

// _Not_ sealed, so that users can extend it
define method clipboard-format-available?
    (clipboard :: <carbon-clipboard>, format)
 => (available? :: <boolean>)
  //---*** SEE IF THERE IS DATA OF THE GIVEN FORMAT
end method clipboard-format-available?;

define macro with-clipboard-lock
  { with-clipboard-lock (?buffer:name = ?buffer-handle:expression) ?body:body end }
    => { begin
	   let _locked? = #f;
	   //---*** LOCK THE CLIPBOARD
	   _locked? := #t;
	   block ()
	     let ?buffer :: <C-string> = XXX;	//---*** GET A DATA BUFFER
	     ?body
	   cleanup
	     when (_locked?)
	       //---*** UNLOCK THE CLIPBOARD
	     end
	   end
	 end }
end macro with-clipboard-lock;

define function string-to-clipboard-buffer
    (string :: <string>) => (handle :: <XXX>)
  let string = convert-to-native-newlines(string);
  //---*** COPY THE STRING INTO THE CLIPBOARD BUFFER
end function string-to-clipboard-buffer;

define function clipboard-buffer-to-string
    (handle :: <XXX>) => (string :: <byte-string>)
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
*/

//---*** Until the code above is commented back in
ignore(convert-from-native-newlines);
