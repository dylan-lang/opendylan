Module:    win32-duim
Synopsis:  Win32 basic clipboard implementation
Author:    Andy Armstrong, Scott McKay, David Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 clipboard handling

define class <win32-clipboard> (<clipboard>)
  sealed slot clipboard-sheet :: <sheet>,
    required-init-keyword: sheet:;
  sealed slot clipboard-cleared? :: <boolean> = #f;
end class <win32-clipboard>;

define variable *clipboard* :: false-or(<win32-clipboard>) = #f;

define sealed method open-clipboard
    (port :: <win32-port>, sheet :: <sheet>)
 => (clipboard :: false-or(<win32-clipboard>))
  let top-sheet = top-level-sheet(sheet);
  when (top-sheet)
    let handle = window-handle(sheet-mirror(top-sheet));
    if (handle)
      check-result("OpenClipboard", OpenClipboard(handle))
    end;
    let clipboard = *clipboard*;
    if (clipboard)
      clipboard-sheet(clipboard) := top-sheet;
      clipboard
    else
      *clipboard* := make(<win32-clipboard>, sheet: top-sheet)
    end
  end
end method open-clipboard;

define sealed method close-clipboard
    (port :: <win32-port>, clipboard :: <win32-clipboard>) => ()
  check-result("CloseClipboard", CloseClipboard());
  clipboard-cleared?(clipboard) := #f
end method close-clipboard;

define sealed method clipboard-owner
    (clipboard :: <win32-clipboard>)
 => (owner :: false-or(<sheet>))
  let handle = GetClipboardOwner();
  unless (null-pointer?(handle))
    handle-sheet(handle)
  end
end method clipboard-owner;

define sealed method add-clipboard-data-as
    (type :: subclass(<string>), clipboard :: <win32-clipboard>, data :: <string>)
 => (success? :: <boolean>)
  let buffer-handle = string-to-clipboard-buffer(data);
  when (buffer-handle)
    maybe-clear-clipboard(clipboard);
    SetClipboardData($CF-TEXT, buffer-handle);
    duim-debug-message("Set clipboard to '%s'", data);
    #t
  end
end method add-clipboard-data-as;

define sealed method maybe-clear-clipboard
    (clipboard :: <win32-clipboard>) => ()
  unless (clipboard-cleared?(clipboard))
    clear-clipboard(clipboard)
  end
end method maybe-clear-clipboard;

define sealed method clear-clipboard
    (clipboard :: <win32-clipboard>) => ()
  next-method();
  check-result("EmptyClipboard", EmptyClipboard());
  clipboard-cleared?(clipboard) := #t
end method clear-clipboard;

define sealed method clipboard-data-available?
    (class :: subclass(<string>), clipboard :: <win32-clipboard>)
 => (available? :: <boolean>)
  ignore(class);
  clipboard-format-available?(clipboard, $CF-TEXT)
end method clipboard-data-available?;

define sealed method get-clipboard-data-as
    (class :: subclass(<string>), clipboard :: <win32-clipboard>)
 => (string :: false-or(<string>))
  ignore(class);
  when (clipboard-format-available?(clipboard, $CF-TEXT))
    let buffer-handle = GetClipboardData($CF-TEXT);
    check-result("GetClipboardData", buffer-handle);
    clipboard-buffer-to-string(buffer-handle)
  end
end method get-clipboard-data-as;


/// Raw clipboard handling

define sealed method clipboard-format-available?
    (clipboard :: <win32-clipboard>, format :: <integer>)
 => (available? :: <boolean>)
  let next-format = 0;
  block (return)
    while (#t)
      SetLastError($NO_ERROR);
      next-format := EnumClipboardFormats(next-format);
      duim-debug-message("Clipboard format %d found -- looking for %d",
                         next-format, format);
      select (next-format by \=)
        format => 
          return(#t);
        0 =>
          //---*** The error code is not setup in Windows 95/98.
          // ensure-no-error("EnumClipboardFormats");
          return(#f);
        otherwise =>
          #f;
      end
    end
  end
end method clipboard-format-available?;

define constant $clipboard-alloc-options
    = %logior($GMEM-MOVEABLE, $GMEM-DDESHARE);

define macro with-clipboard-lock
  { with-clipboard-lock (?buffer:name = ?buffer-handle:expression)
      ?body:body
    failure
      ?failure-body:body
    end }
    => { begin
           let _handle  = ?buffer-handle;
           let _pointer = GlobalLock(_handle);
           if (null-pointer?(_pointer))
             ?failure-body
           else
             block ()
               let ?buffer :: <C-string> = pointer-cast(<C-string>, _pointer);
               ?body
             cleanup
               when (GlobalUnlock(_handle) == #f)
                 //--- This was 'ensure-no-error("GlobalUnlock")',
                 //--- but that blew out on Win-95 from time to time,
                 //--- and there's nothing the user can do anyway...
                 #f
               end
             end
           end
         end }
  { with-clipboard-lock (?buffer:name = ?buffer-handle:expression)
      ?body:body
    end }
    => { with-clipboard-lock (?buffer = ?buffer-handle)
           ?body
         failure
           ensure-no-error("GlobalLock");
           #f
         end }
end macro with-clipboard-lock;

define function string-to-clipboard-buffer
    (string :: <string>) => (buffer-handle :: false-or(<HGLOBAL>))
  let string = convert-to-windows-newlines(string);
  let buffer-size = size(string) + 1;
  let buffer-handle = GlobalAlloc($clipboard-alloc-options, buffer-size);
  check-result("GlobalAlloc", buffer-handle);
  with-clipboard-lock (buffer = buffer-handle)
    without-bounds-checks
      for (i from 0 below buffer-size - 1)
        buffer[i] := string[i]
      end;
      buffer[buffer-size - 1] := '\0'
    end;
    buffer-handle
  failure
    #f
  end
end function string-to-clipboard-buffer;

define function clipboard-buffer-to-string
    (buffer-handle :: <HGLOBAL>) => (string :: false-or(<byte-string>))
  with-clipboard-lock (buffer = buffer-handle)
    let string-size = size(buffer);
    let string = make(<byte-string>, size: string-size);
    without-bounds-checks
      for (i from 0 below string-size)
        string[i] := buffer[i]
      end
    end;
    convert-from-windows-newlines(string)
  failure
    #f
  end
end function clipboard-buffer-to-string;
