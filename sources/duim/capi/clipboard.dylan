Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// CAPI clipboard handling

define class <capi-clipboard> (<clipboard>)
  sealed slot clipboard-sheet :: <sheet>,
    required-init-keyword: sheet:;
  sealed slot clipboard-cleared? :: <boolean> = #f;
end class <capi-clipboard>;

define variable *clipboard* = #f;

define variable *clipboard-owner* :: false-or(<sheet>) = #f;

define method open-clipboard
    (port :: <capi-port>, sheet :: <sheet>)
 => (clipboard :: <capi-clipboard>)
  let clipboard = *clipboard*;
  if (clipboard)
    clipboard-sheet(clipboard) := sheet;
    clipboard
  else
    *clipboard* := make(<capi-clipboard>, sheet: sheet)
  end
end method open-clipboard;

define method close-clipboard
    (port :: <capi-port>, clipboard :: <capi-clipboard>) => ()
  #f
end method close-clipboard;

define method clipboard-owner
    (clipboard :: <capi-clipboard>)
 => (owner :: false-or(<sheet>))
  //---*** This really isn't right, but I don't think the CAPI
  //---*** can give us anything better!
  *clipboard-owner*
end method clipboard-owner;

define method add-clipboard-data-as
    (class :: subclass(<string>), clipboard :: <capi-clipboard>, data :: <string>)
 => (success? :: <boolean>)
  *clipboard-owner* := clipboard-sheet(clipboard);
  capi-set-clipboard(convert-to-screen(), data, data);
  #t
end method add-clipboard-data-as;

define sealed method maybe-clear-clipboard
    (clipboard :: <capi-clipboard>)
 => (cleared? :: <boolean>)
  unless (clipboard.clipboard-cleared?)
    clear-clipboard(clipboard);
    #t
  end;
end method maybe-clear-clipboard;

define method clear-clipboard
    (clipboard :: <capi-clipboard>) => ()
  next-method();
  *clipboard-owner* := clipboard-sheet(clipboard)
end method clear-clipboard;

define method clipboard-data-available?
    (class :: subclass(<string>), clipboard :: <capi-clipboard>)
 => (available? :: <boolean>)
  let lisp-result = capi-get-clipboard(convert-to-screen(), string:);
  instance?(lisp-result, <string>)
end method clipboard-data-available?;

define method get-clipboard-data-as
    (class :: subclass(<string>), clipboard :: <capi-clipboard>)
 => (string :: false-or(<string>))
  let lisp-result = capi-get-clipboard(convert-to-screen(), string:);
  if (instance?(lisp-result, <string>))
    lisp-result
  end
end method get-clipboard-data-as;
