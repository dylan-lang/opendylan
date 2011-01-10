Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Clipboard

define constant <clipboard-type> = type-union(<symbol>, <type>);

define protocol <<clipboard-protocol>> ()
  function open-clipboard
    (port :: <abstract-port>, sheet :: <abstract-sheet>)
 => (clipboard :: false-or(<clipboard>));
  function close-clipboard
    (port :: <abstract-port>, clipboard :: <clipboard>) => ();
  getter clipboard-sheet
    (clipboard :: <clipboard>) => (sheet :: <abstract-sheet>);
  getter clipboard-owner
    (clipboard :: <clipboard>) => (owner :: false-or(<abstract-sheet>));
  function add-clipboard-data
    (clipboard :: <clipboard>, data) => (success? :: <boolean>);
  function add-clipboard-data-as
    (type :: <clipboard-type>, clipboard :: <clipboard>, data)
 => (success? :: <boolean>);
  function clear-clipboard
    (clipboard :: <clipboard>) => ();
  function maybe-clear-clipboard
    (clipboard :: <clipboard>) => ();
  function clipboard-data-available?
    (type :: <clipboard-type>, clipboard :: <clipboard>) => (available? :: <boolean>);
  function get-clipboard-data-as
    (type :: <clipboard-type>, clipboard :: <clipboard>) => (data);
end protocol <<clipboard-protocol>>;

// Note that the variable clipboard will be #f if we fail to get the clipboard
define macro with-clipboard
  { with-clipboard (?clipboard:name = ?sheet:expression)
      ?:body
    end }
    => { begin
	   let ?clipboard = open-clipboard(port(?sheet), ?sheet);
	   block ()
	     ?body
	   cleanup
	     when (?clipboard)
	       close-clipboard(port(?sheet), ?clipboard)
	     end
	   end
         end }
end macro with-clipboard;


/// Default clipboard support

define function clipboard-format-error
    (type :: <clipboard-type>)
  error("Unrecognized clipboard format %=", type)
end function clipboard-format-error;

define method add-clipboard-data-as
    (type :: <clipboard-type>, clipboard :: <clipboard>, object)
 => (success? :: <boolean>)
  clipboard-format-error(type)
end method add-clipboard-data-as;

define method clipboard-data-available?
    (type :: <clipboard-type>, clipboard :: <clipboard>)
 => (available? :: <boolean>)
  clipboard-format-error(type)
end method clipboard-data-available?;

define method get-clipboard-data-as
    (type :: <clipboard-type>, clipboard :: <clipboard>)
 => (data)
  clipboard-format-error(type)
end method get-clipboard-data-as;


/// Clipboard support for strings

// Note: backends do most of the work here
define method add-clipboard-data
    (clipboard :: <clipboard>, string :: <string>)
 => (success? :: <boolean>)
  add-clipboard-data-as(<string>, clipboard, string)
end method add-clipboard-data;


/// Clipboard support for Dylan objects

define variable *dylan-clipboard-owner* :: false-or(<sheet>) = #f;
define variable *dylan-clipboard-value* :: <object> = #f;

define method add-clipboard-data
    (clipboard :: <clipboard>, object :: <object>)
 => (success? :: <boolean>)
  add-clipboard-data-as(<object>, clipboard, object)
end method add-clipboard-data;

define method add-clipboard-data-as
    (type == <object>, clipboard :: <clipboard>, object :: <object>)
 => (success? :: <boolean>)
  maybe-clear-clipboard(clipboard);
  *dylan-clipboard-owner* := clipboard-sheet(clipboard);
  *dylan-clipboard-value* := object;
  #t
end method add-clipboard-data-as;

define method clipboard-data-available?
    (type == <object>, clipboard :: <clipboard>)
 => (available? :: <boolean>)
  clipboard-owner(clipboard) == *dylan-clipboard-owner*
end method clipboard-data-available?;

define method get-clipboard-data-as
    (type == <object>, clipboard :: <clipboard>)
 => (object :: <object>)
  clipboard-data-available?(type, clipboard)
  & *dylan-clipboard-value*
end method get-clipboard-data-as;

define method clear-clipboard
    (clipboard :: <clipboard>) => ()
  *dylan-clipboard-owner* := #f;
  *dylan-clipboard-value* := #f;
end method clear-clipboard;
