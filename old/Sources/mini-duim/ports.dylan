Module:    mini-duim
Synopsis:  Mini-DUIM ports
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Pointers

define sealed class <pointer> (<object>)
  slot port = #f,
    init-keyword: port:;
  slot pointer-sheet :: false-or(<sheet>) = #f;
  slot pointer-button-state :: <integer> = 0;
  slot %x-position :: <coordinate> = 0;
  slot %y-position :: <coordinate> = 0;
end class <standard-pointer>;

define method pointer-position
    (pointer :: <pointer>, #key sheet)
 => (x :: <integer>, x :: <integer>)
  ignore(sheet);
  values(pointer.%x-position, pointer.%y-position)
end method pointer-position;

define method set-pointer-position
    (pointer :: <pointer>, x, y, #key sheet, port-did-it?) => ()
  ignore(sheet, port-did-it?);
  pointer.%x-position := x;
  pointer.%y-position := y
end method set-pointer-position;


/// Ports

// Only a single instance of a port in mini-DUIM
define variable *port* = #f;

define open abstract class <port> (<object>)
end class <port>;

define open abstract class <basic-port> (<port>)
  slot port-pointer :: <pointer> = make(<pointer>);
  slot port-modifier-state :: <integer> = 0;
  slot port-frame-managers = make(<stretchy-vector>);
  slot port-display = #f;
  slot port-font-mapping-table :: <table> = make(<table>);
  slot port-font-mapping-cache :: <pair>  = pair(#f, #f);
end class <basic-port>;

define open generic class-for-make-port
    (type, #key, #all-keys) => (class :: <class>, initargs);

define method find-port
    (#rest initargs) => (_port :: <port>)
  ignore(initargs);
  *port*
  | begin
      let _port = make(class-for-make-port(#"local"));
      *port* := _port;
      find-display(port: _port);
      _port
    end
end method find-port;

define method graft-sheet
    (_port :: <basic-port>, sheet :: <basic-sheet>) => ()
  port(sheet) := _port;
  note-sheet-attached(sheet);
  for (child in sheet-children(sheet))
    graft-sheet(sheet, child)
  end;
end method graft-sheet;

define method text-style-mapping
    (_port :: <basic-port>, text-style :: <text-style>) => (font)
  let cache = port-font-mapping-cache(_port);
  if (text-style == head(cache))
    tail(cache)		// one-element cache hit
  else
    do-text-style-mapping(_port, text-style, #f);
  end
end method text-style-mapping;
