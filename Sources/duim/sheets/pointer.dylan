Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Pointers (aka, the mouse)

define protocol <<pointer-protocol>> ()
  getter pointer-sheet
    (pointer :: <pointer>) => (sheet :: false-or(<abstract-sheet>));
  getter pointer-button-state
    (pointer :: <pointer>) => (state :: <integer>);
  function pointer-position
    (pointer :: <pointer>, #key sheet) => (x :: <integer>, y :: <integer>);
  function do-pointer-position
    (port :: <abstract-port>, pointer :: <pointer>, sheet :: <abstract-sheet>)
 => (x :: <integer>, y :: <integer>);
  function set-pointer-position
    (pointer :: <pointer>, x :: <integer>, y :: <integer>, #key sheet) => ();
  function do-set-pointer-position 
    (port :: <abstract-port>, pointer :: <pointer>, sheet :: <abstract-sheet>,
     x :: <integer>, y :: <integer>) => ();
  getter pointer-cursor
    (pointer :: <pointer>) => (cursor :: <cursor>);
  setter pointer-cursor-setter
    (cursor :: <cursor>, pointer :: <pointer>) => (cursor :: <cursor>);
  function do-set-pointer-cursor
    (port :: <abstract-port>, pointer :: <pointer>, cursor :: <cursor>) => ();
  function do-set-sheet-cursor
    (port :: <abstract-port>, sheet :: <sheet>, cursor :: <cursor>) => ();
  getter pointer-grabbed?
    (pointer :: <pointer>)
 => (sheet :: false-or(<abstract-sheet>));
  getter pointer-grabbed?-setter
    (sheet :: false-or(<abstract-sheet>), pointer :: <pointer>)
 => (sheet :: false-or(<abstract-sheet>));
  function grab-pointer
    (port :: <abstract-port>, pointer :: <pointer>, sheet :: <abstract-sheet>)
 => (success? :: <boolean>);
  function ungrab-pointer
    (port :: <abstract-port>, pointer :: <pointer>)
 => (success? :: <boolean>);
  function do-with-pointer-grabbed
    (port :: <abstract-port>, sheet :: <sheet>, continuation :: <function>)
 => (#rest values);
end protocol <<pointer-protocol>>;


define sealed class <standard-pointer> (<pointer>)
  sealed slot port :: false-or(<port>) = #f,
    init-keyword: port:,
    setter: %port-setter;
  sealed slot pointer-sheet :: false-or(<sheet>) = #f;
  // The pointer button state gets maintained by each back-end
  sealed slot pointer-button-state :: <integer> = 0;
  sealed slot %x-position :: <integer> = 0;
  sealed slot %y-position :: <integer> = 0;
  sealed slot %position-changed? :: <boolean> = #f;
  sealed slot pointer-cursor :: <cursor> = #"default",
    setter: %cursor-setter;
  sealed slot pointer-grabbed? :: false-or(<sheet>) = #f,
    setter: %grabbed?-setter;
end class <standard-pointer>;

define sealed domain make (singleton(<standard-pointer>));
define sealed domain initialize (<standard-pointer>);

define sealed inline method make
    (class == <pointer>, #key port, display)
 => (pointer :: <standard-pointer>)
  ignore(display);
  make(<standard-pointer>, port: port)
end method make;

define method display
    (pointer :: <standard-pointer>) => (display :: false-or(<display>))
  let sheet = pointer-sheet(pointer);
  sheet & display(sheet)
end method display;


define sealed method pointer-cursor-setter
    (cursor :: <cursor>, pointer :: <standard-pointer>) => (cursor :: <cursor>)
  unless (pointer-cursor(pointer) == cursor)
    do-set-pointer-cursor(port(pointer), pointer, cursor);
    pointer.%cursor := cursor
  end;
  cursor
end method pointer-cursor-setter;

define method do-set-sheet-cursor
    (_port :: <port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  ignore(sheet, cursor);
  #f
end method do-set-sheet-cursor;

define method update-pointer-cursor
    (pointer :: <pointer>,
     #key sheet = pointer-sheet(pointer),
          frame = (sheet & sheet-frame(sheet)))
 => ()
  pointer-cursor(pointer)
    := (frame & frame-cursor-override(frame))
         | (sheet & sheet-cursor(sheet))
         | #"default"
end method update-pointer-cursor;


// This is robust against ungrafted sheets and umapped frames,
// because 'with-busy-cursor' often gets used in initialization code...
define macro with-busy-cursor
  { with-busy-cursor (?frame:expression, ?cursor:expression) ?:body end }
    => { begin
	   let with-busy-cursor-body = method () ?body end;
	   do-with-busy-cursor(?frame, ?cursor, with-busy-cursor-body)
         end }
  { with-busy-cursor (?frame:expression) ?:body end }
    => { begin
	   let with-busy-cursor-body = method () ?body end;
	   do-with-busy-cursor(?frame, #"busy", with-busy-cursor-body)
         end }
end macro with-busy-cursor;

define method do-with-busy-cursor
    (sheet :: <sheet>, cursor, continuation :: <function>) => (#rest values)
  let frame = sheet-frame(sheet);
  if (frame)
    do-with-busy-cursor(frame, cursor, continuation)
  else
    continuation()		// sheet is not grafted
  end
end method do-with-busy-cursor;

define method do-with-busy-cursor
    (frame :: <frame>, cursor, continuation :: <function>) => (#rest values)
  let old-cursor = frame-cursor-override(frame);
  block ()
    frame-cursor-override(frame) := cursor;
    continuation()
  cleanup
    frame-cursor-override(frame) := old-cursor
  end
end method do-with-busy-cursor;


// Returns X and Y in sheet's coordinate system
define sealed method pointer-position
    (pointer :: <standard-pointer>, #key sheet)
 => (x :: <integer>, x :: <integer>)
  if (sheet & ~(sheet == pointer-sheet(pointer)))
    // He's asking for the position w.r.t. another sheet,
    // so we have to consult the port directly
    do-pointer-position(port(pointer), pointer, sheet)
  else
    values(pointer.%x-position, pointer.%y-position)
  end
end method pointer-position;

// X and Y are in the sheet's coordinate system
define sealed method set-pointer-position
    (pointer :: <standard-pointer>, x :: <integer>, y :: <integer>, #key sheet) => ()
  pointer.%x-position := x;
  pointer.%y-position := y;
  pointer.%position-changed? := #t;
  when (sheet & ~(sheet == pointer-sheet(pointer)))
    do-set-pointer-position(port(pointer), pointer, sheet, x, y)
  end
end method set-pointer-position;

define method pointer-state-changed? 
    (pointer :: <standard-pointer>, old-sheet, old-x, old-y)
 => (changed? :: <boolean>, sheet :: <sheet>, x :: <integer>, x :: <integer>)
  let sheet = pointer-sheet(pointer);
  let (x-position, y-position) = pointer-position(pointer, sheet: sheet);
  values(~(sheet == old-sheet)
	 // Compare coordinates with eql, not =, because null values can be passed in
	 | ~(old-x == x-position)
	 | ~(old-y == y-position),
	 sheet, x-position, y-position)
end method pointer-state-changed?;


/// Pointer grabbing

//---*** What should we do if one thread tries to grab the pointer
//---*** when another thread already has the grab?  
define sealed method pointer-grabbed?-setter
    (sheet :: false-or(<sheet>), pointer :: <standard-pointer>)
 => (sheet :: false-or(<sheet>))
  let _port = port(pointer);
  unless (pointer-grabbed?(pointer) == sheet)
    if (sheet)
      let new-focus = sheet;
      let old-focus = port-input-focus(_port);
      when (grab-pointer(_port, pointer, sheet))
	when (new-focus ~== old-focus
	      & port-focus-policy(_port) == #"sheet-under-pointer"
	      & sheet-handles-keyboard?(new-focus))
	  port-input-focus(_port) := new-focus;
	end;
	pointer.%grabbed? := sheet
      end
    else
      when (ungrab-pointer(_port, pointer))
	pointer.%grabbed? := #f
      end
    end
  end;
  sheet
end method pointer-grabbed?-setter;

define method grab-pointer
    (_port :: <port>, pointer :: <pointer>, sheet :: <sheet>)
 => (success? :: <boolean>)
  #t
end method grab-pointer;

define method ungrab-pointer
    (_port :: <port>, pointer :: <pointer>)
 => (success? :: <boolean>)
  #t
end method ungrab-pointer;

define macro with-pointer-grabbed
  { with-pointer-grabbed (?sheet:name) ?:body end }
    => { begin
           let with-pointer-grabbed-body = method () ?body end;
	   let _port = port(?sheet);
	   do-with-pointer-grabbed(_port, ?sheet, with-pointer-grabbed-body)
         end }
end macro with-pointer-grabbed;

define sealed method do-with-pointer-grabbed
    (_port :: <port>, sheet :: <sheet>, continuation :: <function>)
 => (#rest values)
  let pointer = port-pointer(_port);
  block ()
    pointer-grabbed?(pointer) := sheet;
    continuation()
  cleanup
    pointer-grabbed?(pointer) := #f;
  end
end method do-with-pointer-grabbed;
