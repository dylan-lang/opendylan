Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	      Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Vanilla port class

define sealed class <vanilla-port> (<basic-port>)
end class <vanilla-port>;

define method initialize (_port :: <vanilla-port>, #key) => ()
  next-method();
end method initialize;
 

define sideways method class-for-make-port
    (type == #"vanilla", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  ignore(initargs);
  //--- You might want to return a new set of initargs here...
  values(<vanilla-port>, #f)
end method class-for-make-port;

// #"local" is the 'default' port type used if none is specified
define sideways method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  apply(class-for-make-port, #"vanilla", initargs)
end method class-for-make-port;

define method port-type (_port :: <vanilla-port>) => (type :: <symbol>)
  #"vanilla"
end method port-type;

define method port-name (_port :: <vanilla-port>) => (name :: false-or(<string>))
  ignoring("port-name");
  #f
end method port-name;


/// Beeping, etc

define method force-display (_port :: <vanilla-port>) => ()
  ignoring("force-display")
end method force-display;

define method synchronize-display (_port :: <vanilla-port>) => ()
  ignoring("synchronize-display")
end method synchronize-display;

define method beep (_port :: <vanilla-port>) => ()
  ignoring("beep")
end method beep;


/// Pointers

define method do-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  //--- Get pointer position w.r.t. sheet
  not-yet-implemented("do-pointer-position")
end method do-pointer-position;

define method do-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  //--- Get pointer position w.r.t. the display
  not-yet-implemented("do-pointer-position")
end method do-pointer-position;

define method do-set-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <integer>, y :: <integer>) => ()
  //--- Set pointer position w.r.t. sheet
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;

define method do-set-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <integer>, y :: <integer>) => ()
  //--- Set pointer position w.r.t. the display
  ignoring("do-set-pointer-position")
end method do-set-pointer-position;

define method do-set-pointer-cursor
    (_port :: <vanilla-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  //--- Set the pointer cursor
  ignoring("do-pointer-position")
end method do-set-pointer-cursor;


define method do-set-sheet-cursor
    (_port :: <vanilla-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  //--- Set the cursor for the sheet
  ignoring("do-pointer-position")
end method do-set-sheet-cursor;


//--- Define the keysyms for the port


/// Input focus handling

define sealed method note-focus-in
    (_port :: <vanilla-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-in")
end method note-focus-in;

define sealed method note-focus-out
    (_port :: <vanilla-port>, sheet :: <sheet>) => ()
  next-method();
  ignoring("note-focus-out")
end method note-focus-out;

