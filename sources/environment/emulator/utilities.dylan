Module:    emulator-environment
Synopsis:  Emulator Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Utilities

define lisp-interface
  functions fork-and-return-window from clue;
end;

define method make-environment-thread
    (portd :: <port-designator>, class :: subclass(<environment-frame>),
     #key name, function)
 => ()
  fork-and-return-window
    (method ()
       function()
     end,
     name)
end method make-environment-thread;
