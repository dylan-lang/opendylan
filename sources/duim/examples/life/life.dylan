Module:       life
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// Startup code.
//

// This is the Life application's start function, specified in the
// Project > Settings dialog.  The initial breakpoint is set here
// (i.e., this is where the debugger stops your application when you
// choose Application > Debug or Application > Interact).

define method life () => ()
  let frame = make(<life-frame>, title: "Life");
  start-frame(frame);
end method life;


begin
  life()
end;
