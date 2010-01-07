Module:    dylan-user
Synopsis:  Play with MIDI
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module plonker
  use functional-dylan;
  use duim;
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use simple-format;
  use simple-random;
  use c-ffi; use win32-multimedia; // Just for testing purposes
  use midi;
end module plonker;
