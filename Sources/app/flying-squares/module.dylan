Module:    dylan-user
Synopsis:  Harlequin Logo Flying Squares OpenGL DUIM demo
Authors:   Jonathan Bachrach, Gary Palter, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module flying-squares
  use functional-dylan;
  use simple-format;
  use simple-random;
  use transcendentals;
  use threads;
  use duim;
  use duim-internals, exclude: { position };
  use win32-duim;
  // use duim-gl;
  use c-ffi;
  use win32-common, exclude: { <POINT> };
  use win32-gdi, exclude: { <PATTERN> };
  use win32-gl;
  use win32-glu;
end module flying-squares;

// eof
