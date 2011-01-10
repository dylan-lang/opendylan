Module:    dylan-user
Synopsis:  Harlequin Logo Flying Squares OpenGL DUIM demo
Authors:   Jonathan Bachrach, Gary Palter, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library flying-squares
  use functional-dylan;
  use duim;
  use win32-duim;
  // use duim-gl;
  use c-ffi;
  use win32-common;
  use win32-gdi;
  use win32-gl;
  use win32-glu;
end library flying-squares;
