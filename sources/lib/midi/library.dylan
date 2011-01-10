Module:    dylan-user
Synopsis:  Raw MIDI interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library midi
  use functional-dylan;
  use win32-common;
  use win32-multimedia;
  use c-ffi;
  use collections;

  export midi;
end library midi;
