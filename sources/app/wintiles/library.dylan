Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library wintiles
  use dylan;
  use simple-draw;
  use tiles-lib;
  use c-ffi;
  use simple-streams;
  use dylan-print;
end library;

define module wintiles
  use dylan;
  use simple-draw;
  use tiles;
  use c-ffi;
  use simple-streams;
  use dylan-print;
end module;

// eof
