Module:    dylan-user
Synopsis:  Imported Lotus Notes COM interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module lotus-notes
  use c-ffi;
  use ole-automation;
  use byte-vector;
  use finalization;
  use functional-dylan;
  use machine-word;
  use simple-format;
  use simple-random;
  use transcendentals;
  use type-library-module, export: all;

  export
    $session-id, cast-object,
    $TEMPLATE-CANDIDATE;

end module lotus-notes;
