Module:    dylan-user
Synopsis:  Imported Lotus Notes COM interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library lotus-notes
  use c-ffi;
  use ole-automation;
  use functional-dylan;

  export lotus-notes;
end library lotus-notes;
