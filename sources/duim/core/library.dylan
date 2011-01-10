Module:       Dylan-User
Synopsis:     DUIM core
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The top-level DUIM library, exports all core functionality
define library duim-core
  use dylan;

  use duim-utilities,         export: all;
  use duim-geometry,          export: all;
  use duim-DCs,               export: all;
  use duim-sheets,            export: all;
  use duim-graphics,          export: all;
  use duim-extended-geometry, export: all;
  use duim-layouts,           export: all;
  use duim-gadgets,           export: all;
  use duim-frames,            export: all;
  use duim-recording,         export: all;

  export duim;
  export duim-internals;
end library duim-core;
