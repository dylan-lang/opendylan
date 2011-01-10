Module:       Dylan-User
Synopsis:     DUIM core
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The DUIM API library, exports all core API functionality
define module duim
  // Use all the API modules, exporting their contents
  use duim-geometry,  export: all;
  use duim-DCs,       export: all;
  use duim-sheets,    export: all;
  use duim-graphics,  export: all;
  use duim-layouts,   export: all;
  use duim-gadgets,   export: all;
  use duim-frames,    export: all;
  use duim-recording, export: all;
end module duim;

// An implementor's library, exports all internal functionality
define module duim-internals
  // Use all the internal modules, exporting their contents
  use duim-utilities,           export: all;
  use duim-geometry-internals,  export: all;
  use duim-DCs-internals,       export: all;
  use duim-sheets-internals,    export: all;
  use duim-graphics-internals,  export: all;
  use duim-layouts-internals,   export: all;
  use duim-gadgets-internals,   export: all;
  use duim-frames-internals,    export: all;
  use duim-recording-internals, export: all;
end module duim-internals;
