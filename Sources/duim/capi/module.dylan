Module:       Dylan-User
Synopsis:     CAPI back-end
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module capi-duim
  use dylan;
  use duim-imports;
  use duim-internals;
  use duim-gadget-panes-internals;
  
  // Export the useful set of CAPI gadget classes, etc.
  export <capi-gadget-mixin>,
         <capi-mirror>,
         <capi-frame-manager>,
         <capi-port>,
         make-capi-mirror;

  export <capi-text-editor-pane>;
end module capi-duim;
