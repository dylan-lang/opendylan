Module:    gui-testworks
Filename:  emulator-progress-window.dylan
Summary:   GUI progress window for Tesworks -- Emulator patches
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Startup/shutdown functions

import-cl-functions(clue(fork-and-return-window) (as: fork-and-return-window));

define function do-start-progress-window ()
  fork-and-return-window
    (method ()
       start-frame(*progress-window*);
       *progress-window*
     end,
     $progress-window-name);
end function do-start-progress-window;
