Module:   timers
Synopsis: Imports of the supporting functionality from the emulator
Language: infix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

import-cl-functions 
  (system(make-cpu-times)(as: make-cpu-times),
   system(get-cpu-times)(as: get-cpu-times),
   system(get-cpu-times-user-secs)(as: cpu-times-user-secs),
   system(get-cpu-times-user-micros)(as: cpu-times-user-micros),
   system(get-cpu-times-system-secs)(as: cpu-times-system-secs),
   system(get-cpu-times-system-micros)(as: cpu-times-system-micros));

// eof
