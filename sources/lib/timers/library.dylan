Module:    dylan-user
Synopsis:  The timers library and its modules
Author:    Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library timers
  use dylan;
end library;

define module timers
  use dylan;
  export
    <execution-timer>, set-start, set-end, system-time, user-time, total-time,
    \with-execution-times;
end module;
