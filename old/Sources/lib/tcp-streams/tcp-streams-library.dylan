Module:    dylan-user
Author:    Marc Ferguson, Scott McKay
Synopsis:  Define TCP streams library and modules
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library tcp-streams
  use dylan;
  use functional-extensions;
  use operating-system;
  use byte-vector;
  use streams;

  export tcp-streams;
end library tcp-streams;

define module tcp-streams
  use dylan;
  use functional-extensions;
  use dylan-direct-c-ffi;
  use operating-system;
  use byte-vector;
  use streams-internals;
  
  export <tcp-stream>,
         input-buffer-handshake, input-buffer-handshake-setter,
         output-buffer-handshake, output-buffer-handshake-setter;
end module tcp-streams;
