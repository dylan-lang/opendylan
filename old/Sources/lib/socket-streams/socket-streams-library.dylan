module: dylan-user
Author: James Casey
Synopsis: A stream suitable for binding to a network socket
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library socket-streams
  use functional-dylan;
  use streams;
  use byte-vector;
  use c-ffi;

  export socket-streams;
  export socket-streams-internals;
end library socket-streams;

define module socket-streams
  // Basic stream classes
  create <socket-stream>;

  // Buffers
  create
    input-buffer-handshake, input-buffer-handshake-setter,
    output-buffer-handshake, output-buffer-handshake-setter;
  
end module socket-streams;

define module socket-streams-internals
  use functional-dylan;
  use streams-internals;
  use byte-vector;
  use c-ffi;

  use socket-streams, export: all;

  export <socket-accessor>, socket-descriptor;
end module socket-streams-internals;




