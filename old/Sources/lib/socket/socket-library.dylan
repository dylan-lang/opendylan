module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library socket
  // HQN
  use functional-dylan;
  use operating-system;
  use byte-vector;

  use streams;
  use standard-io;
  use format;
  use print;

  // FFI
  use c-ffi;

  export socket;
  export socket-internals;
  export inet-address;
  export inet-address-internals;
end library;

//
// Interface modules
//

define module socket
  create <socket>, <socket-stream>,
    socket-create, socket-connect, socket-close,
    socket-read, socket-write;

  create socket-port, socket-localport, 
    socket-inet-address, socket-data-stream;

  create socket-factory-setter;

  // Start/stop system api
  create socket-ffi-initialize, socket-ffi-finalize;

  // Conditions
  create <socket-condition>, <socket-error>, <socket-warning>,
    <unknown-host-error>, <socket-connection-refused-error>,
    socket-error, socket-warning;
end module socket;

// This is a separate module since it imports lots of stuff that is not imported 
// in the *- internals. It oculd be merged into socket-internals;
//
define module socket-conditions
  // Substrate
  use functional-dylan;
  
  use streams;
  use print;
  use format;
  use standard-io;

  // Interface
  use socket;
end module socket-conditions;

define module inet-address
  create get-host-addr-by-name, get-host-name-by-addr, local-host-name;

  create <inet-address>, 
    address, host-name, host-address, host-aliases, address-family;
  create inet-address-by-host-name, any-local-address;
  create *any-local-address*, *local-host*;
  create $AF-UNSPEC, $AF-INET, $AF-UNIX;

  use socket, export: { <unknown-host-error> };
end module;

//
// Implementation Modules
//

define module inet-address-internals
  use functional-dylan;
  use operating-system;

  use c-ffi;
  use byte-vector;
  use format, import: {format-to-string};

  // Interface
  use inet-address, export: all;

  export <sys-in-addr>, <sys-in-addr*>, export-in-addr, import-in-addr;
  
  export $ADDR-ANY;
end module inet-address-internals;

define module socket-internals
  // Substrate modules
  use functional-dylan;
  use c-ffi;
  use byte-vector;
  use format, import: {format-to-string};

  use inet-address-internals; // needs to be -internals to get <sys-in-addr>
  use streams-internals, rename: { close =>  stream/close };

  // Interface
  use socket,  export: all;

  export $SOCK-STREAM, $SOCK-DGRAM, 
    $PF-UNSPEC, $PF-INET, $PF-UNIX;
  export c-socket-create, c-socket-connect, c-socket-close;
  export c-socket-recv, c-socket-send;

  // Error protocol
  export socket-error-string;

  // Streams accessor Protocol
  export <socket-accessor>, socket-descriptor;
end module socket-internals;


