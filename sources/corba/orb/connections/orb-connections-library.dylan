Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library orb-connections
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use portableserver-protocol;
  use orb-utilities;
  use orb-iiop;
  use orb-streams;
  use orb-core;
  use network;
  export orb-connections;
end library;

define module orb-connections
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use portableserver-protocol; // NB for collocation stuff
  use orb-utilities;
  use orb-iiop;
  use orb-streams;
  use orb-core;
  use sockets;
  export
    <request>,
    request-object,
    request-operation-name,
    request-arguments,
    request-in-args,
    request-out-args,
    request-oneway?,
    request-user-exception-typecodes,
    request-context,
    request-context-expression,
    request-result,
    process-request-arguments,
    compute-context-values,
    connection-manager-stream-class,
    connection-manager-error-class,
    receive-connections,
    hostname;

end module;
