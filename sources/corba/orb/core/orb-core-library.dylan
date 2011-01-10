Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library orb-core
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use portableserver-protocol;
  use orb-iiop;
  use orb-utilities;
  use network;
  export orb-core;
  export orb-core-externals;
end library;

define module orb-core-externals
  create
    <object-reference>,
    corba/orb/create-reference,
    orb-service-port,
    orb-service-port-setter;
end module;

define module orb-core
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use portableserver-protocol;
  use orb-iiop;
  use orb-utilities;
  use orb-core-externals, export: all;
  use sockets;
  export
    <adaptor>,
    orb-adaptor, orb-adaptor-setter, 
    orb-adaptor-lock,
    adaptor-create-reference,
    wait-for-orb-port,
    note-adaptor-shutdown,
    corba/object/-is-a,
    \orb-arg-processor-definer,
    \initial-service-definer,
    object-reference-collocated-cache, object-reference-collocated-cache-setter,
    object-reference-collocated-ticket, object-reference-collocated-ticket-setter,
    orb-initial-services-setter,
    orb-connection-manager, orb-connection-manager-setter;
end module;
