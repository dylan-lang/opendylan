Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library orb-poa
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use portableserver-protocol;
  use orb-iiop;
  use orb-streams;
  use orb-utilities;
  use orb-core;
  use orb-connections;
  use network;
  export orb-poa;
end library;

define module orb-poa
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use portableserver-protocol;
  use orb-streams;
  use orb-utilities;
  use orb-core;
  use orb-iiop;
  use orb-connections;
  use sockets;
end module;
