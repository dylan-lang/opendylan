Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dylan-orb
  use functional-dylan;
  use corba-dylan;
  use corba-protocol;
  use portableserver-protocol;
  use iop-protocol;
  use orb-utilities;
  use orb-iiop;
  use orb-streams;
  use orb-connections;
  use orb-core;
  use orb-poa;
  use orb-ir;
  export dylan-orb;
  export dylan-orb-internals;
  export dylan-orb-without-ir;
end library;

define module dylan-orb-internals
  use functional-dylan;
  use corba-protocol, export: all; // ---*** stubs etc need <struct-typecode> etc
  use iop-protocol, export: all;
  use orb-utilities, export: all; // ---*** test suite uses architecture-little-endian?
  use orb-iiop, export: all; // ---*** test suite uses marshall, unmarshall
  use orb-streams, export: all; // ---*** test suite uses with-marshalling-stream, <marshalling-stream>
end module;

define module dylan-orb
  use functional-dylan;
  use corba-protocol-externals, export: all;
  use portableserver-protocol, export: all;
  use orb-ir, export: all;
  use orb-core-externals, export: all;
end module;

define module dylan-orb-without-ir
  use functional-dylan;
  use corba-protocol, export: all;
  use portableserver-protocol, export: all;
  use orb-ir-without-ir, export: all;
  use orb-core-externals, export: all;
end module;

