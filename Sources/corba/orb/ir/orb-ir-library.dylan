Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library orb-ir
  use corba-dylan;
  use corba-protocol;
  use portableserver-protocol;
  use iop-protocol;
  use orb-core;
  use orb-connections;
  use ir-stubs;
  export orb-ir;
  export orb-ir-without-ir;
end library;

define module orb-ir-protocol
  create
    corba/object/get-interface,
    corba/orb/create-operation-list,
    corba/orb/create-struct-tc,
    corba/orb/create-union-tc,
    corba/orb/create-enum-tc,
    corba/orb/create-alias-tc,
    corba/orb/create-interface-tc,
    corba/orb/create-exception-tc,
    corba/orb/create-string-tc,
    corba/orb/create-wstring-tc,
    corba/orb/create-fixed-tc,
    corba/orb/create-sequence-tc,
    corba/orb/create-recursive-sequence-tc,
    corba/orb/create-array-tc;

  create
    call-dii,
    dii,
    invalidate-requests,
    \with-dii,
    \dii-methods-definer,
    <no-interface-definition>,
    <no-operation-definition>;
end module;

define module orb-ir
  use corba-dylan;
  use corba-protocol;
  use portableserver-protocol;
  use iop-protocol;
  use orb-core;
  use orb-connections;
  use ir-stubs, export: all;
  use orb-ir-protocol, export: all;
end module;

define module orb-ir-without-ir
  use corba-dylan;
  use corba-protocol;
  use portableserver-protocol;
  use iop-protocol;
  use orb-core;
  use orb-connections;
  use orb-ir-protocol, export: all;
end module;