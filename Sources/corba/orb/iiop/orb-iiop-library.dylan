Module: dylan-user
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library orb-iiop
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use orb-streams;
  use orb-utilities;
  export orb-iiop;
end library;

define module orb-iiop
  use corba-dylan;
  use corba-protocol;
  use iop-protocol;
  use orb-streams;
  use orb-utilities;
  export
    marshall,
    unmarshall,
    $iiop-version,
    $giop-version,
    $giop-magic,
    parse-ior-from-string, parse-ior-from-file,
    unparse-ior-to-string, unparse-ior-to-file,
    profile-as-ior,
    get-iiop-profile,
    iiop/<location>,
    iiop/location/repository-id, iiop/location/repository-id-setter,
    iiop/location/host, iiop/location/host-setter,
    iiop/location/port, iiop/location/port-setter,
    iiop/location/adaptor-path, iiop/location/adaptor-path-setter,
    iiop/location/objectid, iiop/location/objectid-setter;
end module;
