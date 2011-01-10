Module: orb-poa
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// ---*** DEFAULT-POA maybe useful for future _this method
//
//define method default-poa (object :: PortableServer/<servant>)
// *root-poa*
//    | error("No ORB currently initialized");
//end method;

define sideways method portableserver/servant/primary-interface
    (servant :: PortableServer/<dynamic-servant>, objectid :: <string>, poa :: portableserver/<poa>)
 => (repository-id :: <string>)
  error(make(corba/<no-implement>))
end method;

define sideways method portableserver/servant/primary-interface
    (servant :: PortableServer/<servant>, objectid :: <string>, poa :: portableserver/<poa>)
 => (repository-id :: <string>)
  typecode-repository-id(object-typecode(servant))
end method;

define sideways method ensure-any-typecode-consistent
    (typecode :: <object-reference-typecode>, servant :: portableserver/<servant>)
 => (coerce? :: <boolean>, new-value :: corba/<object>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let current = corba/orb/resolve-initial-references(orb, "POACurrent");
  let poa = portableserver/current/get-poa(current);
  values(#t, as(typecode-native-type(typecode),
		portableserver/poa/servant-to-reference(poa, servant)));
end method;


