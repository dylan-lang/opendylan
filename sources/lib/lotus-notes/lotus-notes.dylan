Module:    lotus-notes
Synopsis:  Imported Lotus Notes COM interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $session-id
  = as(<REFCLSID>, "{29131401-2EED-1069-BF5D-00DD011186B7}");

define method cast-object (class :: <class>, untyped) => (object)
  // There should be an lighter weight way of doing this through casting.
  let typed
    = make(class, disp-interface: untyped, interface-id: $IID-IDispatch);
  Release(untyped);
  typed
end method;

define constant $TEMPLATE-CANDIDATE = 1246;
