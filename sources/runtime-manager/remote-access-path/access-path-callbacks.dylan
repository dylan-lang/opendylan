Module:        remote-access-path
Synopsis:      Distributed Rnub
Author:        Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <AccessPath-implementation> (Rtmgr/<AccessPath-servant>)
  slot AccessPath-reference :: Rtmgr/<AccessPath>;
end class;

define method initialize (path :: <AccessPath-implementation>, #key)
  next-method();
  let orb = CORBA/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = CORBA/Orb/resolve-initial-references(orb, "RootPOA");
  let poa =
    PortableServer/POA/create-poa(root-poa, "Access Path CallBack POA",
				  #f, lifespan-policy: #"transient");
  let reference = PortableServer/POA/servant-to-reference(poa, path);

  path.AccessPath-reference := as(Rtmgr/<AccessPath>, reference);

  let poa-manager = PortableServer/POA/the-poamanager(poa);
  PortableServer/POAManager/activate(poa-manager);
  path
end method;

define variable *access-path-server* = #f;

define method access-path-server()
 => (server :: <AccessPath-implementation>)
  *access-path-server*
  | (*access-path-server* := make(<AccessPath-implementation>));
end method;


define method Rtmgr/AccessPath/create-thread-stop-reason-handler
    (path :: <AccessPath-implementation>,
     process :: Rtmgr/AccessPath/<RNUBPROCESS>,
     thread :: Rtmgr/AccessPath/<RNUBTHREAD>,
     priority :: Rtmgr/AccessPath/<NUBINT>)
 => ()
  create-thread-stop-reason-handler
  (as-remote-pointer(process),
   as-remote-pointer(thread),
   priority);
end method;

define method Rtmgr/AccessPath/debugger-message
    (path :: <AccessPath-implementation>, message :: CORBA/<string>, arg1 :: Rtmgr/AccessPath/<RTARGET-ADDRESS>, arg2 :: Rtmgr/AccessPath/<RTARGET-ADDRESS>)
 => ()
  debugger-message(message, as-remote-value(arg1), as-remote-value(arg2))
end method;

define method Rtmgr/AccessPath/nub-debug-message
    (path :: <AccessPath-implementation>, message :: CORBA/<string>, arg1 :: Rtmgr/AccessPath/<RTARGET-ADDRESS>, arg2 :: Rtmgr/AccessPath/<RTARGET-ADDRESS>)
 => ()
  nub-debug-message(message, as-remote-value(arg1), as-remote-value(arg2))
end method;

define method Rtmgr/AccessPath/debugger-error
    (path :: <AccessPath-implementation>, message :: CORBA/<string>, arg1 :: Rtmgr/AccessPath/<RTARGET-ADDRESS>, arg2 :: Rtmgr/AccessPath/<RTARGET-ADDRESS>)
 => ()
  debugger-error(message, as-remote-value(arg1), as-remote-value(arg2))
end method;

