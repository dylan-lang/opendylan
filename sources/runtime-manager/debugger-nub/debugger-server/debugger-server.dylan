Module:    debugger-server
Synopsis:  Distributed Debugger Server
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $nub-directory = as(<string>, temp-directory() | "");

define class <NubServer-implementation> (Rtmgr/<NubServer-servant>)

  constant slot nubs :: <stretchy-vector> = make(<stretchy-vector>);

  constant slot connections :: <stretchy-vector> = make(<stretchy-vector>);

  constant slot nub-server-ior-file =
    concatenate($nub-directory, "nub-server.ior");

  constant slot refresh-server :: <function>, required-init-keyword: refresh:;

  slot server-password :: <string> = "";

end class;

define class <remote-debugger-nub>(<object>)

  constant slot remote-nub-reference :: Rtmgr/<RemoteNub>,
    required-init-keyword: nub:;

  constant slot remote-process-name :: <string>,
    required-init-keyword: name:;

  constant slot remote-process-id :: <string>,
    required-init-keyword: id:;

  constant slot remote-machine-name :: <string>,
    required-init-keyword: machine:;

end class;

define method debugger-nub-description
    (nub :: <remote-debugger-nub>) => (description :: <string>)
  concatenate("Debugging ", nub.remote-process-name,
	      " on ", nub.remote-machine-name);
end method;

define method debugger-connection-description
    (network-client :: <string>) => (description :: <string>)
  concatenate("Connected to ", network-client);
end method;

define method add-debugger-connection
    (server :: <NubServer-implementation>, network-client :: <string>) => ()
  let connections = server.connections;
  unless (member?(network-client, connections, test: \=))
    add!(connections, network-client);
  end;
end method;


define method Rtmgr/NubServer/nubs (server :: <NubServer-implementation>)
 => (result :: Rtmgr/NubServer/<RemoteNub-seq>)
  map(remote-nub-reference, server.nubs)
end method;

define variable process-count :: <integer> = -1;

define constant DebuggerNubEvent = "Functional Developer Debugger Nub ";

define method Rtmgr/NubServer/CreateNub
    (server :: <NubServer-implementation>,
     process-name :: CORBA/<string>,
     remote-machine :: CORBA/<string>)
 => (remote-nub :: Rtmgr/<RemoteNub>)
  process-count := process-count + 1;

  let process-id = integer-to-string(process-count);
  let nub-ior-file =
    concatenate($nub-directory, "nub-", process-id, ".ior");

  let event =
    create-application-event
    (concatenate(DebuggerNubEvent, process-id));

  run-application(concatenate("remote-nub.exe", " ", nub-ior-file, " ", process-id),
		  under-shell?: #f,
		  inherit-console?: #f,
		  minimize?: #t,
		  activate?: #f,
		  asynchronous?: #t);

  // Wait for the spawned Debugger Nub to signal that it
  // has written its reference
  wait-for-application-event(event);

  let orb = CORBA/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  let remote-nub = 
    as(Rtmgr/<RemoteNub>,
       corba/orb/file-to-object(orb, nub-ior-file));

  Rtmgr/NubServer/RegisterNub
    (server, remote-nub, process-name, process-id, remote-machine);

  remote-nub

end method;

define method Rtmgr/NubServer/DestroyNub
    (server :: <NubServer-implementation>, remote-nub :: Rtmgr/<RemoteNub>)
 => ()

  // Tell the Debugger Nub that it is shut-down time
  Rtmgr/RemoteNub/CloseNub(remote-nub);

  let process-id :: <string> =
    any?(method(nub :: <remote-debugger-nub>)
	     if (nub.remote-nub-reference = remote-nub)
	       nub.remote-process-id
	     end
	 end, server.nubs);

  // Signal the Debugger Nub process that
  // it can now go ahead and shut down
  signal-application-event
    (concatenate(DebuggerNubEvent, process-id));

  Rtmgr/NubServer/DeregisterNub(server, remote-nub);

end method;

define method Rtmgr/NubServer/RegisterNub
    (server :: <NubServer-implementation>,
     remote-nub :: Rtmgr/<RemoteNub>,
     process-name :: CORBA/<string>,
     process-id :: CORBA/<string>,
     remote-machine :: CORBA/<string>)
 => ()
  add!(server.nubs,
       make(<remote-debugger-nub>,
	    nub: remote-nub, name: process-name,
	    id: process-id, machine: remote-machine));
  refresh-server(server)();
end method;

define method Rtmgr/NubServer/DeregisterNub
    (server :: <NubServer-implementation>, remote-nub :: Rtmgr/<RemoteNub>)
 => ()
  remove!(server.nubs, remote-nub,
	  test: method(nub :: <remote-debugger-nub>, remote-nub)
		    nub.remote-nub-reference = remote-nub
		end);
  refresh-server(server)();
end method;



define method Rtmgr/NubServer/get-local-hostname
    (server :: <NubServer-implementation>)
 => (result :: CORBA/<string>)
  let hostname-length = get-local-hostname-length();
  let hostname = make(<byte-string>, size: hostname-length);
  get-local-hostname(hostname-length, hostname);
  hostname
end method;

define method Rtmgr/NubServer/verify-local-password
    (server :: <NubServer-implementation>,
     password :: CORBA/<string>,
     remote-machine :: CORBA/<string>)
 => (result :: Rtmgr/NubServer/<NUBINT>)
  if (server.server-password = password)
    add-debugger-connection(server, remote-machine);
    refresh-server(server)();
    1
  else 0
  end;
end method;

define method Rtmgr/NubServer/update-local-process-list (server :: <NubServer-implementation>)
 => (result :: Rtmgr/NubServer/<NUBINT>)
  update-local-process-list();
end method;

define method Rtmgr/NubServer/local-process-nub-descriptor (server :: <NubServer-implementation>, i :: Rtmgr/NubServer/<NUB-INDEX>)
 => (result :: Rtmgr/NubServer/<RNUBPROCESS>)
  local-process-nub-descriptor(i);
end method;

define method Rtmgr/NubServer/local-process-name (server :: <NubServer-implementation>, i :: Rtmgr/NubServer/<NUB-INDEX>)
 => (result :: CORBA/<string>)
  let nl = local-process-name-length(i);
  let nm = make(<byte-string>, size: nl);
  local-process-name(i, nl, nm);
  nm
end method;

define method Rtmgr/NubServer/local-process-system-identifier (server :: <NubServer-implementation>, i :: Rtmgr/NubServer/<NUB-INDEX>)
 => (result :: CORBA/<string>)
  let sys-idl = local-process-system-identifier-length(i);
  let sys-id = make(<byte-string>, size: sys-idl);
  local-process-system-identifier(i, sys-idl, sys-id);
  sys-id
end method;

define method Rtmgr/NubServer/local-process-actual-identifier (server :: <NubServer-implementation>, i :: Rtmgr/NubServer/<NUB-INDEX>)
 => (result :: Rtmgr/NubServer/<RNUB>)
  local-process-actual-identifier(i);
end method;


// Some hard-coded constants for connecting to the Debugger Server

define constant $DebuggerServerId = "Functional Developer Debugger Server";

define constant $DebuggerServerPOA = "DebuggerServerPOA";

define constant $RootPOA = "RootPOA";

define constant $DebuggerServerPort = 7777;


define method start-debugger-server
    (#key refresh :: <function> = method () => () end)
 => (server :: <NubServer-implementation>)
  let orb = CORBA/orb-init(make(CORBA/<arg-list>), "Functional Developer ORB");

  // First, set up Port agreed with Client Debugger
  orb-service-port(orb) := $DebuggerServerPort;

  let root-poa = CORBA/ORB/resolve-initial-references(orb, $RootPOA);

  // Make a Debugger Server POA with policies that support
  // remote creation of server references
  let debugger-server-poa =
    PortableServer/POA/create-poa
    (root-poa,
     $DebuggerServerPOA,
     #f,
     lifespan-policy: #"persistent",
     id-assignment-policy: #"user-id");
  let server = make(<NubServer-implementation>, refresh: refresh);

  PortableServer/POA/activate-object-with-id
    (debugger-server-poa, $DebuggerServerId, server);

  // For communicating with the Debugger Nub on local machine,
  // pass the server reference in a file
  let server-ref = portableserver/poa/servant-to-reference(debugger-server-poa, server);
  CORBA/ORB/object-to-file(orb, server.nub-server-ior-file, server-ref);

  let poa-manager = PortableServer/POA/the-poamanager(debugger-server-poa);
  PortableServer/POAManager/activate(poa-manager);

  server
end method;


