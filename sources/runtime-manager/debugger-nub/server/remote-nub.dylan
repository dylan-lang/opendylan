Module:        remote-nub
Synopsis:      The CORBA Debugger Nub of the Remote Debugger
Author:        Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $max-spy-function-arguments = 16;
define constant $max-stepping-locations = 512;

define variable *remote-debugger-nub* = #f;

define class <RemoteNub-implementation> (Rtmgr/<RemoteNub-servant>)

  slot nub-process :: Rtmgr/RemoteNub/<RNUB>;

  constant slot nub-process-id :: <string>,
    required-init-keyword: id:;

  slot remote-process :: <ffi-integer>;

  constant slot spy-function-argument-vector :: <REMOTE-ARG-ARRAY>
    = make(<REMOTE-ARG-ARRAY>, element-count: $max-spy-function-arguments);

  constant slot stepping-locations-vector :: <REMOTE-ARG-ARRAY>
    = make(<REMOTE-ARG-ARRAY>, element-count: $max-stepping-locations);

  slot nub-reference :: Rtmgr/<RemoteNub>;

  slot nub-access-path :: Rtmgr/<AccessPath>;

  slot nub-server :: Rtmgr/<NubServer>;

  constant slot nub-ior-file,
    required-init-keyword: ior-file:;

  constant slot nub-orb :: corba/<orb>,
    required-init-keyword: orb:;

end class;

define method Rtmgr/RemoteNub/process (rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RNUB>)
  rnub.nub-process
end method;

define inline method process-setter
    (process :: Rtmgr/RemoteNub/<RNUB>, rnub :: <RemoteNub-implementation>)
 => (result :: Rtmgr/RemoteNub/<RNUB>)
  rnub.remote-process := export-<abstract-integer>(process);
  rnub.nub-process := process;
end method;


define method Rtmgr/RemoteNub/access-path (rnub :: <RemoteNub-implementation>)
 => (ap :: Rtmgr/<AccessPath>)
  rnub.nub-access-path
end method;


define method Rtmgr/RemoteNub/OpenNub
    (rnub :: <RemoteNub-implementation>, path :: Rtmgr/<AccessPath>)
 => ()
  rnub.nub-access-path := path;
end method;

define method Rtmgr/RemoteNub/CloseNub
    (rnub :: <RemoteNub-implementation>)
 => ()

  Rtmgr/RemoteNub/close-application(rnub);

  let event =
    create-application-event
    (concatenate(DebuggerNubEvent, rnub.nub-process-id));

  // create a separate thread to shut down the nub
  make(<thread>, 
       function:
	 method ()
	   // Wait for the Debugger Server to prompt
           // us to shut down on return from this operation
           // to avoid race conditions and deadlocks with POA threads
	   wait-for-application-event(event);
	   exit-application(0);
	 end method);

end method;


define constant $nub-directory = as(<string>, temp-directory() | "");

define constant $nub-server-ior-file = concatenate($nub-directory, "nub-server.ior");

define constant DebuggerNubEvent = "Functional Developer Debugger Nub ";


define method main () => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let poa =
    PortableServer/POA/create-poa(root-poa, "Rnub POA",
				  #f, lifespan-policy: #"transient");
  let application-args = application-arguments();
  let ior-file :: <string> = first(application-args);
  let process-id :: <string> = second(application-args);
  let rnub =
    make(<RemoteNub-implementation>,
	 ior-file: ior-file, orb: orb, id: process-id);
  let reference = portableserver/poa/servant-to-reference(poa, rnub);

  *remote-debugger-nub* := rnub;
  rnub.nub-reference := as(Rtmgr/<RemoteNub>, reference);
  corba/orb/object-to-file(orb, rnub.nub-ior-file, reference);

  // Signal the Debugger Server that it can now read
  // in our reference
  signal-application-event
    (concatenate(DebuggerNubEvent, process-id));

  // Read in the Debugger Server reference
  let server =
    as(Rtmgr/<NubServer>,
       corba/orb/file-to-object(orb, $nub-server-ior-file));
  rnub.nub-server := server;

  let poa-manager = PortableServer/POA/the-poamanager(poa);

  PortableServer/POAManager/activate(poa-manager);
  corba/orb/run(orb);
end method main;

begin
  main();
end;
