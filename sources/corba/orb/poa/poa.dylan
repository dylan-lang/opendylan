Module: orb-poa
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <poa-manager-state> = one-of(#"inactive", #"active", #"holding", #"discarding");

define class <poa-manager> (PortableServer/<PoaManager>) 
  slot poa-manager-state :: <poa-manager-state> = #"holding";
  constant slot poa-manager-state-notification :: <notification>
    = make(<notification>, name: "Waiting for POA Manager state change", lock: make(<lock>));
  slot poa-manager-poas :: <stretchy-vector> = make(<stretchy-vector>);
end class;

define sealed domain make (subclass(<poa-manager>));
define sealed domain initialize (<poa-manager>);

define constant <thread-policy> = type-union(<integer>, one-of(#"orb-ctrl-model", #"single-thread-model"));
define constant <lifespan-policy> = one-of(#"transient", #"persistent");
define constant <id-uniqueness-policy> = one-of(#"unique-id", #"multiple-id");
define constant <id-assignment-policy> = one-of(#"user-id", #"system-id");
define constant <implicit-activation-policy> = one-of(#"implicit-activation", #"no-implicit-activation");
define constant <servant-retention-policy> = one-of(#"retain", #"non-retain");
define constant <request-processing-policy> = one-of(#"use-active-object-map-only",
						     #"use-default-servant",
						     #"use-servant-manager");


define class <poa-policies> (<object>)
  constant slot poa-thread-policy :: <thread-policy> ,
    init-value: #"orb-ctrl-model",
    init-keyword: thread-policy:;
  constant slot poa-lifespan-policy :: <lifespan-policy>,
    init-value: #"transient",
    init-keyword: lifespan-policy:;
  constant slot poa-id-uniqueness-policy :: <id-uniqueness-policy>,
    init-value: #"unique-id",
    init-keyword: id-uniqueness-policy:;
  constant slot poa-id-assignment-policy :: <id-assignment-policy>,
    init-value: #"system-id",
    init-keyword: id-assignment-policy:;
  constant slot poa-implicit-activation-policy :: <implicit-activation-policy>,
    init-value: #"implicit-activation",
    init-keyword: implicit-activation-policy:;
  constant slot poa-servant-retention-policy :: <servant-retention-policy>,
    init-value: #"retain",
    init-keyword: servant-retention-policy:;
  constant slot poa-request-processing-policy :: <request-processing-policy>,
    init-value: #"use-active-object-map-only",
    init-keyword: request-processing-policy:;
end class;

define sealed domain make (subclass(<poa-policies>));
define sealed domain initialize (<poa-policies>);

// NB Cache default POA policies so they can be shared by POAs

define variable *default-poa-policies* :: false-or(<poa-policies>) = #f;

define method make (class == <poa-policies>, #rest initargs, #key)
 => (object :: <poa-policies>)
  if (empty?(initargs))
    *default-poa-policies* | (*default-poa-policies* := next-method());
  else
    next-method();
  end if;
end method;

define class <poa> (PortableServer/<Poa>, <adaptor>)
  constant slot poa-id :: <string> = compute-poa-id();
  constant slot poa-name :: corba/<string>, required-init-keyword: name:;
  constant slot poa-parent :: false-or(<poa>) = #f, required-init-keyword: parent:;
  constant slot poa-manager :: <poa-manager> = make(<poa-manager>), required-init-keyword: poamanager:;
  slot poa-activator :: false-or(portableserver/<adapteractivator>) = #f, init-keyword: activator:;
  constant slot poa-active-object-table :: <string-table> = make(<string-table>);
  slot poa-servant-manager :: false-or(portableserver/<servantmanager>) = #f, init-keyword: servant-manager:;
  slot poa-default-servant :: false-or(portableserver/<servant>) = #f, init-keyword: default-servant:;
  constant slot poa-policies :: <poa-policies>, required-init-keyword: policies:;
  slot poa-children :: <stretchy-vector> = make(<stretchy-vector>), init-keyword: children:;
  slot poa-port :: false-or(<integer>), required-init-keyword: port:;
  constant slot poa-shutdown-notification :: <notification> =
    make(<notification>, name: "Waiting for POA Shutdown", lock: make(<lock>));
  constant slot poa-mailbox :: <mailbox> = make(<mailbox>);
  slot poa-threads :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot poa-lock :: <lock> = make(<lock>);
end class;

define sealed domain make (subclass(<poa>));
define sealed domain initialize (<poa>);

define method initialize (poa :: <poa>, #key)
  next-method();
  let manager = poa-manager(poa);
  poa-manager-poas(manager) := add!(poa-manager-poas(manager), poa);
  let parent = poa-parent(poa);
  if (parent)
    poa-children(parent) := add!(poa-children(parent), poa);
  end if;
  create-poa-threads(poa);
  invalidate-collocations();
end method;

define class <root-poa> (<poa>)
  keyword name: = "RootPOA";
  keyword policies: = make(<poa-policies>);
  keyword activator: = make(<root-poa-activator>);
  constant slot poa-orb :: corba/<orb>, required-init-keyword: orb:;
end class;

define method initialize (poa :: <root-poa>, #key)
  next-method();
  ensure-poa-listener-started(poa);
end method;

define locked variable *poa-thread-id* :: <integer> = 0;

define method create-poa-threads (poa :: <poa>)
  let threads = compute-poa-threads-size(poa);
  for (i from 1 to threads)
    create-poa-thread(poa);
  end for;
end method;

define method compute-poa-threads-size (poa :: <poa>)
  let policy = poa-thread-policy(poa-policies(poa));
  select (policy by instance?)
    <thread-policy> => select (policy)
			 #"orb-ctrl-model" => 1;
			 #"single-thread-model" => 1;
		       end select;
    <integer> => max(policy, 1);
  end select;
end method;

define method create-poa-thread (poa :: <poa>)
  make(<thread>,
       name: format-to-string("POA request processor %d for %s",
			      atomic-increment!(*poa-thread-id*),
			      poa-name(poa)),
       function: method ()
		   receive-request(poa);
		 end method);
end method;

define method destroy-poa-thread (poa :: <poa>)
  push(poa-mailbox(poa),
       make(<destroy-thread-request>));
end method;

define method note-poa-thread-created (poa :: <poa>, thread :: <thread>)
  poa-threads(poa) := add!(poa-threads(poa), thread);
end method;

define method note-poa-thread-destroyed (poa :: <poa>, thread :: <thread>)
  poa-threads(poa) := remove!(poa-threads(poa), thread);
end method;

define class <root-poa-activator> (portableserver/<adapteractivator>)
end class;

define sealed domain make (subclass(<root-poa-activator>));
define sealed domain initialize (<root-poa-activator>);

define method note-poa-manager-state-changed (manager :: <poa-manager>, state :: <poa-manager-state>)
  when (poa-manager-state(manager) = #"inactive")
    error(make(portableserver/poamanager/<adapterinactive>));
  end when;
  poa-manager-state(manager) := state;
  let note = poa-manager-state-notification(manager);
  with-lock(associated-lock(note))
    release-all(note);
  end with-lock;
end method;

define method wait-for-poa-manager-state-change (manager :: <poa-manager>)
  let note = poa-manager-state-notification(manager);
  with-lock (associated-lock(note))
    wait-for(note);
  end with-lock;
end method;

define method note-poa-shutdown (poa :: <poa>)
  let note = poa-shutdown-notification(poa);
  with-lock (associated-lock(note))
    release-all(note);
  end with-lock;
end method;

define method wait-for-poa-shutdown (poa :: <poa>)
  let note = poa-shutdown-notification(poa);
  with-lock (associated-lock(note))
    wait-for(note);
  end with-lock;
end method;

define method print-object (object :: <poa>, stream :: <stream>)
 => ()
  next-method();
  print(stream, poa-name(object));
end method;

define method Portableserver/Poa/the-name (poa :: <poa>)
 => (name :: corba/<string>)
  poa-name(poa)
end method;

define method Portableserver/Poa/the-parent (poa :: <poa>)
 => (parent :: false-or(<poa>))
  poa-parent(poa)
end method;

define method Portableserver/Poa/the-POAmanager (poa :: <poa>)
 => (manager :: <poa-manager>)
  poa-manager(poa)
end method;

define method Portableserver/Poa/the-activator (poa :: <poa>)
 => (activator :: portableserver/<adapteractivator>)
  poa-activator(poa)
end method;

define method Portableserver/Poa/the-activator-setter (activator :: portableserver/<adapteractivator>, poa :: <poa>)
 => (activator :: portableserver/<adapteractivator>)
  poa-activator(poa) := activator
end method;

define method PortableServer/AdapterActivator/unknown-adapter (object :: <root-poa-activator>,
							       parent :: <poa>,
							       name :: corba/<string>)
 => (created? :: corba/<boolean>)
  #f
end method;

define sideways method PortableServer/AdapterActivator/unknown-adapter (object :: portableserver/<adapteractivator>,
							       parent :: portableserver/<poa>,
							       name :: corba/<string>)
 => (created? :: corba/<boolean>)
  #f
end method;

define sideways method PortableServer/ServantActivator/incarnate (object :: PortableServer/<ServantActivator>,
							 objectID :: corba/<string>,
							 adapter :: portableserver/<poa>)
 => (servant :: PortableServer/<servant>)
  error("Default servant activator incarnate method called");
end method;

define sideways method PortableServer/ServantActivator/etherealize (object :: PortableServer/<ServantActivator>,
							   objectID :: corba/<string>,
							   adapter :: portableserver/<poa>,
							   servant :: PortableServer/<servant>,
							   cleanup-in-progress,
							   remaining-activations)
 => ();
  error("Default servant activator etherialize method called");
end method;

define sideways method PortableServer/ServantLocator/preinvoke (object :: PortableServer/<ServantLocator>,
                                                    objectID :: corba/<string>,
                                                    adapter :: portableserver/<poa>,
                                                    operation :: CORBA/<identifier>)
 => (servant :: PortableServer/<servant>, cookie)
  error("Default servant locator preinvoke method called");
end method;

define sideways method PortableServer/ServantLocator/postinvoke (object :: PortableServer/<ServantLocator>,
							objectID :: corba/<string>,
							adapter :: portableserver/<poa>,
							operation :: CORBA/<identifier>,
							cookie,
							servant :: PortableServer/<servant>)
 => ()
  error("Default servant locator postinvoke method called");
end method;

define initial-service RootPOA (orb)
  orb-adaptor(orb)
    | (orb-adaptor(orb) := make(<root-poa>, orb: orb, port: orb-service-port(orb)))
end initial-service;

define method portableserver/poa/create-poa (parent :: <poa>,
					     adapter-name :: corba/<string>,
					     manager :: false-or(<poa-manager>),
					     #rest initargs,
					     #key)
 => (poa :: PortableServer/<poa>)
  with-lock (poa-lock(parent))
    when (lookup-poa(parent, adapter-name))
      error(make(portableserver/poa/<adapteralreadyexists>));
    end when;
    make(<poa>,
	 name: adapter-name,
	 parent: parent,
	 port: poa-port(parent),
	 poamanager: manager | make(<poa-manager>),
	 policies: apply(make, <poa-policies>, initargs));
  end with-lock;
end method;

define method portableserver/poa/destroy (poa :: <poa>,
					  etherealize-objects? :: <boolean>,
					  wait-for-completion? :: <boolean>)
 => ()
  local method do-destroy-poa
	    (poa :: <poa>, etherealize-objects? :: <boolean>, wait-for-completion? :: <boolean>)
	  if (member?(current-thread(), poa-threads(poa))
		& wait-for-completion?)
	    error(make(corba/<bad-inv-order>, minor: 0, completed: #"completed-no"))
	  end if;
	  with-lock (find-poa-lock(poa))
	    update-poa-parent-children(poa);
	    for (child in poa-children(poa))
	      do-destroy-poa(child, etherealize-objects?, wait-for-completion?)
	    end for;
	    for (i from 0 to (size(poa-threads(poa)) - 1)) // kill all but one
	      destroy-poa-thread(poa)
	    end for;
	    push(poa-mailbox(poa),
		 make(<destroy-POA-request>,
		      poa: poa,
		      etherealize-objects?: etherealize-objects?));
	    if (wait-for-completion?)
	      wait-for-poa-shutdown(poa);
	    end if;
	  end with-lock;
	end method;
  do-destroy-poa(poa, etherealize-objects?, wait-for-completion?);
  invalidate-collocations();
end method;
                                          
define method find-poa-lock (poa :: <poa>)
  poa-lock(poa)
end method;

define method find-poa-lock (poa :: <root-poa>)
  orb-adaptor-lock(poa-orb(poa))
end method;

define method update-poa-parent-children (poa :: <poa>)
  let parent = poa-parent(poa);
  poa-children(parent) := remove!(poa-children(parent), poa);
end method;

define method update-poa-parent-children (poa :: <root-poa>)
end method;

define method portableserver/poa/get-servant-manager (poa :: <poa>)
 => (manager :: false-or(PortableServer/<servantmanager>))
  let policies = poa-policies(poa);
  unless (POA-Request-Processing-Policy(policies) = #"use-servant-manager")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  poa-servant-manager(poa);
end method;

define method portableserver/poa/set-servant-manager (poa :: <poa>,
						      new-servant-manager :: PortableServer/<ServantManager>)
 => ()
  let policies = poa-policies(poa);
  unless (POA-Request-Processing-Policy(policies) = #"use-servant-manager")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  poa-servant-manager(poa) := new-servant-manager;
  values();
end method;

define method portableserver/poa/get-servant (poa :: <poa>)
 => (servant :: PortableServer/<servant>)
  let policies = poa-policies(poa);
  unless (POA-Request-Processing-Policy(policies) = #"use-default-servant")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  poa-default-servant(poa)
    | error(make(portableserver/poa/<noservant>))
end method;

define method portableserver/poa/set-servant (poa :: <poa>,
					      new-servant :: PortableServer/<servant>)
 => ()
  let policies = poa-policies(poa);
  unless (POA-Request-Processing-Policy(policies) = #"use-default-servant")
    error(make(portableserver/poa/<wrongpolicy>))
  end unless;
  poa-default-servant(poa) := new-servant;
  values()
end method;

define method portableserver/poa/activate-object-with-id (poa :: <poa>,
							  objectId :: <string>,
							  servant :: PortableServer/<servant>)
 => ()
  let policies = poa-policies(poa);
  unless (POA-Servant-Retention-Policy(policies) = #"retain")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  when (element(poa-active-object-table(poa), objectId, default: #f))
    error(make(portableserver/poa/<objectalreadyactive>));
  end when;
  when ((POA-Id-Uniqueness-Policy(policies) = #"unique-id")
	  & servant-active?(poa, servant))
    error(make(portableserver/poa/<servantalreadyactive>));
  end when;
  element(poa-active-object-table(poa), objectId) := servant;
  values();
end method;

define method portableserver/poa/activate-object (poa :: <poa>,
						  servant :: PortableServer/<servant>)
 => ()
  let policies = poa-policies(poa);
  unless (POA-Id-Assignment-Policy(policies) = #"system-id")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  unless (POA-Servant-Retention-Policy(policies) = #"retain")
    error(make(portableserver/poa/<wrongpolicy>))
  end unless;
  when ((POA-Id-Uniqueness-Policy(policies) = #"unique-id") &
	  servant-active?(poa, servant))
    error(make(portableserver/poa/<servantalreadyactive>))
  end when;
  let objectid = compute-objectid(poa);
  element(poa-active-object-table(poa), objectId) := servant;
  objectid;
end method;

define method servant-active? (poa :: <poa>, servant :: portableserver/<servant>)
 => (active? :: <boolean>)
  find-element(poa-active-object-table(poa), curry(\=, servant))
end method;

define method portableserver/poa/deactivate-object (poa :: <poa>,
						    objectId :: <string>)
 => ()
  let policies = poa-policies(poa);
  unless (POA-Servant-Retention-Policy(policies) = #"retain")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  unless (element(poa-active-object-table(poa), objectId, default: #f))
    error(make(portableserver/poa/<objectnotactive>));
  end unless;
  remove-key!(poa-active-object-table(poa), Objectid);
  invalidate-collocations();
  values();
end method;

define method portableserver/poa/create-reference (poa :: <poa>,
						   repositoryId :: <string>)
 => (object :: corba/<object>)
  let policies = poa-policies(poa);
  unless (POA-Id-Assignment-Policy(policies) = #"system-id")
    error(make(portableserver/poa/<wrongpolicy>))
  end unless;
  let poa-names = compute-poa-path(poa);
  let objectid = compute-objectid(poa);
  make(corba/<object>,
       ior: profile-as-ior(repositoryId,
			   hostname(),
			   find-poa-port(poa),
			   encode-object-key(make(<object-key>,
						  poa-id: poa-id(poa),
						  poa-path: poa-names,
						  objectid: objectid))))
end method;

define method adaptor-create-reference (poa :: <poa>, location :: iiop/<location>)
 => (object :: corba/<object>)
  make(corba/<object>,
       ior: profile-as-ior(iiop/location/repository-id(location),
			   iiop/location/host(location),
			   iiop/location/port(location),
			   encode-object-key(make(<object-key>,
						  poa-id: "",
						  poa-path: iiop/location/adaptor-path(location),
						  objectid: iiop/location/objectid(location)))))
end method;

define method compute-poa-path (poa :: <poa>)
  let current-poa = poa;
  let poa-names = #();
  while (current-poa)
    poa-names := pair(poa-name(current-poa), poa-names);
    current-poa := poa-parent(current-poa);
  end while;
  poa-names;
end method;

define method portableserver/poa/create-reference-with-id (poa :: <poa>,
							   objectid :: <string>,
							   repositoryId :: <string>)
 => (object :: corba/<object>)
  let poa-names = compute-poa-path(poa);
  make(corba/<object>,
       ior: profile-as-ior(repositoryId,
			   hostname(),
			   find-poa-port(poa),
			   encode-object-key(make(<object-key>,
						  poa-id: poa-id(poa),
						  objectid: objectid,
						  poa-path: poa-names))));
end method;

define method portableserver/poa/servant-to-id (poa :: <poa>,
						servant :: PortableServer/<servant>)
 => (id :: <string>)
  let policies = poa-policies(poa);
  unless (poa-servant-retention-policy(policies) = #"retain")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  unless ((poa-id-uniqueness-policy(policies) = #"unique-id")
	    | (poa-implicit-activation-policy(policies) = #"implicit-activation"))
    error(make(portableserver/poa/<wrongpolicy>))
  end unless;
  let existing-objectid = compute-servant-id(poa, servant);
  if (existing-objectid & (poa-id-uniqueness-policy(policies) = #"unique-id"))
    existing-objectid;
  else
    if ((~existing-objectid & (poa-implicit-activation-policy(policies) = #"implicit-activation"))
	  | (poa-id-uniqueness-policy(policies) = #"multiple-id"))
      let new-objectid = compute-objectid(poa);
      element(poa-active-object-table(poa), new-objectId) := servant;
      new-objectid;
    else
      error(make(portableserver/poa/<servantnotactive>));
    end if;
  end if;
end method;

define method compute-servant-id (poa :: <poa>, servant :: portableserver/<servant>)
  find-key(poa-active-object-table(poa),
	   method (the-servant)
	     the-servant = servant
	   end method);
end method;

define method compute-objectid (poa :: <poa>)
  generate-name("System-ID")
end method;

define method compute-poa-id ()
  generate-name(as-iso8601-string(current-date()));
end method;

define method portableserver/poa/servant-to-reference (poa :: <poa>,
						       servant :: PortableServer/<servant>)
 => (object :: corba/<object>)
  let objectid = portableserver/poa/servant-to-id(poa, servant);
  let repository-id = portableserver/servant/primary-interface(servant, objectid, poa);
  portableserver/poa/create-reference-with-id(poa, objectid, repository-id);
end method;

define method portableserver/poa/reference-to-servant (poa :: <poa>, object :: <object-reference>)
 => (servant :: portableserver/<servant>)
  let policies = poa-policies(poa);
  unless (POA-servant-retention-policy(policies) = #"retain")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  let ObjectId = portableserver/poa/reference-to-id(poa, object);
  let servant-active-as-id = element(poa-active-object-table(poa), ObjectId, default: #f);
  if (servant-active-as-id)
    servant-active-as-id
  else
    if ((POA-Request-Processing-Policy(policies) = #"use_default_servant") &
	  poa-default-servant(poa))
      poa-default-servant(poa)
    else
      error(make(portableserver/poa/<objectnotactive>));
    end if;
  end if;
end method;

define method portableserver/poa/reference-to-id (poa :: <poa>,
						  object :: <object-reference>)
 => (id :: <string>)
  let object-key = decode-object-key(iiop/profilebody-1-0/object-key(get-iiop-profile(corba/object/ior(object))));
  values(object-key-objectid(object-key));
end method;

define method portableserver/poa/id-to-servant (poa :: <poa>, ObjectId :: <string>)
 => (servant :: portableserver/<servant>)
  let policies = poa-policies(poa);
  unless (POA-Servant-Retention-Policy(policies) = #"retain")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  let servant-active-as-id = element(poa-active-object-table(poa), ObjectId, default: #f);
  unless (servant-active-as-id)
    error(make(portableserver/poa/<objectnotactive>));
  end unless;
  servant-active-as-id
end method;

define method portableserver/poa/id-to-reference (poa :: <poa>, ObjectId :: <string>)
 => (object :: corba/<object>)
  let policies = poa-policies(poa);
  unless (POA-Servant-Retention-Policy(policies) = #"retain")
    error(make(portableserver/poa/<wrongpolicy>));
  end unless;
  let servant-active-as-id = element(poa-active-object-table(poa), ObjectId, default: #f);
  unless (servant-active-as-id)
    error(make(portableserver/poa/<objectnotactive>));
  end unless;
  portableserver/poa/create-reference-with-id(poa,
					      ObjectId,
					      portableserver/servant/primary-interface(servant-active-as-id, ObjectID, poa))
end method;

/// ---*** invalidate-collocations also goes in DEACTIVATE on POA

define method portableserver/poamanager/activate
    (poa-manager :: <poa-manager>)
 => ()
  note-poa-manager-state-changed(poa-manager, #"active");
end method;

define method portableserver/poamanager/deactivate
    (poa-manager :: <poa-manager>, etherealize-objects :: <boolean>, wait-for-completion :: <boolean>)
 => ()
  // ---*** implement wait-for-completion and etherealize-objects for portableserver/poamanager/deactivate
  note-poa-manager-state-changed(poa-manager, #"inactive");
end method;  

define method portableserver/poamanager/hold-requests
    (poa-manager :: <poa-manager>, wait-for-completion :: <boolean>)
 => ()
  // ---*** implement wait-for-completion for portableserver/poamanager/hold-requests
  note-poa-manager-state-changed(poa-manager, #"holding");
end method;  

define method portableserver/poamanager/discard-requests
    (poa-manager :: <poa-manager>, wait-for-completion :: <boolean>)
 => ()
  // ---*** implement wait-for-completion for portableserver/poamanager/discard-requests
  note-poa-manager-state-changed(poa-manager, #"discarding");
end method;  

define method note-adaptor-shutdown (poa :: <poa>, wait-for-completion? :: <boolean>)
 => ()
  portableserver/poa/destroy(poa, #t, wait-for-completion?);
end method;
