Module: orb-core
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <adaptor> (<object>)
end class;

define open generic note-adaptor-shutdown (adaptor :: <adaptor>, wait-for-completion? :: <boolean>)
 => ();

define method note-adaptor-shutdown (adaptor :: <adaptor>, wait-for-completion? :: <boolean>)
 => ()
end method;

define open generic adaptor-create-reference (adaptor :: <adaptor>, location :: iiop/<location>)
 => (object :: corba/<object>);

define variable *functional-developer-orb* = #f;

define constant $orb-service-port :: <integer> = 3672; //  NB registered with IANA as "harlequinorb"

define constant <orb-state> = one-of(#"uninitialized", #"running", #"shutdown");

define class <orb> (corba/<orb>)
  constant slot orb-arg-list :: corba/<arg-list>, required-init-keyword: arg-list:;
  slot orb-id :: corba/<orbid>, required-init-keyword: orb-id:;
  slot orb-connection-manager, init-keyword: connection-manager:;
  slot orb-service-port :: false-or(<integer>) = #f, setter: %orb-service-port-setter;
  constant slot orb-port-notification :: <notification>
    = make(<notification>, name: "Waiting for port assignment", lock: make(<lock>));
  slot orb-adaptor :: false-or(<adaptor>) = #f, init-keyword: adaptor:;
  constant slot orb-adaptor-lock :: <lock> = make(<lock>);
  slot orb-state :: <orb-state> = #"uninitialized";
  constant slot orb-shutdown-notification :: <notification>
    = make(<notification>, name: "Waiting for ORB Shutdown", lock: make(<lock>));
end class;

define sealed domain make (subclass(<orb>));
define sealed domain initialize (<orb>);

define sideways method make (class == corba/<orb>, #rest initargs, #key)
 => (orb :: <orb>)
  apply(make, <orb>, initargs);
end method;

define method make (class == <orb>, #key)
 => (orb :: <orb>)
  if (*functional-developer-orb*)
    *functional-developer-orb*
  else
    *functional-developer-orb* := next-method();
  end if;
end method;

define method initialize (orb :: <orb>, #key)
  next-method();
  start-sockets();
  process-orb-arg-list(orb);
  orb-state(orb) := #"running";
end method;


// ---*** use of "map-into" is to workaround dispatch problem with
// using "as"
define sideways method corba/orb-init (arg-list :: corba/<arg-list>,
			      orbid :: corba/<orbid>)
 => (orb :: <orb>, arg-list :: corba/<arg-list>)
  values(make(<orb>,
	      arg-list: if (empty?(arg-list))
                          let args = make(corba/<arg-list>);
                          map-into(args, identity, application-arguments());
                          args
			else
			  arg-list
			end if,
	      orb-id: orbid),
	 arg-list)
end method;

define orb-arg-processor
  syntax: "-ORBtrace",
  callback: method (orb :: corba/<orb>)
	      debugging?() := #t;
              debug-parts() := #(#"connection", #"poa");
	    end method
end orb-arg-processor;

define orb-arg-processor
  syntax: "-ORBport",
  value?: #t,
  callback: method (orb :: corba/<orb>, value :: <string>)
	      orb-service-port(orb) :=
		if (value = "orb")
		  $orb-service-port
		else
		  string-to-integer(value);
		end if;
	    end method
end orb-arg-processor;

define method wait-for-orb-port (orb :: corba/<orb>)
 => ()
  let note = orb-port-notification(orb);
  with-lock (associated-lock(note))
    unless (orb-service-port(orb))
      wait-for(note);
    end unless;
  end with-lock;
end method;

define method orb-service-port-setter (port :: <integer>, orb :: corba/<orb>)
 => (port :: <integer>)
  let note = orb-port-notification(orb);
  with-lock (associated-lock(note))
    %orb-service-port(orb) := port;
    release-all(note);
  end with-lock;
  port
end method;

define orb-arg-processor
  syntax: "-ORBid",
  value?: #t,
  callback: method (orb :: corba/<orb>, value :: <string>)
              if (orb-id(orb) = "")
		orb-id(orb) := value
	      end if;
	    end method
end orb-arg-processor;

define orb-arg-processor
  syntax: "-ORBname-service-file",
  value?: #t,
  callback: method (orb :: corba/<orb>, ior-file :: <string>)
	      name-service-ior-file($name-service-settings) := ior-file
	    end method
end orb-arg-processor;

define orb-arg-processor
  syntax: "-ORBname-service",
  value?: #t,
  callback: method (orb :: corba/<orb>, ior-string :: <string>)
	      name-service-ior-string($name-service-settings) := ior-string
	    end method
end orb-arg-processor;

define orb-arg-processor
  syntax: "-ORBinterface-repository-file",
  value?: #t,
  callback: method (orb :: corba/<orb>, ior-file :: <string>)
	      interface-repository-ior-file($interface-repository-settings) := ior-file
	    end method
end orb-arg-processor;

define orb-arg-processor
  syntax: "-ORBinterface-repository",
  value?: #t,
  callback: method (orb :: corba/<orb>, ior-string :: <string>)
	      interface-repository-ior-string($interface-repository-settings) := ior-string
	    end method
end orb-arg-processor;

define orb-arg-processor
  syntax: "-ORBsettings",
  value?: #f,
  callback: method (orb :: corba/<orb>)
	      format(*standard-output*,
		     "\nFunctional Developer ORB Settings:\n"
		       "  Name Service: %s\n"
		       "  Name Service File: %s\n"
		       "  Interface Repository: %s\n"
		       "  Interface Repository File: %s\n",
		     name-service-ior-string($name-service-settings) | "<none>",
		     name-service-ior-file($name-service-settings) | "<none>",
		     interface-repository-ior-string($interface-repository-settings) | "<none>",
		     interface-repository-ior-file($interface-repository-settings) | "<none>")
	    end method
end orb-arg-processor;

define method corba/orb/object-to-string (orb :: <orb>, object :: <object-reference>)
 => (string :: corba/<string>)
  unparse-ior-to-string(corba/object/ior(object))
end method;

define method corba/orb/string-to-object (orb :: <orb>, ior-string :: corba/<string>)
 => (object :: corba/<object>)
  make(corba/<object>, ior: parse-ior-from-string(ior-string));
end method;

define method corba/orb/object-to-file (orb :: <orb>, file :: <string>, object :: corba/<object>)
    => ()
  with-open-file(stream = file, direction: #"output")
    write(stream, corba/orb/object-to-string(orb, object));
  end;
end method;

define method corba/orb/file-to-object (orb :: <orb>, file :: <string>)
    => (object :: corba/<object>)
  with-open-file(stream = file, direction: #"input")
    corba/orb/string-to-object(orb, as(<string>, stream-contents(stream)));
  end;
end method;

define variable *orb-initial-services* = make(<stretchy-vector>);

define method orb-initial-services ()
  *orb-initial-services*
end method;

define method orb-initial-services-setter (services :: <sequence>)
  *orb-initial-services* := services
end method;  

define macro initial-service-definer
  { define initial-service ?name:name (?orb:name) ?body:body end }
    =>
    { begin
	orb-initial-services() := add!(orb-initial-services(), ?"name");
      end;
     define sideways method do-resolve-initial-service (?orb :: <orb>, service == ?#"name")
       ?body
     end method; }
end macro;

define dynamic generic do-resolve-initial-service (orb :: <orb>, service :: <symbol>);

define method do-resolve-initial-service (orb :: <orb>, service :: <symbol>)
  error(make(corba/orb/<invalidname>))
end method;

define method corba/orb/list-initial-services (orb :: <orb>)
 => (list :: corba/orb/<objectidlist>);
  orb-initial-services();
end method;

define method corba/orb/resolve-initial-references (orb :: <orb>,
						    objectid :: <string>)
 => (object :: corba/<object>);
  do-resolve-initial-service(orb, as(<symbol>, objectid))
end method;

define settings <functional-objects-user-settings> (<current-user-software-settings>)
  key-name "Functional Objects";
end settings;

define settings <functional-developer-orb-settings> (<functional-objects-user-settings>)
  key-name "Functional Developer ORB";
end settings;

define settings <functional-developer-orb-settings-1-0> (<functional-developer-orb-settings>)
  key-name "1.0";
end settings;

define settings <functional-developer-orb-settings-1-1> (<functional-developer-orb-settings>)
  key-name "1.1";
end settings;

define settings <functional-developer-orb-settings-2-0> (<functional-developer-orb-settings>)
  key-name "2.0";
end settings;

define constant <current-functional-developer-orb-settings> = <functional-developer-orb-settings-2-0>;

define settings <functional-developer-interface-repository-settings> (<current-functional-developer-orb-settings>)
  key-name "InterfaceRepository";
  slot interface-repository-ior-string :: <string>;
  slot interface-repository-ior-file :: <string>;
end settings;

define constant $interface-repository-settings = make(<functional-developer-interface-repository-settings>);

define initial-service InterfaceRepository (orb)
  let ior-string = interface-repository-ior-string($interface-repository-settings);
  if (ior-string)
    corba/orb/string-to-object(orb, ior-string)
  else
    let ior-file = interface-repository-ior-file($interface-repository-settings);
    if (ior-file)
      corba/orb/file-to-object(orb, ior-file)
    end if;
  end if;
end initial-service;

define settings <functional-developer-name-service-settings> (<current-functional-developer-orb-settings>)
  key-name "NameService";
  slot name-service-ior-string :: <string>;
  slot name-service-ior-file :: <string>;
end settings;

define constant $name-service-settings = make(<functional-developer-name-service-settings>);

define initial-service NameService (orb)
  let ior-string = name-service-ior-string($name-service-settings);
  if (ior-string)
    corba/orb/string-to-object(orb, ior-string)
  else
    let ior-file = name-service-ior-file($name-service-settings);
    if (ior-file)
      corba/orb/file-to-object(orb, ior-file)
    end if;
  end if;
end initial-service;

define method corba/orb/create-reference (orb :: <orb>, location :: iiop/<location>)
 => (object :: <object-reference>)
  let adaptor = corba/orb/resolve-initial-references(orb, "RootPOA");
  adaptor-create-reference(adaptor, location)
end method;

define method corba/orb/work-pending (orb :: <orb>)
  => (work? :: <boolean>)
  #f
end method;

define method corba/orb/perform-work (orb :: <orb>)
  => ()
end method;

/// ---*** implement wait-for-completion
define method corba/orb/shutdown (orb :: <orb>, wait-for-completion? :: corba/<boolean>)
  => ()
  note-adaptor-shutdown(orb-adaptor(orb), wait-for-completion?);
  note-orb-shutdown(orb);
end method;

define method corba/orb/run (orb :: <orb>)
  => ()
  keyboard-interrupt-polling?() := #f; // no auto-polling
  let interrupted? = wait-for-orb-shutdown(orb);
  if (interrupted?)
    note-adaptor-shutdown(orb-adaptor(orb), #t);
  end if;
end method;

define method note-orb-shutdown (orb :: <orb>)
  let note = orb-shutdown-notification(orb);
  with-lock (associated-lock(note))
    orb-state(orb) := #"shutdown";
    release-all(note)
  end with-lock;
end method;

define constant $wait-for-shutdown-timeout :: <integer> = 5; // seconds

define method wait-for-orb-shutdown (orb :: <orb>) => (interrupted? :: <boolean>)
  let note = orb-shutdown-notification(orb);
  block (exit)
    while (#t)
      with-lock (associated-lock(note))
        unless (orb-state(orb) = #"shutdown")
          let synched? = wait-for(note, timeout: $wait-for-shutdown-timeout);
          if (synched?)
            exit(#f);
          else
            if (keyboard-interrupt?())
              exit(#t);
            end if;
          end if;
        end unless;
      end with-lock;
    end while;
  end block;
end method;

define method note-application-shutdown ()
end method;

register-application-exit-function(note-application-shutdown);
