Module: portableserver-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class PortableServer/<servant> (<object>)
end class;

define open abstract class PortableServer/<dynamic-servant> (PortableServer/<servant>)
end class;

define open abstract class PortableServer/<PoaManager> (corba/<object>)
end class;

define open abstract class PortableServer/<AdapterActivator> (corba/<object>)
end class;

define open abstract class PortableServer/<ServantManager> (corba/<object>)
end class;

define open abstract class PortableServer/<ServantActivator> (PortableServer/<ServantManager>)
end class;

define open abstract class PortableServer/<ServantLocator> (PortableServer/<ServantManager>)
end class;

define open abstract class PortableServer/<Poa> (corba/<object>)
end class;

define open generic PortableServer/AdapterActivator/unknown-adapter (object :: PortableServer/<AdapterActivator>,
								     parent :: PortableServer/<poa>,
								     name :: corba/<string>)
 => (created? :: corba/<boolean>);


define open generic PortableServer/ServantActivator/incarnate (object :: PortableServer/<ServantActivator>,
							       objectID :: corba/<string>,
							       adapter :: PortableServer/<poa>)
 => (servant :: PortableServer/<servant>);

define open generic PortableServer/ServantActivator/etherealize (object :: PortableServer/<ServantActivator>,
							   objectID :: corba/<string>,
							   adapter :: PortableServer/<poa>,
							   servant :: PortableServer/<servant>,
							   cleanup-in-progress,
							   remaining-activations)
 => ();

define open generic PortableServer/ServantLocator/preinvoke (object :: PortableServer/<ServantLocator>,
                                                    objectID :: corba/<string>,
                                                    adapter :: PortableServer/<poa>,
                                                    operation :: corba/<identifier>)
 => (servant :: PortableServer/<servant>, cookie);

define open generic PortableServer/ServantLocator/postinvoke (object :: PortableServer/<ServantLocator>,
							      objectID :: corba/<string>,
							      adapter :: PortableServer/<poa>,
							      operation :: CORBA/<identifier>,
							      cookie,
							      servant :: PortableServer/<servant>)
 => ();

define open generic portableserver/poa/create-poa (poa :: PortableServer/<poa>,
						   adapter-name :: corba/<string>,
						   poa-manager :: false-or(portableserver/<poamanager>),
						   #rest
						     initargs,
						   #key,
						   #all-keys)
 => (poa :: PortableServer/<poa>);

define open generic portableserver/poa/destroy (poa :: PortableServer/<poa>,
						etherealize-objects :: corba/<boolean>,
						wait-for-completion :: corba/<boolean>)
 => ();

define open generic portableserver/poa/get-servant-manager (poa :: PortableServer/<poa>)
 => (manager :: false-or(PortableServer/<servantmanager>));

define open generic portableserver/poa/set-servant-manager (poa :: PortableServer/<poa>,
							    new-servant-manager :: PortableServer/<ServantManager>)
 => ();

define open generic portableserver/poa/get-servant (poa :: PortableServer/<poa>)
 => (servant :: PortableServer/<servant>);

define open generic portableserver/poa/set-servant (poa :: PortableServer/<poa>,
						    new-servant :: PortableServer/<servant>)
 => ();

define open generic portableserver/poa/activate-object-with-id (poa :: PortableServer/<poa>,
								objectId :: <string>,
								servant :: PortableServer/<servant>)
 => ();

define open generic portableserver/poa/activate-object (poa :: PortableServer/<poa>,
							servant :: PortableServer/<servant>)
 => ();

define open generic portableserver/poa/deactivate-object (poa :: PortableServer/<poa>,
						    objectId :: <string>)
 => ();

define open generic portableserver/poa/create-reference (poa :: PortableServer/<poa>,
						   repositoryId :: <string>)
 => (object :: corba/<object>);

define open generic portableserver/poa/create-reference-with-id (poa :: PortableServer/<poa>,
							   objectid :: <string>,
							   repositoryId :: <string>)
 => (object :: corba/<object>);

define open generic portableserver/poa/servant-to-id (poa :: PortableServer/<poa>,
						servant :: PortableServer/<servant>)
 => (id :: <string>);

define open generic portableserver/poa/servant-to-reference (poa :: PortableServer/<poa>,
						       servant :: PortableServer/<servant>)
 => (object :: corba/<object>);

define open generic portableserver/poa/reference-to-servant (poa :: PortableServer/<poa>,
							     object :: corba/<object>)
 => (servant :: PortableServer/<servant>);

define open generic portableserver/poa/reference-to-id (poa :: PortableServer/<poa>,
							object :: corba/<object>)
 => (id :: <string>);

define open generic portableserver/poa/id-to-servant (poa :: PortableServer/<poa>, ObjectId :: <string>)
 => (servant :: portableserver/<servant>);

define open generic portableserver/poa/id-to-reference (poa :: PortableServer/<poa>, ObjectId :: <string>)
 => (object :: corba/<object>);

define open generic portableserver/poamanager/activate
    (poa-manager :: PortableServer/<poamanager>)
 => ();

define open generic portableserver/poamanager/hold-requests
    (poamanager :: portableserver/<poamanager>, wait-for-completion :: corba/<boolean>)
 => ();

define open generic portableserver/poamanager/discard-requests
    (poamanager :: portableserver/<poamanager>, wait-for-completion :: corba/<boolean>)
 => ();

define open generic portableserver/poamanager/deactivate
    (poamanager :: portableserver/<poamanager>, etherealize-objects :: corba/<boolean>, wait-for-completion :: corba/<boolean>)
 => ();

define open generic maybe-collocated-invoke (request :: corba/<request>, flags :: corba/<flags>)
 => (invoked? :: <boolean>, forwarded? :: <boolean>);

define open generic Portableserver/Poa/the-name (poa :: PortableServer/<poa>)
 => (name :: corba/<string>);

define open generic Portableserver/Poa/the-parent (poa :: PortableServer/<poa>)
 => (parent :: false-or(PortableServer/<poa>));

define open generic Portableserver/Poa/the-POAmanager (poa :: PortableServer/<poa>)
 => (manager :: PortableServer/<poamanager>);

define open generic Portableserver/Poa/the-activator (poa :: PortableServer/<poa>)
 => (activator :: PortableServer/<AdapterActivator>);

define open generic Portableserver/Poa/the-activator-setter (activator :: PortableServer/<AdapterActivator>, poa :: PortableServer/<poa>)
 => (activator :: PortableServer/<AdapterActivator>);

define open generic invoke-operation (object :: portableserver/<servant>,
				      server-request :: corba/<serverrequest>,
				      operation :: <symbol>)
 => ();

/// ---*** should be able to do these two macros as one but I couldn't
/// get it to work

define macro poa-null-exception-definer
  { define poa-null-exception ?exception:name }
    =>
    { define class "portableserver/poa/<" ## ?exception ## ">" (corba/<user-exception>)
      end class;

      define sealed domain make (subclass("portableserver/poa/<" ## ?exception ## ">"));
      define sealed domain initialize ("portableserver/poa/<" ## ?exception ## ">");

      define constant "portableserver/poa/$" ## ?exception ## "-typecode" = 
	make(<exception-typecode>,
	     name: ?"exception",
	     repository-id: format-to-string("IDL:omg.org/PortableServer/POA/%s:1.0", ?"exception"));

      define method class-typecode (class == "portableserver/poa/<" ## ?exception ## ">")
       => (typecode :: <typecode>)
         "portableserver/poa/$" ## ?exception ## "-typecode"
      end method;

     define method object-typecode (object :: "portableserver/poa/<" ## ?exception ## ">")
       => (typecode :: <typecode>)
        "portableserver/poa/$" ## ?exception ## "-typecode"
      end method;
}
end macro;

define macro poa-manager-null-exception-definer
  { define poa-manager-null-exception ?exception:name }
    =>
    { define class "portableserver/poamanager/<" ## ?exception ## ">" (corba/<user-exception>)
      end class;

      define sealed domain make (subclass("portableserver/poamanager/<" ## ?exception ## ">"));
      define sealed domain initialize ("portableserver/poamanager/<" ## ?exception ## ">");

      define constant "portableserver/poamanager/$" ## ?exception ## "-typecode" = 
	make(<exception-typecode>,
	     name: ?"exception",
	     repository-id: format-to-string("IDL:omg.org/portableserver/poamanager/%s:1/0", ?"exception"));

      define method class-typecode (class == "portableserver/poamanager/<" ## ?exception ## ">")
       => (typecode :: <typecode>)
         "portableserver/poamanager/$" ## ?exception ## "-typecode"
      end method;

     define method object-typecode (object :: "portableserver/poamanager/<" ## ?exception ## ">")
       => (typecode :: <typecode>)
        "portableserver/poamanager/$" ## ?exception ## "-typecode"
      end method;
}
end macro;

define poa-null-exception AdapterAlreadyExists;
define poa-null-exception AdapterNonExistent;
define poa-null-exception NoServant;
define poa-null-exception ObjectAlreadyActive;
define poa-null-exception ObjectNotActive;
define poa-null-exception ServantAlreadyActive;
define poa-null-exception ServantNotActive;
define poa-null-exception WrongAdapter;
define poa-null-exception WrongPolicy;

define poa-manager-null-exception AdapterInactive;

define class Portableserver/Poa/<InvalidPolicy> (corba/<user-exception>)
  slot Portableserver/Poa/InvalidPolicy/index;
end class;

define sealed domain make (subclass(Portableserver/Poa/<InvalidPolicy>));
define sealed domain initialize (Portableserver/Poa/<InvalidPolicy>);

define method class-typecode (class == Portableserver/Poa/<InvalidPolicy>)
 => (typecode :: <typecode>)
  make(<exception-typecode>,
       repository-id: "IDL:omg.org/PortableServer/POA/InvalidPolicy:1.0",
       name: "InvalidPolicy");
end method;

define class Portableserver/<ForwardRequest> (corba/<user-exception>) 
  slot Portableserver/ForwardRequest/forward-reference, required-init-keyword: forward-reference:;
end class;

define sealed domain make (subclass(Portableserver/<ForwardRequest>));
define sealed domain initialize (Portableserver/<ForwardRequest>);

define method class-typecode (object == Portableserver/<ForwardRequest>)
 => (typecode :: <typecode>)
  make(<exception-typecode>,
       repository-id: "IDL:omg.org/PortableServer/ForwardRequest:1.0",
       name: "ForwardRequest",
       members: vector(make(<typecode-member>,
			    name: "FORWARD_REFERENCE",
			    typecode: class-typecode(corba/<object>))));
end method;

define open abstract class portableserver/<current> (corba/<current>)
end class;

define class portableserver/current/<nocontext> (corba/<user-exception>)
end class;

define sealed domain make (subclass(portableserver/current/<nocontext>));
define sealed domain initialize (portableserver/current/<nocontext>);

define method class-typecode (object == Portableserver/current/<nocontext>)
 => (typecode :: <typecode>)
  make(<exception-typecode>,
       repository-id: "IDL:omg.org/PortableServer/Current/NoContext:1.0",
       name: "NoContext");
end method;

define open generic portableserver/current/get-POA (current :: portableserver/<current>)
 => (poa :: portableserver/<poa>);

define open generic portableserver/current/get-object-id (current :: portableserver/<current>)
 => (objectid :: <string>);

define variable *optimize-collocation?* :: <boolean> = #t;

define open generic corba/serverrequest/invoke
    (request :: corba/<serverrequest>, servant :: portableserver/<servant>)
 => ();

define open generic corba/serverrequest/forward
    (request :: corba/<serverrequest>, forward :: corba/<object>, #key context)
 => ();

define open generic portableserver/servant/primary-interface
    (servant :: portableserver/<servant>, objectid :: <string>, poa :: portableserver/<poa>)
 => (repository-id :: <string>);

define open generic portableserver/poa/find-poa
    (poa :: portableserver/<poa>, adapter-name :: corba/<string>, activate-it :: <boolean>)
 => (poa :: portableserver/<poa>);

