Module:    COM
Synopsis:  simple way to create a class factory 
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open COM-interface <class-factory> ( <lib-init-mixin>, <IClassFactory> )
  constant slot instance-class-id, required-init-keyword: clsid:;
  constant slot instance-class :: <class>, required-init-keyword: class:;
  slot instance-args :: <sequence>, init-value: #[] /*, init-keyword: args: */;
  slot registration-id, init-value: 0;
end <class-factory>;

// Include this class in a class that can be made by a <class-factory> just
// in order to absorb the initialization keywords intended for the factory
// rather than the instance.
define open free abstract class <factory-args-mixin> ( <object> )
end class;

define method initialize (this :: <factory-args-mixin>,
			  #key class: instance-class, clsid,
			    server-context, connection-flags, #all-keys) => ();
  next-method()
end method initialize;


define method IClassFactory/CreateInstance(this :: <class-factory>,
					   UnkOuter :: <LPUNKNOWN>,
					   iid :: <REFIID>)
 => ( status :: <HRESULT>, object :: <Interface> );

  Output-Debug-String("IClassFactory/CreateInstance\r\n");
  let instance :: <Interface> =
    apply(make, this.instance-class,
	  controlling-unknown: dylan-interface(UnkOuter),
	  this.instance-args);
  IUnknown/QueryInterface(instance, iid)
end method IClassFactory/CreateInstance;

define method initialize (this :: <class-factory>,
			  #rest all-args,
			  #key
			    args = $unsupplied,
			    server-context = $CLSCTX-LOCAL-SERVER,
			    connection-flags = $REGCLS-SINGLEUSE, #all-keys)
 => ();
  Output-Debug-String("making <class-factory>\r\n");

  next-method();
  this.instance-args := if ( args == $unsupplied )
			  copy-sequence(all-args)
			else
			  args
			end if;
  unless ( zero?(logand(server-context, $CLSCTX-LOCAL-SERVER)) )
    // An .EXE server needs to register the class factory for the external
    // client to find it; this does not apply to a .DLL server, which
    // simply returns the factory from `DllGetClassObject'.
    // Since `CoRegisterClassObject' is only supposed to be used for a local
    // server, it is unclear when it would ever be meaningful for its
    // server-context argument to be anything other than $CLSCTX-LOCAL-SERVER.
    // If the user does have such an unusual situation, they can call
    // `CoRegisterClassObject' directly, since we aren't supporting it here.
    IUnknown/AddRef(this); // balance Release in revoke-registration
    let class-id = this.instance-class-id;
    let ( status :: <HRESULT>, reg ) =
      CoRegisterClassObject(as(<REFCLSID>,class-id), this,
			    server-context, connection-flags);
    if ( FAILED?(status) )
      ole-error(status, "CoRegisterClassObject", this, class-id);
    else
      this.registration-id := reg;
    end if;
  end unless;
  values()
end method initialize;

define method revoke-registration(this :: <class-factory>) => ();

  let reg = this.registration-id;
  unless ( zero?(reg) )
    Output-Debug-String("revoke-registration\r\n");
    this.registration-id := 0;
    let status :: <HRESULT> = CoRevokeClassObject(reg);
    if ( FAILED?(status) )
      ole-cerror(status, "CoRevokeClassObject", this);
    end if;
    // Don't release until after calling CoRevokeClassObject to avoid
    // having the <lib-init-mixin> terminate method calling OleUninitialize
    // while still inside CoRevokeClassObject, leading to recursive Release.
    IUnknown/Release(this);
  end unless;
  values()
end method revoke-registration;

define method revoke-registration(this == #f) => ();
  values()
end method revoke-registration;

define method revoke-registration(this :: <null-interface>) => ();
  values()
end method revoke-registration;

define macro with-class-factory
  { with-class-factory ( ?args:* ) ?body:body end }
    => {let factory = make(<class-factory>, ?args);
	block ()
	  ?body
	cleanup
	  revoke-registration(factory); // un-register and release
	end
	}
end macro with-class-factory;



// Client-side utilities:

define method create-COM-instance (class-id :: <REFCLSID>,
				   #key context = $CLSCTX-ALL,
				   interface-id = $IID-IUnknown,
				   class = <LPUNKNOWN>)
 => interface :: <LPUNKNOWN>;

  let old-lock-count :: <integer> = head(*dll-lock-data*);
  let ( hresult :: <HRESULT> , pointer) =
    CoCreateInstance(class-id, $NULL-interface, context,
		     as(<REFIID>, interface-id));
  if ( FAILED?(hresult) )
    ole-error(hresult, "CoCreateInstance", $NULL-interface, class-id,
	      interface-id);
  end if;
  let new-lock-count :: <integer> = head(*dll-lock-data*);
  if ( ( new-lock-count > old-lock-count ) & zero?(old-lock-count) )
    // If a Dylan client application loads a Dylan DLL server, and has not
    // already incremented the lock count itself, then we need to do it
    // now in order to prevent the DLL from being unloaded because the
    // current memory manager implementation does not support that.
    head(*dll-lock-data*) := new-lock-count + 1;
  end if;
  let c-interface = pointer-cast(class, pointer);
  // If the server is a Dylan in-process server, return the Dylan interface
  // so can do direct Dylan method calls instead of going through the vtable.
  // But only do so if it's of the expected class, otherwise leave
  // well-enough alone.
  let maybe-dylan-interface = dylan-interface(c-interface);
  if (instance?(maybe-dylan-interface, class))
    maybe-dylan-interface
  else
    c-interface
  end;
end method create-COM-instance;

define method create-COM-instance ( class-id-string :: <string>,
			        #rest args, #key, #all-keys )
 => interface :: <LPUNKNOWN>;
  let class-id = as(<REFCLSID>, class-id-string);
  let interface = apply(create-COM-instance, class-id, args);
  destroy(class-id);
  interface
end method create-COM-instance;

define macro with-COM-interface
  { with-COM-interface (?:name :: ?class:expression = ?class-id:expression,
			#key ?context:expression = $CLSCTX-ALL,
			     ?interface-id:expression = $IID-IUnknown)
     ?body:body
    end }
    => { OLE-initialize();  // initialize OLE libraries
	 let ptr = $null-interface;
	 block ()
	   // start server
	   ptr := create-COM-instance(?class-id,
				      class: ?class,
				      context: ?context,
				      interface-id: ?interface-id);
	   let ?name :: ?class = ptr;
	   ?body
	 cleanup
	   IUnknown/Release(ptr); // release server
	   OLE-uninitialize();  // shut down OLE
	 end }

  // Old cruddy syntax
  { with-COM-interface ?:name (?class-id:expression, ?more:* ) ?:body end }
     => {with-COM-interface (?name = ?class-id, ?more) ?body end }

type:
    { <object> } => { <LPUNKNOWN> }
    { ?:expression } => { ?expression }
end macro;
