Module:    COM
Synopsis:  Common code for initialization, registration, and activation of
	   in-process servers.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* // use this when we can unload DLLs individually
define inline function make-dll-local-data() => (data :: <pair>)
  list(0)
end;
*/

// For now, use a global lock count so that no Dylan DLLs are unloaded
// until all of them can be unloaded.  See the discussion of Bug 1508 for
// the reasons for this.
define variable *dll-lock-data* :: <pair> = list(0);

define inline function make-dll-local-data() => (data :: <pair>)
  *dll-lock-data*
end;

define abstract open class <dll-lock-mixin> ( <object> )
  constant slot dll-local-data :: <pair> = list(0),
    init-keyword: dll-local-data:;
end;

define inline sealed method dll-active-interface-counter
    ( obj :: <dll-lock-mixin>, ) => ( count :: <fixnum> );
  head(obj.dll-local-data)
end;

define inline sealed method dll-active-interface-counter-setter
    ( value :: <fixnum>, obj :: <dll-lock-mixin> )
 => ( count :: <fixnum> );
  head(obj.dll-local-data) := value
end;

define abstract open class <object-with-dll-lock> ( <dll-lock-mixin> )
end;

define method initialize ( obj :: <object-with-dll-lock>,
			   #rest args, #key) => ();
  obj.dll-active-interface-counter := obj.dll-active-interface-counter + 1;
  next-method();
  values()
end method initialize;

define method terminate (obj :: <object-with-dll-lock>) => ();
  next-method();
  obj.dll-active-interface-counter := obj.dll-active-interface-counter - 1;
  values()
end method terminate;


define open COM-interface <class-factory-with-lock>
  ( <class-factory> , <dll-lock-mixin> )
end;

define method IClassFactory/LockServer ( self :: <class-factory-with-lock>,
					 lock? :: <boolean> )
	=> status :: <HRESULT>;
  Output-Debug-String("IClassFactory/LockServer ");
  Output-Debug-String(if(lock?) "#t\r\n" else "#f\r\n" end if);
  next-method();
  self.dll-active-interface-counter := self.dll-active-interface-counter +
    if ( lock? ) 1 else -1 end if;
  $S-OK
end IClassFactory/LockServer;




define variable *last-in-process-module* = #f;

// This is a little fragile, but if it's only used in the startup of
// a library which has an initialize-in-process-server in it, it should
// do the right thing...
define function OLE-util-in-process-startup? ()
  *last-in-process-module*
    & *last-in-process-module* ~= application-instance-handle()
end;

define macro initialize-in-process-server
  { initialize-in-process-server( #key ?register-fn:expression,
				 ?unregister-fn:expression,
				 ?get-factory-fn:expression,
				 ?unloading-fn:expression = ignore,
				 ?validate-fn:expression = ignore,
				 ?module-handle-fn:expression = ignore,
				 ?args:expression ) }
    => { 
// These definitions are inside the macro expansion so that they will 
// reside in each user's DLL, not in the shared DLL for this library.

define constant ?=COM%DLL-module-handle :: <hModule>
  = as(<hModule>, primitive-runtime-module-handle());

*last-in-process-module* := ?=COM%DLL-module-handle;

define constant ?=COM%server-args :: <simple-object-vector> = ?args;

?module-handle-fn(?=COM%DLL-module-handle);

define function ?=COM%DllRegisterServer()
 => ( status :: <HRESULT> );
  apply(?register-fn, ?=COM%DLL-module-handle, ?=COM%server-args)
end function;

define function ?=COM%DllUnregisterServer()
 => ( status :: <HRESULT> );
  apply(?unregister-fn, ?=COM%DLL-module-handle, ?=COM%server-args)
end function;

define C-callable-wrapper ?=COM%C-DllRegisterServer
			of ?=COM%DllRegisterServer
  result status :: <C-HRESULT>;
  export: #t,
  c-name: "DllRegisterServer", c-modifiers: "__stdcall";
end;

define C-callable-wrapper ?=COM%C-DllUnregisterServer
			of ?=COM%DllUnregisterServer
  result status :: <C-HRESULT>;
  export: #t,
  c-name: "DllUnregisterServer", c-modifiers: "__stdcall";
end;

define variable ?=COM%*dll-local-data* :: <pair> = make-dll-local-data();

define function ?=COM%DllGetClassObject (rclsid :: <REFCLSID>,
					 riid :: <REFIID> )
 => ( status :: <HRESULT>, pvObj :: <LPVOID> );

  apply(?get-factory-fn, rclsid, riid, ?=COM%*dll-local-data*,
	server-module-handle: ?=COM%DLL-module-handle,
	?=COM%server-args)
end function;

define C-callable-wrapper ?=COM%C-DllGetClassObject
			of ?=COM%DllGetClassObject
  input parameter rclsid :: <REFCLSID>;
  input parameter riid :: <REFIID>;
  output parameter ppvObj :: <C-void**>;
  result status :: <C-HRESULT>;
  export: #t,
  c-name: "DllGetClassObject", c-modifiers: "__stdcall";
end;

define function ?=COM%DllCanUnloadNow () => ( status :: <HRESULT> );
  can-unload-now?(?=COM%*dll-local-data*, ?unloading-fn, ?=COM%server-args)
end function;

define C-callable-wrapper ?=COM%C-DllCanUnloadNow
			of ?=COM%DllCanUnloadNow
  result status :: <C-HRESULT>;
  export: #t,
  c-name: "DllCanUnloadNow", c-modifiers: "__stdcall";
end;

apply(?validate-fn, ?=COM%server-args);

// to suppress "defined but not referenced" warnings:
ignore(?=COM%C-DllRegisterServer, ?=COM%C-DllUnregisterServer,
       ?=COM%C-DllGetClassObject, ?=COM%C-DllCanUnloadNow);
}
end macro;

define function can-unload-now?(dll-local-data :: <pair>,
				unloading-fn :: <function>,
				args :: <sequence>)
 => ( unload? :: <HRESULT> )
  Output-Debug-String("DllCanUnloadNow  ");
  if ( zero?(head(dll-local-data)) )
    Output-Debug-String("yes\r\n");
    apply(unloading-fn, args);
    $S-OK // yes, unload
  else
    Output-Debug-String("no\r\n");
    $S-FALSE // no, keep
  end if
end function;

// Suppress warning (work-around Bug 1542):
ignore(make-dll-local-data, can-unload-now?);
