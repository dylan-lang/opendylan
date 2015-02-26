module:    Dylan-user	
Synopsis:  Dylan library to act as an interface to OLE Automation.  
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library OLE-Automation
  use dylan;
  use common-dylan;
  use C-FFI;
  use COM;
  use win32-automation;

  use Win32-common;
  use Win32-kernel;
  use Win32-registry;
  export OLE-Automation;
end;

define module OLE-Automation
  use common-dylan;
  use simple-format;
  use Dylan-extensions,
    import: {<abstract-integer>, <big-integer>, <double-integer>,
	     <simple-byte-vector>, <simple-double-byte-vector>,
	     <simple-integer-vector>, <simple-machine-word-vector>,
	     <simple-single-float-vector>, <simple-double-float-vector>,
	     <simple-byte-array>, <simple-double-byte-array>,
	     <simple-integer-array>, <simple-machine-word-array>,
	     <simple-single-float-array> };
  use finalization;
  use C-FFI,
    // Export C type designators mentioned in the user documentation
    // for the OLE Automation library:
    export: { <C-int>, <C-short>, <C-long>, <C-float>,
	      <C-double>, <C-signed-char>, <C-character>,
	      <C-int*>, <C-short*>, <C-long*>, <C-float*>,
	      <C-double*>, <C-signed-char*>, <C-character*> };
  use COM, export: all;
  use COM-internal;
  use Win32-common; // this defines types such as <DWORD>, <ULONG>, etc.
  use Win32-kernel, import: { GetLastError, GetModuleFileName,
			      OutputDebugString, GetFileAttributes,
			      win32-error-message,
			      application-instance-handle,
			      application-command-line };
  use Win32-registry;

  use win32-automation, export: all;

  // from "typeinfo.dylan":
  export <function-description>, <variable-description>,
	 <constant-description>, <Disp-Type-Info>, 
	 make-funcdesc, Disp-Invoke, Disp-GetIDsOfNames;
  export LHashValOfName;
  export <Dylan-Type-Info>;
  export copy-automation-value;
  export <OLE-server-condition>, ole-server-status, ole-server-arg-err,
    ole-server-exception, ole-server-exception-setter, exit-invoke;

  // from "dispatch.dylan":
  export <Simple-Dispatch>;
  export apply-to-dispparams,
	 make-dispparams, destroy-dispparams,
	 get-property, set-property, get-property-setter, set-property-ref,
	 call-simple-method, get-id-of-name;
  export get-indexed-property, get-indexed-property-setter, simple-invoke;
  export $NULL-EXCEPINFO;

  // from "factory.dylan":
  export create-dispatch, \with-dispatch-interface;

  // from "coclass.dylan":
  export <coclass-Type-Info>, <Simple-Component-Object>,
    <component-interface-description>, make-object-factory;
  // for use by OLE-Control-Framework:
  export object-typeinfo, object-default-source-interface;
  // for use by DUIM-OLE-Control:
  export instance-class, instance-args;

  // from "register.dylan":
  export register-automation-server, unregister-automation-server,
    register-type-library, unregister-type-library,
    register-coclass, coclass-title-string;

  // from "inproc-auto.dylan":
  export \in-process-automation-server;

  // from "macros.dylan":
  export dispatch-interface-definer, type-information, coclass-definer;
  export dispatch-type-info-definer;

  // from "client-macros.dylan":
  export <dispatch-client>, dispatch-client-definer, dispatch-client-uuid;
    
  // from "vtable-macros.dylan" and "dual.dylan":
  export vtable-type-information, dispatch-type-information;
  export <vtable-type-info>, <dual-type-info>, <vtable-member-description>;
  export vtable-interface-definer, dual-interface-definer;
  export vtable-type-info-definer, dual-type-info-definer;
  export <IDispatch>-vstruct, <simple-dispatch>-vstruct;

end;
