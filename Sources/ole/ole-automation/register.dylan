Module:    OLE-Automation
Synopsis:  self-registration
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// An OLE Automation server application using this library can register
// itself in the system registry by calling the function
// `register-automation-server' below.  If `OLE-util-register-only?()'
// returns true, the program should just register itself and then
// terminate.

define method format-guid( guid :: <REFGUID> ) => ( string :: <byte-string> );
  as(<string>, guid)
end format-guid;


/* example from "Inside OLE":
  
REGEDIT
HKEY_CLASSES_ROOT\Acme.Component.3 = Acme Component Version 3.0
HKEY_CLASSES_ROOT\Acme.Component.3\CLSID = {42754580-16b7-11ce-80eb-00aa003d7352}

HKEY_CLASSES_ROOT\Acme.Component = Acme Component 
HKEY_CLASSES_ROOT\Acme.Component\CurVer = Acme.Component.3

HKEY_CLASSES_ROOT\CLSID\{42754580-16b7-11ce-80eb-00aa003d7352} = Acme Component 3.0
HKEY_CLASSES_ROOT\CLSID\{42754580-16b7-11ce-80eb-00aa003d7352}\ProgID = Acme.Component.3
HKEY_CLASSES_ROOT\CLSID\{42754580-16b7-11ce-80eb-00aa003d7352}\VersionIndependentProgID = Acme.Component
*/  

define constant $clsid-prefix = "CLSID\\";

define constant $clsid-suffix = "\\CLSID";

define method register-automation-server
    (class-id :: <REFCLSID>,
     prog-id :: <string>,
     title-string :: <string>,
     #key unregister? :: <boolean> = OLE-util-unregister?(),
	  versioned-prog-id :: false-or(<string>),
          versioned-title :: false-or(<string>),
          module-handle) => ();

  if ( unregister? )
    // unregister
    unregister-automation-server(class-id, prog-id,
				 module-handle: module-handle);
    if ( versioned-prog-id )
      unregister-automation-server(class-id, versioned-prog-id,
				   module-handle: module-handle);
    end if;
  else // register
    let module-path = get-module-file-name(module-handle: module-handle);
    let class-id-string = format-guid(class-id);
    let empty-string = "";
    if ( null?(module-handle) | module-handle = application-instance-handle() )
      register-item($clsid-prefix, class-id-string, "\\LocalServer32",
	    concatenate-as(<byte-string>, module-path, " /Automation"));
    else
      register-item($clsid-prefix, class-id-string, "\\InprocServer32",
		    module-path);
    end if;
    unless ( empty?(title-string) )
      register-item($clsid-prefix, class-id-string, empty-string,
		    versioned-title | title-string);
    end unless;
    // The `prog-id' is a unique internal name for the class; it must start
    // with a letter, it cannot contain any spaces or punctuation except
    // period, and it must not be more than 39 characters long.
    register-item($clsid-prefix, class-id-string, "\\ProgID",
		  versioned-prog-id | prog-id);
    register-item($clsid-prefix, class-id-string,
		  "\\VersionIndependentProgID", prog-id);
    // recommended by "Inside OLE" for identifying Automation servers:
    register-item($clsid-prefix, class-id-string, "\\Programmable",
		  empty-string);

    register-item(prog-id, empty-string, empty-string, title-string);
    register-item(prog-id, $clsid-suffix, empty-string, class-id-string);

    if ( versioned-prog-id & (versioned-prog-id ~= prog-id) )
      register-item(versioned-prog-id, empty-string, empty-string,
		    versioned-title | title-string);
      register-item(versioned-prog-id, $clsid-suffix, empty-string,
		    class-id-string);
      register-item(prog-id, "\\CurVer", empty-string, versioned-prog-id);
    end if;

    // finally, a quick test

    let ( status, returned-prog-id ) = ProgIDFromCLSID(class-id);
    if ( FAILED?(status) )
      error("Failed to verify registration;\nProgIDFromCLSID status = %=",
	    status);
    elseif ( returned-prog-id ~= (versioned-prog-id | prog-id) )
      error("Registration verification failed;\nexpected \"%s\", got \"%s\"",
	    prog-id, returned-prog-id);
    end if;

    values()
  end if
end register-automation-server;

define method register-automation-server
    (class-id-string :: <string>,
     prog-id :: <string>,
     title-string :: <string>, #rest args, #key, #all-keys) => ();
  // Even though it is the string representation that we really use for
  // registration, convert it to a structure and back to a string in order
  // to ensure that it is in the canonical representation.
  let class-id :: <REFCLSID> = as(<REFCLSID>, class-id-string);
  apply(register-automation-server, class-id, prog-id, title-string, args);
  destroy(class-id);
end method;

define method unregister-automation-server ( class-id :: <REFCLSID>,
					     prog-id :: <string>,
					     #key module-handle ) => ();

  let module-path = get-module-file-name(module-handle: module-handle);
  let class-id-string = format-guid(class-id);
  let value = get-registry-item($clsid-prefix, class-id-string,
				"\\LocalServer32");
  // Note: this pathname comparison is complicated by the fact that the
  // registry value has  " /Automation" appended to the end.
  let path-length :: <integer> = size(module-path);
  if ( ( value & (size(value) >= path-length) & (value[path-length] <= ' ') &
	  (copy-sequence(value, end: path-length) = module-path) )
	| ( get-registry-item($clsid-prefix, class-id-string,
			      "\\InprocServer32") = module-path ) )
    delete-key-recursively($HKEY-CLASSES-ROOT,
			   concatenate($clsid-prefix, class-id-string));
  end if;
  let value = get-registry-item(prog-id, $clsid-suffix, "");
  if ( value = class-id-string )
    delete-key-recursively($HKEY-CLASSES-ROOT, prog-id);
  end if;
  values()
end unregister-automation-server;

define method register-type-library (typeinfo :: <Dylan-Type-Info>,
				     #key module-handle,
				     class-id :: <REFCLSID> =
				       typeinfo.guid-value,
				     unregister? :: <boolean> =
				       OLE-util-unregister?()) => ();

  if ( unregister? )
    unregister-type-library(typeinfo, class-id: class-id,
			    module-handle: module-handle);
  else
   with-ole
    let typelib = fetch-type-library(typeinfo, class-id,
				     force?: #t, module-handle: module-handle);
    block ()
      let class-id-string :: <byte-string> = as(<string>, class-id);
      register-item($clsid-prefix, class-id-string,
		    "\\TypeLib", class-id-string);
      register-item($clsid-prefix, class-id-string, "\\Version",
		    format-to-string("%d.%d", typeinfo.major-version,
				     typeinfo.minor-version));
    exception (<abort>)
    end block;
    Release(typelib);
   end with-ole;
  end if;
  values()
end;

define function file-exists? (path :: <string>) => (exists? :: <boolean>)
  with-c-string (path = path)
    ~ negative?(GetFileAttributes(path))
  end;
end;

define method unregister-type-library (typeinfo :: <ITypeInfo>,
				       #key module-handle,
				       class-id :: <REFCLSID> =
					 typeinfo.guid-value) => ();

  let class-id-string = format-guid(class-id);
  let typelib-path = make-typelib-path(typeinfo, module-handle);
  let typekey = format-to-string("\\%d.%d\\%x\\win32",
				 typeinfo.major-version,
				 typeinfo.minor-version,
				 typeinfo.type-info-locale);
  let old-value = get-registry-item("TypeLib\\", class-id-string, typekey);
  // If the registry specifies the same file that we would have registered,
  // or if it specifies a file that no longer exists, then delete the entry.
  if ( old-value &
	( old-value = typelib-path | ~ file-exists?(old-value) ) )
    delete-key-recursively($HKEY-CLASSES-ROOT,
			   format-to-string("TypeLib\\%s\\%d.%d",
					    class-id-string,
					    typeinfo.major-version,
					    typeinfo.minor-version));
  end if;
  destroy(typelib-path);
  values()
end;

define method coclass-title-string ( coclass-type-info :: <Dylan-Type-Info> )
 => ( title-string :: <byte-string> );
  as(<byte-string>,
     if ( empty?(coclass-type-info.doc-string) )
       coclass-type-info.interface-name
     else
       coclass-type-info.doc-string
     end if)
end coclass-title-string;


define method register-coclass (type-info :: <Dylan-Type-Info>,
				prog-id,
				#key title,
				class-id = #f,
				versioned-prog-id, versioned-title,
				module-handle,
				unregister? :: <boolean> =
				  OLE-util-unregister?())
 => ();

  AddRef(type-info);
  let class-id :: <REFCLSID> = 
    if ( class-id ) as(<REFCLSID>, class-id) else type-info.guid-value end if;
  let title-string = title | coclass-title-string(type-info);
  register-automation-server(class-id, prog-id,
			     title-string,
			     unregister?: unregister?,
			     module-handle: module-handle,
			     versioned-prog-id: versioned-prog-id,
			     versioned-title: versioned-title);
  register-type-library(type-info,
			class-id: class-id,
			unregister?: unregister?,
			module-handle: module-handle);
  Release(type-info);
  values()
end;


/* // Then the user program should do:

 if ( OLE-util-register-only?() ) // just [un]register and terminate
   register-coclass(coclass-type-info, prog-id, title: title);
 else
   ...
 end if;
*/
