Module:    OLE-Server
Synopsis:  self-registration
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// An OLE server application using this library can register itself in the
// system registry by calling the function `register-ole-server' below.
// If `OLE-util-register-only?()' returns true, the program should just
// register itself and then terminate.

define method OLE-util-maybe-just-register
    (class-id :: <REFCLSID>,
     prog-id :: <string>,
     title-string :: <string>,
     #rest key-args, #key, #all-keys) => (done? :: <boolean>);

  if ( OLE-util-register-only?() )
    apply(register-ole-server, class-id, prog-id, title-string, key-args);
    #t
  else
    #f
  end if
end OLE-util-maybe-just-register;

define constant $clsid-prefix = "CLSID\\";

define method register-ole-server ( class-id :: <REFCLSID>,
				    prog-id :: <string>,
				    title-string :: <string>,
				   #key full-name, short-name, app-name,
				    misc-status :: <fixnum> = 0,
				    verbs :: false-or(<vector>),
				    unregister? :: <boolean> =
				       OLE-util-unregister?(),
				    module-handle ) => ();

  if ( unregister? )
    unregister-ole-server(class-id, prog-id, module-handle: module-handle)
  else // register
    let module-path = get-module-file-name(module-handle: module-handle);
    let class-id-string :: <byte-string> = as(<string>, class-id);
    let empty-string = "";
    if ( null?(module-handle) | module-handle = application-instance-handle() )
      register-item($clsid-prefix, class-id-string, "\\LocalServer32",
		    module-path);
      register-item($clsid-prefix, class-id-string, "\\InprocHandler",
		    "OLE2.DLL");
    else
      register-item($clsid-prefix, class-id-string, "\\InprocServer32",
		    module-path);
    end if;
    register-item($clsid-prefix, class-id-string, "\\Insertable",
		  empty-string);
    // The title string is what appears in the "insert object" dialog box to
    // identify the server application.
    unless ( empty?(title-string) )
      register-item($clsid-prefix, class-id-string, empty-string,
		    title-string);
    end unless;
    register-item($clsid-prefix, class-id-string, "\\DefaultIcon",
		  concatenate-as(<byte-string>, module-path, ",0"));
    // The `prog-id' is a unique internal name for the class; it must start
    // with a letter, it cannot contain any spaces or punctuation except
    // period, and it must not be more than 39 characters long.
    register-item($clsid-prefix, class-id-string, "\\ProgID", prog-id);
    if ( full-name )  // The full type name of the class.
      register-item($clsid-prefix, class-id-string,
		    "\\AuxUserType\\1", full-name);
    end if;
    if ( short-name )
      // A short name (maximum of 15 characters)
      // used for popup menus and the Links dialog box.
      register-item($clsid-prefix, class-id-string,
		    "\\AuxUserType\\2", short-name);
    end if;
    // The name of the application servicing the class and
    // used in the Result text in dialog boxes.
    register-item($clsid-prefix, class-id-string, "\\AuxUserType\\3",
		  if ( app-name ) app-name else title-string end if);
    // Misc. object attributes as a bit vector:  (see $OLEMISC-...)
    register-item($clsid-prefix, class-id-string, "\\MiscStatus",
		  format-to-string("%d", misc-status));

    unless ( verbs )
      verbs := vector(vector($OLEIVERB-PRIMARY, "&Edit",
			     $MF-ENABLED, $OLEVERBATTRIB-ONCONTAINERMENU),
		      vector($VERB-OPEN, "&Open",
			     $MF-ENABLED, $OLEVERBATTRIB-ONCONTAINERMENU) );
    end unless;
    for ( verb :: <vector> in verbs )
      // menu item for 32-bit container:
      register-item($clsid-prefix, class-id-string,
		    format-to-string("\\Verb\\%d", verb[0]),
		    format-to-string("%s,%d,%d", verb[1], verb[2], verb[3]));
      // menu item for 16-bit container:
      register-item(prog-id, "\\protocol\\StdFileEditing\\verb\\",
		    format-to-string("%d", verb[0]), verb[1]);
    end for;

    // the following are for compatibility with 16-bit container applications:

    register-item($clsid-prefix, class-id-string, "\\LocalServer",
		  module-path);
    register-item(prog-id, empty-string, empty-string, title-string);
    register-item(prog-id, "\\protocol\\StdFileEditing\\server", empty-string,
		  module-path);
    register-item(prog-id, "\\Insertable", empty-string, empty-string);
    register-item(prog-id, "\\CLSID", empty-string, class-id-string);

    // finally, a quick test

    let ( status, returned-prog-id ) = ProgIDFromCLSID(class-id);
    if ( FAILED?(status) )
      error("Failed to verify registration;\nProgIDFromCLSID status = %=",
	    status);
    elseif ( returned-prog-id ~= prog-id )
      error("Registration verification failed;\nexpected \"%s\", got \"%s\"",
	    prog-id, returned-prog-id);
    end if;

    values()
  end if
end register-ole-server;

define method register-ole-server
    (class-id-string :: <string>,
     prog-id :: <string>,
     title-string :: <string>, #rest args, #key, #all-keys) => ();
  // Even though it is the string representation that we really use for
  // registration, convert it to a structure and back to a string in order
  // to ensure that it is in the canonical representation.
  let class-id :: <REFCLSID> = as(<REFCLSID>, class-id-string);
  apply(register-ole-server, class-id, prog-id, title-string, args);
  destroy(class-id);
end method;

define method unregister-ole-server ( class-id :: <REFCLSID>,
				      prog-id :: <string>,
				      #key module-handle) => ();

  let module-path = get-module-file-name(module-handle: module-handle);
  let class-id-string :: <byte-string> = as(<string>, class-id);
  let value = get-registry-item($clsid-prefix, class-id-string,
				"\\LocalServer32") |
    get-registry-item($clsid-prefix, class-id-string, "\\LocalServer");
  if ( value = module-path |
	module-path = get-registry-item($clsid-prefix, class-id-string,
					"\\InprocServer32") )
    delete-key-recursively($HKEY-CLASSES-ROOT,
			   concatenate($clsid-prefix, class-id-string));
  end if;
  let value =
    get-registry-item(prog-id, "\\protocol\\StdFileEditing\\server", "");
  if ( value = module-path )
    delete-key-recursively($HKEY-CLASSES-ROOT, prog-id);
  end if;
  values()
end unregister-ole-server;

