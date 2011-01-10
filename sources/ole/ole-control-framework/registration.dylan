Module:    OLE-Control-Framework
Synopsis:  self-registration of OLE Controls
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// An OLE Control application using this library can register itself in the
// system registry by calling the function `register-ole-control' below.
// If `OLE-util-register-only?()' returns true, the program should just
// register itself and then terminate.


define constant $clsid-prefix = "CLSID\\";
define constant $clsid-suffix = "\\CLSID";

// Need to add:							???
//          ToolboxBitmap32 = <filename>.DLL,resourceID 

// Need to add 
// Registering a Property Page
//  
//    HKEY_CLASSES_ROOT
//        CLSID
//      {class id of property sheet} = friendly name of property page
//        InprocServer = filename.OCX


define method register-ole-control ( class-id :: <REFCLSID>,
				    prog-id :: <string>,
				    title-string :: <string>,
				   #key title, full-name, short-name, app-name,
				    misc-status :: <fixnum> = 0,
				    verbs :: false-or(<vector>),
				    versioned-prog-id :: false-or(<string>),
				    versioned-title :: false-or(<string>),
				    module-handle :: <HMODULE>,
				    unregister? :: <boolean> =
				      OLE-util-unregister?() )
 => ();

  if ( unregister? )
    // unregister
    unregister-ole-server(class-id, prog-id, module-handle: module-handle);
    unregister-automation-server(class-id, prog-id,
				 module-handle: module-handle);
    if ( versioned-prog-id )
      unregister-automation-server(class-id, versioned-prog-id,
				   module-handle: module-handle);
    end if;
  else // register
    let module-path = get-module-file-name(module-handle: module-handle);
    let class-id-string :: <byte-string> = as(<string>, class-id);
    let empty-string = "";
    register-item($clsid-prefix, class-id-string, "\\InprocServer32",
		  module-path);
    register-item($clsid-prefix, class-id-string, "\\Insertable",
		  empty-string);
    register-item($clsid-prefix, class-id-string, "\\Control",
		  empty-string);
    // The title string is what appears in the "insert object" dialog box to
    // identify the server application.
    unless ( empty?(title-string) )
      register-item($clsid-prefix, class-id-string, empty-string,
		    versioned-title | title-string);
    end unless;
    register-item($clsid-prefix, class-id-string, "\\DefaultIcon",
		  concatenate-as(<byte-string>, module-path, ",0"));
    // The `prog-id' is a unique internal name for the class; it must start
    // with a letter, it cannot contain any spaces or punctuation except
    // period, and it must not be more than 39 characters long.
    register-item($clsid-prefix, class-id-string, "\\ProgID",
		  versioned-prog-id | prog-id);
    register-item($clsid-prefix, class-id-string,
		  "\\VersionIndependentProgID", prog-id);
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
			     $MF-ENABLED, $OLEVERBATTRIB-ONCONTAINERMENU)
		/* // add this when property sheet support is available: ???
		    , vector($OLEIVERB-PROPERTIES, "&Properties",
			     $MF-ENABLED, $OLEVERBATTRIB-ONCONTAINERMENU)
		*/
			);
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
 /*  // do we need to do this here, or is it done as part of
     // registering the type library???
    register-item($clsid-prefix, class-id-string, "\\Version",
		  format-to-string("%d.%d", typeinfo.major-version,
				   typeinfo.minor-version));
  */

    // the following are for compatibility with 16-bit container applications:

    register-item(prog-id, empty-string, empty-string, title-string);
    register-item(prog-id, "\\protocol\\StdFileEditing\\server", empty-string,
		  module-path);
    register-item(prog-id, "\\Insertable", empty-string, empty-string);
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
    elseif ( returned-prog-id ~= prog-id )
      error("Registration verification failed;\nexpected \"%s\", got \"%s\"",
	    prog-id, returned-prog-id);
    end if;
  end if;
  values()
end register-ole-control;

define method register-ole-control
    (class-id-string :: <string>,
     prog-id :: <string>,
     title-string :: <string>, #rest args, #key, #all-keys) => ();
  // Even though it is the string representation that we really use for
  // registration, convert it to a structure and back to a string in order
  // to ensure that it is in the canonical representation.
  let class-id :: <REFCLSID> = as(<REFCLSID>, class-id-string);
  apply(register-ole-control, class-id, prog-id, title-string, args);
  destroy(class-id);
end method;

define method register-control-coclass (coclass-type-info :: <Dylan-Type-Info>,
					prog-id,
					#rest options,
					#key title, unregister?,
					module-handle,
					#all-keys)
 => (status :: <HRESULT>);

  //---- should add error handling; the returned value should be one of:
  // $S-OK, $E-OUTOFMEMORY, $E-UNEXPECTED,
  // $SELFREG-E-TYPELIB, $SELFREG-E-CLASS

  AddRef(coclass-type-info);
  let title-string = title | coclass-title-string(coclass-type-info);
  if ( empty?(title-string) )
    error("No title for %=", coclass-type-info);
  end if;
  apply(register-ole-control, coclass-type-info.guid-value,
			      prog-id, title-string, options);
  if ( unregister? )
    unregister-type-library(coclass-type-info, module-handle: module-handle);
  else
    register-type-library(coclass-type-info, module-handle: module-handle);
  end if;
  Release(coclass-type-info);
  $S-OK
end;
