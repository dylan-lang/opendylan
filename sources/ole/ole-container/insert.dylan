Module:    OLE-Container
Synopsis:  Insert an OLE object into the container application.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define method insert-object-from-dialog 
    (app :: <container-app>,
     doc-window :: <HWND>,
     #rest options,
     #key title :: <string> = "Insert Object",
     failure-title :: <string> = "Insertion failure",
     flags :: <integer> = logior($IOF-SELECTCREATENEW, $IOF-DISABLELINK,
				 $IOF-DISABLEDISPLAYASICON,
				 $IOF-CREATENEWOBJECT, $IOF-CREATEFILEOBJECT),
     exclude :: <sequence> = #[],
     hook-function, hook-data,
     #all-keys)
 => ( doc :: false-or(<contained-object>) )

  // OleUIInsertObject and the associated data structure 
  // <LPOLEUIINSERTOBJECT> are in the OLE-Dialogs library.

  let io :: <LPOLEUIINSERTOBJECT> = make(<LPOLEUIINSERTOBJECT>);

  let szFile :: <LPTSTR> = make(<LPTSTR>, size: $MAX-PATH);

  let ( doc :: <contained-object>, render, format-etc ) =
    apply(create-doc-object, app, doc-window, options);
  let site = doc;

  // clear the structure
  let numbytes = size-of(referenced-type(<LPOLEUIINSERTOBJECT>));
  clear-memory!(io, numbytes);

  // fill the structure
  io.cbStruct-value := numbytes;
  io.dwFlags-value := flags;
  io.hWndOwner-value := doc-window;
  io.lpszCaption-value := title;
  copy-into!(io.iid-value, $IID-IOleObject, size-of(<IID>));
  io.oleRender-value := render;
  io.lpFormatEtc-value := format-etc;
  io.lpIOleClientSite-value := site.document-client-site;
  io.lpIStorage-value := site.document-sub-storage;
  let objptr = make(<C-void**>);
  objptr.pointer-value := site.document-ole-object;
  io.ppvObj-value := objptr;
  io.lpszFile-value := szFile;
  io.cchFile-value := $MAX-PATH;
  clear-memory!(szFile, $MAX-PATH);

  if ( hook-function )
    io.lpfnHook-value := hook-function;
  end if;
  if ( hook-data )
    io.lCustData-value := hook-data;
  end if;

  let exclude-ptr :: <LPCLSID> = null-pointer(<LPCLSID>);
  unless ( empty?(exclude) )
    let xcount :: <fixnum> = size(exclude);
    exclude-ptr := make(<LPCLSID>, element-count: xcount);
    for ( i from 0 below xcount )
      let item = exclude[i];
      let class-id :: <REFCLSID> = as(<REFCLSID>, item);
      copy-into!(pointer-value-address(exclude-ptr, index: i),
		 class-id, size-of(<GUID>));
      unless ( class-id == item )
	destroy(class-id);
      end unless;
    end for;
    io.cClsidExclude-value := xcount;
    io.lpClsidExclude-value := exclude-ptr;
  end unless;

  let return-value = #f;
  block()
    let iret :: <unsigned-fixnum> = OleUIInsertObject(io);
    if ( iret = $OLEUI-OK ) // successful insertion
      let result-flags :: <integer> = io.dwFlags-value;
      unless ( IsEqualCLSID?(io.Clsid-value, $CLSID-NULL) )
	doc.document-class-id := copy-guid(io.Clsid-value);
      end unless;
      unless ( zero?(logand(result-flags, $IOF-SELECTCREATEFROMFILE)) )
	// remember the file name when relevant.
	doc.document-file-name := as(<byte-string>, io.lpszFile-value);
      end unless;
  /*   // unfinished, not yet supported
      unless ( zero?(logand(result-flags, $IOF-CHECKDISPLAYASICON)) |
		null-pointer?(io.hMetaPict-value) )
	doc.document-draw-aspect := $DVASPECT-ICON;
	??? := io.hMetaPict-value;
      end unless;
   */
      let created-new? = ~ zero?(logand(result-flags, $IOF-SELECTCREATENEW));
            init-object-in-site(site, objptr.pointer-value, created-new?);
      return-value := doc;
    else
      unless ( iret = $OLEUI-CANCEL ) // unless user pressed "Cancel" button
	// report an error
	let scode :: <SCODE> = io.sc-value;
	MessageBox(app.container-frame-window,  
		   if ( (iret = $OLEUI-IOERR-SCODEHASERROR) & FAILED?(scode) )
		     format-to-string("OleCreate... scode = %=:\n%s",
				      scode,
				      win32-error-message(scode) | "")
		   else
		     format-to-string("OleUIInsertObject error %d\n(%s)",
				      iret,  insert_error_name(iret))
		   end if, 
		   failure-title, /* window title */ 
		   /* icon and button: */ 
		   logior($MB-ICONEXCLAMATION, $MB-OK));
      end unless;
    end if;
  cleanup
    unless ( return-value == doc )
      abort-doc(doc);
    end;
    destroy(exclude-ptr);
    destroy(objptr);
    destroy(io);
    destroy(szFile);
  exception (<abort>)
    values();
  end block;
  return-value
end method insert-object-from-dialog;

define function abort-doc ( doc :: <contained-object> ) => ()
  IStorage/Revert(doc.document-sub-storage);
  close-doc-object(doc, save?: #f);
end;

define function copy-guid ( id :: <REFGUID> ) => ( copy :: <REFGUID> )
  let new :: <REFGUID> = make(<REFGUID>);
  copy-into!(new, id, size-of(<GUID>));
  new
end;

define method insert_error_name(err :: <fixnum>) => name :: <string>; 
  select ( err ) 
    $OLEUI-IOERR-LPSZFILEINVALID =>       "OLEUI_IOERR_LPSZFILEINVALID";
    $OLEUI-IOERR-LPSZLABELINVALID =>      "OLEUI_IOERR_LPSZLABELINVALID";
    $OLEUI-IOERR-HICONINVALID =>          "OLEUI_IOERR_HICONINVALID";
    $OLEUI-IOERR-LPFORMATETCINVALID =>    "OLEUI_IOERR_LPFORMATETCINVALID";
    $OLEUI-IOERR-PPVOBJINVALID =>         "OLEUI_IOERR_PPVOBJINVALID";
    $OLEUI-IOERR-LPIOLECLIENTSITEINVALID =>
					 "OLEUI_IOERR_LPIOLECLIENTSITEINVALID";
    $OLEUI-IOERR-LPISTORAGEINVALID =>     "OLEUI_IOERR_LPISTORAGEINVALID";
    $OLEUI-IOERR-SCODEHASERROR =>         "OLEUI_IOERR_SCODEHASERROR";
    $OLEUI-IOERR-LPCLSIDEXCLUDEINVALID => "OLEUI_IOERR_LPCLSIDEXCLUDEIVALID";
    $OLEUI-IOERR-CCHFILEINVALID =>        "OLEUI_IOERR_CCHFILEINVALID";
    otherwise =>    "see OLEDLG.H";
  end select;
end method insert_error_name;




define method insert-object-by-class (app :: <container-app>,
				      doc-window :: <HWND>,
				      class-id :: <object>,
				      #rest options)
 => ( doc :: <contained-object> )

  let class-id-ptr :: <REFCLSID> = as(<REFCLSID>, class-id);
  let ( doc :: <contained-object>, render, format-etc ) =
    apply(create-doc-object, app, doc-window, options);
  let inserted? :: <boolean> = #f;
  block ()
    let ( status, interface ) =
      OleCreate(class-id-ptr, $IID-IOleObject, render, format-etc,
		doc.document-client-site, doc.document-sub-storage);
    check-ole-status(status, "OleCreate", $null-interface, class-id-ptr);
    inserted? := #t;
    doc.document-class-id := copy-guid(class-id-ptr);
    init-object-in-site(doc, interface, #t);
  cleanup
    unless ( inserted? )
      abort-doc(doc);
    end;
    unless ( class-id-ptr == class-id )
      destroy(class-id-ptr);
    end;
  end block;
  doc
end insert-object-by-class;

define method insert-object-from-file  (app :: <container-app>,
					doc-window :: <HWND>,
					file-name :: <string>,
					#rest options)
 => ( doc :: <contained-object> )

  let pathname :: <LPOLESTR> = as(<LPOLESTR>, file-name);
  let ( doc :: <contained-object>, render, format-etc ) =
    apply(create-doc-object, app, doc-window, options);
  let inserted? :: <boolean> = #f;
  block ()
    let ( status, interface ) =
      // Or should this be OleCreateLinkToFile ???
      OleCreateFromFile($CLSID-NULL, pathname, $IID-IOleObject,
			render, format-etc,
			doc.document-client-site, doc.document-sub-storage);
    check-ole-status(status, "OleCreateFromFile", $null-interface,
		     file-name);
    inserted? := #t;
    doc.document-file-name := as(<byte-string>, file-name);
    init-object-in-site(doc, interface, #f);
  cleanup
    unless ( inserted? )
      abort-doc(doc);
    end;
    unless ( pathname == file-name )
      destroy(pathname);
    end unless;
  end block;
  doc
end insert-object-from-file;
