Module:    OLE-Container
Synopsis:  Class and methods for managing an embedded document.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open primary COM-interface <contained-object> ( <IUnknown> ) 

  // Information supplied by the container application:

  sealed constant slot document-application :: <container-app>,
		required-init-keyword: app:;
  constant slot document-container-window :: <HWND>,
		required-init-keyword: doc-window:;
  constant slot document-draw-aspect :: <unsigned-fixnum> = $DVASPECT-CONTENT,
		init-keyword: aspect:;
  constant slot containing-document-name :: false-or(<string>) = #f,
		init-keyword: document-name:;

  // How the server was chosen:

  slot document-class-id :: <REFCLSID> = null-pointer(<REFCLSID>);
  slot document-file-name :: false-or(<string>) = #f;

  // Interfaces implemented by the server:

  sealed slot document-ole-object :: <LPOLEOBJECT>
			= null-pointer(<LPOLEOBJECT>);
  sealed slot document-in-place-ole-object :: <LPOLEINPLACEOBJECT>
			= null-pointer(<LPOLEINPLACEOBJECT>);

  // Other data provided by the server:

  sealed slot document-ui-active-window :: <HWND> = $NULL-HWND;
  sealed slot document-olemisc :: <unsigned-fixnum> = 0;

  // Interfaces implemented by the container:
 
  slot document-sub-storage :: <LPSTORAGE> = null-pointer(<LPSTORAGE>);
  slot document-advise-sink :: <container-advise-sink>;	
  slot document-in-place-site :: <container-ole-in-place-site>;
  slot document-client-site :: <container-ole-client-site>;

  // State information:

  slot document-in-place-active? :: <boolean> = #f;
  sealed slot document-open? :: <boolean> = #f; // IOleObject/Close not called?
  sealed slot document-released? :: <boolean> = #f; // Release called?
  // Offset of the embedded window, in pixels:
  sealed slot x-origin :: <fixnum> = 0, init-keyword: x:;
  sealed slot y-origin :: <fixnum> = 0, init-keyword: x:;
  // Size of the object, filled in by IOleObject/GetExtent
  // or IViewObject2/GetExtent:
  sealed slot site-object-size :: <LPSIZEL> = make(<LPSIZEL>), setter: #f;
  slot document-open-out-of-place? :: <boolean> = #f; // from IOleClientSite/OnShowWindow

  slot document-modified-menu :: <HMENU> = $NULL-HMENU; // modified Edit menu
  slot document-menu-position :: <fixnum> = -1; // position of added item
  slot document-cascade-menu :: <HMENU> = $NULL-HMENU; //sub-menu for verbs
	
end <contained-object>;

// This slot is just used as a debugging aid; the value is actually
// referenced through QueryInterface.
ignorable(document-in-place-site);

define method initialize (doc :: <contained-object>,
			  #rest args, #key
			  render, format-etc,
			  hook-function, hook-data, exclude,
			  #all-keys) => ();
  // These options are only used in `insert-object-from-dialog':
  ignore(hook-function, hook-data, exclude);
  // These options are handled in `create-doc-object':
  ignore(render, format-etc);

  OutputDebugString( "initialize(<contained-object>)\r\n");
  next-method();
  let app = doc.document-application;
  app.container-documents := add!(app.container-documents, doc);
  site-initialization(doc);
  values()
end method initialize ;

define open generic create-doc-object (app, doc-window, #key, #all-keys)
 => ( object :: <contained-object>,
     render :: <integer>, format-etc :: <LPFORMATETC> );

// Create a new document and site object to hold an embedded server.
define method create-doc-object(App :: <container-app>, doc-window :: <HWND>,
				#rest options, #key
				doc-class :: <class> =
				  App.container-document-class,
				render :: <integer> = $OLERENDER-DRAW,
				format-etc :: <LPFORMATETC> =
				  null-pointer(<LPFORMATETC>),
				aspect = #f,
				#all-keys)
	=> ( object :: <contained-object>,
	     render :: <integer>,
	     format-etc :: <LPFORMATETC> )


  let aspect-value :: <fixnum> = $DVASPECT-CONTENT;
  if ( aspect )
    aspect-value := aspect
  elseif ( ~ null?(format-etc) )
    let fa = format-etc.dwAspect-value;
    if ( instance?(fa,<integer>) & (fa > 0) )
      aspect-value := fa;
    end if;
  end if;
  // Need some additional handling for interactions between `render'
  // and `format-etc'.	???

  let Doc :: <contained-object> =
    apply(make, doc-class, App: App, doc-window: doc-window, 
	 aspect: aspect-value, options);

  // Add one ref count on our document.  Later in close-doc-object
  // we will release this ref count.  When the document's ref count goes
  // to 0, the document will be deleted.
  AddRef(Doc);

  values( Doc, render, format-etc )
end method create-doc-object;

define open generic container-document-class (app) => (class :: <class>);
// override this method to use a subclass of <contained-object>
define method container-document-class (App :: <container-app>)
 => ( class :: <class> )
  <contained-object>
end;

define open generic close-doc-object(doc-site, #key save?) => ();

define method close-doc-object(this :: <contained-object>,
			       #key save? = #t) => ();


  // Use `save?: #t' to save the document to storage before closing, such as
  //  when exiting the application.
  // Use `save?: #f' to delete the document's storage, such as when 
  //  deleting an object from the compound document.

  OutputDebugString("close-doc-object\r\n");

  // Close the OLE object in our document
  if ( save? )
    close-OLE-object(this, $OLECLOSE-SAVEIFDIRTY);
  end if;
  unload-OLE-object(this);

  unless ( this.document-released? )
    this.document-released? := #t;
    // Release the ref count added in create-doc-object. this will make
    // the document's ref count go to 0, and the document will be deleted.
    Release(this);
  end unless;

  let app = this.document-application;
  if ( ( ~ save? ) & empty?(app.container-documents) )
    // when last contained document deleted, delete the storage also.
    let storage = app.container-storage;
    unless ( null?(storage) ) 
      IStorage/Revert(storage);
      Release(storage);
      app.container-storage := $null-interface;
    end unless;
  end if;

  values()
end method close-doc-object;

define method terminate (this :: <contained-object>) => ();

  OutputDebugString( "terminate(<contained-object>)\r\n");

  let site = this;
  unload-OLE-object(site);
  Release(site.document-sub-storage);
  site.document-sub-storage := null-pointer(<LPSTORAGE>);
  destroy(site.site-object-size);

  let app = this.document-application;
  app.container-documents := remove!(app.container-documents, this);

  destroy(this.document-class-id);
  next-method();
  values()
end method terminate;



//**********************************************************************
//
// CSimpleDoc::lResizeDoc
//
// Purpose:
//
//      Resizes the document
//
// Parameters:
//
//      LPRECT lpRect   -   The size of the client are of the "frame"
//                          Window.
//
// Return Value:
//
//      NULL
//
// Function Calls:
//      Function                                Location
//
//      IOleInPlaceActiveObject::ResizeBorder   Object
//      MoveWindow                              Windows API
//
// Comments:
//
//********************************************************************

define method container-size-changed (this :: <contained-object>,
				      rectangle :: <LPRECT>)
 => ()

  // if we are InPlace, then call ResizeBorder on the object, otherwise
  // just move the document window.

  // The semantics of this need to be reconsidered. ???

  if ( this.document-in-place-active? )
    let app = this.document-application;
    IOleInPlaceActiveObject/ResizeBorder(app.in-place-active-object, rectangle,
					 app.app-ole-in-place-frame, #t); 
  else
    MoveWindow(this.document-container-window,
	       rectangle.left-value, rectangle.top-value,
	       rectangle.right-value, rectangle.bottom-value, #t);
  end if;
end method container-size-changed;

// Adds the object's verbs to the edit menu, if not already done.
define method container-add-verbs(this :: <contained-object>,
				  edit-menu :: <HMENU>,
				  first-command-code :: <integer>,
				  #key max-command :: <integer> = 0,
				  name :: <string> = $NULL-string,
				  convert)
 => ( modified? :: <boolean>, cascade-menu :: <HMENU> );

  if ( this.document-modified-menu = edit-menu ) // already done
    values( #t, this.document-cascade-menu )
  elseif ( null?(this.document-ole-object) | this.document-in-place-active? )
    values( #f, $NULL-HMENU )
  else
    container-remove-verbs(this);
    let count :: <fixnum> = GetMenuItemCount(edit-menu);
    let ( ok? :: <boolean>, cascade-menu :: <HMENU> ) =
      OleUIAddVerbMenu(this.document-ole-object,
		       name, // object's short name
		       edit-menu,
		       count + 1,    // position in menu
		       first-command-code, // uIDVerbMin
		       max-command, // uIDVerbMax; 0 => no maximum enforced
		       convert ~== #f,     // whether to add Convert item
		       convert | 0); // value to use for the Convert item
    this.document-modified-menu := edit-menu;
    this.document-menu-position := count;
    this.document-cascade-menu := cascade-menu;
    values( #t, cascade-menu )
  end if
end method container-add-verbs;

define method container-add-verbs(this == #f, 
                                  old-menu, command, #key, #all-keys)
                        => ( modified? :: <boolean>, cascade-menu :: <HMENU> );
 values( #f, $NULL-HMENU )
end;

// un-do effect of container-add-verbs
define method container-remove-verbs (this :: <contained-object>) => ()
  let edit-menu = this.document-modified-menu;
  unless ( null-handle?(edit-menu) )
    let count :: <fixnum> = GetMenuItemCount(edit-menu);
    RemoveMenu(edit-menu, this.document-menu-position, $MF-BYPOSITION);
    this.document-modified-menu := $NULL-HMENU;
    for ( other in this.document-application.container-documents )
      if ( other.document-menu-position > this.document-menu-position )
	other.document-menu-position := other.document-menu-position - 1;
      end if;
    end for;
    let cascade-menu = this.document-cascade-menu;
    unless ( null-handle?(cascade-menu) )
      DestroyMenu(cascade-menu);
      this.document-cascade-menu := $NULL-HMENU;
    end unless;
  end unless;
end method container-remove-verbs;

define method container-remove-verbs (this == #f) => ()
  values()
end;


// for application to call:
define method container-do-verb(doc :: <contained-object>,
				verb :: <integer>,
				message :: <LPMSG>)
 => ( status :: <HRESULT> )

  let site = doc;
  let ole-object = site.document-ole-object;
  if ( null?(ole-object) )
    $S-FALSE	// can this ever happen?
  else
    with-stack-structure ( pRect :: <LPRECT> )
      // get the rectangle of the object
      document-rectangle(site, pRect);
      let status = IOleObject/DoVerb(ole-object, verb, message,
				     site.document-client-site,
				     0, doc.document-container-window, pRect);
      if ( FAILED?(status) )
	ole-cerror(status, "IOleObject/DoVerb", ole-object, verb);
      end if;
      status
    end with-stack-structure
  end if
end container-do-verb;

define method container-UI-deactivate ( doc :: <contained-object> ) => ()
  if ( doc.document-in-place-active? )
    let ( status , interface ) =
      QueryInterface(doc.document-ole-object, $IID-IOleInPlaceObject);
    if ( FAILED?(status) )
      OutputDebugString( "container-UI-deactivate failed\r\n");
    else
      let in-place-object :: <LPOLEINPLACEOBJECT> =
	pointer-cast(<LPOLEINPLACEOBJECT>, interface);

      let status = IOleInPlaceObject/UIDeactivate(in-place-object);
      if ( FAILED?(status) )
	OutputDebugString( "IOleInPlaceObject/UIDeactivate failed\r\n");
      end if;

      Release(in-place-object);
    end if;
  end if;
end method container-UI-deactivate;

define method container-UI-deactivate ( app :: <container-app> ) => ()
  do(container-UI-deactivate, app.container-documents);
end;


define method get-server-name ( doc :: <contained-object>, which :: <integer> )
 => (name :: <byte-string>);
  let ole-object :: <LPOLEOBJECT> = doc.document-ole-object;
  let (status :: <HRESULT>, szUserType :: <LPOLESTR>) =
    IOleObject/GetUserType(ole-object, which);
  check-ole-status(status, "IOleObject/GetUserType", ole-object, which);
  let result :: <byte-string> = as(<byte-string>, szUserType);
  ole-free(szUserType);
  result
end method;



