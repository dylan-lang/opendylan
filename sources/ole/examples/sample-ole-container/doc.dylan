Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//**********************************************************************
//
// CSimpleDoc::Create
//
// Purpose:
//
//      Creation for the CSimpleDoc Class
//
// Parameters:
//
//      CSimpleApp FAR * lpApp  -   Pointer to the CSimpleApp Class
//
//      LPRECT lpRect           -   Client area rect of "frame" window
//
//      HWND hWnd               -   Window Handle of "frame" window
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      StgCreateDocfile            OLE API
//      CreateWindow                Windows API
//      ShowWindow                  Windows API
//      UpdateWindow                Windows API
//
// Comments:
//
//      This routine was added so that failure could be returned
//      from object creation.
//
//********************************************************************


define method CSimpleDoc-Create(lpApp :: <CSimpleApp>, lpRect :: <LPRECT>,
				hWnd :: <HWND>)
	=> value :: <CSimpleDoc>;

  block(return)
	
    let lpTemp :: <CSimpleDoc> = make(<CSimpleDoc>, lpApp: lpApp, hWnd: hWnd);

    // create storage for the doc.
    let ( hErr :: <HRESULT>, storage ) =
      StgCreateDocfile($NULL-OLESTR,
		       logior($STGM-READWRITE, $STGM-TRANSACTED,
			      $STGM-SHARE-EXCLUSIVE),
		       0);
    lpTemp.m-lpStorage := storage;
    if ( hErr ~= $NOERROR )
      return($null-interface)
    end if;

    // create the document Window
    lpTemp.m-hDocWnd :=
      CreateWindow($doc-window-class,
		   $NULL-string,
		   %logior( $WS-CHILD, $WS-CLIPCHILDREN),
		   lpRect.left-value,
		   lpRect.top-value,
		   lpRect.right-value,
		   lpRect.bottom-value,
		   hWnd,
		   null-pointer(<HMENU>),
		   lpApp.m-hInst,
		   $NULL-VOID);

    ShowWindow( lpTemp.m-hDocWnd, $SW-SHOWNORMAL);  // Show the window
    UpdateWindow( lpTemp.m-hDocWnd);               // Sends WM_PAINT message

    // Ensable InsertObject menu choice
    EnableMenuItem(lpApp.m-hEditMenu, 0, logior($MF-BYPOSITION, $MF-ENABLED));

    // we will add one ref count on our document. later in CSimpleDoc::CloseDoc
    // we will release this  ref count. when the document's ref count goes
    // to 0, the document will be deleted.
    AddRef(lpTemp);

    lpTemp
  end block
end method CSimpleDoc-Create;

//**********************************************************************
//
// CSimpleDoc::CloseDoc
//
// Purpose:
//
//      Close CSimpleDoc object.
//      when the document's reference count goes to 0, the document
//      will be destroyed.
//
// Parameters:
//
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      RevokeDragDrop              OLE API
//      CoLockObjectExternal        OLE API
//      OleFlushClipboard           OLE API
//      ShowWindow                  Windows API
//
// Comments:
//
//********************************************************************


define method CloseDoc(this :: <CSimpleDoc>) => ();

  OutputDebugString( "In CSimpleDoc::CloseDoc\r\n");

  ShowWindow( this.m-hDocWnd, $SW-HIDE);  // Hide the window

  // Close the OLE object in our document
  unless ( null?(this.m-lpSite) )
    CloseOleObject(this.m-lpSite);
  end unless;

  // Release the ref count added in CSimpleDoc::Create. this will make
  // the document's ref count go to 0, and the document will be deleted.
  Release(this);
  values()
end method CloseDoc;

//**********************************************************************
//
// CSimpleDoc::CSimpleDoc
//
// Purpose:
//
//      Constructor for the CSimpleDoc Class
//
// Parameters:
//
//      CSimpleApp FAR * lpApp  -   Pointer to the CSimpleApp Class
//
//      HWND hWnd               -   Window Handle of "frame" window
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      GetMenu                     Windows API
//      GetSubMenu                  Windows API
//
// Comments:
//
//********************************************************************


define method initialize (this :: <CSimpleDoc>,
			  #key lpApp :: <CSimpleApp>, hWnd :: <HWND>,
			  #all-keys) => ();

  next-method();
  OutputDebugString( "In CSimpleDoc's Constructor\r\n");
  this.m-lpApp := lpApp;
  this.m-lpSite := #f;
  // set up menu handles
  lpApp.m-hMainMenu := GetMenu(hWnd);
  lpApp.m-hFileMenu := GetSubMenu(lpApp.m-hMainMenu, 0);
  lpApp.m-hEditMenu := GetSubMenu(lpApp.m-hMainMenu, 1);
  lpApp.m-hHelpMenu := GetSubMenu(lpApp.m-hMainMenu, 2);
  lpApp.m-hCascadeMenu := null-handle(<HMENU>);

  this.m-lpActiveObject := $null-interface;

  // flags
  this.m-fInPlaceActive := #f;
//  this.m-fAddMyUI := #f;
  this.m-fModifiedMenu := #f;
  values()
end method initialize ;

//**********************************************************************
//
// CSimpleDoc::~CSimpleDoc
//
// Purpose:
//
//      Destructor for CSimpleDoc
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      CSimpleSite::Release        SITE.CPP
//      IStorage::Release           OLE API
//
// Comments:
//
//********************************************************************


define method terminate (this :: <CSimpleDoc>) => ();

  OutputDebugString( "In CSimpleDoc's Destructor\r\n");
  next-method();

  // Release all pointers we hold to the OLE object. also release
  // the ref count added in CSimpleSite::Create. this will make
  // the Site's ref count go to 0, and the Site will be deleted.
  unless ( null?(this.m-lpSite) ) 
    UnloadOleObject(this.m-lpSite);
    Release(this.m-lpSite);
    this.m-lpSite := #f;
  end unless;

  // Release the Storage
  unless ( null?(this.m-lpStorage) ) 
    Release(this.m-lpStorage);
    this.m-lpStorage := $null-interface;
  end unless;

  // if the edit menu was modified, remove the menu item and
  // destroy the popup if it exists
  if ( this.m-fModifiedMenu )
	        
    let nCount :: <integer> = GetMenuItemCount(this.m-lpApp.m-hEditMenu);
    RemoveMenu(this.m-lpApp.m-hEditMenu, nCount -  1, $MF-BYPOSITION);
    unless ( null-handle?(this.m-lpApp.m-hCascadeMenu) )
      DestroyMenu(this.m-lpApp.m-hCascadeMenu);
    end unless;
  end if;
  
  DestroyWindow(this.m-hDocWnd);
  values()
end method terminate;


//**********************************************************************
//
// CSimpleDoc::QueryInterface
//
// Purpose:
//
//      Return a pointer to a requested interface
//
// Parameters:
//
//      REFIID riid         -   ID of interface to be returned
//      LPVOID FAR* ppvObj  -   Location to return the interface
//
// Return Value:
//
//      S_FALSE -   Always
//
// Function Calls:
//      Function                    Location
//
//      OutputDebugString           Windows API
//      ResultFromScode             OLE API
//
// Comments:
//
//      In this implementation, there are no doc level interfaces.
//      In an MDI application, there would be an IOleInPlaceUIWindow
//      associated with the document to provide document level tool
 //     space negotiation.
//
//********************************************************************


define method IUnknown/QueryInterface(this :: <CSimpleDoc>, riid :: <REFIID>)
	=> ( status :: <HRESULT>, interface :: <Interface> );

  OutputDebugString( "In CSimpleDoc::QueryInterface\r\n");

  // Not a supported interface
  values( $E-NOINTERFACE, $null-interface )
end method IUnknown/QueryInterface;


//**********************************************************************
//
// CSimpleDoc::InsertObject
//
// Purpose:
//
//      Inserts a new object to this document
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      CSimpleSite::CSimpleSite    SITE.CPP
//      CSimpleSite::InitObject     SITE.CPP
//      memset                      C Runtime
//      OleUIInsertObject           OUTLUI function
//      CSimpleDoc::DisableInsertObject DOC.CPP
//
// Comments:
//
//      This implementation only allows one object to be inserted
//      into a document.  Once the object has been inserted, then
//      the Insert Object menu choice is greyed out, to prevent
//      the user from inserting another.
//
//********************************************************************

define constant $insert-object-caption = TEXT("Insert Object");

define method InsertObject(this :: <CSimpleDoc>) => ();

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  let io :: <LPOLEUIINSERTOBJECT> = make(<LPOLEUIINSERTOBJECT>);

  let szFile :: <LPTSTR> = make(<LPTSTR>, size: $MAX-PATH);

  let site :: <CSimpleSite> = CSimpleSite-Create(this);
  this.m-lpSite := site;

  // clear the structure
  let numbytes = size-of(referenced-type(<LPOLEUIINSERTOBJECT>));
  clear-memory!(io, numbytes);

  // fill the structure
  io.cbStruct-value := numbytes;
  io.dwFlags-value := logior($IOF-SELECTCREATENEW, $IOF-DISABLELINK,
			     $IOF-DISABLEDISPLAYASICON, $IOF-CREATENEWOBJECT,
			     $IOF-CREATEFILEOBJECT);
  io.hWndOwner-value := this.m-hDocWnd;
  io.lpszCaption-value := $insert-object-caption;
  copy-into!(io.iid-value, $IID-IOleObject, size-of(<IID>));
  io.oleRender-value := $OLERENDER-DRAW;
  io.lpIOleClientSite-value := site.m-OleClientSite;
  io.lpIStorage-value := site.m-lpObjStorage;
  let objptr = make(<C-void**>);
  objptr.pointer-value := site.m-lpOleObject;
  io.ppvObj-value := objptr;
  io.lpszFile-value := szFile;
  io.cchFile-value := $MAX-PATH;
  clear-memory!(szFile, $MAX-PATH);
  
  // call OUTLUI to do all the hard work
  let iret :: <integer> = OleUIInsertObject(io);

  site.m-lpOleObject := dylan-interface(pointer-cast(<LPOLEOBJECT>,
						     objptr.pointer-value));
  if ( iret = $OLEUI-OK )
    InitObject(site,
	       ~ zero?(logand(io.dwFlags-value, $IOF-SELECTCREATENEW)));
    // disable Insert Object menu item
    DisableInsertObject(this);
  else
    unless ( iret = $OLEUI-CANCEL )
      MessageBox($NULL-HWND,  
		 format-to-string("OleUIInsertObject error %d\n(%s)"
				    "\n\nOleCreate* scode = %=",
				  iret,  insert_error_name(iret),
				  io.sc-value), 
		 "Insertion failure", /* window title */ 
		 /* icon and button: */ 
		 (logior( $MB-ICONEXCLAMATION, $MB-OK)));
    end unless;
    Release(site);
    this.m-lpSite := #f;
    IStorage/Revert(this.m-lpStorage);
  end if;
  destroy(objptr);
  destroy(io);
  destroy(szFile);
 values()
end method InsertObject;

define method insert_error_name(err :: <integer>) => name :: <string>; 
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

define method lResizeDoc(this :: <CSimpleDoc>, lpRect :: <LPRECT>)
	=> value :: <integer>;

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  // if we are InPlace, then call ResizeBorder on the object, otherwise
  // just move the document window.
  if ( this.m-fInPlaceActive )
    IOleInPlaceActiveObject/ResizeBorder(this.m-lpActiveObject, lpRect,
					 this.m-lpApp.m-OleInPlaceFrame, #t); 
  else
    MoveWindow(this.m-hDocWnd, lpRect.left-value, lpRect.top-value,
	       lpRect.right-value, lpRect.bottom-value, #t);
  end if;
  
  0
end method lResizeDoc;

//**********************************************************************
//
// CSimpleDoc::lAddVerbs
//
// Purpose:
//
//      Adds the objects verbs to the edit menu.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      NULL
//
// Function Calls:
//      Function                    Location
//
//      GetMenuItemCount            Windows API
//      OleUIAddVerbMenu            OUTLUI function
//
// Comments:
//
//********************************************************************


define method lAddVerbs(this :: <CSimpleDoc>) => value :: <integer>;

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  // m_fModifiedMenu is TRUE if the menu has already been modified
  // once.  Since we only support one object every time the application
  // is run, then once the menu is modified, it doesn't have
  // to be done again.
  if ( ~ null?(this.m-lpSite)
	& ~ this.m-fInPlaceActive & ~ this.m-fModifiedMenu )
	        
    let nCount :: <integer> = GetMenuItemCount( this.m-lpApp.m-hEditMenu);

    let ( ok :: <boolean>, menu :: <HMENU> ) =
      OleUIAddVerbMenu(this.m-lpSite.m-lpOleObject,
		       $NULL-string,
		       this.m-lpApp.m-hEditMenu,
		       nCount + 1,
		       $IDM-VERB0,
		       0,          // no maximum verb IDM enforced
		       #f,
		       0);
    this.m-lpApp.m-hCascadeMenu := menu;
    this.m-fModifiedMenu := #t;
  end if;
  0
end method lAddVerbs;

//**********************************************************************
//
// CSimpleDoc::PaintDoc
//
// Purpose:
//
//      Paints the Document
//
// Parameters:
//
//      HDC hDC -   hDC of the document Window
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      CSimpleSite::PaintObj       SITE.CPP
//
// Comments:
//
//********************************************************************


define method PaintDoc(this :: <CSimpleDoc>, hDC :: <HDC>) => ();

  // let stabilize :: <CStabilize> = make(<CStabilize>, this);

  // if we supported multiple objects, then we would enumerate
  // the objects and call paint on each of them from here.

  unless ( null?(this.m-lpSite) )
    PaintObj(this.m-lpSite, hDC);
  end unless;

  values()
end method PaintDoc;

//**********************************************************************
//
// CSimpleDoc::DisableInsertObject
//
// Purpose:
//
//      Disable the ability to insert a new object in this document.
//
// Parameters:
//
//      None
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                    Location
//
//      EnableMenuItem              Windows API
//
// Comments:
//
//      This implementation only allows one object to be inserted
//      into a document.  Once the object has been inserted, then
//      the Insert Object menu choice is greyed out, to prevent
//      the user from inserting another.
//
//********************************************************************


define method DisableInsertObject(this :: <CSimpleDoc>) => ();

  // Disable InsertObject menu choice
  EnableMenuItem(this.m-lpApp.m-hEditMenu, 0,
		 logior($MF-BYPOSITION, $MF-DISABLED, $MF-GRAYED));
  values()
end method DisableInsertObject;
