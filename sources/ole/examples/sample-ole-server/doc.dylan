Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//**********************************************************************
//
// CSimpSvrDoc::Create
//
// Purpose:
//
//      Creation for the CSimpSvrDoc Class
//
// Parameters:
//
//      CSimpSvrApp FAR * lpApp  -   Pointer to the CSimpSvrApp Class
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
//      CSimpSvrDoc::CSimpSvrDoc    DOC.CPP
//      CreateHatchWindow           OLESTD
//
// Comments:
//
//      This routine was added so that failure could be returned
//      from object creation.
//
//********************************************************************


define method Create(lpApp :: <CSimpSvrApp>,
		     lpRect :: <LPRECT>, hWnd :: <HWND>)
		=> value :: false-or(<CSimpSvrDoc>);

  let lpTemp :: <CSimpSvrDoc> = make(<CSimpSvrDoc>, lpApp: lpApp, hWnd: hWnd);

  // create the document Window
  lpTemp.m-hDocWnd := CreateWindow(
				   $doc-window-class,
				   $NULL-string,
				   %logior($WS-CHILD,$WS-CLIPSIBLINGS),
				   lpRect.left-value,
				   lpRect.top-value,
				   lpRect.right-value,
				   lpRect.bottom-value,
				   hWnd,
				   null-pointer(<HMENU>),
				   GethInst(lpApp),
				   $NULL-VOID);

  if ( null?(lpTemp.m-hDocWnd) )
    #f
  else

    ShowDocWnd(lpTemp);

    lpTemp.m-hHatchWnd := CreateHatchWindow( lpTemp.m-hDocWnd,
					    GethInst(lpApp));

    HideHatchWnd(lpTemp);

    lpTemp
  end if;

end method Create;

//**********************************************************************
//
// CSimpSvrDoc::CSimpSvrDoc
//
// Purpose:
//
//      Constructor for the CSimpSvrDoc Class
//
// Parameters:
//
//      CSimpSvrApp FAR * lpApp  -   Pointer to the CSimpSvrApp Class
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


define method initialize (this :: <CSimpSvrDoc>, #key lpApp :: <CSimpSvrApp>,
			  hWnd :: <HWND>, #all-keys ) => ();
	
  OutputDebugString("In CSimpSvrDoc's Constructor\r\n");
  next-method();

  this.m-lpApp := lpApp;
  this.m-lpObj := #f;
  this.m-fClosing := #f;

  // set up menu handles

  values()
end method initialize ;

//**********************************************************************
//
// CSimpSvrDoc::~CSimpSvrDoc
//
// Purpose:
//
//      Destructor for CSimpSvrDoc
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
//      DestroyWindow               Windows API
//      CSimpSvrApp::ClearDoc       APP.CPP
//
// Comments:
//
//********************************************************************


define method terminate (this :: <CSimpSvrDoc>) => ();
  OutputDebugString("In CSimpSvrDoc's Destructor\r\n");
  next-method();
  ClearDoc(this.m-lpApp);
  DestroyWindow(this.m-hHatchWnd);
  this.m-hHatchWnd := $NULL-HWND;
end method terminate ;


//**********************************************************************
//
// CSimpSvrDoc::lResizeDoc
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
//      MoveWindow                              Windows API
//
// Comments:
//
//********************************************************************


define method lResizeDoc(this :: <CSimpSvrDoc>, lpRect :: <LPRECT>)
	=> value :: <integer>;

  MoveWindow(this.m-hDocWnd, lpRect.left-value, lpRect.top-value,
	     lpRect.right-value, lpRect.bottom-value, #t);
  0
end method lResizeDoc;


//**********************************************************************
//
// CSimpSvrDoc::PaintDoc
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
//      CSimpSvrObj::Draw                   OBJ.CPP
//      CSimpSvrObj::GetDataAdviseHolder    OBJ.H
//      CSimpSvrObj::GetDataObject          OBJ.H
//      CSimpAppObj::IsStartedByOle         APP.CPP
//      IDataAdviseHolder::SendOnDataChange OLE API
//
// Comments:
//
//********************************************************************


define method PaintDoc(this :: <CSimpSvrDoc>, hDC :: <HDC>) => ();
	
  // if the object hasn't been created yet, then don't draw
  unless ( null?(this.m-lpObj) )
    Draw(this.m-lpObj, hDC, #f ); 
    
    // Sending a data change every time we paint, but only if we
    // were started by OLE
    if ( IsStartedByOle(this.m-lpApp) )
      SendOnDataChange(this.m-lpObj);
    end if;
  end unless;
  values()
end method PaintDoc;



//**********************************************************************
//
// CSimpSvrDoc::CreateObject
//
// Purpose:
//
//
// Parameters:
//
//
// Return Value:
//
//      NOERROR if the function succeeds, otherwise E_FAIL
//
// Function Calls:
//      Function                    Location
//
//      CSimpSvrObj::CSimpSvrObj    OBJ.CPP
//      CSimpSvrOjb::QueryInterface OBJ.CPP
//
// Comments:
//
//********************************************************************


define method CreateObject(this :: <CSimpSvrDoc>, riid :: <REFIID>)
		=> ( status :: <HRESULT>, Object :: <Interface> );

  this.m-lpObj := make(<CSimpSvrObj>, lpSimpSvrDoc: this);

  /* return */ QueryInterface(this.m-lpObj, riid)

end method CreateObject;

//**********************************************************************
//
// CSimpSvrDoc::CloseObject
//
// Purpose:
//
//      Closes the object
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
//      Function                                Location
//
//      OutputDebugString                       Windows API
//      CSimpSvrObj::AddRef                     OBJ.CPP
//      CSimpSvrObj::Release                    OBJ.CPP
//      CSimpSvrObj::IsInPlaceActive            OBJ.H
//      CSimpSvrObj::GetOleInPlaceObject        OBJ.H
//      CSimpSvrObj::ClearOleClientSite         OBJ.H
//      CSimpSvrObj::GetDataAdviseHolder        OBJ.H
//      CSimpSvrObj::GetOleClientSite           OBJ.H
//      CSimpSvrObj::ClearDataAdviseHolder      OBJ.H
//      CSimpSvrObj::GetOleAdviseHolder         OBJ.H
//      CSimpSvrObj::ClearOleAdviseHolder       OBJ.H
//      IOleInPlaceObject::InPlaceDeactivate    Container
//      IOleClientSite::SaveObject              Container
//      IOleClientSite::OnShowWindow            Container
//      IOleClientSite::Release                 Container
//      IDataAdviseHolder::SendOnDataChange     OLE
//      IDataAdviseHolder::Release              OLE
//      IOleAdviseHolder::SendOnClose           OLE
//      IOleAdviseHolder::Release               OLE
//
// Comments:
//
//********************************************************************


define method CloseObject (this :: <CSimpSvrDoc>) => ();

 block(return)
	
   OutputDebugString("In CSimpSvrDoc::CloseObject() \r\n");

   let svrObj :: <CSimpSvrObj> = this.m-lpObj;

   AddRef(svrObj); // hold object alive

   if ( this.m-fClosing )
     return();
   end if;

   this.m-fClosing := #t;

   // if the object is currently inplace active, then deactivate
   if ( IsInPlaceActive(svrObj) )
     IOleInPlaceObject/InPlaceDeactivate(GetOleInPlaceObject(svrObj));
   end if;

   // unregister from the ROT...
   if ( GetRotRegister(svrObj) ~= 0 )
	        
     let ( status :: <HRESULT>, lpRot :: <LPRUNNINGOBJECTTABLE> ) = 
       GetRunningObjectTable(0);
     if ( status = $NOERROR )
       IRunningObjectTable/Revoke(lpRot, GetRotRegister(svrObj));
       Release(lpRot);
     end if;
   end if;

   // if we have a clientsite, instruct it to save the object
   unless ( null?(GetOleClientSite(svrObj)) )
	        
     IOleClientSite/SaveObject(GetOleClientSite(svrObj));
     IOleClientSite/OnShowWindow(GetOleClientSite(svrObj), #f);
   end unless;

   // Do a final SendOnDataChange for those containers that have specified the
   // ADF_DATAONSTOP flag.
   unless ( null?(GetDataAdviseHolder(svrObj)) )
	        
     IDataAdviseHolder/SendOnDataChange(GetDataAdviseHolder(svrObj),
					GetDataObject(svrObj), 0,
					$ADVF-DATAONSTOP);
     Release(GetDataAdviseHolder(svrObj));
     ClearDataAdviseHolder(svrObj);
   end unless;


   // Tell the container that we are shutting down.
   unless ( null?(GetOleAdviseHolder(svrObj)) )
	        
     IOleAdviseHolder/SendOnClose(GetOleAdviseHolder(svrObj));
     Release(GetOleAdviseHolder(svrObj));
     ClearOleAdviseHolder(svrObj);
   end unless;

   unless ( null?(GetOleClientSite(svrObj)) )
	        
     Release(GetOleClientSite(svrObj));
     ClearOleClientSite(svrObj);
   end unless;

   // release our streams and storage
   ReleaseStreamsAndStorage(GetPersistStorage(svrObj));

   // Disconnect the object.  NOTE: This call should not do anything
   // unless the container has cause a GP Fault or some other problem
   // has occurred...
   OutputDebugString("*** Before CoDisconnectObject *** \r\n");
   CoDisconnectObject( svrObj, 0);
   OutputDebugString("*** After CoDisconnectObject *** \r\n");

   Release(svrObj); // let object close

 end block;
  values()
end method CloseObject;


//**********************************************************************
//
// CSimpSvrDoc::SetStatusText
//
// Purpose:
//
//      Sets the Container's status bar text
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
//      Function                        Location
//
//      CSimpSvrObj::IsInPlaceActive    OBJ.CPP
//      IOleInPlaceFrame::SetStatusText Container
//
// Comments:
//
//      Even though there is no status line in this sample, this
//      method must be called on WM_MENUSELECT to clear the last
//      message in the status line.
//
//********************************************************************


define method SetStatusText(this :: <CSimpSvrDoc>) => ();
  if ( IsInPlaceActive(this.m-lpObj) )
    IOleInPlaceFrame/SetStatusText(GetInPlaceFrame(this.m-lpObj), OLESTR("\0"))
  end if;
  values()
end method SetStatusText;

//**********************************************************************
//
// CSimpSvrDoc::ShowDocWnd
//
// Purpose:
//
//      Shows the Document Window
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
//      Function                        Location
//
//      ShowWindow                      Windows API
//      UpdateWindow                    Windows API
//
// Comments:
//
//********************************************************************


define method ShowDocWnd(this :: <CSimpSvrDoc>) => ();

  ShowWindow(this.m-hDocWnd, $SW-SHOWNORMAL);  // Show the window
  UpdateWindow(this.m-hDocWnd);               // Sends WM_PAINT message
  values()
end method ShowDocWnd;

//**********************************************************************
//
// CSimpSvrDoc::ShowHatchWnd
//
// Purpose:
//
//      Shows the hatch Window
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
//      Function                        Location
//
//      ShowWindow                      Windows API
//
// Comments:
//
//********************************************************************


define method ShowHatchWnd(this :: <CSimpSvrDoc>) => ();

  ShowWindow(this.m-hHatchWnd, $SW-SHOW);
  values()
end method ShowHatchWnd;

//**********************************************************************
//
// CSimpSvrDoc::HideDocWnd
//
// Purpose:
//
//      Hides the DocumentWindow
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
//      Function                        Location
//
//      ShowWindow                      Windows API
//
// Comments:
//
//********************************************************************

/*
define method HideDocWnd(this :: <CSimpSvrDoc>) => ();

  ShowWindow(this.m-hDocWnd, $SW-HIDE);
  values()
end method HideDocWnd;
*/
//**********************************************************************
//
// CSimpSvrDoc::HideHatchWnd
//
// Purpose:
//
//      Hides the Hatch Window
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
//      Function                        Location
//
//      ShowWindow                      Windows API
//
// Comments:
//
//********************************************************************


define method HideHatchWnd(this :: <CSimpSvrDoc>) => ();

  ShowWindow(this.m-hHatchWnd, $SW-HIDE);
  values()
end method HideHatchWnd;
