Module:    sample-OLE-container
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *lpCSimpleApp* :: false-or(<CSimpleApp>) = #f;

//**********************************************************************
//
// WinMain
//
// Purpose:
//
//      Program entry point
//
// Parameters:
//
//      HANDLE hInstance        - Instance handle for this instance
//
//      HANDLE hPrevInstance    - Instance handle for the last instance
//
//      LPSTR lpCmdLine         - Pointer to the command line
//
//      int nCmdShow            - Window State
//
// Return Value:
//
//      msg.wParam
//
// Function Calls:
//      Function                        Location
//
//      CSimpleApp::CSimpleApp          APP.CPP
//      CSimpleApp::fInitApplication    APP.CPP
//      CSimpleApp::fInitInstance       APP.CPP
//      CSimpleApp::HandleAccelerators  APP.CPP
//      CSimpleApp::~CSimpleApp         APP.CPP
//      GetMessage                      Windows API
//      TranslateMessage                Windows API
//      DispatchMessage                 Windows API
//
// Comments:
//
//********************************************************************

define method WinMain(hInstance :: <HANDLE>, hPrevInstance :: <HANDLE>,
		      lpCmdLine :: <LPSTR>, nCmdShow :: <integer>)
 => value :: <integer>;

 block(return)
	
   // Not needed for Win32:
   //  // recommended size for OLE apps
   //  SetMessageQueue(96);

   *lpCSimpleApp* := make(<CSimpleApp>);

   // we will add one ref count on our App. later when we want to destroy
   // the App object we will release this  ref count. when the App's ref
   // count goes to 0, it will be deleted.
   AddRef(*lpCSimpleApp*);

   // app initialization
   if ( null-handle?(hPrevInstance) )
     if ( ~ fInitApplication(*lpCSimpleApp*, hInstance) )
       return(GetLastError());
     end if;
   end if;

   // instance initialization
   if ( ~ fInitInstance(*lpCSimpleApp*, hInstance, nCmdShow) )
     return(GetLastError());
   end if;

   let msg :: <LPMSG> = make(<LPMSG>);

   // message loop
   while( GetMessage(msg, $NULL-HWND, 0, 0) )
     unless ( HandleAccelerators(*lpCSimpleApp*, msg) )
       TranslateMessage(msg);    /* Translates virtual key codes */ 
       DispatchMessage(msg);     /* Dispatches message to window */ 
     end unless;
   end while;

   // Release the ref count added on the App above. this will make
   // the App's ref count go to 0, and the App object will be deleted.
   Release(*lpCSimpleApp*);

   return(msg.wParam-value);   /* Returns the value from PostQuitMessage */ 
 end block
end method WinMain;


//**********************************************************************
//
// MainWndProc
//
// Purpose:
//
//      Processes messages for the frame window
//
// Parameters:
//
//      HWND hWnd       - Window handle for frame window
//
//      UINT message    - Message value
//
//      WPARAM wParam   - Message info
//
//      LPARAM lParam   - Message info
//
// Return Value:
//
//      long
//
// Function Calls:
//      Function                        Location
//
//      CSimpleApp::lCommandHandler     APP.CPP
//      CSimpleApp::DestroyDocs         APP.CPP
//      CSimpleApp::lCreateDoc          APP.CPP
//      CSimpleApp::lSizeHandler        APP.CPP
//      CSimpleDoc::lAddVerbs           DOC.CPP
//      PostQuitMessage                 Windows API
//      DefWindowProc                   Windows API
//
// Comments:
//
//********************************************************************

define method main-wnd-proc(hWnd :: <HWND>, message :: <integer>,
			    wParam, lParam)
	=> value :: <integer>;

  block(return)

    select ( message )
	        
      $WM-COMMAND =>            // message: command from application menu
	return(lCommandHandler(*lpCSimpleApp*, hWnd, message, wParam, lParam));

      $WM-CREATE => 
	return(lCreateDoc(*lpCSimpleApp*, hWnd, message, wParam, lParam));

      $WM-DESTROY =>                   // message: window being destroyed
	DestroyDocs(*lpCSimpleApp*);  // need to destroy the doc...
	PostQuitMessage(0);

      $WM-INITMENUPOPUP => 
	// is this the edit menu?
	// Message packing is the same for WIN16 and 32
	if ( LOWORD(lParam) = 1 )
	  return(lAddVerbs(*lpCSimpleApp*.m-lpDoc));
	end if;

      // this code is needed for 256 color objects to work properly.
      $WM-QUERYNEWPALETTE => 
	unless ( *lpCSimpleApp*.m-fAppActive )
	  return(0);
	end unless;
	return(QueryNewPalette(*lpCSimpleApp*));

      $WM-PALETTECHANGED => 
	begin 
	  let hWndPalChg :: <HWND> =  as(<HWND>, wParam);
	  
	  if ( hWnd ~= hWndPalChg )
	    wSelectPalette(hWnd, *lpCSimpleApp*.m-hStdPal,
			   #t // fBackground
			     );
	  end if;

	  // OLE2NOTE: always forward the WM_PALETTECHANGED message (via
          //    SendMessage) to any in-place objects that currently have
          //    their window visible. this gives these objects the chance
          //    to select their palettes. this is
	  //    REQUIRED by all in-place containers independent of
	  //    whether they use color palettes themselves--their objects
	  //    may use color palettes.
	  //    (see ContainerDoc_ForwardPaletteChangedMsg for more info)
	  // 
	  let doc = *lpCSimpleApp*.m-lpDoc;
	  if( doc &
	       doc.m-lpSite &
	       ~ null?(doc.m-lpSite.m-hwndIPObj) )
	    SendMessage(doc.m-lpSite.m-hwndIPObj,
			$WM-PALETTECHANGED, wParam, lParam);
	    return(0);
	  end if;
	end;
	  
      $WM-ACTIVATEAPP =>
	let active? :: <boolean> = ~ zero?(wParam);
	*lpCSimpleApp*.m-fAppActive := active?;
	if ( active? )
	  QueryNewPalette(*lpCSimpleApp*);
	end if;
	
	let doc = *lpCSimpleApp*.m-lpDoc;
	if ( doc )
	  let activeobj = doc.m-lpActiveObject;
	  unless ( null?(activeobj) )
	    IOleInPlaceActiveObject/OnFrameWindowActivate(activeobj, active?);
	  end unless;
	end if;

      $WM-SIZE => 
	return(lSizeHandler(*lpCSimpleApp*, hWnd, message, wParam, lParam));

      otherwise =>                           // Passes it on if unproccessed
	return(DefWindowProc(hWnd, message, wParam, lParam));
	        
    end select;
    return(0);
  end block;
end method main-wnd-proc;

define callback MainWndProc :: <WNDPROC> = main-wnd-proc;

//**********************************************************************
//
// About
//
// Purpose:
//
//      Processes dialog box messages
//
// Parameters:
//
//      HWND hWnd       - Window handle for dialog box
//
//      UINT message    - Message value
//
//      WPARAM wParam   - Message info
//
//      LPARAM lParam   - Message info
//
// Return Value:
//
// Function Calls:
//      Function                    Location
//
//      EndDialog                   Windows API
//
// Comments:
//
//********************************************************************

define method about-proc(hDlg :: <HWND>, message :: <integer>,
			 wParam, lParam)
 => value :: <boolean>;

  select ( message ) 
    $WM-INITDIALOG =>			/* message: initialize dialog box */ 
      #t;

    $WM-COMMAND =>			   /* message: received a command */ 
      let button :: <integer> = LOWORD(wParam);
      if ( button = $IDOK	   /* "OK" box selected?	  */
	    |	 button = $IDCANCEL) /* System menu close command? */ 
	EndDialog(hDlg, 1);	      /* Exits the dialog box	     */ 
	#t
      end if;
    otherwise =>
      #f		     /* Didn't process a message    */ 
  end select
end method about-proc;

define callback About :: <DLGPROC> = about-proc;

//**********************************************************************
//
// DocWndProc
//
// Purpose:
//
//      Processes dialog box messages
//
// Parameters:
//
//      HWND hWnd       - Window handle for doc window
//
//      UINT message    - Message value
//
//      WPARAM wParam   - Message info
//
//      LPARAM lParam   - Message info
//
// Return Value:
//
// Function Calls:
//      Function                            Location
//
//      CSimpleApp::PaintApp                APP.CPP
//      BeginPaint                          Windows API
//      EndPaint                            Windows API
//      DefWindowProc                       Windows API
//      IOleObject::QueryInterface          Object
//      IOleInPlaceObject::UIDeactivate     Object
//      IOleObject::DoVerb                  Object
//      IOleInPlaceObject::Release          Object
//
// Comments:
//
//********************************************************************

define method Doc-Wnd-Proc(hWnd :: <HWND>,
			   message :: <integer>,
			   wParam, lParam)
 => value :: <integer>;

  block(return)
	
    select ( message ) 
      $WM-PAINT => 
	
	begin
	  let ps :: <LPPAINTSTRUCT> = make(<LPPAINTSTRUCT>);
	  let hDC :: <HDC> = BeginPaint(hWnd, ps);

	  if ( *lpCSimpleApp* )
	    PaintApp(*lpCSimpleApp*, hDC);
	  end if;

	  EndPaint(hWnd, ps);
	  destroy(ps);
	end;

      $WM-LBUTTONDBLCLK => 
	begin 
	  let pt :: <LPPOINT> = make(<LPPOINT>);
	  let ( x, y ) = LPARAM-TO-XY(lParam);
	  pt.x-value := x;
	  pt.y-value := y;
	  let site = *lpCSimpleApp*.m-lpDoc.m-lpSite;
	  if ( (~ null?(site)) & (~ null?(site.m-lpOleObject)) )
	    let rect :: <LPRECT> = make(<LPRECT>);

	    GetObjRect(site, rect);

	    if ( PtInRect(rect, pt) )
	                                
	      // Execute object's default verb
	      let pmsg :: <LPMSG> = make(<LPMSG>);
	      pmsg.hwnd-value := hWnd;
	      pmsg.message-value := message;
	      pmsg.wParam-value := wParam;
	      pmsg.lParam-value := lParam;
	      IOleObject/DoVerb(site.m-lpOleObject,
				$OLEIVERB-PRIMARY, pmsg,
				site.m-OleClientSite, 0, hWnd, rect);
	      destroy(pmsg);
	      destroy(rect);
	    end if;
	  end if;
	  destroy(pt);
	end;

      // no code is added to WM_LBUTTONDOWN for context sensitive help,
      // because this app does not do context sensitive help.
      $WM-LBUTTONDOWN => 

	let doc = *lpCSimpleApp*.m-lpDoc;
	if ( doc.m-fInPlaceActive )
	  let ( status , interface ) =
	    QueryInterface(doc.m-lpSite.m-lpOleObject, $IID-IOleInPlaceObject);
	  let lpObject :: <LPOLEINPLACEOBJECT> =
	    pointer-cast(<LPOLEINPLACEOBJECT>, interface);

	  IOleInPlaceObject/UIDeactivate(lpObject);

	  // this code is needed because we don't support inside out.
	  let rect :: <LPRECT> = make(<LPRECT>);
	  let pmsg :: <LPMSG> = make(<LPMSG>);
	  GetObjRect(doc.m-lpSite, rect);
	  pmsg.hwnd-value := hWnd;
	  pmsg.message-value := message;
	  pmsg.wParam-value := wParam;
	  pmsg.lParam-value := lParam;
	  IOleObject/DoVerb(doc.m-lpSite.m-lpOleObject,
			    $OLEIVERB-HIDE, pmsg,
			    doc.m-lpSite.m-OleClientSite,
			    0, hWnd, rect);
	  Release(lpObject);
	  destroy(pmsg);
	  destroy(rect);
	end if;

      otherwise => // Passes it on if unproccessed
	return(DefWindowProc(hWnd, message, wParam, lParam));
    end select;
    return(0);
  end block
end method Doc-Wnd-Proc;

define callback DocWndProc :: <WNDPROC> = Doc-Wnd-Proc;
