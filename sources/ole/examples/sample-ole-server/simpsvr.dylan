Module:    sample-OLE-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *CSimpSvrApp* :: false-or(<CSimpSvrApp>) = #f;
define variable *lpClassFactory* :: <Interface> = $null-interface;

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
//      CSimpSvrApp::CSimpSvrApp          APP.CPP
//      CSimpSvrApp::fInitApplication    APP.CPP
//      CSimpSvrApp::fInitInstance       APP.CPP
//      CSimpSvrApp::HandleAccelerators  APP.CPP
//      CSimpSvrApp::~CSimpSvrApp         APP.CPP
//      GetMessage                      Windows API
//      TranslateMessage                Windows API
//      DispatchMessage                 Windows API
//
// Comments:
//
//********************************************************************

define method WinMain(hInstance :: <HINSTANCE>, hPrevInstance :: <HINSTANCE>,
		      lpCmdLine :: <LPSTR>, nCmdShow :: <integer>)
		=> value :: <integer>;

  block(return)

    // Not needed for Win32:
    //  // recommended size for OLE apps
    //  SetMessageQueue(96);

    *CSimpSvrApp* := make(<CSimpSvrApp>);

    AddRef(*CSimpSvrApp*);      // need the app ref. count at 1 to hold the
	                         // app alive.

    ParseCommandLine(*CSimpSvrApp*, lpCmdLine);

    // app initialization
    if ( null?(hPrevInstance) )
      if ( ~ fInitApplication(*CSimpSvrApp*, hInstance) )
	return(GetLastError());
      end if;
    end if;

    // instance initialization
    let ( ok :: <boolean>, factory :: <Interface> ) =
      fInitInstance(*CSimpSvrApp*, hInstance, nCmdShow);
    if ( ~ok )
      return(GetLastError());
    else *lpClassFactory* := factory;
    end if;

    let msg :: <LPMSG> = make(<LPMSG>);

    // message loop
    while ( GetMessage(msg, $NULL-HWND, 0, 0) )
	        
      block(continue)
	if ( IsInPlaceActive(*CSimpSvrApp*) )
	  
	  // Only key messages need to be sent to OleTranslateAccelerator.
	  // Any other message would result in an extra FAR call to occur for
	  // that message processing...
	  
	  if ( (msg.message-value >= $WM-KEYFIRST) &
		(msg.message-value <= $WM-KEYLAST) )

	    // OleTranslateAccelerator MUST be called, even though this
	    // application does not have an accelerator table.  This has to
	    // be done in order for the mnemonics for the top level menu
	    // items to work properly.
	    let svr-obj :: <CSimpSvrObj> = GetObj(GetDoc(*CSimpSvrApp*));
	    if ( OleTranslateAccelerator(GetInPlaceFrame(svr-obj),
					 GetFrameInfo(svr-obj),
					 msg) = $NOERROR )
	      continue();
	    end if;
	  end if;
	end if;

	TranslateMessage(msg);    /* Translates virtual key codes           */ 
	DispatchMessage(msg);     /* Dispatches message to window           */ 
	
      end block;
    end while;
			   
   return(msg.wParam-value);    /* Returns the value from PostQuitMessage */ 
 end block;
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
//      CSimpSvrApp::lCommandHandler     APP.CPP
//      CSimpSvrApp::DestroyDocs         APP.CPP
//      CSimpSvrApp::lCreateDoc          APP.CPP
//      CSimpSvrApp::lSizeHandler        APP.CPP
//      CGameDoc::lAddVerbs           DOC.CPP
//      PostQuitMessage                 Windows API
//      DefWindowProc                   Windows API
//
// Comments:
//
//********************************************************************

define method main-wnd-proc (hWnd :: <HWND>, message :: <integer>,
			     wParam, lParam)
			=> value :: <integer>;
  block(return)

    select ( message )
	        
      $WM-CLOSE => 
	OutputDebugString("*** In WM_CLOSE *** \r\n");
	
	// if there is still a document
	let doc = GetDoc(*CSimpSvrApp*);
	if ( ~ null?(doc) )

	  // if there is still an object within a document
	  if ( ~ null?( GetObj(doc) ) )
	    // this case occurs if there is still
	    // an outstanding Ref count on the object
	    // when the app is trying to go away.
	    // typically this case will occur in
	    // the "open" editing mode.
	    //  Close the document
	    CloseObject(doc);
	  end if;
	end if;

	// hide the app window
	HideAppWnd(*CSimpSvrApp*);

	// if we were started by OLE, unregister the class factory, otherwise
	// remove the ref count on our dummy OLE object
	if ( IsStartedByOle(*CSimpSvrApp*) )
	  CoRevokeClassObject(GetRegisterClass(*CSimpSvrApp*)); 
	else
	  Release(GetOleObject(*CSimpSvrApp*));
	end if;

	Release(*CSimpSvrApp*);  // This should close the app.


      $WM-COMMAND =>            // message: command from application menu
	return(lCommandHandler(*CSimpSvrApp*, hWnd, message, wParam, lParam));

      $WM-CREATE => 
	return(lCreateDoc(*CSimpSvrApp*, hWnd, message, wParam, lParam));

      $WM-DESTROY =>                   // message: window being destroyed
	PostQuitMessage(0);

      $WM-SIZE => 
	return(lSizeHandler(*CSimpSvrApp*, hWnd, message, wParam, lParam));

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
		=> processed :: <boolean>;

  select ( message ) 
    $WM-INITDIALOG => #t;                /* message: initialize dialog box */ 
    $WM-COMMAND =>                       /* message: received a command */ 
      let button = LOWORD(wParam);
      if (button = $IDOK           /* "OK" box selected? */
	    | button = $IDCANCEL)  /* System menu close command? */ 
	EndDialog(hDlg, 1);        /* Exits the dialog box        */ 
	#t
      else #f
      end if;
    otherwise => #f;                   /* Didn't process a message    */
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
//      CSimpSvrApp::PaintApp                APP.CPP
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

define method Doc-Wnd-Proc(hWnd :: <HWND>, message :: <integer>,
			   wParam, lParam)
		=> value :: <integer>;

  block(return)
	
    select ( message ) 
      $WM-COMMAND =>            // message: command from application menu
	return(lCommandHandler(*CSimpSvrApp*, hWnd, message, wParam, lParam));

      $WM-PAINT => 
	// tell the app class to paint itself
	unless ( null?(*CSimpSvrApp*) )
	  let ps :: <PPAINTSTRUCT> = make(<PPAINTSTRUCT>);
	  let hDC :: <HDC> = BeginPaint(hWnd, ps);
	  PaintApp(*CSimpSvrApp*, hDC);
	  EndPaint(hWnd, ps);
	  destroy(ps);
	end unless;

      $WM-MENUSELECT => 
	SetStatusText(*CSimpSvrApp*);

      otherwise =>                     /* Passes it on if unproccessed    */ 
        return(DefWindowProc(hWnd, message, wParam, lParam));
	
    end select;
    return(0);
  end block;
end method Doc-Wnd-Proc;

define callback DocWndProc :: <WNDPROC> = Doc-Wnd-Proc;
