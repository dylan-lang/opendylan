Module:    sample-OLE-server
Synopsis:  Main program and window message handling.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Process messages for the frame window

define method main-wnd-proc (hWnd :: <HWND>,
                             message :: <ffi-integer>,
			     wParam :: <ffi-integer>,
                             lParam :: <ffi-integer>)
			=> (value :: <integer>);
  let app = window-app(hWnd);

  select (app & message)

    $WM-CLOSE => 		// window being closed
      Output-Debug-String("$WM-CLOSE\r\n");
      if (app.app-in-process?)
	// In process "close" just means terminate the out-of-place activation,
	// i.e. hide the external frame.
	when (app.app-ole-object)
	  OLE-part-hide(app.app-ole-object)
	end;
      else
	// Local server "close" means really shut down the process.
	// If the client didn't already disconnect from us, disconnect from the
	// client. Typically this case will occur in the "open" editing mode.
	when (app.app-ole-object)
	  OLE-util-close-server(app.app-ole-object);
	  when (app.app-ole-object)
	    // This happens when running standalone: we're holding on to
	    // a reference to the object ourselves, so release it.
	    Release(app.app-ole-object);
	  end;
	end;
      end;
      unless (app.app-ole-object)
	terminate-frame(app);
      end;
      0;	// end of $WM-CLOSE

    $WM-COMMAND =>		// command from application menu
      Output-Debug-String("$WM-COMMAND\r\n");
      command-handler(app, hWnd, wParam, lParam);

    $WM-DESTROY =>		// window being destroyed
      Output-Debug-String("$WM-DESTROY\r\n");
      PostQuitMessage(0);
      0;

    $WM-SIZE => 		// window size changed
      resize-doc-window(app);
      0;

    otherwise =>		// passes it on if unprocessed
      DefWindowProc(hWnd, message, wParam, lParam);
  end select

end main-wnd-proc;

define callback MainWndProc :: <WNDPROC> = main-wnd-proc;


// Process document window messages

define method doc-wnd-proc (hWnd :: <HWND>,
			    message :: <ffi-integer>,
			    wParam :: <ffi-integer>,
			    lParam :: <ffi-integer>)
		=> value :: <integer>;

  let app = window-app(hWnd);
  select (app & message)
    $WM-COMMAND =>            // command from application menu
      command-handler(app, hWnd, wParam, lParam);
    $WM-PAINT => 
      with-stack-structure (ps :: <PPAINTSTRUCT>)
	let hDC :: <HDC> = BeginPaint(hWnd, ps);
	paint-doc(app, hDC);
	EndPaint(hWnd, ps);
	0
      end with-stack-structure;
    $WM-MENUSELECT => 
      set-status-text(app);
      0;
    otherwise =>	// Passes it on if unprocessed
      DefWindowProc(hWnd, message, wParam, lParam);
  end select;
end doc-wnd-proc;

define callback DocWndProc :: <WNDPROC> = doc-wnd-proc;

// Process dialog box messages

define method about-proc (hDlg :: <HWND>,
			  message :: <ffi-integer>,
			  wParam :: <ffi-integer>,
			  lParam :: <ffi-integer>)
 => processed? :: <boolean>;

  select (message)
    $WM-INITDIALOG => #t;		 // initialize dialog box
    $WM-COMMAND =>			 // received a command
      if (LOWORD(wParam) = $IDOK		// "OK" box selected?
	    | LOWORD(wParam) = $IDCANCEL)	// system menu close command?
	EndDialog(hDlg, 1);			// Exits the dialog box
	#t
      else #f
      end if;
    otherwise => #f;	 // Didn't process a message
  end select
end about-proc;

define callback About :: <DLGPROC> = about-proc;

