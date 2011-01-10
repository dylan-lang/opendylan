Module:    example-support
Synopsis:  event handling framework
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *creating-window-resource-id* = 0;

define constant $DLGWINDOWEXTRA = 30;

define method register-duim-frame-class(hInstance :: <HINSTANCE>) => ok :: <boolean>;
  let cursor = $null-handle;
  let icon = $null-handle;
  with-stack-structure (wc :: <PWNDCLASS>)

    // Fill in window class structure with parameters that describe the
    // main window.
    wc.style-value := logior($CS-HREDRAW, $CS-VREDRAW); // Class style(s).
    wc.lpfnWndProc-value := FrameProc;       // Window Procedure
    wc.cbClsExtra-value := 0;                      // No per-class extra data.
    wc.cbWndExtra-value := $DLGWINDOWEXTRA;
    wc.hInstance-value := hInstance;              // Owner of this class
    icon := LoadIcon($null-hInstance, $IDI-APPLICATION);
    if(null-handle?(icon))
      print-last-error();
    end;
    wc.hIcon-value := icon;
    cursor := LoadCursor($null-hInstance, $IDC-ARROW);
    wc.hCursor-value := cursor;
    wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1); // Default color
    wc.lpszMenuName-value := $null-string; //MAKEINTRESOURCE(0);
    wc.lpszClassName-value := "duim_frame";             // Name to register as

    // Register the window class and return success/failure code.
    let class-handle = RegisterClass(wc);
    debug-out("Registered duim_frame class = %d\n", class-handle);
    if(zero?(class-handle))
      print-last-error();
      #f
    else
      #t
    end
  end
end;

define method register-duim-gadget-class(hInstance :: <HINSTANCE>) => ok :: <boolean>;
  with-stack-structure (wc :: <PWNDCLASS>)

  // Fill in window class structure with parameters that describe the
  // work area window

  wc.style-value := logior($CS-HREDRAW, $CS-VREDRAW); // Class style(s).
  wc.lpfnWndProc-value := GadgetProc;       // Window Procedure
  wc.cbClsExtra-value := 0;                      // No per-class extra data.
  wc.cbWndExtra-value := 0;
  wc.hInstance-value := hInstance;              // Owner of this class
  wc.hIcon-value := LoadIcon($null-hInstance, $IDI-APPLICATION);
  wc.hCursor-value := LoadCursor($null-hInstance, $IDC-ARROW);
  wc.hbrBackground-value := as(<HBRUSH>, $COLOR-APPWORKSPACE); // Default color
  wc.lpszMenuName-value := $null-string;
  wc.lpszClassName-value := "drawing_area";             // Name to register as

  // Register the window class and return success/failure code.
  /* return */
  let class-handle = RegisterClass(wc);
//  debug-out("Registered drawing_area class = %d\n", class-handle);
  if(zero?(class-handle))
    print-last-error();
    #f
  else
    #t
  end
 end
end;

define callback GadgetProc :: <WNDPROC> = gadget-window-function;

define function
    gadget-window-function(
			   hWnd :: <HWND>,         // window handle
			   message :: <unsigned-int>,      // type of message
			   uParam :: <unsigned-int>,     // additional information
			   lParam :: <integer>)   // additional information
  => value :: <integer>;
  block(return)
    select(message)
      $WM-NCCREATE =>
	let win-struct = make(<LPCREATESTRUCT>, address: lParam);
	let my-id = as(<integer>, win-struct.hMenu-value.pointer-address);

	debug-out("About to create gadget window : %s hMenu: %=\n", 
		  as(<byte-string>, win-struct.lpszName-value),
		  my-id);
	// register message table if there is one
	// assume no command table for drawing_area
	let msg-table = element(*message-dispatching-tables*, my-id, default: #f);
	if(msg-table)
	  register-message-table-internal(hWnd, msg-table);
	else
	  debug-out("No message table found for win %=\n", hWnd);
	end;
	return(1); // TRUE

      // we may handle some messages in a generic way
      otherwise =>
	unless(dispatch-message(hWnd, message, uParam, lParam))
//	  debug-out("Calling default message handler\n");
	  return(DefWindowProc(hWnd, message, uParam, lParam));
	end;
    end select;
    return(0);
  end block;
end;

define callback FrameProc :: <WNDPROC> = duim-frame-function;

define function duim-frame-function(
                hWnd :: <HWND>,         // window handle
                message :: <unsigned-int>,      // type of message
                uParam :: <unsigned-int>,     // additional information
                lParam :: <integer>)   // additional information
	=> value :: <integer>;
  block(return)
	
    select ( message ) 

      $WM-COMMAND =>   // message: command from application menu

	let wmId :: <signed-int> = LOWORD(uParam);
	/* let wmEvent :: <signed-int> = HIWORD(uParam); */

	select ( wmId ) 
	  // we may want to handle some commands by default
	  otherwise => 
	    let status = dispatch-command(hWnd, wmId, uParam, lParam);
	    unless(status)
	      //debug-out("Calling default command handler\n");
	      return(DefWindowProc(hWnd, message, uParam, lParam));
	    end;
                        
	end select;

      $WM-NCCREATE =>
	let win-struct = make(<LPCREATESTRUCT>, address: lParam);
	let my-id = *creating-window-resource-id*;
	
	// This would be the place to initialize duim gadget structures
	// using resource-database, lookup-control(...)
	debug-out("About to create frame window : %s id %=\n", 
		  as(<byte-string>, win-struct.lpszName-value), my-id);
	if(my-id = 0) // ??? $null-handle
	  ErrorHandler("We are having problems, WM-NCCRETAE: id = 0");
	else
	  let cmd-table = element(*command-dispatching-tables*, my-id, default: #f);
	  if(cmd-table)
	    register-command-table-internal(hWnd, cmd-table);
	  else 
	    debug-out("\tNo command table found\n");
	  end;
	end;
	
	return(1); // true;

      $WM-DESTROY =>   // message: window being destroyed

	PostQuitMessage(0);

      otherwise =>           // Passes it on if unproccessed
	return(DefWindowProc(hWnd, message, uParam, lParam));
      
    end select;
    return(0);
  end block;
end method duim-frame-function;

define method Null(hDlg :: <HWND>, message :: <unsigned-int>,
		  wParam :: <integer>, lParam :: <integer>)
 => value :: <boolean>;
  #f
end;

define callback Null-Proc :: <DLGPROC> = Null;

define method dialog-function(hDlg :: <HWND>, message :: <unsigned-int>,
			      wParam :: <integer>, lParam :: <integer>)
 => value :: <boolean>;

  select ( message ) 
    $WM-INITDIALOG =>  	// initialize dialog box
      let my-id = *creating-window-resource-id*;
      
      // This would be the place to initialize duim gadget structures
      // using resource-database, lookup-control(...)
      debug-out("About to create dialog : %=\n", my-id);
		
      if(my-id = 0)
	ErrorHandler("We are having problems, WM-INITDIALOG: id = 0");
      else
	let cmd-table = element(*command-dispatching-tables*, my-id, default: #f);
	if(cmd-table)
	  register-command-table-internal(hDlg, cmd-table);
	end;
      end;
      #t;
    $WM-COMMAND => 		// input received
	let wmId :: <signed-int> = LOWORD(wParam);
	/* let wmEvent :: <signed-int> = HIWORD(wParam); */

      select ( wmId ) 
	otherwise => 
	  let status = dispatch-command(hDlg, wmId, wParam, lParam);
	  if(wmId = $IDOK | wmId = $IDCANCEL)
	    EndDialog(hDlg, 1);
	    #t;
	  elseif(status)
	    #t
	  else
	    #f
	  end;
      end select;
    otherwise => #f;
  end select
end method About;

define callback Dialog-Proc :: <DLGPROC> = dialog-function;

define variable *app-instance* :: <HINSTANCE> = null-handle(<HINSTANCE>);
define variable *cmd-show* :: <signed-int> = 0;
define variable $szAppName = as(<LPTSTR>, "Duim App");

define function initialize-application(hInstance :: <HINSTANCE>,
				       cmd-show :: <signed-int>,
				       name :: <LPTSTR>) 
 => (success? :: <boolean>);
  $szAppName := name;
  *app-instance* := hInstance;
  *cmd-show* := cmd-show;
  register-duim-frame-class(hInstance) & register-duim-gadget-class(hInstance)
end;

define function create-modal-window(id :: <resource-id>, 
				    #key 
				      parent :: <HWND> = $NULL-HWND,
				    show :: <boolean> = #f)
 => (window :: <HWND>);

//  let win-resource = lookup-resource($RT-DIALOG, id);
//  let win-resource = encode-resource(id);
  *creating-window-resource-id* := id;
  let hWnd :: <HWND> = DialogBox(*app-instance*,
				 encode-resource(id),
				 parent,
				 Dialog-Proc);
  hWnd
end;


define function create-window(id :: <resource-id>, 
			      #key 
//			        command-table :: <command-table> = unsupplied(),
				parent :: <HWND> = $NULL-HWND,
			        show :: <boolean> = #f)
 => (window :: <HWND>);
//  let win-resource = lookup-resource($RT-DIALOG, id);
  *creating-window-resource-id* := id;
  // use CreateDialogIndirect since we have the dialog loaded to memory ?
  let hWnd :: <HWND> = CreateDialog(*app-instance*,
				    encode-resource(id),
				    parent,
				    Null-Proc);
				    
  // If window could not be created, return "failure"
  if ( null-handle?(hWnd) ) 
    debug-out("CreateDialog failed for id: %=\n", decode-resource(id));
  else
    // Make the window visible; update its client area; and return "success"
    if(show)
      ShowWindow(hWnd, *cmd-show*); // Show the window
      UpdateWindow(hWnd);         // Sends WM_PAINT message and returns status
    end;
  end if;
  *creating-window-resource-id* := 0;
  hWnd
end;

define function show-window(hWnd :: <HWND>) => (status :: <object>); // status type?
  ShowWindow(hWnd, *cmd-show*); // Show the window
  UpdateWindow(hWnd);         // Sends WM_PAINT message and returns status
end;

define function message-loop(hAccelTable :: <HACCEL>) => (value :: <unsigned-int>);

  // Acquire and dispatch messages until a WM_QUIT message is received.

  let pMsg :: <PMSG> = make(<PMSG>);
  while( GetMessage(pMsg, // message structure
		    $NULL-HWND,  // handle of window receiving the message
		    0,      // lowest message to examine
		    0))     // highest message to examine
    
    if ( TranslateAccelerator(pMsg.hwnd-value, hAccelTable, pMsg) = 0 )
      TranslateMessage(pMsg); // Translates virtual key codes
      DispatchMessage(pMsg);  // Dispatches message to window
    end if;
  end while;
  pMsg.wParam-value
end;
