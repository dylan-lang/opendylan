Module:    windows-resource-example
Synopsis:  Example usage of the win32-resource-database library
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $szAppName = as(<LPTSTR>, "DylanWinExamp2");

define variable *drawing-area-window* = $NULL-HWND;

define constant $null-hPen = null-handle(<HPEN>);
define constant $null-hBrush = null-handle(<HBRUSH>);

define variable *hDashPen*  = $null-hPen; /* "---" pen handle */
define variable *hGreenBrush*  = $null-hBrush; /* green brush */

define variable *app-instance* :: <HINSTANCE> = application-instance-handle();
define variable *cmd-show* :: <signed-int> = application-show-window();

define variable *main-window* = $NULL-HWND;

define method Main-program() => exit-status :: <signed-int>;

  initialize-application(*app-instance*, *cmd-show*, $szAppName);
  load-default-resources();
  describe-database();

  let main-win = lookup-resource($RT-DIALOG, $IDD-MAIN-WINDOW);
  describe-resource(main-win);
  let button = lookup-control(main-win, $ID-DRAW-LINE);
  describe-resource(button);

  let (x,y) = window-position(main-win);
  debug-out("Main win pos %d,%d\n", x,y);
  let (x1,y1) = window-position(button);
  debug-out("Button win pos %d,%d\n", x1,y1);

  if ( Init-Application(*app-instance*)  // Initialize shared things
	// Perform initializations that apply to a specific instance 
	& Init-Instance(*app-instance*, *cmd-show*) )
    // initialized OK

    let hAccelTable :: <HACCEL> =
      LoadAccelerators(*app-instance*, MAKEINTRESOURCE($IDR-GENERIC));

    message-loop(hAccelTable);
  else
    ErrorHandler("Couldn't initialize application");
  end if
end method Main-program;


define method Init-Application(hInstance :: <HINSTANCE>) => ok :: <boolean>;
  #t
end method Init-Application;

define method Init-Instance(
        hInstance :: <HINSTANCE>, nCmdShow :: <signed-int>) => ok :: <boolean>;

  let cmd-table = make(<command-table>);
  add-command(cmd-table, $IDM-ABOUT, about-command);
  add-command(cmd-table, $IDM-EXIT, exit-command);
  add-command(cmd-table, $IDM-HELPCONTENTS, help-command);
  add-command(cmd-table, $IDM-HELPSEARCH, search-help-command);
  add-command(cmd-table, $IDM-HELPHELP, help-help-command);
  add-command(cmd-table, $ID-DRAW-LINE, draw-line-command);
  add-command(cmd-table, $ID-DRAW-SQUARE, draw-square-command);
  add-command(cmd-table, $ID-CLEAR, clear-command);

  register-command-table($IDD-MAIN-WINDOW, cmd-table);

  let msg-table = make(<command-table>);
  add-command(msg-table, $WM-CREATE, handle-create);
  add-command(msg-table, $WM-PAINT, handle-paint);
  add-command(msg-table, $WM-DESTROY, handle-destroy);
  register-message-table($ID-DRAW-AREA, msg-table);

  // Create a main window for this application instance.
  let win = create-window($IDD-MAIN-WINDOW);
  
  if(null-handle?(win))
    print-last-error();
    #f
  else
    show-window(win);
    #t
  end;
end method Init-Instance;

define constant $message :: <LPTSTR> = as(<LPTSTR>, "Dylan lives!");

define variable *display-list* = make(<vector>, size: 10);
define variable *display-pointer* = 0;

// this handler is for the drawing area
define function handle-create(hWnd :: <HWND>,
			      commandId :: <unsigned-int>,
			      uParam :: <unsigned-int>,  
			      lParam :: <integer>) => ();
  // $WM-CREATE => 

  *drawing-area-window* := hWnd;
  /* Create the brush objects */ 

  *hGreenBrush* := CreateSolidBrush(RGB(0, 255, 0));

  /* Create the "---" pen */ 

  *hDashPen* := CreatePen($PS-DASH,           /* style */ 
			  1,		      /* width */ 
			  RGB(0, 0, 255));      /* color */ 
end;

define function draw-line(hDC :: <HDC>) => ();

  // Select a "---" pen, save the old value
  let hOldPen = SelectObject(hDC, *hDashPen*);

  // Move to a specified point
  MoveToEx(hDC, 100, 100, $NULL-POINT);

  // Draw a line
  LineTo(hDC, 250, 100);

  // Restore the old pen  

  SelectObject(hDC, hOldPen);

end;

// those commands are run on the main window
define function draw-line-command(window :: <HWND>,
				  commandId :: <unsigned-int>,
				  uParam :: <unsigned-int>,  
				  lParam :: <integer>) => ();
  if(*display-pointer* < 10)
    InvalidateRgn(*drawing-area-window*, null-handle(<HRGN>), #f);
    *display-list*[*display-pointer*] := draw-line;
    *display-pointer* := *display-pointer* + 1;
  end;
end;

define function draw-square(hDC :: <HDC>) => ();

  // Draw a green rectangle
  debug-out("drawing square: saving old brush\n");
  let hOldBrush = SelectObject(hDC, *hGreenBrush*);
  debug-out("drawing square: drawing\n");
  Rectangle(hDC, 20, 20, 70, 70);

  // Restore the old brush  
  debug-out("drawing square: restoring old brush\n");
  SelectObject(hDC, hOldBrush);
  debug-out("drawing square: done\n");
end;

define function draw-square-command(window :: <HWND>,
				    commandId :: <unsigned-int>,
				    uParam :: <unsigned-int>,  
				    lParam :: <integer>) => ();
  if(*display-pointer* < 10)
    InvalidateRgn(*drawing-area-window*, null-handle(<HRGN>), #f);
    *display-list*[*display-pointer*] := draw-square;
    *display-pointer* := *display-pointer* + 1;
  end;
end;

define function clear-command(window :: <HWND>,
			      commandId :: <unsigned-int>,
			      uParam :: <unsigned-int>,  
			      lParam :: <integer>) => ();
  let hWnd = *drawing-area-window*;
  *display-pointer* := 0;
  InvalidateRgn(hWnd, null-handle(<HRGN>), #t);
end;

define function handle-paint(hWnd :: <HWND>,
			     commandId :: <unsigned-int>,
			     uParam :: <unsigned-int>,  
			     lParam :: <integer>) => ();
  // $WM-PAINT => 
  begin 
    let ps :: <PPAINTSTRUCT> = make(<PPAINTSTRUCT>);
    
    // Set up a display context to begin painting 
    let hDC :: <HDC> = BeginPaint(hWnd, ps);

    for(i from 0 below *display-pointer*)
      *display-list*[i](hDC);
    end;

    // Tell Windows you are done painting  

    EndPaint(hWnd, ps);
    %free(ps);
  end;
end;

define function handle-destroy(hWnd :: <HWND>,
			       commandId :: <unsigned-int>,
			       uParam :: <unsigned-int>,  
			       lParam :: <integer>) => ();
  // $WM-DESTROY =>   // message: window being destroyed
  DeleteObject(*hGreenBrush*);
  DeleteObject(*hDashPen*);

  *hGreenBrush* := $null-brush;
  *hDashPen* := $null-pen;

end;

define function about-command(hWnd :: <HWND>,
			      commandId :: <unsigned-int>,
			      uParam :: <unsigned-int>,  
			      lParam :: <integer>) => ();
  //	  $IDM-ABOUT => 
  let dlg = create-modal-window($IDD-ABOUTBOX, parent: hWnd);
end;
define function exit-command(hWnd :: <HWND>,
			     commandId :: <unsigned-int>,
			     uParam :: <unsigned-int>,  
			     lParam :: <integer>) => ();
  //	  $IDM-EXIT => 
  DestroyWindow(hWnd);
end;

define function help-command(hWnd :: <HWND>,
			     commandId :: <unsigned-int>,
			     uParam :: <unsigned-int>,  
			     lParam :: <integer>) => ();
  //	  $IDM-HELPCONTENTS => 
  if ( ~ WinHelp(hWnd, TEXT("EXAMPLE.HLP"), $HELP-KEY,
		 pointer-address(TEXT("CONTENTS"))) )
    MessageBox(GetFocus(),
	       TEXT("Unable to activate help"),
	       $szAppName,
	       logior($MB-SYSTEMMODAL, $MB-OK, $MB-ICONHAND));
  end if;
end;

define function search-help-command(hWnd :: <HWND>,
				    commandId :: <unsigned-int>,
				    uParam :: <unsigned-int>,  
				    lParam :: <integer>) => ();
  //	  $IDM-HELPSEARCH => 
  if ( ~ WinHelp(hWnd, TEXT("EXAMPLE.HLP"), $HELP-PARTIALKEY,
		 pointer-address(TEXT(""))) )
    MessageBox(GetFocus(),
	       TEXT("Unable to activate help"),
	       $szAppName,
	       logior($MB-SYSTEMMODAL, $MB-OK, $MB-ICONHAND));
  end if;
end;

define function help-help-command(hWnd :: <HWND>,
				  commandId :: <unsigned-int>,
				  uParam :: <unsigned-int>,  
				  lParam :: <integer>) => ();	    
  //	  $IDM-HELPHELP => 
  if ( ~ WinHelp(hWnd, $NULL-string, $HELP-HELPONHELP, 0) )
    MessageBox(GetFocus(),
	       TEXT("Unable to activate help"),
	       $szAppName,
	       logior($MB-SYSTEMMODAL, $MB-OK, $MB-ICONHAND));
  end if;
end;

// Here are all the other possible menu options,
// all of these are currently disabled:
/*
$IDM-NEW, $IDM-OPEN, $IDM-SAVE, $IDM-SAVEAS, $IDM-UNDO, $IDM-CUT,
$IDM-COPY, $IDM-PASTE, $IDM-LINK, $IDM-LINKS => 
*/

Main-program(); // run the main program
