Module:       duim-resource-example
Synopsis:     Example using windows resources in DUIM
Author:       Roman Budzianowski, Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM version of windows-resource-example

define constant $message       = "Dylan lives!";
define constant $title         = "Duim Resource Example";

define frame <example-window> (<simple-frame>)
  slot %draw-line   = #f;
  slot %draw-circle = #f;
  pane drawing-surface (frame)
    make(<drawing-pane>,
	 resource-id: $ID-DRAW-AREA,
	 display-function: draw-graphics);
  pane draw-line-button (frame)
    make(<push-button>,
	 resource-id: $ID-DRAW-LINE,
	 activate-callback:
	   method (button)
	     let frame = sheet-frame(button);
	     frame.%draw-line   := ~frame.%draw-line;
	     repaint-sheet(frame.drawing-surface, $everywhere)
	   end);
  pane draw-square-button (frame)
    make(<push-button>,
	 resource-id: $ID-DRAW-SQUARE,
	 activate-callback:
	   method (button)
	     let frame = sheet-frame(button);
	     frame.%draw-circle := ~frame.%draw-circle;
	     repaint-sheet(frame.drawing-surface, $everywhere)
	   end);
  pane clear-button (frame)
    make(<push-button>,
	 resource-id: $ID-CLEAR,
	 activate-callback:
	   method (button)
	     let frame = sheet-frame(button);
	     frame.%draw-line   := #f;
	     frame.%draw-circle := #f;
	     repaint-sheet(frame.drawing-surface, $everywhere)
	   end);
  layout (frame)
    make(<fixed-layout>,
         children: vector(frame.drawing-surface,
			  frame.draw-line-button,
			  frame.draw-square-button,
			  frame.clear-button));
  command-table (frame) *example-command-table*;
  /* menu-bar (frame) 
       make(<menu-bar>, resource-id: $IDD-MAIN-WINDOW); */
  keyword resource-id: = $IDD-MAIN-WINDOW;
end frame <example-window>;

define method draw-graphics 
    (pane :: <drawing-pane>, medium :: <medium>, region :: <region>) => ()
  clear-box*(medium, sheet-region(pane));
  draw-text(medium, $message, 10, 20);
  when (sheet-frame(pane).%draw-line)
    draw-line(medium, 10, 40, 20, 70)
  end;
  when (sheet-frame(pane).%draw-circle)
    draw-circle(medium, 50, 50, 15, filled?: #f)
  end
end method draw-graphics;


define function nyi (frame :: <frame>) => ()
  notify-user("Not yet implemented!", frame: frame)
end function nyi;

define method about-example (frame :: <example-window>) => ()
  with-frame-manager (frame-manager(frame))
    let text-layout
      = make(<column-layout>,
	     x-alignment: #"center",
	     children: vector(make(<label>,
				   label: "Functional Objects, Inc."),
			      make(<label>,
				   label: "Dylan example program"),
			      make(<label>,
				   label: "Version 0.0")));
    make(<dialog-frame>,
	 layout: text-layout,
	 title: "About Example",
	 owner: frame,
	 cancel-function: #f,
	 mapped?: #t);
  end
end method about-example;

define command-table *example-command-table* (*global-command-table*)
  menu-item "New"         = nyi,           resource-id: $IDM-NEW;
  menu-item "Open"        = nyi,           resource-id: $IDM-OPEN;
  menu-item "Save"        = nyi,           resource-id: $IDM-SAVE;
  menu-item "Save As"     = nyi,           resource-id: $IDM-SAVEAS;
  menu-item "Print"       = nyi,           resource-id: $IDM-PRINT;
  menu-item "Print Setup" = nyi,           resource-id: $IDM-PRINTSETUP;
  menu-item "Exit"        = nyi,           resource-id: $IDM-EXIT;
  menu-item "Undo"        = nyi,           resource-id: $IDM-UNDO;
  menu-item "About"       = about-example, resource-id: $IDM-ABOUT;
end command-table *example-command-table*;


//--- For debugging
define function describe-resources () => ()
  describe-database();
  let main-win = lookup-resource($RT-DIALOG, $IDD-MAIN-WINDOW);
  describe-resource(main-win);
  let button = lookup-control(main-win, $ID-DRAW-LINE);
  describe-resource(button);
end function describe-resources;

define method main
    (application :: <string>, arguments :: <sequence>)
 => (status-code :: false-or(<integer>))
  notify-user("Starting DUIM resource example!");
/*
  duim-debug-message("Starting %s with arguments %=", application, arguments);
  let port = default-port();
  describe-resources();
*/
  let frame = make(<example-window>, title: $title);
  let status-code = start-frame(frame);
  duim-debug-message("Exiting %s with status code %d", application, status-code);
  status-code
end method main;

main(application-name(), application-arguments());
