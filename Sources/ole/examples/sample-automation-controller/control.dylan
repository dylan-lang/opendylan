Module:    sample-automation-controller
Synopsis:  This file has the application-specific code.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $IDB-CLEAR	= 101;
define constant $IDB-COLOR	= 102;
define constant $IDB-CIRCLE	= 103;
define constant $IDB-SQUARE	= 104;
define constant $IDB-QUIT	= 105;

define method make-button (window :: <HWND>,
                           y :: <integer>,
                           label :: <string>,
                           ID :: <integer>)
 => handle :: <HWND>;

  CreateWindow("BUTTON",	 // predefined class 
	       label,	   // button text 
	       %logior($WS-VISIBLE, $WS-CHILD, $BS-PUSHBUTTON), // styles 
	       10,	   // starting x position 
	       y,	   // starting y position 
	       100,	   // button width 
	       20,	   // button height 
	       window,     // parent window 
	       as(<HMENU>, ID), // identifier is coded in place of menu
	       application-instance-handle(), 
	       $NULL-VOID) // pointer not needed
end method make-button;

// Names appearing in the controller's user interface:
define constant *button-labels* = #["Clear",
				    "Set Color ...",
				    "Draw Circle",
				    "Draw Square",
				    "Quit"];

// Names of the automation dispatch interface methods and properties:
define constant *disp-names* = #["clear", "color", "circle", "square"];

// Dispatch identifiers corresponding to the names in *disp-names*:
define variable *dispids* = make(<vector>, size: size(*disp-names*), fill: #f);

define method make-buttons (hwnd :: <HWND>) => ();
  for (y from 10 by 25,
       label in *button-labels*,
       ID from $IDB-CLEAR by 1)
    make-button(hwnd, y, label, ID);
  end for;
end method make-buttons;

define method disp-id-for-index (server :: <LPDISPATCH>, index :: <integer>)
 => dispid :: <integer>;
  *dispids*[index] |
    (*dispids*[index] := get-id-of-name(server, *disp-names*[index]))
end;

define method process-button (hwnd :: <HWND>,
                              server :: <LPDISPATCH>,
                              button-ID :: <integer>)
 => ();
  let index :: <integer> = button-ID - $IDB-CLEAR;
  select (button-ID)

    $IDB-CLEAR,
    $IDB-CIRCLE,
    $IDB-SQUARE =>
      call-simple-method(server, disp-id-for-index(server, index));

    $IDB-COLOR => // pop up dialog box for showing and changing the color.
      let dispid = disp-id-for-index(server, index);
      let current-color = get-property(server, dispid);
      let new-color = choose-color(hwnd, current-color);
      if (new-color)
        set-property(server, dispid, new-color);
      end;

    $IDB-QUIT =>
      PostMessage(hwnd, $WM-SYSCOMMAND, $SC-CLOSE, 0);

  end select;
end method process-button;

define variable *custom-colors* = make(<LPCOLORREF>, element-count: 16);

define function choose-color (hwnd, default) => color :: false-or(<integer>);
  with-stack-structure (lpcc :: <LPCHOOSECOLOR>)
    lpcc.lStructSize-value := size-of(<CHOOSECOLOR>);
    lpcc.hwndOwner-value := hwnd;
    lpcc.rgbResult-value := default;
    lpcc.Flags-value := $CC-RGBINIT;
    lpcc.lpCustColors-value := *custom-colors*;
    if (ChooseColor(lpcc)) // set new color
      lpcc.rgbResult-value;
    end if;
  end with-stack-structure;
end;
