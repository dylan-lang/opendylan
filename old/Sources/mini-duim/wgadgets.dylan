Module:    win32-duim
Synopsis:  Win32 basic gadget implementation
Author:    Andy Armstrong, David Gray, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Win32 gadgets

define open abstract class <win32-pane-mixin>
    (<standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <win32-pane-mixin>;


/// Top level mirrors

define constant $first-gadget-id = 1000;

define sealed class <top-level-mirror> (<window-mirror>)
  slot %next-resource-id = $first-gadget-id;
  slot %resource-id-table = make(<table>);
end class <top-level-mirror>;

define method top-level-mirror
    (sheet :: <sheet>)
 => (mirror :: false-or(<top-level-mirror>))
  let sheet = top-level-sheet(sheet);
  sheet & sheet-direct-mirror(sheet)
end method top-level-mirror;


/// Top level sheets

define sealed class <win32-top-level-sheet>
    (<standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <win32-pane-mixin>,
     <top-level-sheet>)
end class <win32-top-level-sheet>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <top-level-sheet>,
     #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-top-level-sheet>, #f)
end method class-for-make-pane;

define method do-make-mirror
    (_port :: <win32-port>, sheet :: <top-level-sheet>)
 => (mirror :: <win32-mirror>)
  ignore(_port);
  let container = sheet-parent-window(sheet);
  if (container)
    next-method()
  else
    make-top-level-mirror(sheet)
  end
end method do-make-mirror;

define method make-top-level-mirror
    (sheet :: <win32-top-level-sheet>) => (mirror :: <top-level-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let frame :: <frame> = sheet-frame(sheet);
  let title = frame-title(frame) | "DUIM Window";
  let x = frame-geometry(frame)[0];
  let y = frame-geometry(frame)[1];
  //--- Call compute-default-foreground/background/text-style to
  //--- figure out what characteristics the mirror should have
  let hWnd :: <HWND>
    //---*** We want to use CreateWindowEx to control Win95 parameters
    = CreateWindow($window-class-name,		// See RegisterClass call
		   title,			// Text for window title bar
		   $WS-OVERLAPPEDWINDOW,	// Window style
		   x | $CW-USEDEFAULT,		// x position
		   y | $CW-USEDEFAULT,		// y position
		   right - left,		// width
		   bottom - top,		// height
		   $NULL-HWND,			// no parent
		   $null-hMenu,			// Use the window class menu
		   application-instance-handle(),
		   $NULL-VOID);			// No data in our WM_CREATE
  check-result("CreateWindow", hWnd);
  make(<top-level-mirror>,
       sheet:  sheet,
       handle: hWnd,
       region: make-bounding-box(left, top, right, bottom))
end method make-top-level-mirror;

define method map-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <top-level-mirror>) => ()
  let hWnd :: <HWND> = mirror.%window-handle;
  ShowWindow(hWnd, application-show-window());
  // Temporary for the VM-Tether environment: need to explicitly raise the
  // new window above the LispWorks window.  This would be redundant when
  // linked as a stand-alone application.
  raise-mirror(_port, sheet, mirror);
  UpdateWindow(hWnd)	// Sends WM_PAINT message and returns status
end method map-mirror;


/// Gadgets

define open abstract class <win32-gadget-mixin> (<gadget>, <win32-pane-mixin>)
end class <win32-gadget-mixin>;

define method generate-next-gadget-id 
    (gadget :: <gadget>) => (id :: <integer>)
  let mirror = top-level-mirror(gadget);
  let id = mirror.%next-resource-id;
  mirror.%resource-id-table[id] := gadget;
  mirror.%next-resource-id := id + 1;
  id
end method generate-next-gadget-id;

define method gadget-for-id 
    (sheet :: <sheet>, id :: <integer>)
 => (gadget :: <gadget>)
  let mirror = top-level-mirror(sheet);
  (mirror & mirror.%resource-id-table[id])
    | error("failed to find gadget with id %= for sheet %=", id, sheet)
end method gadget-for-id;

define method do-make-mirror
    (_port :: <win32-port>, gadget :: <win32-gadget-mixin>)
 => (mirror :: <window-mirror>)
  ignore(_port);
  let (left, top, right, bottom) = sheet-native-edges(gadget);
  let hwnd
    = make-gadget-control
        (gadget,
	 sheet-parent-window(gadget),
	 %logior($WS-CHILD, $WS-VISIBLE,
	         if (gadget-enabled?(gadget)) 0 else $WS-DISABLED end),
	 x: left, y: top, width: right - left, height: bottom - top);
  let mirror
    = make(<window-mirror>,
	   sheet: gadget, handle: hwnd,
	   region: make-bounding-box(left, top, right, bottom));
  initialize-gadget-mirror(gadget, mirror);
  mirror
end method do-make-mirror;

define method initialize-gadget-mirror
    (gadget :: <win32-gadget-mixin>, mirror :: <window-mirror>) => ()
  #f
end method initialize-gadget-mirror;

define method note-gadget-label-changed
    (gadget :: <win32-gadget-mixin>) => ()
  let handle = gadget.%window-handle;
  if (handle)
    unless (SetWindowText(handle, gadget-label(gadget)))
      report-error("Failed to change gadget label")
    end
  end
end method note-gadget-label-changed;

define method note-gadget-enabled
    (client, gadget :: <win32-gadget-mixin>) => ()
  ignore(client);
  let handle = gadget.%window-handle;
  handle & EnableWindow(handle, #t)
end method note-gadget-enabled;

define method note-gadget-disabled
    (client, gadget :: <win32-gadget-mixin>) => ()
  ignore(client);
  let handle = gadget.%window-handle;
  handle & EnableWindow(handle, #f)
end method note-gadget-disabled;

//--- Returns #t meaning that the port generates damage events itself
define method port-handles-repaint?
    (sheet :: <win32-gadget-mixin>) => (handles-repaint? :: <boolean>);
  #t
end method port-handles-repaint?;

//---*** What do we do about setting the color and font of a gadget?


/// Labels

define sealed class <win32-label> 
    (<win32-gadget-mixin>,
     <label>,
     <leaf-pane>)
end class <win32-label>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <label>, #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-label>, #f)
end method class-for-make-pane;

define method make-gadget-control
    (gadget :: <win32-label>, parent :: <HWND>, options :: <unsigned-int>,
     #key x, y, width, height)
 => (hwnd :: <HWND>)
  let hWnd :: <HWND>
    = CreateWindow("STATIC",
		   gadget-label(gadget),
		   %logior(options, $SS-LEFTNOWORDWRAP),
		   x, y, width, height,
		   parent,
		   $null-hMenu,
		   application-instance-handle(),
		   $NULL-VOID);
  check-result("CreateWindow (STATIC)", hWnd);
  hWnd
end method make-gadget-control;

define method do-compose-space 
    (pane :: <win32-label>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  //---*** Implement this for real...
  let (width, height) = gadget-label-size(frame-manager(pane), pane);
  make(<space-requirement>,
       width: width, height: height)
end method do-compose-space;


/// Text fields

define open abstract class <win32-text-field-mixin> (<win32-gadget-mixin>)
end class <win32-text-field-mixin>;

define method make-gadget-control
    (gadget :: <win32-text-field-mixin>, parent :: <HWND>, options :: <unsigned-int>,
     #key x, y, width, height)
 => (hwnd :: <HWND>)
  let hWnd :: <HWND>
    = CreateWindow("EDIT",
		   gadget-label(gadget),
	           %logior(options,
                           $WS-BORDER, $WS-TABSTOP, $ES-AUTOHSCROLL,
		           if (gadget-editable?(gadget)) 0 else $EM-SETREADONLY end,
                           select (gadget by instance?)
		             <password-field> => $ES-PASSWORD;
		             otherwise        => 0;
		           end),
	           x, y, width, height,
                   parent,
	           $null-hMenu,
	           application-instance-handle(),
	           $NULL-VOID);
  check-result("CreateWindow (EDIT)", hWnd);
  hWnd
end method make-gadget-control;

//---*** Implement this for real...
define method do-compose-space 
    (pane :: <win32-text-field-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 200 , height: height | 100)
end method do-compose-space;

define method handle-command
    (gadget :: <win32-text-field-mixin>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $EN-CHANGE    => handle-text-field-changing(gadget);
    $EN-KILLFOCUS => handle-text-field-changed(gadget);
    otherwise     => next-method();
  end
end method handle-command;

define method handle-text-field-changing
    (gadget :: <win32-text-field-mixin>)
 => (handled? :: <boolean>)
  //---*** Implement this!
  debug-message("Text field changing");
  #t
end method handle-text-field-changing;

define method handle-text-field-changed
    (gadget :: <win32-text-field-mixin>)
 => (handled? :: <boolean>)
  let handle = gadget.%window-handle;
  distribute-value-changed-callback(gadget, text-control-text(handle));
  #t
end method handle-text-field-changed;

//--- Hack until we get GetWindowTextLength!
define constant $text-control-max-size = 1024;

define method text-control-text
    (handle :: <HWND>) => (text :: <string>)
  //---*** Highly inefficient!
  let length = $text-control-max-size; //--- GetWindowTextLength(handle);
  let buffer = make(<C-string>, size: length + 1);
  let actual-length = GetWindowText(handle, buffer, length);
  as(<string>, buffer)
end method text-control-text;


// Text fields
define sealed class <win32-text-field>
    (<win32-text-field-mixin>,
     <leaf-pane>,
     <text-field>)
end class <win32-text-field>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <text-field>,
     #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-text-field>, #f)
end method class-for-make-pane;


// Password fields

define sealed class <win32-password-field>
    (<win32-text-field-mixin>,
     <leaf-pane>,
     <password-field>)


end class <win32-password-field>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <password-field>,
     #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-password-field>, #f)
end method class-for-make-pane;


/// Buttons

define open abstract class <win32-button-mixin>
    (<win32-gadget-mixin>)
end class <win32-button-mixin>;

define method do-compose-space
    (pane :: <win32-button-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  //---*** Implement this for real...
  let (width, height) = gadget-label-size(frame-manager(pane), pane);
  make(<space-requirement>, width: width + 10, height: height + 10)
end method do-compose-space;

// See WIG pg.388 for the description of this rule
define method button-box-spacing
    (framem :: <win32-frame-manager>, box :: <button-box>)
 => (spacing :: <integer>)
  4 * win32-dialog-base-units(framem)
end method button-box-spacing;

//---*** Need to support images properly in here
define method text-or-image-from-gadget-label
    (gadget :: <gadget>)
 => (text :: false-or(<string>), 
     image :: false-or(<image>),
     mnemonic :: false-or(<character>))
  let label = gadget-label(gadget);
  select (label by instance?)
    <string> =>
      let (text, mnemonic) = compute-mnemonic-from-label(gadget, label);
      values(text, #f, mnemonic);
    <image>  =>
      values("Images not yet supported!", #f, #f);
  end
end method text-or-image-from-gadget-label;

define method make-win32-mnemonic-label 
    (label :: <string>, char :: <character>)
 => (new-label :: <C-string>)
  let new-label = make(<C-string>, size: 1 + size(label));
  let added-marker? = #f;
  let j = 0;
  for (i from 0 below size(label))
    let label-char = label[i];
    if (~added-marker? & label-char = char)
      added-marker? := #t;
      new-label[j] := '&';
      new-label[j + 1] := label-char;
      j := j + 2
    else
      new-label[j] := label-char;
      j := j + 1
    end
  end;
  new-label
end method make-win32-mnemonic-label;

define method make-win32-mnemonic-label 
    (label :: <string>, char == #f)
 => (new-label :: <C-string>)
  as(<C-string>, label)
end method make-win32-mnemonic-label;

//---*** Still need to implement image buttons, presumably using
//---*** the Windows button style $BS-OWNERDRAW.
define method make-gadget-control
    (gadget :: <button>, parent :: <HWND>, options :: <unsigned-int>,
     #key x, y, width, height)
 => (hwnd :: <HWND>)
  let button-style
    = select (gadget-selection-mode(gadget))
	#"none" =>
	  case
	    default-button?(gadget) => $BS-DEFPUSHBUTTON;
	    otherwise               => $BS-PUSHBUTTON;
	  end;
	#"single" =>   $BS-RADIOBUTTON;
	#"multiple" => $BS-CHECKBOX;
      end;
  let (text, image, mnemonic) = text-or-image-from-gadget-label(gadget);
  let hWnd :: <HWND>
    = CreateWindow("BUTTON",
		   make-win32-mnemonic-label(text, mnemonic),
		   %logior(options, button-style, $WS-TABSTOP),
		   x, y, width, height,
		   parent,
		   $null-hMenu,
		   application-instance-handle(),
		   $NULL-VOID);
  check-result("CreateWindow (BUTTON)", hWnd);
  hWnd
end method make-gadget-control;  

define method update-button-selection (gadget :: <win32-button-mixin>) => ()
  let handle = gadget.%window-handle;
  if (handle)
    let selected? = gadget-value(gadget);
    SendMessage(handle,
		$BM-SETCHECK,
		if (selected?) 1 else 0 end,
		0)
  end
end method update-button-selection;

define method initialize-gadget-mirror
    (gadget :: <win32-button-mixin>, mirror :: <window-mirror>) => ()
  update-button-selection(gadget)
end method initialize-gadget-mirror;

define method note-gadget-value-changed 
    (gadget :: <win32-button-mixin>, old-value) => ()
  ignore(old-value);
  next-method();
  update-button-selection(gadget)
end method note-gadget-value-changed;

define method handle-command
    (gadget :: <win32-button-mixin>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $BN-CLICKED => handle-button-gadget-click(gadget);
    otherwise   => next-method();
  end
end method handle-command;


// Push buttons
define sealed class <win32-push-button>
    (<win32-button-mixin>,
     <push-button>,
     <leaf-pane>)
end class <win32-push-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <push-button>, #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-push-button>, #f)
end method class-for-make-pane;


// Radio buttons
define sealed class <win32-radio-button>
    (<win32-button-mixin>,
     <radio-button>,
     <leaf-pane>)
end class <win32-radio-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <radio-button>, #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-radio-button>, #f)
end method class-for-make-pane;


// Check buttons
define sealed class <win32-check-button>
    (<win32-button-mixin>,
     <check-button>,
     <leaf-pane>)
end class <win32-check-button>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <check-button>, #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-check-button>, #f)
end method class-for-make-pane;


/// Scroll bars

define sealed class <win32-scroll-bar>
    (<win32-gadget-mixin>,
     <leaf-pane>,
     <scroll-bar>)
end class <win32-scroll-bar>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <scroll-bar>,
     #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-scroll-bar>, #f)
end method class-for-make-pane;

define method make-gadget-control
    (gadget :: <win32-scroll-bar>, parent :: <HWND>, options :: <unsigned-int>,
     #key x, y, width, height)
 => (hwnd :: <HWND>)
  let hWnd :: <HWND>
    = CreateWindow("SCROLLBAR",
		   "",
		   %logior(options,
		           select (gadget-orientation(gadget))
			     #"horizontal" => $SBS-HORZ;
			     #"vertical"   => $SBS-VERT;
			     //---*** Needed to avoid crash in VM tether
			     otherwise     => error("Invalid gadget-orientation!");
		           end),
		   x, y, width, height,
		   parent,
		   $null-hMenu,
		   application-instance-handle(),
		   $NULL-VOID);
  check-result("CreateWindow (SCROLLBAR)", hWnd);
  hWnd
end method make-gadget-control;

define method initialize-gadget-mirror
    (gadget :: <win32-scroll-bar>, mirror :: <window-mirror>) => ()
  update-scroll-bar(gadget)
end method initialize-gadget-mirror;

//---*** Is there a way to compute this?
define constant $minimum-scroll-shaft-length = 50;

define method do-compose-space
    (gadget :: <win32-scroll-bar>, #key width, height)
 => (space-requirement :: <space-requirement>)
  select (gadget-orientation(gadget))
    #"horizontal" =>
      let arrow-width  = GetSystemMetrics($SM-CXHSCROLL);
      let min-width    = arrow-width * 2 + $minimum-scroll-shaft-length;
      let min-height   = GetSystemMetrics($SM-CYHSCROLL);
      make(<space-requirement>,
	   width:      #f,         height:     min-height,
	   min-width:  min-width,  min-height: min-height,
	   max-width:  $fill,      max-height: min-height);
    #"vertical" =>
      let min-width    = GetSystemMetrics($SM-CXVSCROLL);
      let arrow-height = GetSystemMetrics($SM-CYVSCROLL);
      let min-height   = arrow-height * 2 + $minimum-scroll-shaft-length;
      make(<space-requirement>,
	   width:      min-width,  height:     #f,
	   min-width:  min-width,  min-height: min-height,
	   max-width:  min-width,  max-height: $fill);
  end
end method do-compose-space;

define method scroll-bar-adjusted-contents
    (gadget :: <win32-scroll-bar>)
 => (position :: <integer>, slug-size :: <integer>,
     min :: <integer>, max :: <integer>)
  let value = gadget-value(gadget);
  let scroll-range = gadget-value-range(gadget);
  let first-value = scroll-range[0];
  let range-increment = scroll-range[1] - first-value;
  let pos = floor/(value - first-value, range-increment);
  let slug-size = floor/(gadget-slug-size(gadget), range-increment);
  let min = 0;
  let max = size(scroll-range);
  values(pos, slug-size, min, max)
end method scroll-bar-adjusted-contents;

define method update-scroll-bar (gadget :: <win32-scroll-bar>) => ()
  let handle :: false-or(<hWnd>) = gadget.%window-handle;
  if (handle)
    let (pos, size, min, max) = scroll-bar-adjusted-contents(gadget);
    //---*** Once SetScrollInfo exists we should use this...
    with-stack-structure (scroll-info :: <LPCSCROLLINFO>)
      scroll-info.cbSize-value := size-of(<SCROLLINFO>);
      scroll-info.fMask-value := $SIF-ALL;
      scroll-info.nMin-value  := min;
      scroll-info.nMax-value  := max;
      scroll-info.nPage-value := size; 
      scroll-info.nPos-value  := pos;
      SetScrollInfo(handle, $SB-CTL, scroll-info, #t);
    end;
    /*
    unless (SetScrollRange(handle, $SB-CTL, min, max, 0))
      report-error("SetScrollRange")
    end;
    unless (SetScrollPos(handle, $SB-CTL, pos, 1))
      report-error("SetScrollPos")
    end
    */
  end
end method update-scroll-bar;

define method gadget-slug-size-setter
    (size :: <real>, gadget :: <win32-scroll-bar>)
 => (size :: <real>)
  next-method();
  update-scroll-bar(gadget);
  size
end method gadget-slug-size-setter;
    
define method note-gadget-value-changed
    (gadget :: <win32-scroll-bar>, old-value) => ()
  ignore(old-value);
  update-scroll-bar(gadget);
  //---*** Update viewports...
end method note-gadget-value-changed;

define method distribute-scrolling-event
    (gadget :: <win32-scroll-bar>, function :: <function>, #rest args) => ()
  distribute-function-event
    (gadget,
     method ()
       apply(function, gadget, args)
     end)
end method distribute-scrolling-event;

define method handle-scrolling
    (gadget :: <win32-scroll-bar>,
     scroll-code :: <integer>, position :: <integer>)
 => (handled? :: <boolean>)
  block (return)
    //--- Slightly disgusting hack: Windows uses different names for
    //--- horizontal and vertical scrolling, but luckily the left/top
    //--- and right/bottom names have the same values, so this code 
    //--- works for both. Coercing the values seems too inefficient.
    select (scroll-code)
      $SB-BOTTOM =>
	distribute-scrolling-event(gadget, scroll-to-position, #"bottom");
      $SB-ENDSCROLL =>
	//--- We don't seem to need this one... should we worry about that?
	return(#f);
      $SB-LINELEFT =>
	distribute-scrolling-event(gadget, scroll-up-line);
      $SB-LINERIGHT =>
	distribute-scrolling-event(gadget, scroll-down-line);
      $SB-PAGELEFT =>
	distribute-scrolling-event(gadget, scroll-up-page);
      $SB-PAGERIGHT =>
	distribute-scrolling-event(gadget, scroll-down-page);
      $SB-THUMBPOSITION =>
	let value-range = gadget-value-range(gadget);
	distribute-scrolling-event
	  (gadget, scroll-to-position, value-range[position]);
      $SB-THUMBTRACK =>
	let value-range = gadget-value-range(gadget);
        distribute-value-changing-callback(gadget, value-range[position]);
      $SB-TOP =>
	distribute-scrolling-event(gadget, scroll-to-position, #"top");
      otherwise =>
	return(#f)
    end;
    #t
  end
end method handle-scrolling;


/// List boxes

define sealed class <win32-list-box> 
    (<win32-gadget-mixin>,
     <leaf-pane>,
     <list-box>)
end class <win32-list-box>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <list-box>, #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-list-box>, #f)
end method class-for-make-pane;

define method make-gadget-control
    (gadget :: <win32-list-box>, parent :: <HWND>, options :: <unsigned-int>,
     #key x, y, width, height)
 => (hwnd :: <HWND>)
  let hWnd :: <HWND>
    = CreateWindow("LISTBOX",
		   "",
		   %logior(options, $WS-TABSTOP,
			   $LBS-DISABLENOSCROLL, $LBS-NOTIFY,
		           case 
			     gadget-scrolling-vertically?(gadget) => $WS-VSCROLL;
			     otherwise => 0;
		           end,
		           select (gadget-selection-mode(gadget))
			     #"single"   => 0;
			     #"multiple" => $LBS-EXTENDEDSEL;
		           end),
		   x, y, width, height,
		   parent,
		   $null-hMenu,
		   application-instance-handle(),
		   $NULL-VOID);
  check-result("CreateWindow (LISTBOX)", hWnd);
  hWnd
end method make-gadget-control;

//---*** Implement this for real...
define method do-compose-space 
    (pane :: <win32-list-box>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 200 , height: height | 100,
       max-width: $fill, min-width: $fill)
end method do-compose-space;

define method initialize-gadget-mirror
    (gadget :: <win32-list-box>, mirror :: <window-mirror>) => ()
  //---*** Fill in the items
end method initialize-mirror;


/// Option boxes

define sealed class <win32-option-box> 
    (<win32-gadget-mixin>,
     <option-box>,
     <leaf-pane>)
end class <win32-option-box>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <option-box>,
     #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-option-box>, #f)
end method class-for-make-pane;

define method make-gadget-control
    (gadget :: <win32-option-box>, parent :: <HWND>, options :: <unsigned-int>,
     #key x, y, width, height)
 => (hwnd :: <HWND>)
  let hWnd :: <HWND>
    = CreateWindow("COMBOBOX",
		   "",
		   %logior(options, $WS-TABSTOP,
		           $CBS-AUTOHSCROLL, $CBS-HASSTRINGS,
		           case 
			     gadget-editable?(gadget) => $CBS-DROPDOWN;
			     otherwise                => $CBS-DROPDOWNLIST;
		           end),
		   x, y, width, height,
		   parent,
		   $null-hMenu,
		   application-instance-handle(),
		   $NULL-VOID);
  check-result("CreateWindow (COMBOBOX)", hWnd);
  hWnd
end method make-gadget-control;

//---*** Implement this for real...
define method do-compose-space 
    (pane :: <win32-option-box>, #key width, height)
 => (space-req :: <space-requirement>)
  make(<space-requirement>,
       width: width | 200 , height: height | 100,
       max-width: $fill, min-width: $fill)
end method do-compose-space;

//--- Should this be moved into DUIM?  Seems useful...
define method collection-gadget-item-label
    (sheet :: <collection-gadget-mixin>, item) => (label :: <string>)
  let string = gadget-name-key(sheet)(item);
  //---*** Shouldn't be needed with full return type checking...
  check-type(string, <string>);
  string
end method collection-gadget-item-label;

define method note-gadget-items-changed
    (gadget :: <win32-option-box>, old-items :: <sequence>) => ()
  next-method();
  let handle = gadget.%window-handle;
  if (handle)
    SendMessage(handle, $CB-RESETCONTENT, 0, 0);
    for (item in gadget-items(gadget))
      let label = collection-gadget-item-label(gadget, item);
      SendMessage(handle, $CB-ADDSTRING, 0, as(<LPARAM>, label))
    end
  end
end method note-gadget-items-changed;

define method note-gadget-value-changed
    (gadget :: <win32-option-box>, old-value) => ()
  next-method();
  let handle = gadget.%window-handle;
  if (handle)
    let selection = gadget-selection(gadget);
    SendMessage(handle, 
		$CB-SETCURSEL, 
		if (empty?(selection)) -1 else 0 end,
		0)
  end
end method note-gadget-value-changed;

define method initialize-gadget-mirror
    (gadget :: <win32-option-box>, mirror :: <window-mirror>) => ()
  next-method();
  note-gadget-items-changed(gadget, #[])
end method initialize-gadget-mirror;


/// Viewports
//---*** This implementation needs work
define sealed class <win32-viewport>
    (<viewport>,
     <win32-pane-mixin>,
     <permanent-medium-mixin>,
     <mirrored-sheet-mixin>,
     <single-child-composite-pane>)
end class <win32-viewport>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <viewport>, #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-viewport>, #f)
end method class-for-make-pane;


/// Other gadgets

//--- Missing
//---  <slider>
//---  <progress-control>
//---  <separator>
//---  <text-editor>
//---  <spin-box>
//---  <tool-bar>
//---  <status-bar>
//---  <splitters>
//---  <tab-control>
//---  <list-control>
//---  <tree-control>
//---  <table-control>
