Module:    sample-OLE-server
Synopsis:  "Hatch window" (border around in-place activation)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Note: this code was patterned after Microsoft examples in
//  "\MSTOOLS\samples\ole\olestd\HATCH.C" (dated July 1995) and
//  "\INOLE\CODE\CLASSLIB\CHATCH.CPP" (dated May 1995)


// class name of hatch window
define constant $hatch-class-name :: <LPTSTR> = as(<LPTSTR>,"DWHatchW");

define function RegisterHatchWindowClass (instance-handle :: <HINSTANCE>)
 => ( ok? :: <boolean> )
  // Register Hatch Window Class
  with-stack-structure( wc :: <PWNDCLASS> )
      wc.style-value := $CS-BYTEALIGNWINDOW;
      wc.lpfnWndProc-value := HatchWndProc;
      wc.cbClsExtra-value := 0;
      wc.cbWndExtra-value := 0;
      wc.hInstance-value := instance-handle;
      wc.hIcon-value := null-handle(<HICON>);
      wc.hCursor-value := LoadCursor($NULL-HINSTANCE, $IDC-ARROW);
      wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1);
      wc.lpszMenuName-value := $NULL-string;
      wc.lpszClassName-value := $hatch-class-name;
       ~ zero?(RegisterClass(wc))  // register the window class
  end with-stack-structure;
end RegisterHatchWindowClass;

define function CreateHatchWindow (parent-window :: <HWND>,
				   instance-handle :: <HINSTANCE>)
  => (hatch-window :: <HWND>)
  CreateWindowEx($WS-EX-NOPARENTNOTIFY,
		 $hatch-class-name,
		 TEXT("Hatch"),
		 %logior($WS-CHILDWINDOW,$WS-CLIPCHILDREN,$WS-CLIPSIBLINGS),
		 0, 0, 0, 0,
		 parent-window,
		 $NULL-HMENU,
		 instance-handle,
		 $NULL-VOID)
end CreateHatchWindow;

define constant $DEFAULT-HATCHBORDER-WIDTH = 4;

define variable *hatch-border-width* :: <integer> = $DEFAULT-HATCHBORDER-WIDTH;

define function SetHatchWindowSize
    (hatch-window :: <HWND>,	// hatch window handle
     obj-rect  :: <LPRECT>,	// full size of in-place server object window
     clip-rect :: <LPRECT>)	// clipping rect imposed by in-place container
 => ( x :: <integer>, y :: <integer> ); // position of document window

  with-stack-structure ( visible-rect :: <LPRECT> )
   with-stack-structure ( hatch-rect :: <LPRECT> )
    CopyRect(hatch-rect, obj-rect);
    let border-width :: <integer> = *hatch-border-width* + 1;

    //Calculate the rectangle for the hatch window, then clip it.
    InflateRect(hatch-rect, border-width, border-width);
    if ( clip-rect == obj-rect )
      // hack for when called from IOleObject/SetExtent
      clip-rect := hatch-rect;
    end if;
    IntersectRect(visible-rect, hatch-rect, clip-rect);

    OutputDebugString(
      format-to-string("  hatch window rect = %d, %d; %d, %d\r\n",
	      visible-rect.left-value, visible-rect.top-value,
	      visible-rect.right-value, visible-rect.bottom-value));

    let flags = logior($SWP-NOZORDER, $SWP-NOACTIVATE);
    SetWindowPos(hatch-window, $NULL-HWND, 
		 visible-rect.left-value,
		 visible-rect.top-value,
		 visible-rect.right-value - visible-rect.left-value,
		 visible-rect.bottom-value - visible-rect.top-value,
		 flags);

    // Set the rectangle of the child window to be at border-width
    // from the top and left but with the same size as obj-rect
    // contains.  The hatch window will clip it.
    let doc-x = hatch-rect.left-value - visible-rect.left-value + border-width;
    let doc-y = hatch-rect.top-value - visible-rect.top-value + border-width;
    values(doc-x, doc-y)
   end with-stack-structure
  end with-stack-structure
end SetHatchWindowSize;

// Drawing of resize handles is currently suppressed; there is no code here
// yet to make them functional, so why show them when they don't do anything?
define constant draw-handles? = #f;

// callback function for window messages
define function hatch-window-function(hatch-window :: <HWND>,
				      Message :: <integer>,
				      wParam, lParam)
	=> value :: <integer>;

  select ( Message ) 

    $WM-CREATE => 
    /*
      // "Inside OLE" shows doing this, but GetProfileInt is an obsolete
      // Win16 function and there is no documentation telling anyone about
      // setting this property.
      *hatch-border-width* :=
	GetProfileInt(TEXT("windows"),
		      TEXT("oleinplaceborderwidth"),
		      $DEFAULT-HATCHBORDER-WIDTH);
    */
      0;

    $WM-PAINT => 
      begin
	let ps :: <LPPAINTSTRUCT> = make(<LPPAINTSTRUCT>);
	let hatch-rect :: <LPRECT> = make(<LPRECT>);
	let border-width :: <integer> = *hatch-border-width*;
	let hDC :: <HDC> = BeginPaint(hatch-window, ps);
	GetClientRect(hatch-window, hatch-rect);
	draw-shading(hatch-rect, hDC, border-width);
	InflateRect(hatch-rect, - border-width, - border-width);
	if ( draw-handles? )
	  draw-hatch-handles(hatch-rect, hDC, border-width + 1);
	end if;
	FrameRect(hDC, hatch-rect, GetStockObject($BLACK-BRUSH));
	EndPaint(hatch-window, ps);
	destroy(ps);
	destroy(hatch-rect);
      end;
      0;

    // NOTE: Any window that is used during in-place activation
    //    must handle the WM_SETCURSOR message or else the cursor
    //    of the in-place parent will be used. if WM_SETCURSOR is
    //    not handled, then DefWindowProc sends the message to the
    //    window's parent.
    // 
    $WM-SETCURSOR => 
      SetCursor(LoadCursor($NULL-HINSTANCE, $IDC-ARROW));
      1; // true

    otherwise => 
      DefWindowProc(hatch-window, Message, wParam, lParam);
  end select
end hatch-window-function;

define callback HatchWndProc :: <WNDPROC> = hatch-window-function;



define function draw-hatch-handles
    (inside-rect :: <LPRECT>, // inner edge of border
     hdc :: <HDC>,
     box-size :: <integer>) => ();

  with-stack-structure( rc :: <LPRECT> )
    let bkmodeOld = SetBkMode(hdc, $TRANSPARENT);

    CopyRect(rc, inside-rect);
    InflateRect(rc, box-size - 1, box-size - 1);
    let left   :: <integer> = rc.left-value;
    let right  :: <integer> = rc.right-value;
    let top    :: <integer> = rc.top-value;
    let bottom :: <integer> = rc.bottom-value;

    // Draw the handles inside the rectangle boundary
    draw-handle(hdc, left, top, box-size);
    draw-handle(hdc, left, top + truncate/(bottom - top - box-size, 2),
		box-size);
    draw-handle(hdc, left, bottom - box-size, box-size);
    draw-handle(hdc, left + truncate/(right - left - box-size, 2),
		top, box-size);
    draw-handle(hdc, left + truncate/(right - left - box-size, 2),
		bottom - box-size, box-size);
    draw-handle(hdc, right - box-size, top, box-size);
    draw-handle(hdc, right - box-size,
		top + truncate/(bottom - top - box-size, 2), box-size);
    draw-handle(hdc, right - box-size, bottom - box-size, box-size);
  
    SetBkMode(hdc, bkmodeOld);
  end with-stack-structure;
  values()
end draw-hatch-handles;


// Draw a handle box at the specified coordinates.
define function draw-handle(hdc :: <HDC>, x :: <integer>, y :: <integer>,
			    box-size :: <integer>)
 => ();

  let hbr = GetStockObject($BLACK-BRUSH);
  let hpen = GetStockObject($BLACK-PEN);
  let hpenOld = SelectObject(hdc, hpen);
  let hbrOld  = SelectObject(hdc, hbr);
  Rectangle(hdc, x, y, x + box-size, y + box-size);
  SelectObject(hdc, hpenOld);
  SelectObject(hdc, hbrOld);
  values()
end draw-handle;


define constant $bitmap-data =
  #[#x11, #x22, #x44, #x88, #x11, #x22, #x44, #x88];

define variable *hatch-bits* :: <PWORD> = null-pointer(<PWORD>);

define constant $DPa = #x00A000C9; // undocumented magic?

// draw the "hatch" border around an in-place activation
define function draw-shading (outer-rect :: <LPRECT>, // outside edges
			      hdc :: <HDC>,
			      border-width :: <integer>)
	=> ();
 
  if ( null-pointer?(*hatch-bits*) )
    *hatch-bits* := make(<PWORD>, element-count: size($bitmap-data));
    for ( bits in $bitmap-data, i from 0 )
      pointer-value(*hatch-bits*, index: i) := bits;
    end for;
  end if;

  let hbm :: <HBITMAP> = CreateBitmap(8, 8, 1, 1, *hatch-bits*);
  let hbr :: <HBRUSH> = CreatePatternBrush(hbm);
  let hbrOld = SelectObject(hdc, hbr);

  let rc = outer-rect;

  let cvText = SetTextColor(hdc, RGB(255, 255, 255));
  let cvBk   = SetBkColor(hdc, RGB(0, 0, 0));

  PatBlt(hdc, rc.left-value, rc.top-value, rc.right-value - rc.left-value,
	 border-width, $DPa);
  PatBlt(hdc, rc.left-value, rc.top-value, border-width,
	 rc.bottom-value - rc.top-value, $DPa);
  PatBlt(hdc, rc.right-value - border-width, rc.top-value, border-width,
	 rc.bottom-value - rc.top-value, $DPa);
  PatBlt(hdc, rc.left-value, rc.bottom-value - border-width,
	 rc.right-value - rc.left-value, border-width, $DPa);

  SetTextColor(hdc, cvText);
  SetBkColor(hdc, cvBk);
  SelectObject(hdc, hbrOld);
  DeleteObject(hbr);
  DeleteObject(hbm);
  values()
end draw-shading;
