Module:    sample-OLE-server
Synopsis:  server object class and methods
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define COM-interface <simple-server> (<ole-server-framework>)
  slot server-app :: <simple-server-app>;
  constant slot started-by-ole? :: <boolean> = #t,
    init-keyword: started-by-ole?:;
  slot part-color :: <integer> = RGB(128, 128, 0);  // current color
  slot server-storage-stream :: <LPSTREAM> = $null-istream;
  slot OLE-part-dirty? :: <boolean> = #f;
end <simple-server>;

define method initialize (obj :: <simple-server>,
			  #key server-module-handle = application-instance-handle(),
			  #all-keys)
  next-method();
  obj.server-app
    := make(<simple-server-app>,
	    module-handle: server-module-handle,
	    ole-object: obj);
end;

// This is called when container disconnects from the object.
define method terminate (obj :: <simple-server>) => ();
  next-method();
  obj.server-app.app-ole-object := #f;
  server-disconnected(obj.server-app);
end;

define method part-rotate-color (obj :: <simple-server>)
  let color = obj.part-color;
  part-set-color(obj,
		 red: GetRValue(color) + 10,
		 green: GetGValue(color) + 10,
		 blue: GetBValue(color) + 10);
end part-rotate-color;

define method part-set-color (obj :: <simple-server>,
			      #key red :: <integer> = 0,
			           green :: <integer> = 0,
			           blue :: <integer> = 0)
  obj.part-color := RGB(max(0, red), max(0, green), max(0, blue));
  part-data-changed(obj);
end part-set-color;

define method part-data-changed (obj :: <simple-server>)
  obj.OLE-part-dirty? := #t;
  doc-changed(obj.server-app);
  when (obj.started-by-ole?)
    OLE-util-view-changed(obj); // force update of container's copy
  end;
end part-data-changed;

define constant $message = "Dylan";

// Draw the object into an arbitrary DC
define method OLE-part-draw-metafile (obj :: <simple-server>, hDC :: <HDC>)
 => (status :: <HRESULT>);

  with-stack-structure (rect :: <LPRECTL>)
    let (w :: <integer>, h :: <integer>) = OLE-util-current-size(obj);
    rect.top-value := 0;
    rect.left-value := 0;
    rect.right-value := w;
    rect.bottom-value := h;
    OLE-part-draw(obj, hDC, rect);
  end;
end OLE-part-draw-metafile;


define method OLE-part-draw (obj :: <simple-server>,
			     hDC :: <HDC>,
			     rect :: <LPRECTL>)
 => status :: <HRESULT>;

  let x-org :: <integer> = rect.left-value;
  let y-org :: <integer> = rect.top-value;
  let x-end :: <integer> = rect.right-value;
  let y-end :: <integer> = rect.bottom-value;

  // create the brush
  let hBrush = CreateSolidBrush(obj.part-color);
  let hPen = CreatePen($PS-INSIDEFRAME, 6, RGB(0, 0, 0));

  // select the brush
  let hOldBrush = SelectObject(hDC, hBrush);
  let hOldPen = SelectObject(hDC, hPen);

  // draw the rectangle
  Rectangle(hDC, x-org, y-org, x-end, y-end);

  // restore the pen
  hPen := SelectObject(hDC, hOldPen);

  // free the pen
  DeleteObject(hPen);

  // restore the old brush
  hBrush := SelectObject(hDC, hOldBrush);

  // free the brush
  DeleteObject(hBrush);

  // write a label in the rectangle
  let old-color = SetTextColor(hDC, RGB(#xC0,#xC0,#xC0));
  let old-mode = SetBkMode(hDC, $TRANSPARENT);
  TextOut(hDC, x-org + 16, y-org + truncate/(y-end - y-org, 2) - 8,
	  $message, size($message));
  SetTextColor(hDC, old-color);
  SetBkMode(hDC, old-mode);

  // All done
  $S-OK;
end OLE-part-draw;


define method OLE-part-requested-size (obj :: <simple-server>)
 => (width :: <integer>, height :: <integer>);
  values(100, 100)
end OLE-part-requested-size;


// Return the document window:
define method OLE-part-doc-window (obj :: <simple-server>)
 => (doc-window :: <HWND>);
  app-doc-window(obj.server-app);
end OLE-part-doc-window;

// Open as separate top-level window (instead of in-place).
define method OLE-part-open-out (obj :: <simple-server>)
 => (window :: <HWND>);
  let app = obj.server-app;
  show-app-frame(app, $SW-SHOWNORMAL);
  app.app-frame;
end OLE-part-open-out;

// Install our menu in the container's menu bar:
define method OLE-part-insert-menus (obj :: <simple-server>,
				     shared-menu :: <HMENU>,
				     edit-position :: <integer>,
				     object-position :: <integer>,
				     help-position :: <integer>)
 => (nedit :: <integer>, nobject :: <integer>, nhelp :: <integer>);

  InsertMenu(shared-menu, edit-position,
	     logior($MF-BYPOSITION,$MF-POPUP), 
	     pointer-address(obj.server-app.app-color-menu),
	     TEXT("&Color"));
  values(1, 0, 0)
end OLE-part-insert-menus;


// Persistence

define constant $color-stream-name = OLESTR("color");

define method OLE-part-Create-Streams (obj :: <simple-server>,
				       storage :: <LPSTORAGE>)
 => ();
  OLE-part-Release-Streams(obj);
  obj.server-storage-stream
    := OLE-util-Create-Stream(storage, $color-stream-name);
end OLE-part-Create-Streams;


define method OLE-part-Open-Streams (obj :: <simple-server>,
				     storage :: <LPSTORAGE>)
 => ();
  OLE-part-Release-Streams(obj);
  obj.server-storage-stream
    := OLE-util-open-stream(storage, $color-stream-name);
end OLE-part-Open-Streams;


define method OLE-part-Save-To-Storage (obj :: <simple-server>,
					storage :: <Interface>,
					same-as-load? :: <boolean>) => ();
  Output-Debug-String("OLE-part-Save-To-Storage\r\n");
  let stream :: <LPSTREAM>
    = if (~same-as-load?)
	OLE-util-Create-Stream(storage, $color-stream-name);
      else
	AddRef(obj.server-storage-stream);
	obj.server-storage-stream
      end if;
  obj.OLE-part-dirty? := #f;
  // discard old stream contents and prepare to write from the beginning
  istream-rewrite(stream);
  write-to-stream(obj, stream);
  Release(stream);
end OLE-part-Save-To-Storage;

define method OLE-part-Load-From-Storage (obj :: <simple-server>,
					  width :: <integer>,
					  height :: <integer>) => ();

  Output-Debug-String("OLE-part-Load-From-Storage\r\n");

  // Read the color
  read-from-stream(obj, obj.server-storage-stream);
end OLE-part-Load-From-Storage;

define method OLE-part-Release-Streams (obj :: <simple-server>) => ();
  let stream = obj.server-storage-stream;
  obj.server-storage-stream := $null-istream;
  Release(stream);
end OLE-part-Release-Streams;

define method write-to-stream (obj :: <simple-server>, istm :: <LPSTREAM>)
  istream-write-integer(istm, obj.part-color);
end write-to-stream;

define method read-from-stream (obj :: <simple-server>, istm :: <LPSTREAM>)
  obj.part-color := istream-read-integer(istm);
end read-from-stream;



