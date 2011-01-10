Module:    sample-automation-server
Synopsis:  Object definition and implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $initial-offset = 10;

define dispatch-interface <painter> (<simple-dispatch>)
  uuid "{ef462961-bb53-11cf-89f8-02070119f639}";
  documentation "sample-automation-server";
  property current-color :: <integer> = RGB(#x7F, #x7F, #x7F),
    property-setter: color-setter, name: "color", type: <C-long>;
  property drawing-offset :: <integer> = $initial-offset,
    name: "offset", type: <C-short>;
  property figure-size :: <integer> = 40,
    name: "size", type: <C-short>;
  function clear, name: "clear";
  function draw-circle, name: "circle";
  // "circle" is a reserved word in BASIC, so add a synonym.
  function draw-circle, name: "cirkle";
  function draw-square, name: "square";
  // Private data
  slot window-handle :: <HWND> = application-init-window(),
     init-keyword: window-handle:;
  slot current-brush :: <HBRUSH>
       = pointer-cast(<HBRUSH>, GetStockObject($GRAY-BRUSH));
end;

define method clear (this :: <painter>) => status :: <HRESULT>;
  InvalidateRect(this.window-handle, $NULL-RECT, #t);
  this.drawing-offset := $initial-offset;
  $S-OK
end;

define method color-setter (RGB-color :: <integer>, this :: <painter>) => ();
  unless (RGB-color = this.current-color)
    DeleteObject(this.current-brush);
    this.current-brush := CreateSolidBrush(RGB-color);
    this.current-color := RGB-color;
  end unless;
end;

define method scode-for-last-error() => status :: <HRESULT>;
  hresult-from-win32(GetLastError())
end;

define method draw-figure (this :: <painter>, function :: <function>)
 => status :: <HRESULT>;
  let hwnd = this.window-handle;
  let hdc :: <HDC> = GetDC(hwnd);
  if (null-pointer?(hdc))
    scode-for-last-error()
  else
    let old-brush = SelectObject(hdc, this.current-brush);
    let offset = this.drawing-offset;
    let size = this.figure-size;
    let ok? = function(hdc, offset, offset, offset + size, offset + size);
    let status = if (ok?) $S-OK else scode-for-last-error() end;
    SelectObject(hdc, old-brush);
    ReleaseDC(hwnd, hdc);  
    this.drawing-offset := offset + truncate/(size, 2);
    status
  end if
end;

define method draw-circle (this :: <painter>) => status :: <HRESULT>;
  draw-figure(this, Ellipse)
end;

define method draw-square (this :: <painter>) => status :: <HRESULT>;
  draw-figure(this, Rectangle)
end;

