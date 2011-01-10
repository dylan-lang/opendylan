Module:    win32-duim
Synopsis:  Win32 mirror implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 mirrors

//--- <HMENU> is a subclass of <HANDLE>, but not <HWND>.  Is that right?
define constant <HWND/HMENU> = type-union(<HWND>, <HMENU>);

define open abstract class <win32-mirror> (<mirror>)
  sealed slot mirror-sheet :: <sheet>,
    required-init-keyword: sheet:;
end class <win32-mirror>;

define sealed method window-handle
    (mirror :: <win32-mirror>) => (handle :: false-or(<HWND/HMENU>))
  #f
end method window-handle;

define constant $mirror-hwnd-table  :: <object-table> = make(<table>);
define constant $mirror-hmenu-table :: <object-table> = make(<table>);

define sealed method window-mirror
    (handle :: <HMENU>) => (mirror :: false-or(<win32-mirror>))
  element($mirror-hmenu-table, pointer-address(handle), default: #f)
end method window-mirror;

define sealed method window-mirror
    (handle :: <HWND>) => (mirror :: false-or(<win32-mirror>))
  element($mirror-hwnd-table, pointer-address(handle), default: #f)
end method window-mirror;

define sealed method window-mirror-setter
    (mirror :: <win32-mirror>, handle :: <HMENU>)
 => (mirror :: <win32-mirror>)
  element($mirror-hmenu-table, pointer-address(handle)) := mirror
end method window-mirror-setter;

define sealed method window-mirror-setter
    (mirror :: <win32-mirror>, handle :: <HWND>)
 => (mirror :: <win32-mirror>)
  element($mirror-hwnd-table, pointer-address(handle)) := mirror
end method window-mirror-setter;

define sealed method window-mirror-setter
    (mirror == #f, handle :: <HMENU>) => (mirror == #f)
  remove-key!($mirror-hmenu-table, pointer-address(handle));
  #f
end method window-mirror-setter;

define sealed method window-mirror-setter
    (mirror == #f, handle :: <HWND>) => (mirror == #f)
  remove-key!($mirror-hwnd-table, pointer-address(handle));
  #f
end method window-mirror-setter;

define method initialize (mirror :: <win32-mirror>, #key) => ()
  next-method();
  sheet-direct-mirror(mirror-sheet(mirror)) := mirror;
  let handle = window-handle(mirror);
  when (handle)
    window-mirror(handle) := mirror
  end
end method initialize;

define sealed method window-handle 
    (sheet :: <mirrored-sheet-mixin>)
 => (handle :: false-or(<HWND/HMENU>))
  let mirror = sheet-direct-mirror(sheet);
  mirror & window-handle(mirror)
end method window-handle;

define sealed method handle-sheet
    (handle :: <HWND/HMENU>)
 => (sheet :: false-or(<mirrored-sheet-mixin>))
  let mirror = window-mirror(handle);
  mirror & mirror-sheet(mirror)
end method handle-sheet;


/// Empty methods on non-window mirrors

define method mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <win32-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(0, 0, 100, 100)        //--- kludge city
end method mirror-edges;

// The real methods are on more specific classes, such as <window-mirror>
define method set-mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <win32-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  #f
end method set-mirror-edges;

// Ditto...
define method map-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <win32-mirror>) => ()
  #f
end method map-mirror;

// Ditto...
define method unmap-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <win32-mirror>) => ()
  #f
end method unmap-mirror;

// Ditto...
define method destroy-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <win32-mirror>) => ()
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;


/// Window class registration

// Names of "classes" (in Microsoft Windows terminology) for windows:
define constant $window-class-name = "DUIM-top-level-window";
define constant $canvas-class-name = "DUIM-drawing-pane";
define constant $simple-class-name = "DUIM-simple-pane";
define constant $dialog-class-name = "DUIM-dialog";

// Unique IDs of the above classes as returned by RegisterClass:
define variable $window-class-atom :: <integer> = 0;
define variable $canvas-class-atom :: <integer> = 0;
define variable $simple-class-atom :: <integer> = 0;
define variable $dialog-class-atom :: <integer> = 0;


define function register-window-classes (_port :: <win32-port>) => ()
  when ($window-class-atom = 0)
    $window-class-atom
      := register-window-class(_port, $window-class-name,
                               own-dc?: #t,
                               resource?: #t)
  end;
  when ($canvas-class-atom = 0)
    $canvas-class-atom
      := register-window-class(_port, $canvas-class-name,
                               //---*** Should be 'own-dc?: _port.%os-name == #"Windows-NT"'
                               own-dc?: #t,
                               use-3d?: #f)
  end;
  when ($simple-class-atom = 0)
    $simple-class-atom
      := register-window-class(_port, $simple-class-name,
                               own-dc?: #f,
                               use-3d?: #f)
  end;
  when ($dialog-class-atom = 0)
    $dialog-class-atom
      := register-window-class(_port, $dialog-class-name,
                               resource?: #t)
  end;
end function register-window-classes;

define function register-window-class
    (_port :: <win32-port>, name :: <string>,
     #key resource? = #f, own-dc? = #f, use-3d? = _port.%use-3d?)
 => (class-id :: <integer>)
  with-stack-structure (wc :: <PWNDCLASS>)
    // Fill in window class structure with parameters that describe the
    // main window.
    wc.lpszClassName-value := name;             // name of window class
    wc.style-value := %logior($CS-HREDRAW, $CS-VREDRAW, $CS-DBLCLKS,
                              if (own-dc?) $CS-OWNDC else 0 end);
    wc.lpfnWndProc-value := WndProc;            // Window Procedure
    wc.cbClsExtra-value := 0;                   // No per-class extra data.
    wc.cbWndExtra-value := if (resource?) $DLGWINDOWEXTRA else 0 end;
    wc.hInstance-value := application-instance-handle(); // owner
    wc.hIcon-value := LoadIcon($null-hInstance, $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor($null-hInstance, $IDC-ARROW);
    wc.hbrBackground-value
      := as(<HBRUSH>,
            1 + if (use-3d?) $COLOR-3DFACE else $COLOR-WINDOW end);
    wc.lpszMenuName-value := $null-string;      // no menu yet
    let class-id = RegisterClass(wc);
    when (zero?(class-id))                      // register the window class
      report-error("RegisterClass")
    end;
    class-id
  end
end function register-window-class;


define function unregister-window-classes () => ()
  when ($window-class-atom ~= 0)
    $window-class-atom
      := unregister-window-class($window-class-name)
  end;
  when ($canvas-class-atom ~= 0)
    $canvas-class-atom
      := unregister-window-class($canvas-class-name)
  end;
  when ($simple-class-atom ~= 0)
    $simple-class-atom
      := unregister-window-class($simple-class-name)
  end;
  when ($dialog-class-atom ~= 0)
    $dialog-class-atom
      := unregister-window-class($dialog-class-name)
  end;
end function unregister-window-classes;

define function unregister-window-class
    (name :: <string>) => (class-id :: singleton(0))
  check-result("UnregisterClass",
               UnregisterClass(name, application-instance-handle()));
  0
end function unregister-window-class;


define sealed method dialog-window?
    (hWnd :: <HWND>) => (dialog-window? :: <boolean>)
  let window-class = GetClassLong(hWnd, $GCW-ATOM);
  when (zero?(window-class))
    report-error("GetClassLong")
  end;
  window-class = $dialog-class-atom
end method dialog-window?;


/// Mirror creation and destruction

define sealed class <window-mirror> (<win32-mirror>)
  sealed slot window-handle :: <HWND>,
     required-init-keyword: handle:;
  sealed slot %region :: <bounding-box>,
    required-init-keyword: region:;
  sealed slot %DC :: <HDC> = $null-hDC;
  sealed slot %background-brush = #f;
  // Store the actual resource for the mirror if any
  sealed slot %mirror-resource :: false-or(<window-resource>) = #f, 
    init-keyword: resource:;
end class <window-mirror>;

define sealed domain make (singleton(<window-mirror>));
define sealed domain initialize (<window-mirror>);

// This is intentionally _not_ sealed
define method do-make-mirror
    (_port :: <win32-port>, sheet :: <sheet>)
 => (mirror :: <win32-mirror>)
  ignore(_port);
  let parent = sheet-device-parent(sheet);
  let resource-id = sheet-resource-id(sheet);
  let mirror
    = if (resource-id & sheet-direct-mirror(parent).%mirror-resource)
        make-sheet-mirror-from-resource(parent, sheet, resource-id)
      else
        make-sheet-mirror(parent, sheet)
      end;
  note-mirror-created(sheet, mirror);
  mirror
end method do-make-mirror;

define generic make-sheet-mirror
    (parent :: <abstract-sheet>, sheet :: <abstract-sheet>)
 => (mirror :: <win32-mirror>);

define generic make-sheet-mirror-from-resource
    (parent :: <abstract-sheet>, sheet :: <abstract-sheet>, 
     resource-id :: <resource-id>)
 => (mirror :: <win32-mirror>);

define generic note-mirror-created
    (sheet :: <abstract-sheet>, mirror :: <win32-mirror>) => ();

define method note-mirror-created
    (sheet :: <sheet>, mirror :: <win32-mirror>) => ()
  #f
end method note-mirror-created;

define method note-mirror-created
    (sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let parent = sheet-parent(sheet);
  parent & lower-non-device-parents(port(sheet), sheet);
end method note-mirror-created;

// Lower the mirror for any ancestor sheets that are considered
// siblings rather than parents. We have to lower them so that
// the 'virtual child' is higher in the z-order. Note that parent
// can be #f for certain fake gadgets like the combo box text field.
define method lower-non-device-parents
    (_port :: <win32-port>, sheet :: <sheet>) => ()
  let device-parent = sheet-device-parent(sheet);
  for (ancestor = sheet-parent(sheet) then sheet-parent(ancestor),
       while: ancestor & ancestor ~== device-parent)
    let mirror = sheet-direct-mirror(ancestor);
    mirror & lower-mirror(_port, ancestor, mirror)
  end
end method lower-non-device-parents;

define sealed method make-sheet-mirror
    (parent :: <sheet>, sheet :: <sheet>)
 => (mirror :: <window-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let own-dc? = instance?(sheet, <permanent-medium-mixin>);
  let options
    = %logior($WS-CHILD,
              case
                sheet-tab-stop?(sheet) & sheet-accepts-focus?(sheet) =>
                  %logior($WS-GROUP, $WS-TABSTOP);
                otherwise =>
                  0;
              end);
  let handle :: <HWND>
    = CreateWindowEx
        (0,                             // Extended styles
         if (own-dc?) $canvas-class-name else $simple-class-name end,
         $null-string,                  // no title
         options,                       // Window style
         left,                          // x position
         top,                           // y position
         right - left,                  // width
         bottom - top,                  // height
         window-handle(parent),         // parent window
         $null-hMenu,                   // Use the window class menu
         application-instance-handle(),
         $NULL-VOID);                   // No data in our WM_CREATE
  check-result("CreateWindow", handle);
  let mirror
    = make(<window-mirror>,
           sheet: sheet, handle: handle,
           region: make-bounding-box(left, top, right, bottom));
  when (own-dc?)
    let background = get-default-background(port(sheet), sheet);
    when (color?(background))
      let (red, green, blue) = color-rgb(background);
      let color
        = RGB(truncate(red   * $max-int-color),
              truncate(green * $max-int-color),
              truncate(blue  * $max-int-color));
      let hDC = get-DC(mirror);
      SetBkColor(hDC, color)
    end
  end;
  when (gadget?(sheet))
    let documentation = gadget-documentation(sheet);
    documentation & register-tooltip-for-sheet(sheet, documentation)
  end;
  mirror
end method make-sheet-mirror;

define sealed method make-sheet-mirror-from-resource
    (parent :: <sheet>, sheet :: <sheet>, resource-id :: <resource-id>)
 => (mirror :: <window-mirror>)
  let _port = port(sheet);
  let resource :: <window-resource>
    = lookup-control(sheet-mirror(parent).%mirror-resource, resource-id);
  let handle :: <HWND> = GetDlgItem(window-handle(parent), resource-id);
  check-result("GetDlgItem", handle);
  let (x, y)          = window-position(resource);
  let (width, height) = window-size(resource);
  let (x, y)          = win32-dialog-units->pixels(_port, x, y);
  let (width, height) = win32-dialog-units->pixels(_port, width, height);
  duim-debug-message("Sheet geometry %= from resource: %d x %d at %d, %d",
                     sheet, width, height, x, y);
  initialize-sheet-geometry(sheet, x, y, width, height);
  initialize-sheet-from-resource(sheet, handle);
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let mirror
    = make(<window-mirror>,
           sheet: sheet, handle: handle,
           resource: resource,
           region: make-bounding-box(left, top, right, bottom));
  when (gadget?(sheet))
    let documentation = gadget-documentation(sheet);
    documentation & register-tooltip-for-sheet(sheet, documentation)
  end;
  mirror
end method make-sheet-mirror-from-resource;

// Methods for this function are intended to initialize sheet objects from
// the mirror that was created from a resource.  For example, the method
// on buttons might initialize 'gadget-label' and 'gadget-value'.
define open generic initialize-sheet-from-resource
    (sheet :: <abstract-sheet>, handle :: <HWND>) => ();

define sealed method initialize-sheet-from-resource
    (sheet :: <sheet>, handle :: <HWND>) => ()
  #f
end method initialize-sheet-from-resource;

// This is only ever called on mirrored sheets
define method erase-background
    (sheet :: <sheet>, mirror :: <window-mirror>, hDC :: <HDC>) => ()
  let brush = mirror-background-brush(sheet, mirror);
  let (width, height) = sheet-size(sheet);
  let pen :: <HPEN> = $null-hpen;
  //---*** Do we need to restore the DC afterwards?
  check-result("SelectObject (null pen)", SelectObject(hDC, pen));
  check-result("SelectObject (brush)",    SelectObject(hDC, brush));
  //--- '+ 1' because Windows doesn't draw the lower-right of rectangles
  Rectangle(hDC, 0, 0, width + 1, height + 1)
end method erase-background;

define method mirror-background-brush
    (sheet :: <sheet>, mirror :: <window-mirror>)
 => (brush :: <HBRUSH>)
  mirror.%background-brush
  | begin
      let background = get-default-background(port(sheet), sheet);
      assert(color?(background),
             "Non-color background %= not currently supported for sheet %=",
             sheet, background);
      case
        background == $white =>
          mirror.%background-brush := $white-hbrush;
        background == $black =>
          mirror.%background-brush := $black-hbrush;
        otherwise =>
          let color = %color->native-color(background);
          mirror.%background-brush
            := check-result("CreateSolidBrush", CreateSolidBrush(color));
      end
    end
end method mirror-background-brush;

define sealed method destroy-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let background-brush = mirror.%background-brush;
  when (background-brush)
    let background-brush :: <HBRUSH> = background-brush;
    check-result("DeleteObject", DeleteObject(background-brush));
    mirror.%background-brush := #f
  end;
  let handle :: <HWND> = window-handle(mirror);
  // For our own subclassed windows, this will get us a WM_DESTROY
  unless (null-handle?(handle))
    DestroyWindow(handle)       // error-checking probably won't buy anything...
  end;
  // Note that top level sheets invoke 'note-mirror-destroyed' when
  // a WM_DESTROY is issued, but non-top level sheets won't see that
  unless (instance?(sheet, <top-level-sheet>))
    note-mirror-destroyed(sheet, mirror)
  end;
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define sealed method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let handle :: <HWND> = window-handle(mirror);
  window-mirror(handle) := #f;
  window-handle(mirror) := $NULL-HWND
end method note-mirror-destroyed;

// Cope with an embedded sheet being destroyed by its container,
// as happens with an OLE component
define sealed method note-mirror-destroyed
    (sheet :: <win32-embedded-top-level-sheet>, mirror :: <top-level-mirror>) => ()
  local method note-destroyed (sheet :: <sheet>)
          let mirror = sheet-direct-mirror(sheet);
          when (instance?(mirror, <win32-mirror>))
            do-sheet-children(note-destroyed, sheet);
            note-mirror-destroyed(sheet, mirror)
          end
        end method;
  do-sheet-children(note-destroyed, sheet);
  next-method()
end method note-mirror-destroyed;

define sealed method note-mirror-destroyed
    (gadget :: <win32-subclassed-gadget-mixin>, mirror :: <window-mirror>) => ()
  let handle = window-handle(mirror);
  when (handle)
    // Need to remove the override command handler before disconnecting the
    // handle from the mirror
    SetWindowLong(handle, $GWL-WNDPROC, pointer-address(gadget.%old-WndProc))
  end;
  next-method()
end method note-mirror-destroyed;


/// Mirror DC handling

define sealed method get-DC 
    (mirror :: <window-mirror>) => (hDC :: <HDC>)
  let hDC :: <HDC> = mirror.%DC;
  when (null-handle?(hDC))
    hDC := GetDC(window-handle(mirror));
    check-result("GetDC", hDC);
    mirror.%DC := hDC
  end;
  hDC
end method get-DC;

define sealed method release-DC
    (mirror :: <window-mirror>) => ()
  let hDC :: <HDC> = mirror.%DC;
  unless (null-handle?(hDC))
    let handle = window-handle(mirror);
    unless (zero?(ReleaseDC(handle, hDC)))
      // For CS_OWNDC windows, 'ReleaseDC' is a no-op.  Invalidate
      // the drawing state only if we actually released the DC.
      // Note that we need the DC in order to invalidate the cached info,
      // so we do a local get and release here.
      mirror.%DC := $null-hDC;          // force 'get-DC' to really call GetDC
      let hDC :: <HDC> = get-DC(mirror);// now really get a DC
      let medium = sheet-medium(mirror-sheet(mirror));
      when (medium)
        medium-drawing-state-cache(medium) := 0
      end;
      // Note that mustn't invalidate this slot until AFTER we've decached
      // the drawing state.  Otherwise decaching will cause GetDC to be
      // called without a corresponding ReleaseDC, which is a Bad Thing.
      ReleaseDC(handle, hDC);
      mirror.%DC := $null-hDC
    end
  end
end method release-DC;

// No-op method, since it appears this can get called on menu mirrors
define sealed method release-DC
    (mirror :: <win32-mirror>) => ()
  #f
end method release-DC;

define sealed method repaint-sheet-with-DC
    (sheet :: <sheet>, hDC :: <HDC>) => ()
  when (sheet-handles-repaint?(sheet))
    let mirror :: <window-mirror> = sheet-mirror(sheet);
    let old-hDC = mirror.%DC;
    let parent = find-sheet-with-medium(sheet);
    let medium = sheet-medium(parent);
    block ()
      mirror.%DC := hDC;
      medium-drawing-state-cache(medium) := 0;
      //---*** Should first fill the background with the appropriate color
      //---*** if not using the default.
      // Not going through 'repaint-sheet' because we don't want it to
      // do the children.  Most of what it does is not relevant here anyway.
      handle-repaint(sheet, medium, sheet-region(sheet));
    cleanup
      medium-drawing-state-cache(medium) := 0;
      mirror.%DC := old-hDC;
    end;
  end
end method repaint-sheet-with-DC;

// Draw sheet and its children into the given DC, scaled and offset.
define function repaint-in-DC-recursive
    (sheet :: <basic-sheet>, hDC :: <HDC>,
     x-scale  :: <real>, y-scale  :: <real>,
     x-offset :: <real>, y-offset :: <real>) => ()
  duim-debug-message("  repaint-in-dc-recursive(%=,,,,%=,%=)\n",
                     sheet, x-offset, y-offset);
  if (sheet-handles-repaint?(sheet)
      & instance?(sheet, <sheet-with-medium-mixin>))
    // Use the sheet's 'handle-repaint' method
    let medium = sheet-medium(sheet);
    let xform
      = if (x-scale = 1 & y-scale = 1)
          // Special case hack because 'make-translation-transform' is always 
          // available, but 'make-transform' only works if the optional
          // 'duim-extended-geometry' library is loaded.  Don't require the
          // caller to 'use duim-extended-geometry' unless scaling is needed.
          make-translation-transform(x-offset, y-offset)
        else
          make-transform(x-scale, 0, 0, y-scale, x-offset, y-offset)
        end;
    with-transform (medium, xform)
      repaint-sheet-with-DC(sheet, hDC)
    end;
/*
  // Not useful until we figure out how to do the coordinate transformation.
  // Might need to use w/ModifyWorldTransform                   ???
  // Or maybe draw into a bit map that can be pasted into the proper place?
  elseif (instance?(sheet, <win32-gadget-mixin>))
    // This may be a Windows control that we can ask Windows to draw.
    let handle = window-handle(sheet);
    when (handle)
      let drawn? = zero?(SendMessage(handle, $WM-PAINT, pointer-address(hDC), 0));
      duim-debug-message("  WM_PAINT %s\n",
                         if(drawn?) "OK" else "not handled" end);
    end;
*/
  end;
  for (child :: <basic-sheet> in sheet-children(sheet))
    let (x, y) = sheet-position(child);
    repaint-in-DC-recursive(child, hDC,
                            x-scale, y-scale,
                            (x * x-scale) + x-offset, (y * y-scale) + y-offset);
  end
end function repaint-in-DC-recursive;


/// Mirror manipulation

// For non-top-level sheets, we just show the window
define sealed method map-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let handle :: <HWND> = window-handle(mirror);
  ShowWindow(handle, $SW-SHOWNORMAL);   // no status code for this
  // Sends WM_PAINT message and returns status
  check-result("UpdateWindow", UpdateWindow(handle))
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let handle :: <HWND> = window-handle(mirror);
  ShowWindow(handle, $SW-HIDE)          // no status code for this
end method unmap-mirror;

define sealed method raise-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>,
     #key activate? = #t) => ()
  ignore(activate?);
  let handle :: <HWND> = window-handle(mirror);
  check-result("SetWindowPos ($HWND-TOP)",
               SetWindowPos(handle, $HWND-TOP, 0, 0, 0, 0,
                            %logior($SWP-NOMOVE, $SWP-NOSIZE)))
end method raise-mirror;

define sealed method lower-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>) => ()
  let handle :: <HWND> = window-handle(mirror);
  check-result("SetWindowPos ($HWND-BOTTOM)",
               SetWindowPos(handle, $HWND-BOTTOM, 0, 0, 0, 0,
                            %logior($SWP-NOMOVE, $SWP-NOSIZE)))
end method lower-mirror;

define sealed method mirror-visible? 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (visible? :: <boolean>)
  let handle :: <HWND> = window-handle(mirror);
  IsWindowVisible(handle)
end method mirror-visible?;


/// Window mirrors

define sealed method mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region)
end method mirror-edges;

define sealed method set-mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <window-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  let handle :: <HWND> = window-handle(mirror);
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom);
  duim-debug-message("Setting mirror edges for %= to %d x %d at %d,%d",
                     sheet, right - left, bottom - top, left, top);
  // Just change the size and position without doing anything else
  let flags = %logior($SWP-NOACTIVATE, $SWP-NOZORDER);
  // Note that using 'MoveWindow(handle, left, top, right, bottom, #f)'
  // would send a WM_SIZE message, which would cause redundant overhead
  // at best, or infinite recursion at worst.
  check-result("SetWindowPos",
               SetWindowPos(handle, $NULL-HWND,
                            left, top, right - left, bottom - top, flags))
end method set-mirror-edges;

// Returns the position of the sheet in "absolute" (screen) coordinates
define sealed method sheet-screen-position
    (_port :: <win32-port>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let (mirror :: <window-mirror>, transform :: <transform>)
    = if (sheet-direct-mirror(sheet))
        values(sheet-direct-mirror(sheet), $identity-transform)
      else
        let parent    = sheet-device-parent(sheet);
        let transform = sheet-delta-transform(sheet, parent);
        values(sheet-direct-mirror(parent), transform)
      end;
  // Get the position of the sheet in its mirrored parent's coordinates
  let (x, y) = transform-position(transform, 0, 0);
  client-to-screen-position(mirror, x, y)
end method sheet-screen-position;

define sealed method client-to-screen-position
    (mirror :: <window-mirror>, x :: <integer>, y :: <integer>)
 => (screen-x :: <integer>, screen-y :: <integer>)
  with-stack-structure (point :: <LPPOINT>)
    point.x-value := x;
    point.y-value := y;
    ClientToScreen(window-handle(mirror), point);
    values(point.x-value, point.y-value)
  end
end method client-to-screen-position;

define sealed method screen-to-client-position
    (mirror :: <window-mirror>, x :: <integer>, y :: <integer>)
 => (client-x :: <integer>, client-y :: <integer>)
  with-stack-structure (point :: <LPPOINT>)
    point.x-value := x;
    point.y-value := y;
    ScreenToClient(window-handle(mirror), point);
    values(point.x-value, point.y-value)
  end
end method screen-to-client-position;


/// Win32 panes

define open abstract class <win32-pane-mixin>
    (<standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <win32-pane-mixin>;

// Returns #t, meaning that the port will take care of repainting mirrors
define method port-handles-repaint?
    (_port :: <win32-port>, sheet :: <mirrored-sheet-mixin>)
 => (true? :: <boolean>)
  #t
end method port-handles-repaint?;


define sealed method do-with-atomic-redisplay
    (sheet :: <win32-pane-mixin>, continuation :: <function>)
 => (#rest values)
  let handle = window-handle(sheet);
  if (handle)
    with-delayed-drawing (handle)
      continuation(sheet)
    end
  else
    continuation(sheet)
  end
end method do-with-atomic-redisplay;


/// Port defaults

define method port-default-foreground
    (_port :: <win32-port>, sheet :: <sheet>)
 => (foreground :: false-or(<ink>))
  %native-color->color(GetSysColor($COLOR-WINDOWTEXT))
end method port-default-foreground;

// Most sheets should show up with the standard 3d "gray" background...
define method port-default-background
    (_port :: <win32-port>, sheet :: <sheet>)
 => (background :: false-or(<ink>));
  %native-color->color(GetSysColor(if (_port.%use-3d?) $COLOR-3DFACE else $COLOR-WINDOW end))
end method port-default-background;

// ...but drawing panes should defaultly have a "white" background
define method port-default-background
    (_port :: <win32-port>, sheet :: <drawing-pane>)
 => (background :: false-or(<ink>));
  %native-color->color(GetSysColor($COLOR-WINDOW))
end method port-default-background;

// Viewports try to take their background from their child
define method port-default-background
    (_port :: <win32-port>, sheet :: <viewport>)
 => (background :: false-or(<ink>))
  let child = sheet-child(sheet);
  if (child)
    port-default-background(_port, child)
  else
    next-method()
  end
end method port-default-background;

define method port-default-background
    (_port :: <win32-port>, sheet :: <homegrown-control-mixin>)
 => (background :: false-or(<ink>))
  %native-color->color(GetSysColor($COLOR-WINDOW))
end method port-default-background;

define method port-default-background
    (_port :: <win32-port>, sheet :: <homegrown-control-layout-mixin>)
 => (background :: false-or(<ink>))
  %native-color->color(GetSysColor($COLOR-WINDOW))
end method port-default-background;

// FYI, the normal size on Windows is 8-points
// We arrange to map this to something close to ANSI_VAR_FONT
define constant $win32-default-text-style
    = make(<text-style>,
           family: #"sans-serif", weight: #"normal",
           slant: #"roman", size: #"normal");

// Note that this "default default" text style is _not_ the one that we use
// for gadgets.  There's another method for that on <win32-gadget-mixin>.
define method port-default-text-style
    (_port :: <win32-port>, sheet :: <sheet>)
 => (text-style :: false-or(<text-style>))
  $win32-default-text-style
end method port-default-text-style;
