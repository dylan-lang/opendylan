Module:    win32-duim
Synopsis:  Win32 common controls
Author:    Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic Win32 constants

//---*** All of the following should be computed
define constant $progress-bar-min-width  :: <integer> = 50;
define constant $progress-bar-height     :: <integer> = 18;

define constant $tab-control-border :: <integer> = 2;	// in dialog units

define constant $slider-page-size   :: <integer> =  4;
define constant $slider-min-length  :: <integer> = 50;	// in pixels
define constant $slider-min-breadth :: <integer> = 30;	// in pixels

define constant $list-view-minimum-visible-lines :: <integer> =  3;
define constant $list-view-default-visible-lines :: <integer> = 10;
define constant $list-view-extra-height          :: <integer> =  2;

define constant $tree-view-minimum-visible-lines :: <integer> =  3;
define constant $tree-view-default-visible-lines :: <integer> = 10;
define constant $tree-view-extra-height          :: <integer> =  2;

define constant $status-bar-border  :: <integer> = 1;	// in pixels
define constant $status-bar-spacing :: <integer> = 4;	// in pixels

define constant $spin-box-spacing      :: <integer> =  0;
define constant $up-down-control-width :: <integer> = 12;


/// Win32 common controls

define constant $KERNEL32       = "KERNEL32";
define constant $CREATEACTCTX   = "CreateActCtxA";
define constant $ACTIVATEACTCTX = "ActivateActCtx";

//---*** NOTE: Be sure to change this whenever the DLL name is changed (e.g., when released)
define constant $ME             = "DXWDUIM";

define sealed method initialize-common-controls
    (_port :: <win32-port>) => ()
  unless (_port.%common-controls-initialized?)
    let kernel32 :: <HMODULE> = GetModuleHandle($KERNEL32);
    if (~null-handle?(kernel32))
      let fCreateActCtx   :: <C-function-pointer> = GetProcAddress(kernel32, $CREATEACTCTX);
      let fActivateActCtx :: <C-function-pointer> = GetProcAddress(kernel32, $ACTIVATEACTCTX);
      // If support for activation contexts is present, establish and activate a context
      // which enables the use of Windows XP visual styles in common controls using the
      // manifest embedded as a resource in the DUIM DLL...
      if (~null-pointer?(fCreateActCtx) & ~null-pointer?(fActivateActCtx))
	with-stack-structure (act :: <PACTCTX>)
	  with-stack-structure (moduleName :: <LPSTR>, size: $MAX-PATH + 1)
	    GetModuleFileName(null-handle(<HMODULE>), moduleName, $MAX-PATH);
	    act.cbSize-value := safe-size-of(<ACTCTX>);
	    act.dwFlags-value := logior($ACTCTX-FLAG-RESOURCE-NAME-VALID,
					$ACTCTX-FLAG-HMODULE-VALID);
	    act.lpSource-value := moduleName;
	    act.lpResourceName-value := MAKEINTRESOURCE(2);
	    act.hModule-value := GetModuleHandle($ME);
	    let hActCtx :: <HANDLE> = CreateActCtx(fCreateActCtx, act);
	    if (~null-handle?(hActCtx))
	      //---*** Should we save the activation handle & cookie and deactivate on exit?
	      ActivateActCtx(fActivateActCtx, hActCtx);
	    end;
	  end;
	end;
      end;
    end;
    InitCommonControls();
    _port.%common-controls-initialized? := #t
  end
end method initialize-common-controls;

define open abstract class <win32-control-mixin> (<win32-gadget-mixin>)
end class <win32-control-mixin>;

define sealed method do-make-mirror
    (_port :: <win32-port>, gadget :: <win32-control-mixin>)
 => (mirror :: <window-mirror>)
  initialize-common-controls(_port);
  next-method()
end method do-make-mirror;


/// Progress controls

define sealed class <win32-progress-bar>
    (<win32-control-mixin>,
     <progress-bar>,
     <leaf-pane>)
end class <win32-progress-bar>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <progress-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-progress-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-progress-bar>));
define sealed domain initialize (<win32-progress-bar>);

define sealed method make-gadget-control
    (gadget :: <win32-progress-bar>, 
     parent :: <HWND>, 
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     $PROGRESS-CLASS,
		     "",
		     options,
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (PROGRESS_CLASS)", handle);
  handle
end method make-gadget-control;

define sealed method note-mirror-created
    (gadget :: <win32-progress-bar>, mirror :: <window-mirror>) => ()
  next-method();
  update-progress-bar(gadget)
end method note-mirror-created;

define sealed method do-compose-space
    (gadget :: <win32-progress-bar>, #key width, height)
 => (space-requirement :: <space-requirement>)
  let height = $progress-bar-height;
  let min-width = $progress-bar-min-width;
  let width = max(min-width, width | 0);
  make(<space-requirement>,
       width:  width, min-width: min-width, max-width: $fill,
       height: height)
end method do-compose-space;

/*
//---*** Why does this method not work?
define sealed method do-compose-space
    (gadget :: <win32-progress-bar>, #key width, height)
 => (space-requirement :: <space-requirement>)
  let min-width  = $progress-bar-min-width;
  let max-width  = $fill;
  // Status bars look nice if they have the same height as a text field
  let min-height = $progress-bar-min-height;
  let max-height = $fill;
  let width  = constrain-size(width  | min-width,  min-width,  max-width);
  let height = constrain-size(height | $progress-bar-height, min-height, max-height);
  make(<space-requirement>,
       width:  width,  min-width:  min-width,  max-width:  max-width,
       height: height, min-height: height,     max-height: height)
end method do-compose-space;
*/

define sealed method update-progress-bar
    (gadget :: <win32-progress-bar>) => ()
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    let handle = window-handle(mirror);
    let _port = port(gadget);
    let value = gadget-value(gadget);
    let progress-range = gadget-value-range(gadget);
    let first-value    = progress-range[0];
    let range-increment
      = if (size(progress-range) <= 1) 1 else progress-range[1] - first-value end;
    let pos = floor/(value - first-value, range-increment);
    let min-value = 0;
    let max-value = max(min-value, size(progress-range) - 1);
    SendMessage(handle, $PBM-SETRANGE, 0, MAKELPARAM(min-value, max-value));
    SendMessage(handle, $PBM-SETPOS, pos, 0)
  end
end method update-progress-bar;

define sealed method note-gadget-value-changed
    (gadget :: <win32-progress-bar>) => ()
  next-method();
  update-progress-bar(gadget)
end method note-gadget-value-changed;


/// Sliders

define sealed class <win32-slider>
    (<win32-control-mixin>,
     <slider>,
     <leaf-pane>)
end class <win32-slider>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <slider>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-slider>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-slider>));
define sealed domain initialize (<win32-slider>);

define sealed method make-gadget-control
    (gadget :: <win32-slider>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let ticks? = slider-tick-marks(gadget);
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget, default-border?: #f),
		     $TRACKBAR-CLASS,
		     "",
		     %logior(options,
			     if (sheet-tab-stop?(gadget)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
			     select (gadget-orientation(gadget))
			       #"horizontal" => $TBS-HORZ;
			       #"vertical"   => $TBS-VERT;
			     end,
			     if (ticks?) $TBS-AUTOTICKS else 0 end),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (TRACKBAR_CLASS)", handle);
  handle
end method make-gadget-control;

define sealed method note-mirror-created
    (gadget :: <win32-slider>, mirror :: <window-mirror>) => ()
  next-method();
  update-slider-mirror(gadget)
end method note-mirror-created;

define sealed method do-compose-space
    (gadget :: <win32-slider>, #key width, height)
 => (space-requirement :: <space-requirement>)
  select (gadget-orientation(gadget))
    #"horizontal" =>
      let min-width    = $slider-min-length;
      let min-height   = $slider-min-breadth;
      let width = constrain-size(width | min-width, min-width, $fill);
      make(<space-requirement>,
	   width:      width,      height:     min-height,
	   min-width:  min-width,  min-height: min-height,
	   max-width:  $fill,      max-height: min-height);
    #"vertical" =>
      let min-width    = $slider-min-breadth;
      let min-height   = $slider-min-length;
      let height = constrain-size(height | min-height, min-height, $fill);
      make(<space-requirement>,
	   width:      min-width,  height:     height,
	   min-width:  min-width,  min-height: min-height,
	   max-width:  min-width,  max-height: $fill);
  end
end method do-compose-space;

//--- Maybe this should be in DUIM-Gadgets?
define sealed method range-gadget-adjusted-contents
    (gadget :: <range-gadget-mixin>)
 => (position :: <integer>, min :: <integer>, max :: <integer>)
  let value = gadget-value(gadget);
  let value-range = gadget-value-range(gadget);
  let first-value = value-range[0];
  let range-increment
    = if (size(value-range) <= 1) 1 else value-range[1] - first-value end;
  let pos = floor/(value - first-value, range-increment);
  let min = 0;
  let max = size(value-range) - 1;
  values(pos, min, max)
end method range-gadget-adjusted-contents;

define sealed method update-slider-mirror
    (gadget :: <win32-slider>) => ()
  let handle :: false-or(<hWnd>) = window-handle(gadget);
  when (handle)
    let (pos, min, max) = range-gadget-adjusted-contents(gadget);
    SendMessage(handle, $TBM-SETRANGE, $true, MAKELONG(min, max));
    SendMessage(handle, $TBM-SETPOS, $true, pos);
    let tick-marks = slider-tick-marks(gadget);
    when (tick-marks)
      SendMessage(handle, $TBM-SETTICFREQ, tick-marks, pos)
    end
  end
end method update-slider-mirror;

define sealed method note-gadget-value-changed
    (gadget :: <win32-slider>) => ()
  next-method();
  update-slider-mirror(gadget)
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <win32-slider>) => ()
  next-method();
  update-slider-mirror(gadget)
end method note-gadget-value-range-changed;

define sealed method handle-scrolling
    (gadget :: <win32-slider>, 
     scroll-code :: <integer>, position :: <integer>)
 => (handled? :: <boolean>)
  block (return)
    let value-range = gadget-value-range(gadget);
    select (scroll-code)
      $TB-THUMBTRACK =>
	let (min-pos, max-pos) = values(0, size(value-range) - 1);
	when (position >= min-pos & position <= max-pos)
	  distribute-value-changing-callback(gadget, value-range[position])
	end;
      $TB-THUMBPOSITION =>
	let (min-pos, max-pos) = values(0, size(value-range) - 1);
	when (position >= min-pos & position <= max-pos)
	  distribute-value-changed-callback(gadget,  value-range[position])
	end;
      $TB-PAGEUP =>
	handle-slider-increment(gadget, - $slider-page-size);
      $TB-PAGEDOWN =>
	handle-slider-increment(gadget,   $slider-page-size);
      $TB-LINEUP =>
	handle-slider-increment(gadget, -1);
      $TB-LINEDOWN =>
	handle-slider-increment(gadget,  1);
      $TB-BOTTOM =>
	let position = size(value-range) - 1;
        distribute-value-changed-callback(gadget, value-range[position]);
      $TB-TOP =>
	let position = 0;
	distribute-value-changed-callback(gadget, value-range[position]);
      otherwise =>
	return(#f)
    end;
    #t
  end
end method handle-scrolling;

define sealed method handle-slider-increment
    (gadget :: <win32-slider>, increment :: <integer>) => ()
  let old-value   = gadget-value(gadget);
  let value-range = gadget-value-range(gadget);
  let pos      = position(value-range, old-value, test: \=);
  let last-pos = size(value-range) - 1;
  let pos      = max(0, min(pos + increment, last-pos));
  when (pos ~= last-pos)
    let value = value-range[pos];
    distribute-value-changed-callback(gadget, value)
  end
end method handle-slider-increment;


/// Tool bars

//---*** Someday we should do these for real!

define sealed class <win32-tool-bar>
    (<tool-bar>, <single-child-wrapping-pane>)
  //--- The way we do this separator stuff is just loathsome...
  sealed slot tool-bar-decoration :: <sheet>;
  sealed slot %separator :: false-or(<separator>) = #f;
end class <win32-tool-bar>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <tool-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-tool-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-tool-bar>));
define sealed domain initialize (<win32-tool-bar>);

define method initialize
    (gadget :: <win32-tool-bar>, #key frame-manager: framem) => ()
  next-method();
  let framem = framem | port-default-frame-manager(default-port());
  with-frame-manager (framem)
    gadget.%separator := make(<separator>);
    tool-bar-decoration(gadget)
      := vertically (spacing: 2)
           gadget.%separator;
           gadget
         end
  end
end method initialize;

// When we map a tool-bar, we also map its enclosing decoration.
// This manages to avoid an infinite loop because Silica is careful
// not to map any sheet that is already mapped.
define method note-sheet-mapped (gadget :: <win32-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #t
  end
end method note-sheet-mapped;

// Ditto, for unmapping
define method note-sheet-unmapped (gadget :: <win32-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #f
  end
end method note-sheet-unmapped;


/// Status bars

define sealed class <win32-status-bar>
    (<win32-control-mixin>,
     <status-bar>,
     <row-layout>)
  sealed slot status-bar-simple? :: <boolean> = #f,
    setter: %simple?-setter;
  slot status-bar-simple-text :: <string> = "";
  keyword border:      = $status-bar-border;
  keyword spacing:     = $status-bar-spacing;
  keyword y-alignment: = #"center";
end class <win32-status-bar>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <status-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-status-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-status-bar>));
define sealed domain initialize (<win32-status-bar>);

define sealed method make-gadget-control
    (gadget :: <win32-status-bar>, 
     parent :: <HWND>, 
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget, default-border?: #f),
		     $STATUSCLASSNAME,
		     "",
		     %logior(options, $SBARS-SIZEGRIP),
		     0, 0, 0, 0,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (STATUSCLASSNAME)", handle);
  handle
end method make-gadget-control;

define sealed method do-compose-space
    (gadget :: <win32-status-bar>, #key width, height)
 => (space-requirement :: <space-requirement>)
  // We want a little extra width to keep the final field from being
  // obscured by the resize grip
  let extra-width :: <integer> = GetSystemMetrics($SM-CXVSCROLL);
  let space-req = next-method(gadget,
			      width:  width & (width - extra-width),
			      height: height);
  space-requirement+(gadget, space-req,
		     width: extra-width, min-width: extra-width, max-width: extra-width)
end method do-compose-space;

//---*** We should be more careful that the height is set up right, taking
//---*** into account borders etc.
define sealed method do-allocate-space
    (gadget :: <win32-status-bar>, width :: <integer>, height :: <integer>) => ()
  let extra-width :: <integer> = GetSystemMetrics($SM-CXVSCROLL);
  // Do the usual allocation on the child sheets, but don't let them
  // use the extra space we need for the resize grip
  next-method(gadget, width - extra-width, height);
  let handle   = window-handle(gadget);
  let children = sheet-children(gadget);
  // The idea here is to allocate a new part at the end if the final
  // field is not a label, otherwise we stretch out the final label
  let final-child  = last(children);
  let final-label? = instance?(final-child, <label>);
  let n-children :: <integer> = size(children);
  let n-parts    :: <integer> = n-children + if (final-label?) 0 else 1 end;
  with-stack-structure (widths :: <c-unsigned-int*>, element-count: n-parts)
    let min-height :: <integer> = 0;
    let final-x    :: <integer> = 0;
    duim-debug-message("Laying out %=:", gadget);
    for (i :: <integer> from 0 below n-children,
	 child in children)
      let (left, top, right, bottom) = sheet-device-edges(child);
      ignore(left);
      min-height := max(min-height, bottom - top);
      final-x    := right;
      duim-debug-message("  part %d has right edge at %d", i, right);
      pointer-value(widths, index: i) := right
    end;
    // Allocate a little extra space so the size grip doesn't
    // overlap the final part
    pointer-value(widths, index: n-parts - 1) := final-x + extra-width;
    inc!(min-height, $status-bar-border * 2);
    duim-debug-message("  fixed height %d", min-height);
    SendMessage(handle, $SB-SETMINHEIGHT, min-height, 0);
    SendMessage(handle, $SB-SETPARTS, n-parts, pointer-address(widths))
  end;
  if (final-label?)
    // If the last child needs the extra space, extend it
    let (w, h) = sheet-size(final-child);
    set-sheet-size(final-child, w + extra-width, h)
  else
    // Otherwise, ensure the part that holds the size grip has no border
    SendMessage(handle, $SB-SETTEXT,
		%logior(n-parts - 1, $SBT-NOBORDERS), 
		pointer-address($empty-c-string))
  end;
  // Remove the borders for the non-label parts
  for (i :: <integer> from 0 below n-children,
       child in children)
    unless (instance?(child, <label>))
      SendMessage(handle, $SB-SETTEXT, 
		  %logior(i, $SBT-NOBORDERS), 
		  pointer-address($empty-c-string));
    end
  end
end method do-allocate-space;

define class <status-label-mirror> (<win32-mirror>)
  sealed slot status-label-status-bar :: <status-bar>,
    required-init-keyword: status-bar:;
  sealed slot status-label-part-number :: <integer>,
    required-init-keyword: part-number:;
end class <status-label-mirror>;

define sealed method make-sheet-mirror
    (status-bar :: <win32-status-bar>, gadget :: <win32-label>)
 => (mirror :: <status-label-mirror>)
  let children = sheet-children(status-bar);
  let part-number
    = position(children, gadget)
      | error("Gadget %= not a direct child of status bar %=",
	      gadget, status-bar);
  make(<status-label-mirror>, 
       sheet: gadget,
       status-bar: status-bar,
       part-number: part-number)
end method make-sheet-mirror;

define sealed method update-mirror-label
    (gadget :: <win32-label>, mirror :: <status-label-mirror>) => ()
  let status-bar = status-label-status-bar(mirror);
  let primary-label = status-bar-label-pane(status-bar);
  let label
    = if (status-bar-simple?(status-bar) & gadget = primary-label)
	status-bar-simple-text(status-bar)
      else
	defaulted-gadget-label(gadget)
      end;
  let label :: <string> = if (instance?(label, <string>)) label else "" end;
  let handle = window-handle(status-bar);
  let part-number = status-label-part-number(mirror);
  with-c-string (c-string = label)
    SendMessage(handle, $SB-SETTEXT,
		%logior(part-number, 0),
		pointer-address(c-string));
    UpdateWindow(handle)
  end
end method update-mirror-label;


/// Simple status bars

//--- Note that this protocol isn't part of DUIM, it is just used
//--- to show the documentation for the currently highlighted menu.
define sealed method status-bar-simple?-setter
    (simple? :: <boolean>, gadget :: <win32-status-bar>)
 => (simple? :: <boolean>)
  when (status-bar-simple?(gadget) ~= simple?)
/*---*** This doesn't work, so let's just fix up the label by hand
    let handle = window-handle(gadget);
    duim-debug-message("Making %= simple status bar %=", gadget, simple?);
    SendMessage(handle, $SB-SIMPLE, if (simple?) $true else $false end, 0);
*/
    gadget.%simple? := simple?;
    //--- The following code is just a hack because the $SB-SIMPLE stuff
    //--- doesn't work (why?)
    update-status-bar-label(gadget)
  end;
  simple?
end method status-bar-simple?-setter;

define sealed method status-bar-simple-label-setter
    (label :: <string>, gadget :: <win32-status-bar>)
 => (label :: <string>)
  let handle = window-handle(gadget);
  gadget.%simple? := #t;
  status-bar-simple-text(gadget) := label;
/*---*** The simple label code doesn't work for some reason
  with-c-string (c-string = label)
    duim-debug-message("Setting simple label for %= to '%s'", gadget, label);
    SendMessage(handle, $WM-SETTEXT, 255, pointer-address(c-string));
    UpdateWindow(handle)
  end;
*/
  //--- The following code is just a hack because the $SB-SIMPLE stuff
  //--- doesn't seem to work (why?)
  update-status-bar-label(gadget);
  label
end method status-bar-simple-label-setter;

//---*** This shouldn't be needed if we could get $SB-SIMPLE to work
define function update-status-bar-label
    (gadget :: <win32-status-bar>) => ()
  let label = status-bar-label-pane(gadget);
  let mirror = label & sheet-direct-mirror(label);
  mirror & update-mirror-label(label, mirror)
end function update-status-bar-label;

define sealed method frame-status-bar-simple?-setter
    (simple? :: <boolean>, frame :: <frame>)
 => (simple? :: <boolean>)
  let status-bar = frame-status-bar(frame);
  when (status-bar)
    status-bar-simple?(status-bar) := simple?
  end;
  simple?
end method frame-status-bar-simple?-setter;

// Note: this is open to allow methods to be added for OLE support
define open generic update-frame-documentation
    (frame :: <frame>, documentation :: false-or(<string>)) => ();

define method update-frame-documentation
    (frame :: <frame>, documentation :: false-or(<string>)) => ()
  let status-bar = frame-status-bar(frame);
  when (status-bar)
    if (documentation)
      status-bar-simple-label(status-bar) := documentation | ""
    else
      status-bar-simple?(status-bar) := #f
    end
  end
end method update-frame-documentation;


/// Tab controls

// Tab controls are unusual among mirrored gadgets in that they can have as
// children ordinary DUIM sheets.
//--- For now, proper repainting of tab control pages only works if each page
//--- is mirrored; this is mostly OK because, e.g., <drawing-pane> is mirrored.
//--- Consider parenting the pages into a drawing pane to fix this?
define sealed class <win32-tab-control> 
    (<win32-control-mixin>,
     <win32-layout-gadget-mixin>,
     <tab-control>,
     <single-child-composite-pane>)
end class <win32-tab-control>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <tab-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-tab-control>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-tab-control>));
define sealed domain initialize (<win32-tab-control>);

define sealed method initialize
    (pane :: <win32-tab-control>, #key frame-manager: framem)
  next-method();
  let pages = tab-control-pages(pane);
  sheet-child(pane) := make(<stack-layout>, children: pages);
  tab-control-map-page(pane)
end method initialize;

define sealed method make-gadget-control
    (gadget :: <win32-tab-control>, 
     parent :: <HWND>,
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     $WC-TABCONTROL,
		     "",
		     //---*** Add $TCS-BOTTOM if 'tab-control-tabs-position' is #"bottom"
		     %logior(options,
			     if (sheet-tab-stop?(gadget)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (WC_TABCONTROL)", handle);
  handle
end method make-gadget-control;

define sealed method note-mirror-created
    (pane :: <win32-tab-control>, mirror :: <window-mirror>) => ()
  next-method();
  create-tab-control-pages(pane, mirror)
end method note-mirror-created;

define sealed method create-tab-control-pages
    (pane :: <win32-tab-control>, mirror :: <window-mirror>) => ()
  let handle = window-handle(mirror);
  let labels = tab-control-labels(pane);
  with-stack-structure (item :: <LPTC-ITEMA>)
    for (page-number :: <integer> from 0 below size(labels),
	 label in labels)
      item.mask-value := $TCIF-TEXT;
      with-c-string (c-string = label)
	item.pszText-value := c-string;
        SendMessage(handle, $TCM-INSERTITEM, page-number, 
		    pointer-address(item))
      end
    end
  end;
  update-selected-tab(pane, mirror)
end method create-tab-control-pages;

define sealed method update-selected-tab
    (pane :: <win32-tab-control>, mirror :: <window-mirror>) => ()
  let handle = window-handle(mirror);
  let page = tab-control-current-page(pane);
  let index = position(tab-control-pages(pane), page);
  when (index)
    SendMessage(handle, $TCM-SETCURSEL, index, 0)
  end
end method update-selected-tab;

define sealed method note-pages-changed
    (pane :: <win32-tab-control>) => ()
  let mirror = sheet-direct-mirror(pane);
  let old-page = tab-control-mapped-page(pane);
  let new-page = tab-control-current-page(pane);
  let swap-page? = (old-page ~== new-page);
  when (swap-page? & old-page)
    sheet-withdrawn?(old-page) := #t
  end;
  let stack :: <stack-layout> = sheet-child(pane);
  sheet-children(stack) := tab-control-pages(pane);
  //---*** This is surely too aggressive, but seems to be needed
  //---*** to get things layed out right
  invalidate-space-requirements(stack);
  when (sheet-attached?(stack))
    relayout-parent(stack);
    //---*** Can this be removed if we fix 'set-sheet-edges'?
    update-all-mirror-positions(stack)
  end;
  when (mirror)
    let handle = window-handle(mirror);
    SendMessage(handle, $TCM-DELETEALLITEMS, 0, 0);
    create-tab-control-pages(pane, mirror)
  end;
  swap-page? & tab-control-map-page(pane)
end method note-pages-changed;

define sealed method tab-control-mapped-page
    (pane :: <win32-tab-control>)
 => (page :: false-or(<sheet>))
  let stack :: <stack-layout> = sheet-child(pane);
  stack-layout-mapped-page(stack)
end method tab-control-mapped-page;

define sealed method tab-control-map-page
    (pane :: <win32-tab-control>) => ()
  let new-page = tab-control-current-page(pane);
  let stack :: <stack-layout> = sheet-child(pane);
  stack-layout-mapped-page(stack) := new-page;
  when (new-page & sheet-mapped?(new-page))
    // If nothing has the focus, try to set the focus now
    when (GetFocus() = $NULL-HWND)
      let frame = sheet-frame(new-page);
      let focus = page-initial-focus(new-page) | find-child-for-focus(pane);
      frame & (frame-input-focus(frame) := focus)
    end
  end
end method tab-control-map-page;

define sealed method note-gadget-value-changed
    (pane :: <win32-tab-control>) => ()
  next-method();
  let mirror = sheet-direct-mirror(pane);
  mirror & update-selected-tab(pane, mirror);
  tab-control-map-page(pane)
end method note-gadget-value-changed;

define sealed method do-compose-space
    (pane :: <win32-tab-control>, #key width, height)
 => (space-requirement :: <space-requirement>)
  let (tabs-width, tabs-height) = tab-control-tabs-size(pane);
  ignore(tabs-height);
  let (extra-width, extra-height)
    = tab-control-extra-space(pane, width: width, height: height);
  let layout-width  = width  & (width  - extra-width);
  let layout-height = height & (height - extra-height);
  let stack :: <stack-layout> = sheet-child(pane);
  let space-req
    = compose-space(stack, width: layout-width, height: layout-height);
  let (w, w-, w+, h, h-, h+)
    = space-requirement-components(stack | pane, space-req);
  let min-width  = max(w- + extra-width, tabs-width);
  let max-width  = $fill;	// was 'min(w+ + extra-width, min-width)'
  let min-height = h- + extra-height;
  let max-height = $fill;	// was 'min(h+ + extra-height, min-height)'
  let best-width  = constrain-size(w + extra-width,  min-width,  max-width);
  let best-height = constrain-size(h + extra-height, min-height, max-height);
  make(<space-requirement>,
       width:  best-width,  min-width:  min-width,  max-width:  max-width,
       height: best-height, min-height: min-height, max-height: max-height)
end method do-compose-space;

define sealed method tab-control-tabs-size
    (pane :: <win32-tab-control>)
 => (width :: <integer>, height :: <integer>)
  let handle = window-handle(pane);
  // We get the right and bottom coordinates of the right-most tab
  // to work out the width and height of the whole set.
  with-stack-structure (rect :: <LPRECT>)
    let last-index = size(tab-control-pages(pane)) - 1;
    SendMessage(handle, $TCM-GETITEMRECT, last-index, pointer-address(rect));
    let width  = rect.right-value;
    let height = rect.bottom-value;
    values(width, height)
  end
end method tab-control-tabs-size;

define sealed method tab-control-extra-space
    (pane :: <win32-tab-control>, #key width, height)
 => (width :: <integer>, height :: <integer>)
  let framem = frame-manager(pane);
  let handle = window-handle(pane);
  let border*2 = win32-dialog-x-pixels(framem, $tab-control-border) * 2;
  with-stack-structure (rect :: <LPRECT>)
    width  := width  | 100;
    height := height | 100;
    rect.left-value   := 0;
    rect.top-value    := 0;
    rect.right-value  := width;
    rect.bottom-value := height;
    SendMessage(handle, $TCM-ADJUSTRECT, $true, pointer-address(rect));
    let extra-width  = rect.right-value  - rect.left-value - width;
    let extra-height = rect.bottom-value - rect.top-value  - height;
    values(extra-width + border*2, extra-height + border*2)
  end
end method tab-control-extra-space;

define sealed method do-allocate-space
    (pane :: <win32-tab-control>, width :: <integer>, height :: <integer>) => ()
  let stack :: <stack-layout> = sheet-child(pane);
  let (left, top, right, bottom)
    = tab-control-child-edges(pane, width, height);
  let available-width  = right  - left;
  let available-height = bottom - top;
  let space-req
    = compose-space(stack, width: available-width, height: available-height);
  let (w, w-, w+, h, h-, h+)
    = space-requirement-components(stack, space-req);
  set-sheet-edges(stack, left, top, left + w, top + h)
end method do-allocate-space;

define sealed method tab-control-child-edges
    (pane :: <win32-tab-control>, width :: <integer>, height :: <integer>)
 => (left :: <integer>, top :: <integer>, 
     right :: <integer>, bottom :: <integer>)
  let framem = frame-manager(pane);
  let handle = window-handle(pane);
  let border = win32-dialog-x-pixels(framem, $tab-control-border);
  with-stack-structure (rect :: <LPRECT>)
    rect.left-value   := 0;
    rect.top-value    := 0;
    rect.right-value  := width;
    rect.bottom-value := height;
    SendMessage(handle, $TCM-ADJUSTRECT, $false, pointer-address(rect));
    let left   = rect.left-value   + border;
    let top    = rect.top-value    + border;
    let right  = rect.right-value  - border;
    let bottom = rect.bottom-value - border;
    values(left, top, right, bottom)
  end
end method tab-control-child-edges;

define sealed method handle-notify
    (pane :: <win32-tab-control>, mirror :: <window-mirror>,
     wParam :: <wparam-type>, lParam :: <lparam-type>,
     id :: <integer>, code :: <integer>)
 => (handled? :: <boolean>)
  ignore(wParam, lParam);
  select (code)
    $TTN-NEEDTEXT =>
      let ttt = make(<LPTOOLTIPTEXT>, address: lParam);
      //---*** How do we know how big the text buffer is?  Also how do
      //---*** we know which tab is being queried?
      #f;
    $TCN-SELCHANGE =>
      let handle = window-handle(mirror);
      let page-number = SendMessage(handle, $TCM-GETCURSEL, 0, 0);
      duim-debug-message("Selected tab page %d in %=", page-number, pane);
      let page = tab-control-pages(pane)[page-number];
      gadget-value(pane, do-callback?: #t) := gadget-value-key(pane)(page);
      #t;
    $TCN-KEYDOWN =>
      when (gadget-key-press-callback(pane))
	let keydown :: <LPTC-KEYDOWN>
	  = make(<LPTC-KEYDOWN>, address: lParam);
	let vkey :: <integer> = keydown.wVKey-value;
	let keysym = virtual-key->keysym(vkey);
	distribute-key-press-callback(pane, keysym);
	#t
      end;
    otherwise =>
      next-method();
  end
end method handle-notify;


/// Up-down controls

define sealed class <win32-up-down-control>
    (<win32-control-mixin>,
     <oriented-gadget-mixin>,
     <range-gadget-mixin>,
     <basic-value-gadget>,
     <leaf-pane>)
end class <win32-up-down-control>;

define sealed domain make (singleton(<win32-up-down-control>));
define sealed domain initialize (<win32-up-down-control>);

define sealed method make-gadget-control
    (gadget :: <win32-up-down-control>, 
     parent :: <HWND>,
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let horizontal? = gadget-orientation(gadget) == #"horizontal";
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     $UPDOWN-CLASS,
		     "",
		     %logior($UDS-ARROWKEYS, $UDS-WRAP, $UDS-AUTOBUDDY,
			     if (horizontal?) $UDS-HORZ else 0 end,
			     options),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (UPDOWN_CLASS)", handle);
  handle
end method make-gadget-control;

define sealed method update-gadget-mirror
    (gadget :: <win32-up-down-control>, mirror :: <window-mirror>) => ()
  let handle = window-handle(mirror);
  let (pos, min, max) = range-gadget-adjusted-contents(gadget);
  SendMessage(handle, $UDM-SETRANGE, 0, MAKELONG(max, min));
  SendMessage(handle, $UDM-SETPOS,   0, pos);
end method update-gadget-mirror;

define sealed method note-mirror-created
    (gadget :: <win32-up-down-control>, mirror :: <window-mirror>) => ()
  next-method();
  update-gadget-mirror(gadget, mirror)
end method note-mirror-created;

define sealed method do-compose-space
    (gadget :: <win32-up-down-control>, #key width, height)
 => (space-requirement :: <space-requirement>)
  //--- We may want to do something smarter than this!
  default-space-requirement(gadget, width: width, height: height)
end method do-compose-space;

define sealed method handle-scrolling
    (gadget :: <win32-up-down-control>,
     scroll-code :: <integer>, position :: <integer>)
 => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  let value-range = gadget-value-range(gadget);
  let value    = SendMessage(handle, $UDM-GETPOS, 0, 0);
  let position = LOWORD(value);
  let error    = HIWORD(value);
  //---*** This always returns 1, are we doing something wrong?
  // unless (zero?(error)) report-error("UDM_GETPOS", error: error) end;
  distribute-value-changed-callback(gadget, value-range[position]);
  #t
end method handle-scrolling;


/// Spin boxes

// Wraps up a text field and an up-down control
define sealed class <win32-spin-box> 
    (<spin-box>,
     <single-child-wrapping-pane>)
  sealed slot %text-field      :: <text-field>;
  sealed slot %up-down-control :: <win32-up-down-control>;
end class <win32-spin-box>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <spin-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-spin-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-spin-box>));
define sealed domain initialize (<win32-spin-box>);

define sealed method initialize
    (gadget :: <win32-spin-box>,
     #key documentation,
	  width, min-width, max-width, height, min-height, max-height) => ()
  let enabled? = gadget-enabled?(gadget);
  let text-field 
    = make(<text-field>, 
	   // Pass along the space requirement to the text field
	   width:      width     & (width     - $up-down-control-width),
	   min-width:  min-width & (min-width - $up-down-control-width),
	   max-width:  max-width & (max-width - $up-down-control-width),
	   height: height, min-height: min-height, max-height: max-height,
	   // Pass along the documentation to the text field, too
	   documentation: documentation,
	   //---*** Need to handle value-changed and activate callbacks
	   enabled?: enabled?);
  let up-down-control
    = make(<win32-up-down-control>,
	   orientation: #"vertical",
	   enabled?: enabled?,
	   width: $up-down-control-width, fixed-width?: #t,
	   // We equalize the heights of the up-down control and the text field
	   // in the layout below, so ensure the up-down control is smaller
	   height: 1, fixed-height?: #t,
	   value-changed-callback: 
	     method (up-down-control)
	       let value = gadget-value(up-down-control);
	       gadget-selection(gadget, do-callback?: #t) := vector(value)
	     end method);
  gadget.%up-down-control := up-down-control;
  gadget.%text-field      := text-field;
  next-method();
  update-win32-spin-box(gadget);
  sheet-child(gadget)
    := horizontally (x-spacing: $spin-box-spacing,
		     fixed-height?: #t, equalize-heights?: #t)
         text-field;
         up-down-control
       end
end method initialize;

define sealed method update-win32-spin-box
    (gadget :: <win32-spin-box>) => ()
  let value     = gadget-value(gadget);
  let n-items   = size(gadget-items(gadget));
  let selection = gadget-selection(gadget);
  let up-down-control = gadget.%up-down-control;
  gadget-value-range(up-down-control) := range(from: 0, below: n-items);
  gadget-value(up-down-control)    := ~empty?(selection) & selection[0];
  gadget-value(gadget.%text-field) := gadget-label(gadget)
end method update-win32-spin-box;

define sealed method note-gadget-selection-changed
    (gadget :: <win32-spin-box>) => ()
  next-method();
  update-win32-spin-box(gadget)
end method note-gadget-selection-changed;

define sealed method note-gadget-enabled
    (client, gadget :: <win32-spin-box>) => ()
  gadget-enabled?(gadget.%up-down-control) := #t;
  gadget-enabled?(gadget.%text-field)      := #t;
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <win32-spin-box>) => ()
  gadget-enabled?(gadget.%up-down-control) := #f;
  gadget-enabled?(gadget.%text-field)      := #f;
end method note-gadget-disabled;


/// List and table controls

// On Windows, list and table controls are implemented using the
// same underlying Windows common control, the List View
define class <win32-list-view-mixin> 
    (<win32-subclassed-gadget-mixin>,
     <win32-control-mixin>,
     <collection-gadget>)
  sealed constant slot %items :: <stretchy-object-vector> = make(<stretchy-vector>);
  // These next three are for maintaining the List View's image list
  sealed constant slot %icons :: <object-table> = make(<table>);
  sealed slot %small-icons :: false-or(<HIMAGELIST>) = #f;
  sealed slot %large-icons :: false-or(<HIMAGELIST>) = #f;
  sealed slot %always-show-selection? :: <boolean> = #f,
    init-keyword: always-show-selection?:;
end class <win32-list-view-mixin>;

define sealed method make-gadget-control
    (pane :: <win32-list-view-mixin>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  //--- Nothing in the world seems to get list controls to obey
  //--- $LVM-SETCOLUMN to set the column width, so just make the
  //--- control very wide initially.  Layout will fix it up.
  when (x = 0 & y = 0 & width = 100 & height = 100)
    width := 1000
  end;
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(pane),
		     $WC-LISTVIEW,
		     "",
		     win32-list-view-options(pane, options),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindow (List View)", handle);
  SendMessage(handle, $LVM-SETCOLUMNWIDTH, -1, $LVSCW-AUTOSIZE);
  handle
end method make-gadget-control;

define generic win32-list-view-options
    (pane :: <win32-list-view-mixin>, options :: <options-type>)
 => (options :: <options-type>);

define sealed method destroy-mirror 
    (_port :: <win32-port>, pane :: <win32-list-view-mixin>, mirror :: <window-mirror>) => ()
  when (pane.%small-icons)
    ImageList-Destroy(pane.%small-icons);
    pane.%small-icons := #f
  end;
  when (pane.%large-icons)
    ImageList-Destroy(pane.%large-icons);
    pane.%large-icons := #f
  end;
  release-images(pane.%icons);
  // Unmirrored list views always have no items, we only make them
  // when they are mirrored.  Clearing them here reduces confusion when
  // we later try to mirror this same gadget again.
  size(pane.%items) := 0;
  next-method()
end method destroy-mirror;

// Build the items for the first time when the sheet is fully attached
define sealed method note-sheet-attached
    (pane :: <win32-list-view-mixin>) => ()
  next-method();
  note-gadget-items-changed(pane);
  note-gadget-selection-changed(pane)
end method note-sheet-attached;

define method update-gadget
    (pane :: <win32-list-view-mixin>) => ()
  // No, we don't call 'next-method' here!
  update-list-view-items(pane, force?: #t)
end method update-gadget;

define sealed method note-gadget-items-changed 
    (pane :: <win32-list-view-mixin>) => ()
  next-method();
  update-list-view-items(pane, force?: #f)
end method note-gadget-items-changed;

define sealed method update-list-view-items
    (pane :: <win32-list-view-mixin>, #key force? = #f) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle  = window-handle(mirror);
    let items   = pane.%items;
    let objects = gadget-items(pane);
    let test    = gadget-test(pane);
    // The existing size is given by 'n-items', the new size by 'n-objects'
    let n-items   :: <integer> = size(items);
    let n-objects :: <integer> = size(objects);
    let selection = choose(rcurry(\<, n-objects), gadget-selection(pane));
    case
      n-objects = n-items
      | (n-objects > n-items & n-objects <= n-items * 1.25) =>
	// Adding either zero or a few more items
	// The idea of the following code is to insert just enough
	// new items -- preferably in the right place -- to make the
	// set of items as long as the set of objects, then update
	// the remaining items in place as needed.  This makes it
	// pretty fast to insert just a few new items into any
	// part of a much larger set of items.
	let delta :: <integer> = n-objects - n-items;
	for (object in objects,
	     index :: <integer> from 0)
	  // If the new and the existing objects are the same,
	  // no need to do anything -- except if we are forcibly
	  // updating in order to get new labels, e.g.
	  when (force?
		| index >= n-items
		| ~test(object, item-object(items[index])))
	    //--- Do we really need to make a new item?
	    let item = make-item(pane, object);
	    if (delta = 0)
	      // If we've inserted the right number of new items,
	      // we can now just update the remaining ones in place
	      items[index] := item;
	      do-update-item(pane, handle, item, index, message: $LVM-SETITEM)
	    else
	      // Otherwise, insert a new item and count it off
	      insert-at!(items, item, index);
	      do-update-item(pane, handle, item, index, message: $LVM-INSERTITEM);
	      inc!(n-items);
	      dec!(delta)
	    end
	  end
	end;
      (n-objects < n-items & n-objects >= n-items * 0.75) =>
	// Removing a few items
	// The analog of the insertion case, except that we delete
	// just enough items and fix the remaining ones in place
	let delta :: <integer> = n-items - n-objects;
	for (object in objects,
	     index :: <integer> from 0)
	  when (force?
		| index >= n-items
		| ~test(object, item-object(items[index])))
	    if (delta = 0)
	      let item = make-item(pane, object);
	      items[index] := item;
	      do-update-item(pane, handle, item, index, message: $LVM-SETITEM)
	    else
	      remove-at!(items, index);
	      SendMessage(handle, $LVM-DELETEITEM, index, 0);
	      dec!(n-items);
	      dec!(delta)
	    end
	  end
	end;
      otherwise =>
	// Major change, just do everything from scratch
	size(items) := 0;
	with-busy-cursor (pane)
	  with-delayed-drawing (handle)
	    SendMessage(handle, $LVM-DELETEALLITEMS, 0, 0);
	    SendMessage(handle, $LVM-SETITEMCOUNT, n-objects, 0);
	    for (object in objects,
		 index :: <integer> from 0)
	      let item = make-item(pane, object);
	      add!(items, item);
	      do-update-item(pane, handle, item, index, message: $LVM-INSERTITEM)
	    end
	  end
	end;
    end;
    // Restore the old selection
    //--- Note that we have to do it this way instead of just calling
    //--- 'note-gadget-selection-changed' because the calls to 'update-item'
    //--- above somehow manage to get us into the $LVN-ITEMCHANGED part of
    //--- 'handle-notify', which in turn removes each updated item from the
    //--- selection (because 'update-item' starts each item as unselected)
    //--- The following now gets done in the front-end
    /* gadget-selection(pane, do-callback?: #f) := selection */
  end
end method update-list-view-items;

define sealed method note-gadget-selection-changed
    (pane :: <win32-list-view-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(pane);
  when (~*port-did-it?* & mirror)
    let handle    = window-handle(mirror);
    let selection = gadget-selection(pane);
    let n-items :: <integer> = size(pane.%items);
    let first-item = #f;
    //--- Surely there's a faster way to do this?
    //--- Does this happen when we click, or just when we set 'gadget-selection'?
    with-stack-structure (lvitem :: <LPLV-ITEM>)
      lvitem.mask-value      := $LVIF-STATE;
      lvitem.iSubItem-value  := 0;
      lvitem.stateMask-value := $LVIS-SELECTED;
      for (index :: <integer> from 0 below n-items)
	let selected? = member?(index, selection);
	when (selected?) first-item := index end;
	lvitem.iItem-value   := index;
	lvitem.state-value   := if (selected?) $LVIS-SELECTED else 0 end;
	SendMessage(handle, $LVM-SETITEM, 0, pointer-address(lvitem))
      end
    end;
    when (first-item & gadget-keep-selection-visible?(pane))
      SendMessage(handle, $LVM-ENSUREVISIBLE, first-item, 0)
    end
  end
end method note-gadget-selection-changed;

define sealed method handle-control-message
    (pane :: <win32-list-view-mixin>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  maybe-handle-keyboard-interrupt(pane, message, wParam);
  select (message)
    // When we get a WM_COMMAND, it's probably for something like a menu
    // selection in a pop-up menu parented into the control or for accelerators
    // typed  while the focus is within the control.  Handle it.
    //---*** We need a coherent theory about what to do here
    $WM-COMMAND =>
      handle-wm-command(pane, wParam, lParam);
    otherwise =>
      #f;
  end
end method handle-control-message;

define sealed method handle-notify
    (pane :: <win32-list-view-mixin>, mirror :: <window-mirror>,
     wParam :: <wparam-type>, lParam :: <lparam-type>,
     id :: <integer>, code :: <integer>)
 => (handled? :: <boolean>)
  ignore(wParam);
  select (code)
    $LVN-ITEMCHANGED =>
      let nmlistview :: <LPNM-LISTVIEW>
	= make(<LPNM-LISTVIEW>, address: lParam);
      let index :: <integer> = nmlistview.iItem-value;
      let changed   :: <unsigned-int> = nmlistview.uChanged-value;
      let old-state :: <unsigned-int> = nmlistview.uOldState-value;
      let new-state :: <unsigned-int> = nmlistview.uNewState-value;
      when (~zero?(logand(changed, $LVIF-STATE)))
	dynamic-bind (*port-did-it?* = #t)
	  select (gadget-selection-mode(pane))
	    #"none" =>
	      #f;
	    #"single" =>
	      // NB: single-selection gadgets can never deselect everything
	      // We look at the old state of $LVIS-SELECTED because we might
	      // get a state change for $LVIS-FOCUSED, etc.
	      when (~zero?(logand(new-state, $LVIS-SELECTED))
		    & zero?(logand(old-state, $LVIS-SELECTED)))
		let selection = vector(index);
		//--- Was: 'distribute-selection-changed-callback(pane, selection)'
		gadget-selection(pane, do-callback?: #t) := selection
	      end;
	    #"multiple" =>
	      let old-selection = gadget-selection(pane);
	      let new-selection		// this code intentionally copies the selection
		= case
		    ~zero?(logand(new-state, $LVIS-SELECTED))
		    & zero?(logand(old-state, $LVIS-SELECTED)) =>
		      //--- If 'note-gadget-selection-changed' bound *port-did-it?*
		      //--- and this code checked, we might not need to use 'add-new!'
		      add-new!(old-selection, index);
		    zero?(logand(new-state, $LVIS-SELECTED))
		    & ~zero?(logand(old-state, $LVIS-SELECTED)) =>
		      remove(old-selection, index);
		    otherwise =>
		      old-selection;
		  end;
	      when (new-selection ~= old-selection)
		//--- Was: 'distribute-selection-changed-callback(pane, selection)'
		gadget-selection(pane, do-callback?: #t) := new-selection
	      end;
	  end
	end
      end;
      #t;
    $NM-DBLCLK =>
      // The selection is already set when we get here
      unless (empty?(gadget-selection(pane)))
	activate-win32-gadget(pane)
      end;
      #t;
    $NM-RCLICK =>
      // The selection is already set when we get here
      when (gadget-popup-menu-callback(pane))
	let handle :: <HWND> = window-handle(mirror);
	let (x, y) = pointer-position-within-window(handle);
	let target = #f;
	// Check to see if the user clicked within one of the items.  If so,
	// that's the target, otherwise the target is #f (i.e., background)
	with-stack-structure (hit :: <LPLV-HITTESTINFO>)
	  let point :: <LPPOINT> = hit.pt-value;
	  point.x-value := x;
	  point.y-value := y;
	  let index = SendMessage(handle, $LVM-HITTEST, 0, pointer-address(hit));
	  when (index >= 0
		& ~zero?(logand(hit.flags-value, $LVHT-ONITEM)))
	    target := gadget-items(pane)[index]
	  end
	end;
	distribute-popup-menu-callback(pane, target, x: x, y: y);
	#t
      end;
    $LVN-KEYDOWN =>
      when (gadget-key-press-callback(pane))
	let keydown :: <LPLV-KEYDOWN>
	  = make(<LPLV-KEYDOWN>, address: lParam);
	let vkey :: <integer> = keydown.wVKey-value;
	let keysym = virtual-key->keysym(vkey);
	distribute-key-press-callback(pane, keysym);
	#t
      end;
    otherwise =>
      next-method();
  end
end method handle-notify;
    

/// List controls

define sealed class <win32-list-control>
    (<win32-list-view-mixin>,
     <list-control>,
     <leaf-pane>)
end class <win32-list-control>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <list-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-list-control>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-list-control>));
define sealed domain initialize (<win32-list-control>);

define sealed domain gadget-selection-mode (<win32-list-control>);
define sealed domain gadget-items (<win32-list-control>);
define sealed domain gadget-items-setter (<sequence>, <win32-list-control>);
define sealed domain gadget-selection (<win32-list-control>);
define sealed domain gadget-selection-setter (<sequence>, <win32-list-control>);

define sealed method make-gadget-control
    (pane :: <win32-list-control>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND> = next-method();
  when (list-control-icon-function(pane))
    pane.%large-icons := make-image-list($SM-CXICON,   $SM-CYICON);
    pane.%small-icons := make-image-list($SM-CXSMICON, $SM-CYSMICON);
    SendMessage(handle, $LVM-SETIMAGELIST, $LVSIL-NORMAL, pointer-address(pane.%large-icons));
    SendMessage(handle, $LVM-SETIMAGELIST, $LVSIL-SMALL,  pointer-address(pane.%small-icons))
  end;
  // There seems to be no way to get List Views in their list view
  // _not_ to ellipsize their labels after only a few characters,
  // so use a single-column details view
  with-stack-structure (lvcol :: <LPLV-COLUMN>)
    lvcol.mask-value := %logior($LVCF-FMT, $LVCF-SUBITEM, $LVCF-TEXT, $LVCF-WIDTH);
    lvcol.iSubitem-value   := 0;
    lvcol.pszText-value    := $null-string;
    lvcol.cchTextMax-value := 0;
    lvcol.cx-value         := 100;
    lvcol.fmt-value        := $LVCFMT-LEFT;
    /* lvcol.fmt-value     := select (gadget-alignment(pane))
				#"left"   => $LVCFMT-LEFT;
				#"right"  => $LVCFMT-RIGHT;
				#"center" => $LVCFMT-CENTER;
			      end; */
    SendMessage(handle, $LVM-INSERTCOLUMN, 0, pointer-address(lvcol))
  end;
  handle
end method make-gadget-control;

define sealed method win32-list-view-options
    (pane :: <win32-list-control>, options :: <options-type>)
 => (options :: <options-type>)
  %logior(options,
	  // Note that $WS-EX-CLIENTEDGE subsumes $WS-BORDER
	  if (sheet-tab-stop?(pane)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
	  select (list-control-view(pane))
	    #"list"       => $LVS-REPORT;
	    #"small-icon" => $LVS-SMALLICON;
	    #"large-icon" => $LVS-ICON;
	  end,
	  $LVS-NOCOLUMNHEADER,		// because we use the report view...
	  /*--- For some reason, turning off scroll bars breaks things...
	  select (gadget-scroll-bars(pane))
	    #f, #"none" => $LVS-NOSCROLL;
	    otherwise   => 0;
	  end, */
	  select (gadget-selection-mode(pane))
	    #"single" => $LVS-SINGLESEL;
	    otherwise => 0;
	  end,
	  if (pane.%always-show-selection?) $LVS-SHOWSELALWAYS else 0 end)
end method win32-list-view-options;

define sealed method do-compose-space
    (pane :: <win32-list-control>, #key width, height)
 => (space-req :: <space-requirement>)
  let icon-height
    = if (list-control-icon-function(pane))
	if (list-control-view(pane) == #"large-icon")
	  GetSystemMetrics($SM-CYICON)
	else
	  GetSystemMetrics($SM-CYSMICON)
	end
      else
	0
      end;
  compose-space-for-list-box(pane,
			     width: width, height: height,
			     default-lines: $list-view-default-visible-lines,
			     minimum-lines: $list-view-minimum-visible-lines,
			     extra-height:  $list-view-extra-height,
			     extra-lines:   1,
			     icon-height:   icon-height)
end method do-compose-space;

define sealed method set-mirror-edges
    (_port :: <win32-port>, pane :: <win32-list-control>, mirror :: <window-mirror>,
     left :: <integer>, top :: <integer>, 
     right :: <integer>, bottom :: <integer>) => ()
  next-method();
  let handle :: <HWND> = window-handle(mirror);
  // Ensure that the single column width takes the full width of the window
  let text-style  = get-default-text-style(_port, pane);
  let char-width  = font-width(text-style, _port);
  with-stack-structure (lvcol :: <LPLV-COLUMN>)
    lvcol.mask-value := %logior($LVCF-WIDTH, $LVCF-FMT);
    lvcol.cx-value   := right - left - char-width;	// avoid hscroll bar...
    lvcol.fmt-value  := $LVCFMT-LEFT;
    /* lvcol.fmt-value  := select (gadget-alignment(pane))
			     #"left"   => $LVCFMT-LEFT;
			     #"right"  => $LVCFMT-RIGHT;
			     #"center" => $LVCFMT-CENTER;
			   end; */
    SendMessage(handle, $LVM-SETCOLUMN, 0, pointer-address(lvcol))
  end
end method set-mirror-edges;

define sealed method list-control-view-setter
    (view :: <list-control-view>, pane :: <win32-list-control>)
 => (view :: <list-control-view>)
  next-method();
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let old-style = GetWindowLong(handle, $GWL-STYLE);
    let new-style = %logior(%logand(old-style, %lognot($LVS-TYPEMASK)),
			    select (view)
			      #"list"       => $LVS-REPORT;
			      #"small-icon" => $LVS-SMALLICON;
			      #"large-icon" => $LVS-ICON;
			    end);
    SetWindowLong(handle, $GWL-STYLE, new-style)
  end;
  view
end method list-control-view-setter;


define sealed class <win32-list-item> (<list-item>)
  sealed slot %list :: false-or(<list-control>) = #f;
end class <win32-list-item>;

define sealed domain make (singleton(<win32-list-item>));
define sealed domain initialize (<win32-list-item>);

define sealed method do-make-item 
    (pane :: <win32-list-control>, class == <list-item>, #key object)
 => (item :: <win32-list-item>)
  make(<win32-list-item>, object: object)
end method do-make-item;

define sealed method do-find-item
    (pane :: <win32-list-control>, object, #key)
 => (node :: false-or(<win32-list-item>))
  let key  = gadget-value-key(pane);
  let test = gadget-test(pane);
  let the-key = key(object);
  block (return)
    for (item :: <win32-list-item> in pane.%items)
      when (test(key(item-object(item)), the-key))
	return(item)
      end
    end;
    #f
  end
end method do-find-item;

define sealed method do-add-item
    (pane :: <win32-list-control>, item :: <win32-list-item>, #key after) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let items  = pane.%items;
    let index  = (after & position(items, after)) | size(items);
    insert-at!(items, item, index);
    item.%list := pane;
    do-update-item(pane, handle, item, index, message: $LVM-INSERTITEM);
    when (after)
      note-gadget-selection-changed(pane)
    end
  end
end method do-add-item;

define sealed method do-update-item
    (pane :: <win32-list-control>, handle :: <HWND>, item :: <win32-list-item>, index,
     #key message = $LVM-SETITEM, label = $unsupplied, icon = $unsupplied) => ()
  let index :: <integer> = index | size(pane.%items) - 1;
  let label-function = gadget-label-key(pane);
  let icon-function  = list-control-icon-function(pane);
  let object = item-object(item);
  let label
    = if (supplied?(label)) label
      else (label-function & label-function(object)) | "" end;
  let (small-icon, large-icon)
    = if (supplied?(icon)) values(icon, icon)
      elseif (icon-function) icon-function(object)
      else values(#f, #f) end;
  with-stack-structure (lvitem :: <LPLV-ITEM>)
    lvitem.mask-value
      := %logior(if (label) $LVIF-TEXT  else 0 end,
		 if (small-icon | large-icon) $LVIF-IMAGE else 0 end,
		 $LVIF-STATE);
    lvitem.iItem-value     := index;
    lvitem.iSubItem-value  := 0;
    lvitem.state-value     := 0;
    lvitem.stateMask-value := $LVIS-SELECTED;
    when (label)
      lvitem.pszText-value    := label;
      lvitem.cchTextMax-value := size(label)
    end;
    when (small-icon | large-icon)
      case
	~small-icon => small-icon := large-icon;
	~large-icon => large-icon := small-icon;
      end;
      let small-index
	= find-image(small-icon, pane.%icons, pane.%small-icons);
      let large-index
	= find-image(large-icon, pane.%icons, pane.%large-icons);
      assert(small-index == large-index,
	     "Small and large icons must have the same index");
      lvitem.iImage-value := small-index
    end;
    SendMessage(handle, message, 0, pointer-address(lvitem))
  end
end method do-update-item;

define sealed method do-remove-item
    (pane :: <win32-list-control>, item :: <win32-list-item>) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let items  = pane.%items;
    let index  = position(items, item);
    when (index)
      remove-at!(items, index);
      SendMessage(handle, $LVM-DELETEITEM, index, 0)
    end;
    item.%list := #f;
    note-gadget-selection-changed(pane)
  end
end method do-remove-item;

define sealed method item-label-setter
    (label :: false-or(<string>), item :: <win32-list-item>) => (label :: false-or(<string>))
  let pane   = item.%list;
  let mirror = pane & sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let index  = position(pane.%items, item);
    do-update-item(pane, handle, item, index,
		   message: $LVM-SETITEM, label: label)
  end;
  label
end method item-label-setter;

define sealed method item-icon-setter
    (icon :: false-or(<image>), item :: <win32-list-item>) => (icon :: false-or(<image>))
  let pane   = item.%list;
  let mirror = pane & sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let index  = position(pane.%items, item);
    do-update-item(pane, handle, item, index,
		   message: $LVM-SETITEM, icon: icon)
  end;
  icon
end method item-icon-setter;


/// Table controls

define sealed class <win32-table-control>
    (<win32-list-view-mixin>,
     <table-control>,
     <leaf-pane>)
end class <win32-table-control>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <table-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-table-control>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-table-control>));
define sealed domain initialize (<win32-table-control>);

define sealed domain gadget-selection-mode (<win32-table-control>);
define sealed domain gadget-items (<win32-table-control>);
define sealed domain gadget-items-setter (<sequence>, <win32-table-control>);
define sealed domain gadget-selection (<win32-table-control>);
define sealed domain gadget-selection-setter (<sequence>, <win32-table-control>);

define sealed method make-gadget-control
    (pane :: <win32-table-control>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND> = next-method();
  when (table-control-icon-function(pane))
    pane.%large-icons := make-image-list($SM-CXICON,   $SM-CYICON);
    pane.%small-icons := make-image-list($SM-CXSMICON, $SM-CYSMICON);
    SendMessage(handle, $LVM-SETIMAGELIST, $LVSIL-NORMAL, pointer-address(pane.%large-icons));
    SendMessage(handle, $LVM-SETIMAGELIST, $LVSIL-SMALL,  pointer-address(pane.%small-icons))
  end;
  // Now add the columns
  with-stack-structure (lvcol :: <LPLV-COLUMN>)
    lvcol.mask-value := %logior($LVCF-FMT, $LVCF-SUBITEM, $LVCF-TEXT, $LVCF-WIDTH);
    for (i :: <integer> from 0,
	 column :: <table-column> in table-control-columns(pane))
      lvcol.iSubitem-value   := i;
      lvcol.pszText-value    := table-column-heading(column);
      lvcol.cchTextMax-value := size(table-column-heading(column));
      lvcol.cx-value         := table-column-width(column);
      lvcol.fmt-value        := select (table-column-alignment(column))
				  #"left"   => $LVCFMT-LEFT;
				  #"right"  => $LVCFMT-RIGHT;
				  #"center" => $LVCFMT-CENTER;
				end;
      SendMessage(handle, $LVM-INSERTCOLUMN, i, pointer-address(lvcol))
    end
  end;
  handle
end method make-gadget-control;

define sealed method win32-list-view-options
    (pane :: <win32-table-control>, options :: <options-type>)
 => (options :: <options-type>)
  %logior(options,
	  // Note that $WS-EX-CLIENTEDGE subsumes $WS-BORDER
	  if (sheet-tab-stop?(pane)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
	  select (table-control-view(pane))
	    #"table"      => $LVS-REPORT;
	    #"list"       => $LVS-LIST;
	    #"small-icon" => $LVS-SMALLICON;
	    #"large-icon" => $LVS-ICON;
	  end,
	  /*--- For some reason, turning off scroll bars breaks things...
	  select (gadget-scroll-bars(pane))
	    #f, #"none" => $LVS-NOSCROLL;
	    otherwise   => 0;
	  end, */
	  select (gadget-selection-mode(pane))
	    #"single" => $LVS-SINGLESEL;
	    otherwise => 0;
	  end,
	  if (pane.%always-show-selection?) $LVS-SHOWSELALWAYS else 0 end)
end method win32-list-view-options;

define sealed method do-compose-space
    (pane :: <win32-table-control>, #key width, height)
 => (space-req :: <space-requirement>)
  let icon-height
    = if (table-control-icon-function(pane))
	if (table-control-view(pane) == #"large-icon")
	  GetSystemMetrics($SM-CYICON)
	else
	  GetSystemMetrics($SM-CYSMICON)
	end
      else
	0
      end;
  let extra-height
    = if (table-control-view(pane) == #"table")
	$list-view-extra-height + GetSystemMetrics($SM-CYHSCROLL) * 2
      else
	$list-view-extra-height
      end;
  compose-space-for-list-box(pane,
			     width: width, height: height,
			     default-lines: $list-view-default-visible-lines,
			     minimum-lines: $list-view-minimum-visible-lines,
			     extra-height:  extra-height,
			     extra-lines:   1,
			     icon-height:   icon-height)
end method do-compose-space;

define sealed method table-control-view-setter
    (view :: <table-control-view>, pane :: <win32-table-control>)
 => (view :: <table-control-view>)
  next-method();
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let old-style = GetWindowLong(handle, $GWL-STYLE);
    let new-style = %logior(%logand(old-style, %lognot($LVS-TYPEMASK)),
			    select (view)
			      #"table"      => $LVS-REPORT;
			      #"list"       => $LVS-LIST;
			      #"small-icon" => $LVS-SMALLICON;
			      #"large-icon" => $LVS-ICON;
			    end);
    SetWindowLong(handle, $GWL-STYLE, new-style)
  end;
  view
end method table-control-view-setter;


define sealed method handle-notify
    (pane :: <win32-table-control>, mirror :: <window-mirror>,
     wParam :: <wparam-type>, lParam :: <lparam-type>,
     id :: <integer>, code :: <integer>)
 => (handled? :: <boolean>)
  ignore(wParam);
  select (code)
    $LVN-COLUMNCLICK =>
      let nmlistview :: <LPNM-LISTVIEW>
	= make(<LPNM-LISTVIEW>, address: lParam);
      let column :: <integer> = nmlistview.iSubitem-value;
      let column = table-control-columns(pane)[column];
      when (table-column-callback(column))
	distribute-column-click-callback(pane, column)
      end;
      #t;
    otherwise =>
      next-method();
  end
end method handle-notify;


define sealed class <win32-table-item> (<table-item>)
  sealed slot %table :: false-or(<table-control>) = #f;
end class <win32-table-item>;

define sealed domain make (singleton(<win32-table-item>));
define sealed domain initialize (<win32-table-item>);

define sealed method do-make-item 
    (pane :: <win32-table-control>, class == <table-item>, #key object)
 => (item :: <win32-table-item>)
  make(<win32-table-item>, object: object)
end method do-make-item;

define sealed method do-find-item
    (pane :: <win32-table-control>, object, #key)
 => (node :: false-or(<win32-table-item>))
  let key  = gadget-value-key(pane);
  let test = gadget-test(pane);
  let the-key = key(object);
  block (return)
    for (item :: <win32-table-item> in pane.%items)
      when (test(key(item-object(item)), the-key))
	return(item)
      end
    end;
    #f
  end
end method do-find-item;

define sealed method do-add-item
    (pane :: <win32-table-control>, item :: <win32-table-item>, #key after) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let items  = pane.%items;
    let index  = (after & position(items, after)) | size(items);
    insert-at!(items, item, index);
    item.%table := pane;
    do-update-item(pane, handle, item, index, message: $LVM-INSERTITEM);
    when (after)
      note-gadget-selection-changed(pane)
    end
  end
end method do-add-item;

define sealed method do-update-item
    (pane :: <win32-table-control>, handle :: <HWND>, item :: <win32-table-item>, index,
     #key message = $LVM-SETITEM, label = $unsupplied, icon = $unsupplied) => ()
  let index :: <integer> = index | size(pane.%items) - 1;
  let columns = table-control-columns(pane);
  let column :: <table-column> = columns[0];
  let generator = table-column-generator(column);
  let icon-function  = table-control-icon-function(pane);
  let object = item-object(item);
  let label
    = if (supplied?(label)) label
      else gadget-item-label(pane, generator(object)) end;
  let (small-icon, large-icon)
    = if (supplied?(icon)) values(icon, icon)
      elseif (icon-function) icon-function(object)
      else values(#f, #f) end;
  with-stack-structure (lvitem :: <LPLV-ITEM>)
    // First insert the "main" item
    lvitem.mask-value
      := %logior(if (label) $LVIF-TEXT  else 0 end,
		 if (small-icon | large-icon) $LVIF-IMAGE else 0 end,
		 $LVIF-STATE);
    lvitem.iItem-value     := index;
    lvitem.iSubItem-value  := 0;
    lvitem.state-value     := 0;
    lvitem.stateMask-value := $LVIS-SELECTED;
    when (label)
      lvitem.pszText-value    := label;
      lvitem.cchTextMax-value := size(label)
    end;
    when (small-icon | large-icon)
      case
	~small-icon => small-icon := large-icon;
	~large-icon => large-icon := small-icon;
      end;
      let small-index
	= find-image(small-icon, pane.%icons, pane.%small-icons);
      let large-index
	= find-image(large-icon, pane.%icons, pane.%large-icons);
      assert(small-index == large-index,
	     "Small and large icons must have the same index");
      lvitem.iImage-value := small-index
    end;
    SendMessage(handle, message, 0, pointer-address(lvitem));
    // Now use LVM_SETITEM to insert the rest of the subitems
    let n-columns = size(columns);
    lvitem.mask-value := $LVIF-TEXT;
    for (i :: <integer> from 1 below n-columns)
      let column :: <table-column> = columns[i];
      let generator = table-column-generator(column);
      let label = gadget-item-label(pane, generator(object));
      lvitem.iSubItem-value   := i;
      lvitem.pszText-value    := label;
      lvitem.cchTextMax-value := size(label);
      SendMessage(handle, $LVM-SETITEM, 0, pointer-address(lvitem))
    end
  end
end method do-update-item;

define sealed method do-remove-item
    (pane :: <win32-table-control>, item :: <win32-table-item>) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let items  = pane.%items;
    let index  = position(items, item);
    when (index)
      remove-at!(items, index);
      SendMessage(handle, $LVM-DELETEITEM, index, 0)
    end;
    item.%table := #f;
    note-gadget-selection-changed(pane)
  end
end method do-remove-item;

define sealed method item-label-setter
    (label :: false-or(<string>), item :: <win32-table-item>) => (label :: false-or(<string>))
  let pane   = item.%table;
  let mirror = pane & sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let index  = position(pane.%items, item);
    do-update-item(pane, handle, item, index,
		   message: $LVM-SETITEM, label: label)
  end;
  label
end method item-label-setter;

define sealed method item-icon-setter
    (icon :: false-or(<image>), item :: <win32-table-item>) => (icon :: false-or(<image>))
  let pane   = item.%table;
  let mirror = pane & sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let index  = position(pane.%items, item);
    do-update-item(pane, handle, item, index,
		   message: $LVM-SETITEM, icon: icon)
  end;
  icon
end method item-icon-setter;


define sealed method do-add-column
    (pane :: <win32-table-control>, column :: <table-column>, index :: <integer>) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    with-stack-structure (lvcol :: <LPLV-COLUMN>)
      lvcol.mask-value       := %logior($LVCF-FMT, $LVCF-SUBITEM, $LVCF-TEXT, $LVCF-WIDTH);
      lvcol.iSubitem-value   := index;
      lvcol.pszText-value    := table-column-heading(column);
      lvcol.cchTextMax-value := size(table-column-heading(column));
      lvcol.cx-value         := table-column-width(column);
      lvcol.fmt-value        := select (table-column-alignment(column))
				  #"left"   => $LVCFMT-LEFT;
				  #"right"  => $LVCFMT-RIGHT;
				  #"center" => $LVCFMT-CENTER;
				end;
      SendMessage(handle, $LVM-INSERTCOLUMN, index, pointer-address(lvcol))
    end
  end
end method do-add-column;

define sealed method do-remove-column
    (pane :: <win32-table-control>, index :: <integer>) => ()
  let mirror = sheet-direct-mirror(pane);
  let handle = window-handle(mirror);
  SendMessage(handle, $LVM-DELETECOLUMN, index, 0)
end method do-remove-column;


/// Tree controls

define sealed class <win32-tree-control>
    (<win32-subclassed-gadget-mixin>,
     <win32-control-mixin>,
     <tree-control>,
     <leaf-pane>)
  sealed constant slot %nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  // These next two are for maintaining the Tree View's image list
  sealed constant slot %icons :: <object-table> = make(<table>);
  sealed slot %small-icons :: false-or(<HIMAGELIST>) = #f;
  sealed constant slot %always-show-selection? :: <boolean> = #f,
    init-keyword: always-show-selection?:;
  // Expand/contract nodes only when the little icon is clicked on,
  // never when double clicking on a node
  sealed constant slot %use-buttons-only? :: <boolean> = #t,
    init-keyword: use-buttons-only?:;
end class <win32-tree-control>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <tree-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-tree-control>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-tree-control>));
define sealed domain initialize (<win32-tree-control>);

define sealed domain gadget-selection-mode (<win32-tree-control>);
define sealed domain gadget-items (<win32-tree-control>);
define sealed domain gadget-items-setter (<sequence>, <win32-tree-control>);
define sealed domain gadget-selection (<win32-tree-control>);
define sealed domain gadget-selection-setter (<sequence>, <win32-tree-control>);

define sealed method make-gadget-control
    (pane :: <win32-tree-control>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(pane),
		     $WC-TREEVIEW,
		     "",
		     win32-tree-view-options(pane, options),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindow (Tree View)", handle);
  when (tree-control-icon-function(pane))
    pane.%small-icons := make-image-list($SM-CXSMICON, $SM-CYSMICON);
    SendMessage(handle, $TVM-SETIMAGELIST, $TVSIL-NORMAL, pointer-address(pane.%small-icons))
  end;
  handle
end method make-gadget-control;

define sealed method win32-tree-view-options
    (pane :: <win32-tree-control>, options :: <options-type>)
 => (options :: <options-type>)
  %logior(options,
	  // Note that $WS-EX-CLIENTEDGE subsumes $WS-BORDER
	  if (sheet-tab-stop?(pane)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
	  if (tree-control-show-buttons?(pane))    $TVS-HASBUTTONS    else 0 end,
	  if (tree-control-show-edges?(pane))      $TVS-HASLINES      else 0 end,
	  if (tree-control-show-root-edges?(pane)) $TVS-LINESATROOT   else 0 end,
	  if (pane.%always-show-selection?)        $TVS-SHOWSELALWAYS else 0 end)
end method win32-tree-view-options;

define sealed method destroy-mirror 
    (_port :: <win32-port>, pane :: <win32-tree-control>, mirror :: <window-mirror>) => ()
  when (pane.%small-icons)
    ImageList-Destroy(pane.%small-icons);
    pane.%small-icons := #f
  end;
  release-images(pane.%icons);
  next-method()
end method destroy-mirror;

define sealed method do-compose-space
    (pane :: <win32-tree-control>, #key width, height)
 => (space-req :: <space-requirement>)
  let icon-height
    = if (tree-control-icon-function(pane))
	GetSystemMetrics($SM-CYSMICON)
      else
	0
      end;
  compose-space-for-list-box(pane,
			     width: width, height: height,
			     default-lines: $tree-view-default-visible-lines,
			     minimum-lines: $tree-view-minimum-visible-lines,
			     extra-height:  $tree-view-extra-height,
			     extra-lines:   1,
			     icon-height:   icon-height)
end method do-compose-space;

// Build the items for the first time when the sheet is fully attached
define sealed method note-sheet-attached
    (pane :: <win32-tree-control>) => ()
  next-method();
  //--- Too bad we've lost the 'value:' initarg by the time we get here
  note-tree-control-roots-changed(pane)
end method note-sheet-attached;

define sealed method note-gadget-selection-changed
    (pane :: <win32-tree-control>) => ()
  next-method();
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle    = window-handle(mirror);
    let selection = gadget-selection(pane);
    let items     = gadget-items(pane);
    let nodes     = pane.%nodes;
    //--- We can only have single selection tree controls, unfortunately
    let selected-index = ~empty?(selection) & selection[0];
    let selected-node  = selected-index & nodes[selected-index];
    if (selected-node & selected-node.%handle)
      let item-handle = selected-node.%handle;
      SendMessage(handle, $TVM-SELECTITEM, $TVGN-CARET, pointer-address(item-handle));
      unless (*port-did-it?*)
	when (gadget-keep-selection-visible?(pane))
	  SendMessage(handle, $TVM-ENSUREVISIBLE, 0, pointer-address(item-handle))
	end
      end
    else
      let item-handle = null-pointer(<LPTV-ITEM>);
      SendMessage(handle, $TVM-SELECTITEM, $TVGN-CARET, pointer-address(item-handle))
    end;
/*
    //--- Surely there's a faster way to do this?
    //--- Does this happen when we click, or just when we set 'gadget-selection'?
    with-stack-structure (tvitem :: <LPTV-ITEM>)
      tvitem.mask-value      := $TVIF-STATE;
      tvitem.stateMask-value := $TVIS-SELECTED;
      for (node :: <win32-tree-node> in pane.%nodes)
	let index = position(items, node-object(node));
	tvitem.hItem-value   := node.%handle;
	tvitem.state-value   := if (member?(index, selection)) $TVIS-SELECTED else 0 end;
	SendMessage(handle, $TVM-SETITEM, 0, pointer-address(tvitem))
      end
    end
*/
  end
end method note-gadget-selection-changed;

define sealed method note-tree-control-roots-changed
     (pane :: <win32-tree-control>, #key value = $unsupplied) => ()
  with-busy-cursor (pane)
    next-method();
    gadget-selection(pane) := #[];
    pane.%nodes.size := 0;
    let mirror = sheet-direct-mirror(pane);
    when (mirror)
      let handle = window-handle(mirror);
      with-delayed-drawing (handle)
	SendMessage(handle, $TVM-DELETEITEM, 0, pointer-address($TVI-ROOT));
	let roots = tree-control-roots(pane);
	let children-predicate = tree-control-children-predicate(pane);
	let children-generator = tree-control-children-generator(pane);
	local method add-one (node, object, depth) => ()
		let child-node = make-node(pane, object);
		add-node(pane, node, child-node, setting-roots?: #t);
		when (depth > 0 & children-predicate(object))
		  for (child in children-generator(object))
		    add-one(child-node, child, depth - 1)
		  end;
		  node-state(child-node) := #"expanded";
		  do-expand-node(pane, child-node)
		end
	      end method;
	for (root in roots)
	  duim-debug-message("Adding root object %= to tree %=", root, pane);
	  add-one(pane, root, tree-control-initial-depth(pane))
	end;
	let items = gadget-items(pane);
	// Try to preserve the old value and selection
	select (gadget-selection-mode(pane))
	  #"single", #"multiple" =>
	    unless (empty?(items))
	      let index = supplied?(value) & position(items, value);
	      if (index)
		gadget-selection(pane) := vector(index)
	      else
		gadget-selection(pane) := #[0]
	      end
	    end;
	  /* #"multiple" =>		//---*** doesn't work in Win32 Tree Views...
	       let selection :: <stretchy-object-vector> = make(<stretchy-vector>);
	       when (supplied?(value))
		 for (v in value)
		   let index = position(items, v);
		   when (index)
		     add!(selection, index)
		   end
		 end
	       end;
	       unless (empty?(selection))
		 gadget-selection(pane) := selection
	       end; */
	  #"none" =>
	    #f;
	end
      end
    end
  end
end method note-tree-control-roots-changed;


define sealed method handle-control-message
    (pane :: <win32-tree-control>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  maybe-handle-keyboard-interrupt(pane, message, wParam);
  select (message)
    // When we get a WM_COMMAND, it's probably for something like a menu
    // selection in a pop-up menu parented into the control or for accelerators
    // typed  while the focus is within the control.  Handle it.
    //---*** We need a coherent theory about what to do here
    $WM-COMMAND =>
      handle-wm-command(pane, wParam, lParam);
    // Normally in Windows, a double-click in a tree control will expand
    // the node even if something else (e.g., "activate") will also happen.
    // We stop that from happening unless (1) there are no expand/contract
    // buttons, or (2) the user explicitly asked for the Windows behavior.
    $WM-LBUTTONDBLCLK =>
      let mirror = sheet-direct-mirror(pane);
      if (~mirror
	  | ~tree-control-show-buttons?(pane)
	  | ~pane.%use-buttons-only?)
	#f
      else
	let (target, x, y, on-item?)
	  = force-tree-control-selection(pane, mirror);
	ignore(target, x, y);
	when (on-item? & ~empty?(gadget-selection(pane)))
	  activate-win32-gadget(pane)
	end;
	#t
      end;
    otherwise =>
      #f;
  end
end method handle-control-message;

define sealed method handle-notify
    (pane :: <win32-tree-control>, mirror :: <window-mirror>,
     wParam :: <wparam-type>, lParam :: <lparam-type>,
     id :: <integer>, code :: <integer>)
 => (handled? :: <boolean>)
  ignore(wParam);
  select (code)
    //---*** Hack for the moment to support both Unicode and ordinary events,
    //---*** until we better understand the problem.
    $TVN-ITEMEXPANDINGA, $TVN-ITEMEXPANDINGW =>
      let handle :: <HWND> = window-handle(mirror);
      let (x, y) = pointer-position-within-window(handle);
      let on-button?
	= ~tree-control-show-buttons?(pane)	// no buttons, OK to expand/contract
	  | ~pane.%use-buttons-only?		// hacker says double-clicking is OK
	  | with-stack-structure (hit :: <LPTV-HITTESTINFO>)
	      // Otherwise, only expand/contract when user clicks on the button
	      let point :: <LPPOINT> = hit.pt-value;
	      point.x-value := x;
	      point.y-value := y;
	      let item-handle :: <HTREEITEM>
		= make(<HTREEITEM>,
		       address: SendMessage(handle, $TVM-HITTEST, 0, pointer-address(hit)));
	      ~null-pointer?(item-handle)
	      & ~zero?(logand(hit.flags-value, $TVHT-ONITEMBUTTON))
	    end;
      when (on-button?)
	let nmtreeview :: <LPNM-TREEVIEW>
	  = make(<LPNM-TREEVIEW>, address: lParam);
	let tvitem :: <LPTV-ITEM> = nmtreeview.itemNew-value;
	let action = nmtreeview.action-value;
	let item-handle :: <HTREEITEM> = tvitem.hItem-value;
	let node = item-handle->tree-node(pane, item-handle);
	when (node)
	  dynamic-bind (*port-did-it?* = #t)
	    select (action)
	      $TVE-EXPAND =>
		expand-node(pane, node);
	      $TVE-COLLAPSE =>
		contract-node(pane, node);
	      $TVE-TOGGLE =>
		if (node-state(node) == #"expanded")
		  contract-node(pane, node)
		else
		  expand-node(pane, node)
		end;
	      $TVE-COLLAPSERESET =>
		contract-node(pane, node);
		node-children(node) := #[];
	    end;
	    distribute-node-state-changed-callback(pane, node);
	  end
	end
      end;
      // Don't let Windows expand/contract, because we've already done
      // whatever it is that needs to be done
      #t;
    //---*** Hack for the moment to support both Unicode and ordinary events,
    //---*** until we better understand the problem.
    $TVN-SELCHANGEDA, $TVN-SELCHANGEDW =>
      let nmtreeview :: <LPNM-TREEVIEW>
        = make(<LPNM-TREEVIEW>, address: lParam);
      let old :: <LPTV-ITEM> = nmtreeview.itemOld-value;
      let new :: <LPTV-ITEM> = nmtreeview.itemNew-value;
      let old-handle :: <HTREEITEM> = old.hItem-value;
      let new-handle :: <HTREEITEM> = new.hItem-value;
      let old-node = item-handle->tree-node(pane, old-handle);
      let new-node = item-handle->tree-node(pane, new-handle);
      let old-index = old-node & position(gadget-items(pane), node-object(old-node));
      let new-index = new-node & position(gadget-items(pane), node-object(new-node));
      dynamic-bind (*port-did-it?* = #t)
	select (gadget-selection-mode(pane))
	  #"single", #"multiple" =>
	    when (new-index)
	      let selection = vector(new-index);
	      //--- Was: 'distribute-selection-changed-callback(pane, selection)'
	      gadget-selection(pane, do-callback?: #t) := selection
	    end;
	  /* #"multiple" =>	//---*** doesn't work in Win32 Tree Views...
	       // This code intentionally copies the selection...
	       let selection = if (old-index) remove(gadget-selection(pane), old-index)
			       else gadget-selection(pane) end;
	       let selection = if (new-index) add!(selection, new-index)
			       else selection end;
	       //--- Was: 'distribute-selection-changed-callback(pane, selection)'
	       gadget-selection(pane, do-callback?: #t) := selection; */
	  #"none" =>
	    #f;
	end
      end;
      #t;
    $TVN-GETDISPINFO =>
      //---*** Do we need to do this?
      #t;
    $NM-DBLCLK =>
      let (target, x, y, on-item?)
	= force-tree-control-selection(pane, mirror);
      ignore(target, x, y);
      when (on-item? & ~empty?(gadget-selection(pane)))
	activate-win32-gadget(pane)
      end;
      #t;
    $NM-RCLICK =>
      let (target, x, y, on-item?)
	= force-tree-control-selection(pane, mirror);
      when (on-item? & gadget-popup-menu-callback(pane))
	distribute-popup-menu-callback(pane, target, x: x, y: y);
	#t
      end;
    $TVN-KEYDOWN =>
      when (gadget-key-press-callback(pane))
	let keydown :: <LPTV-KEYDOWN>
	  = make(<LPTV-KEYDOWN>, address: lParam);
	let vkey :: <integer> = keydown.wVKey-value;
	let keysym = virtual-key->keysym(vkey);
	distribute-key-press-callback(pane, keysym);
	#t
      end;
    otherwise =>
      next-method();
  end
end method handle-notify;

// For some notifications, Windows doesn't tell what got clicked on,
// so we have to do it ourselves
define sealed method force-tree-control-selection
    (pane :: <win32-tree-control>, mirror :: <window-mirror>)
 => (target, x :: <integer>, y :: <integer>, on-item? :: <boolean>)
  let handle :: <HWND> = window-handle(mirror);
  let (x, y)   = pointer-position-within-window(handle);
  let target   = #f;
  let on-item? = #f;
  // Check to see if the user clicked within one of the items.  If so,
  // set the selection to that item.
  with-stack-structure (hit :: <LPTV-HITTESTINFO>)
    let point :: <LPPOINT> = hit.pt-value;
    point.x-value := x;
    point.y-value := y;
    let item-handle :: <HTREEITEM>
      = make(<HTREEITEM>,
	     address: SendMessage(handle, $TVM-HITTEST, 0, pointer-address(hit)));
    when (~null-pointer?(item-handle)
	  & ~zero?(logand(hit.flags-value, $TVHT-ONITEM)))
      let node  = item-handle->tree-node(pane, item-handle);
      let index = node & position(gadget-items(pane), node-object(node));
      on-item? := #t;
      dynamic-bind (*port-did-it?* = #t)
	select (gadget-selection-mode(pane))
	  #"single", #"multiple" =>
	    when (index)
	      target := gadget-items(pane)[index];
	      when (empty?(gadget-selection(pane))	// be robust...
		    | gadget-selection(pane)[0] ~= index)
		//--- Was: 'distribute-selection-changed-callback(pane, selection)'
		gadget-selection(pane, do-callback?: #t) := vector(index)
	      end
	    end;
	  /* #"multiple" =>	//---*** doesn't work in Win32 Tree Views...
	       when (index)
		 target := gadget-items(pane)[index];
		 //--- Was: 'distribute-selection-changed-callback(pane, selection)'
		 gadget-selection(pane, do-callback?: #t) := vector(index)
	       end; */
	  #"none" =>
	    #f;
	end
      end
    end
  end;
  values(target, x, y, on-item?)
end method force-tree-control-selection;


define sealed class <win32-tree-node> (<tree-node>)
  sealed slot %handle :: false-or(<HTREEITEM>) = #f;
  sealed slot %tree   :: false-or(<tree-control>) = #f;
end class <win32-tree-node>;

define sealed domain make (singleton(<win32-tree-node>));
define sealed domain initialize (<win32-tree-node>);

define function item-handle->tree-node
    (pane :: <win32-tree-control>, item-handle :: <HTREEITEM>)
 => (node :: false-or(<tree-node>))
  unless (null-pointer?(item-handle))
    find-value(pane.%nodes,
	       method (node) node.%handle = item-handle end)
  end
end function item-handle->tree-node;

define sealed method do-make-node 
    (pane :: <win32-tree-control>, class == <tree-node>, #key object)
 => (item :: <win32-tree-node>)
  make(<win32-tree-node>, 
       object: object)
end method do-make-node;

define sealed method do-find-node
    (pane :: <win32-tree-control>, object, #key node: parent-node)
 => (node :: false-or(<win32-tree-node>))
  let key  = gadget-value-key(pane);
  let test = gadget-test(pane);
  let the-key = key(object);
  block (return)
    for (node :: <win32-tree-node> in pane.%nodes)
      when (test(key(node-object(node)), the-key))
	// Is it a child of the requested node?
	when (~parent-node | member?(node, node-children(parent-node)))
	  return(node)
	end
      end
    end;
    #f
  end
end method do-find-node;

define sealed method do-add-node
    (pane :: <win32-tree-control>, parent, node :: <win32-tree-node>, 
     #key after) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    add!(pane.%nodes, node);
    node.%tree := pane;
    let label-function = gadget-label-key(pane);
    let icon-function  = tree-control-icon-function(pane);
    let object = node-object(node);
    let label  = (label-function & label-function(object)) | "";
    let (icon, selected-icon)
      = if (icon-function) icon-function(object) else values(#f, #f) end;
    selected-icon := selected-icon | icon;	// a favor for our users
    let has-children? = tree-control-children-predicate(pane)(object);
    let root-node?    = ~instance?(parent, <tree-node>);
    with-stack-structure (tvinsert :: <LPTV-INSERTSTRUCT>)
      let tvitem :: <LPTV-ITEM> = tvinsert.item-value;
      tvitem.mask-value
        := %logior(if (label) $TVIF-TEXT  else 0 end,
		   if (icon)  $TVIF-IMAGE else 0 end,
		   if (selected-icon) $TVIF-SELECTEDIMAGE else 0 end,
		   $TVIF-STATE,
		   $TVIF-CHILDREN);
      tvitem.state-value     := 0;
      tvitem.stateMask-value := $TVIS-SELECTED;
      when (label)
	tvitem.pszText-value    := label;
	tvitem.cchTextMax-value := size(label)
      end;
      when (icon)
	tvitem.iImage-value := find-image(icon, pane.%icons, pane.%small-icons)
      end;
      when (selected-icon)
	tvitem.iSelectedImage-value := find-image(selected-icon, pane.%icons, pane.%small-icons)
      end;
      tvitem.cChildren-value := if (has-children?) 1 else 0 end;
      if (root-node?)
	tvinsert.hParent-value := $TVI-ROOT
      else
	tvinsert.hParent-value := parent.%handle
      end;
      if (after)
	tvinsert.hInsertAfter-value := after.%handle
      else
	tvinsert.hInsertAfter-value := $TVI-LAST
      end;
      duim-debug-message("Adding %s for object %= to tree %=:\n"
			 "  [label %=, has-children? %=]", 
			 if (root-node?) "root" else "node" end,
			 object, pane, label, has-children?);
      let item-handle :: <HTREEITEM>
	= make(<HTREEITEM>,
	       address: SendMessage(handle, $TVM-INSERTITEM, 0, pointer-address(tvinsert)));
      node.%handle := item-handle
    end
  end
end method do-add-node;

define sealed method do-add-nodes
    (pane :: <win32-tree-control>, parent, nodes :: <sequence>, #key after) => ()
  let selected-nodes = gadget-selected-nodes(pane);
  gadget-selection(pane) := #[];
  for (node in nodes)
    add-node(pane, parent, node, after: after)
  end;
  gadget-selection(pane) := compute-gadget-selection(pane, selected-nodes)
end method do-add-nodes;

define sealed method do-remove-node
    (pane :: <win32-tree-control>, node :: <win32-tree-node>) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let item-handle :: <HTREEITEM> = node.%handle;
    remove!(pane.%nodes, node);
    node.%tree := #f;
    SendMessage(handle, $TVM-DELETEITEM, 0, pointer-address(item-handle))
  end
end method do-remove-node;

define sealed method node-children-setter
    (children :: <sequence>, node :: <win32-tree-node>)
 => (children :: <sequence>)
  let pane   = node.%tree;
  let mirror = pane & sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let old-children   = node-children(node);
    let selected-nodes = gadget-selected-nodes(pane);
    gadget-selection(pane) := #[];
    for (old in old-children)
      let item-handle :: <HTREEITEM> = old.%handle;
      remove!(pane.%nodes, old);
      SendMessage(handle, $TVM-DELETEITEM, 0, pointer-address(item-handle))
    end;
    node-children(node).size := 0;
    for (new in children)
      add-node(pane, node, new)
    end;
    gadget-selection(pane) := compute-gadget-selection(pane, selected-nodes)
  end;
  children
end method node-children-setter;

define sealed method node-label-setter
    (label :: false-or(<string>), node :: <win32-tree-node>) => (label :: false-or(<string>))
  let pane   = node.%tree;
  let mirror = pane & sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let item-handle :: <HTREEITEM> = node.%handle;
    with-stack-structure (tvitem :: <LPTV-ITEM>)
      tvitem.mask-value := $TVIF-TEXT;
      when (label)
	tvitem.pszText-value    := label;
	tvitem.cchTextMax-value := size(label)
      end;
      SendMessage(handle, $TVM-SETITEM, 0, pointer-address(tvitem))
    end
  end;
  label
end method node-label-setter;

define sealed method node-icon-setter
    (icon :: false-or(<image>), node :: <win32-tree-node>) => (icon :: false-or(<image>))
  let pane   = node.%tree;
  let mirror = pane & sheet-direct-mirror(pane);
  when (mirror)
    let handle = window-handle(mirror);
    let item-handle :: <HTREEITEM> = node.%handle;
    with-stack-structure (tvitem :: <LPTV-ITEM>)
      tvitem.mask-value := $TVIF-IMAGE;
      when (icon)
	tvitem.iImage-value := find-image(icon, pane.%icons, pane.%small-icons)
      end;
      SendMessage(handle, $TVM-SETITEM, 0, pointer-address(tvitem))
    end
  end;
  icon
end method node-icon-setter;

define sealed method gadget-selected-nodes
    (pane :: <win32-tree-control>) => (nodes :: <sequence>)
  let nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let selection = gadget-selection(pane);
  let items     = gadget-items(pane);
  for (index :: <integer> in selection)
    let object = items[index];
    let node = find-node(pane, object);
    add!(nodes, pane.%nodes[index])
  end;
  nodes
end method gadget-selected-nodes;

define sealed method compute-gadget-selection
    (pane :: <win32-tree-control>, selected-nodes :: <sequence>) => (selection :: <sequence>)
  let new-selection :: <stretchy-object-vector> = make(<stretchy-vector>);
  let items = gadget-items(pane);
  for (node in selected-nodes)
    let index = position(items, node-object(node));
    when (index)
      add!(new-selection, index)
    end
  end;
  new-selection
end method compute-gadget-selection;

define sealed method do-expand-node
    (pane :: <win32-tree-control>, node :: <win32-tree-node>) => ()
  unless (*port-did-it?*)
    unless (empty?(node-children(node)))
      let mirror = sheet-direct-mirror(pane);
      when (mirror)
	let handle = window-handle(mirror);
	let item-handle :: <HTREEITEM> = node.%handle;
	duim-debug-message("Expanding node object %= for tree %=",
			   node-object(node), pane);
	SendMessage(handle, $TVM-EXPAND, $TVE-EXPAND, pointer-address(item-handle))
      end
    end
  end
end method do-expand-node;

define sealed method do-contract-node
    (pane :: <win32-tree-control>, node :: <win32-tree-node>) => ()
  unless (*port-did-it?*)
    unless (empty?(node-children(node)))
      let mirror = sheet-direct-mirror(pane);
      when (mirror)
	let handle = window-handle(mirror);
	let item-handle :: <HTREEITEM> = node.%handle;
	duim-debug-message("Contracting node object %= for tree %=",
			   node-object(node), pane);
	SendMessage(handle, $TVM-EXPAND, $TVE-COLLAPSE, pointer-address(item-handle))
      end
    end
  end
end method do-contract-node;

// The idea here is to add all the expanded node children first, then
// expand them all from the bottom up.  This code is a hybrid of the
// 'tree-control-expanded-objects-setter' and 'expand-node' methods on
// <tree-control>, but rearranged to minimize flicker during 'update-gadget'.
define method tree-control-expanded-objects-setter
    (objects :: <sequence>, tree :: <win32-tree-control>, #key depth = 1)
 => (objects :: <sequence>)
  let children-predicate = tree-control-children-predicate(tree);
  let children-generator = tree-control-children-generator(tree);
  local method add-one (node :: <tree-node>) => ()
	  when (member?(node-object(node), objects, test: gadget-test(tree)))
	    when (~node-state(node) & children-predicate(node-object(node)))
	      let objects = children-generator(node-object(node));
	      let nodes = map-as(<simple-vector>,
				 method (object) make-node(tree, object) end, objects);
	      do-add-nodes(tree, node, nodes)
	    end;
	    node-state(node) := #"contracted";
	    when (node-generation(node) <= depth)
	      do(add-one, node-children(node))
	    end
	  end
	end method,
        method expand-one (node :: <tree-node>) => ()
	  when (member?(node-object(node), objects, test: gadget-test(tree)))
	    when (node-generation(node) <= depth)
	      do(expand-one, node-children(node))
	    end;
	    when (node-state(node) == #"contracted")
	      node-state(node) := #"expanded";
	      do-expand-node(tree, node)
	    end
	  end
	end method;
  let root-nodes = tree-control-root-nodes(tree);
  do(add-one,    root-nodes);
  do(expand-one, root-nodes);
  objects
end method tree-control-expanded-objects-setter;


/// A little bit of glue to the homegrown graph controls

define method initialize-tree-control-icons
    (port :: <win32-port>, graph :: <graph-control-pane>) => ()
  let expand-icon
    = with-output-to-pixmap (medium = graph, width: 9, height: 9)
	with-drawing-options (medium, brush: $tree-control-gray)
	  draw-rectangle(medium, 0, 0, 8, 8, filled?: #f)
	end;
	with-drawing-options (medium, brush: $tree-control-black)
	  draw-line(medium, 2, 4, 7, 4);
	  draw-line(medium, 4, 2, 4, 7)
	end;
      end;
  let contract-icon
    = with-output-to-pixmap (medium = graph, width: 9, height: 9)
	with-drawing-options (medium, brush: $tree-control-gray)
	  draw-rectangle(medium, 0, 0, 8, 8, filled?: #f)
	end;
	with-drawing-options (medium, brush: $tree-control-black)
	  draw-line(medium, 2, 4, 7, 4)
	end;
      end;
  tree-control-expand-icon(graph)   := expand-icon;
  tree-control-contract-icon(graph) := contract-icon;
end method initialize-tree-control-icons;


/// Image lists utilities for List and Tree Views

//--- Note that we create a masked image list, but that 'find-image' is really
//--- not prepared to handle masked bitmaps in the <bitmap> case
define function make-image-list
    (width :: <integer>, height :: <integer>) => (image-list :: <HIMAGELIST>)
  let cx :: <integer> = GetSystemMetrics(width);
  let cy :: <integer> = GetSystemMetrics(height);
  check-result("ImageList-Create",
	       ImageList-Create(cx, cy, %logior($ILC-COLOR8, $ILC-MASK), 8, 8))
end function make-image-list;

// Interns the bitmap or icon into the image list, returning the index
define sealed method find-image
    (bitmap :: <win32-bitmap>, image-table :: <object-table>, image-list :: <HIMAGELIST>)
 => (index :: <integer>)
  let index = gethash(image-table, bitmap);
  index
  | begin
      //--- We should either used ImageList-AddMasked or take a mask bitmap
      //--- NB: Win32 SDK says to be sure to destroy the image...
      let index = ImageList-Add(image-list, image-handle(bitmap), null-pointer(<HBITMAP>));
      when (index < 0)
	report-error("ImageList-Add (bitmap)")
      end;
      gethash(image-table, bitmap) := index;
      index
    end
end method find-image;

define sealed method find-image
    (icon :: <win32-icon>, image-table :: <object-table>, image-list :: <HIMAGELIST>)
 => (index :: <integer>)
  let index = gethash(image-table, icon);
  index
  | begin
      //--- NB: Win32 SDK says to be sure to destroy the image...
      let index = ImageList-AddIcon(image-list, image-handle(icon));
      when (index < 0)
	report-error("ImageList-Add (icon)")
      end;
      gethash(image-table, icon) := index;
      index
    end
end method find-image;

define sealed method find-image
    (image :: <pattern>, image-table :: <object-table>, image-list :: <HIMAGELIST>)
 => (index :: <integer>)
  error("DUIM <pattern> objects are not yet supported in Windows controls")
end method find-image;

define sealed method release-images
    (image-table :: <object-table>) => ()
/*--- Can't do this, since someone else might be holding on to them!
  for (index keyed-by image in image-table)
    ignore(index);
    DeleteObject(image-handle(image))
  end;
*/
  remove-all-keys!(image-table)
end method release-images;


/// Other gadgets

//--- Missing
//---  <splitters>
