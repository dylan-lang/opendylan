Module:    win32-duim
Synopsis:  Win32 basic gadget implementation
Author:    Andy Armstrong, David Gray, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic Win32 constants

//---*** All of the following should be computed
define constant $default-label :: <byte-string>  = "";

// The thickness of the nice 3d border we use
define constant $gadget-border-thickness :: <integer> = 2;	// in pixels

define constant $default-vertical-spacing :: <integer> = 1;

define constant $minimum-visible-characters :: <integer> = 25;
define constant $minimum-visible-lines      :: <integer> =  3;

define constant $push-button-extra-text-width  :: <integer> = 12;
define constant $push-button-extra-text-height :: <integer> = 4;
define constant $push-button-extra-icon-width  :: <integer> = 2;
define constant $push-button-extra-icon-height :: <integer> = 2;
define constant $button-icon-width             :: <integer> = 16;

define constant $list-box-minimum-visible-lines :: <integer> = 3;
define constant $list-box-default-visible-lines :: <integer> = 5;
define constant $list-box-extra-height          :: <integer> = 2;

define constant $option-box-maximum-popup-height :: <integer> = 200;
define constant $option-box-extra-height         :: <integer> =   8;

define constant $minimum-scroll-shaft-length :: <integer> = 50;

define constant $text-field-extra-width  :: <integer> = 0;	// in pixels
define constant $text-field-extra-height :: <integer> = 8;	// in pixels

define constant $text-editor-extra-width  :: <integer> = 0;	// in pixels
define constant $text-editor-extra-height :: <integer> = 12;	// in pixels


/// The Win32 backend gadgets protocol

define protocol <<win32-gadgets-protocol>> ()
  function make-gadget-control
    (gadget :: <win32-gadget-mixin>, parent :: <HWND>, 
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>);
  function update-mirror-label
    (gadget :: <win32-gadget-mixin>, mirror :: <win32-mirror>) => ();
  function update-gadget-image
    (gadget :: <win32-gadget-mixin>, handle :: <HWND>, image :: <image>) => ();
end protocol <<win32-gadgets-protocol>>;


/// Win32 gadgets

define open abstract class <win32-gadget-mixin> (<gadget>, <win32-pane-mixin>)
end class <win32-gadget-mixin>;

define sealed domain defaulted-gadget-accelerator (<win32-frame-manager>, <win32-gadget-mixin>);
define sealed domain defaulted-gadget-mnemonic    (<win32-frame-manager>, <win32-gadget-mixin>);
define sealed domain compute-mnemonic-from-label  (<win32-pane-mixin>, <object>);

// We arrange to map this to DEFAULT_GUI_FONT
define constant $win32-default-gadget-text-style
    = make(<text-style>,
	   family: #"system", weight: #"normal",
	   slant: #"roman", size: #"normal");

define sealed method port-default-text-style
    (_port :: <win32-port>, gadget :: <win32-gadget-mixin>)
 => (text-style :: false-or(<text-style>))
  $win32-default-gadget-text-style
end method port-default-text-style;

define sealed method make-sheet-mirror
    (parent :: <sheet>, gadget :: <win32-gadget-mixin>)
 => (mirror :: <window-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(gadget);
  let parent-handle = window-handle(parent);
  let handle
    = make-gadget-control
        (gadget,
	 parent-handle,
	 gadget-default-options(gadget),
	 x: left, y: top, width: right - left, height: bottom - top);
  make(<window-mirror>,
       sheet: gadget, handle: handle,
       region: make-bounding-box(left, top, right, bottom))
end method make-sheet-mirror;

define sealed method gadget-default-options
    (gadget :: <gadget>) => (options :: <options-type>)
  %logior($WS-CHILD, $WS-CLIPSIBLINGS,
	  if (gadget-enabled?(gadget)) 0 else $WS-DISABLED end)
end method gadget-default-options;

define sealed method gadget-default-options
    (gadget :: <scrolling-gadget-mixin>) => (options :: <options-type>)
  %logior(next-method(),
	  if (gadget-scrolling-vertically?(gadget))   $WS-VSCROLL else 0 end,
	  if (gadget-scrolling-horizontally?(gadget)) $WS-HSCROLL else 0 end)
end method gadget-default-options;

define sealed method gadget-extended-options
    (gadget :: <gadget>, #key) => (options :: <options-type>)
  0
end method gadget-extended-options;

define sealed method gadget-extended-options
    (gadget :: <bordered-gadget-mixin>, #key default-border? = #t)
 => (options :: <options-type>)
  let has-border?
    = select (border-type(gadget))
	#f        => default-border?;
	#"none"   => #f;
	otherwise => #t;
      end;
  %logior(next-method(),
	  if (has-border?) $WS-EX-CLIENTEDGE else 0 end)
end method gadget-extended-options;

define sealed method make-sheet-mirror-from-resource
    (parent :: <sheet>, gadget :: <win32-gadget-mixin>, resource-id :: <resource-id>)
 => (mirror :: <window-mirror>)
  let _port = port(gadget);
  // If for some reason the parent is not a dialog resource,
  // we have to make sure that lookup can handle it
  //--- Update Win32-Resources library
  let resource :: <window-resource>
    = lookup-control(sheet-mirror(parent).%mirror-resource, resource-id);
  let handle :: <HWnd> = GetDlgItem(window-handle(parent), resource-id);
  check-result("GetDlgItem", handle);
  let (x, y)          = window-position(resource);
  let (width, height) = window-size(resource);
  let (x, y)          = win32-dialog-units->pixels(_port, x, y);
  let (width, height) = win32-dialog-units->pixels(_port, width, height);
  duim-debug-message("Gadget geometry %= from resource: %d x %d at %d, %d",
		     gadget, width, height, x, y);
  initialize-sheet-geometry(gadget, x, y, width, height);
  initialize-sheet-from-resource(gadget, handle);
  let (left, top, right, bottom) = sheet-native-edges(gadget);
  make(<window-mirror>,
       sheet: gadget, handle: handle,
       resource: resource,
       region: make-bounding-box(left, top, right, bottom))
end method make-sheet-mirror-from-resource;
  
define sealed method update-gadget-font
    (gadget :: <win32-gadget-mixin>, mirror :: <window-mirror>) => ()
  let handle = window-handle(mirror);
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let font = text-style-mapping(_port, text-style);
  SendMessage(handle,
	      $WM-SETFONT,
	      pointer-address(font.%font-handle),
	      0)
end method update-gadget-font;

define sealed method note-mirror-created
    (gadget :: <win32-gadget-mixin>, mirror :: <window-mirror>) => ()
  next-method();
  let documentation = gadget-documentation(gadget);
  documentation & register-tooltip-for-sheet(gadget, documentation);
  update-gadget-font(gadget, mirror)
end method note-mirror-created;

// Update the tool tip when the gadget documentation changes
define method gadget-documentation-setter
    (documentation, gadget :: <win32-gadget-mixin>) => (documentation :: false-or(<string>))
  unregister-tooltip-for-sheet(gadget);
  next-method();
  documentation & register-tooltip-for-sheet(gadget, documentation);
  documentation
end method gadget-documentation-setter;


define sealed method defaulted-gadget-label
    (gadget :: <gadget>) => (label)
  gadget-label(gadget) | $default-label
end method defaulted-gadget-label;

define sealed method note-gadget-label-changed
    (gadget :: <win32-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-label(gadget, mirror)
end method note-gadget-label-changed;

define sealed method get-window-text
    (handle :: <HWND>) => (text :: <string>)
  let length = SendMessage(handle, $WM-GETTEXTLENGTH, 0, 0);
  if (length = 0)
    ""
  else
    let buffer-size = length + 1;
    with-stack-structure (buffer :: <C-string>, size: buffer-size)
      let actual-length = GetWindowText(handle, buffer, buffer-size);
      when (actual-length = 0) ensure-no-error("GetWindowText") end;
      as(<byte-string>, buffer)
    end
  end
end method get-window-text;

define sealed method update-mirror-label
    (gadget :: <win32-gadget-mixin>, mirror :: <window-mirror>) => ()
  let handle = window-handle(mirror);
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  when (text)
    check-result("SetWindowText", SetWindowText(handle, text))
  end;
  when (image)
    update-gadget-image(gadget, handle, image)
  end
end method update-mirror-label;

define sealed method note-gadget-enabled
    (client, gadget :: <win32-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let handle = window-handle(gadget);
  handle & EnableWindow(handle, #t)
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <win32-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let handle = window-handle(gadget);
  handle & EnableWindow(handle, #f)
end method note-gadget-disabled;

define sealed method activate-win32-gadget
    (gadget :: <action-gadget>) => (activated? :: <boolean>)
  when (gadget-activate-callback(gadget))
    distribute-activate-callback(gadget);
    #t
  end
end method activate-win32-gadget;

define sealed method activate-win32-gadget
    (gadget :: <text-field>) => (activated? :: <boolean>)
  handle-text-gadget-changed(gadget);
  next-method()
end method activate-win32-gadget;


/// Gadget id handling

define sealed method gadget->id
    (gadget :: <gadget>) => (id :: <integer>)
  let top-mirror = top-level-mirror(gadget, error?: #t);
  block (return)
    when (top-mirror)
      for (g keyed-by id in top-mirror.%resource-id-table)
	when (gadget == g)
	  return(id)
	end
      end
    end;
    error("Failed to find id for gadget %=", gadget)
  end
end method gadget->id;

define sealed method gadget->id-setter
    (id :: <integer>, gadget :: <gadget>)
 => (id :: <integer>)
  let top-mirror = top-level-mirror(gadget, error?: #t);
  top-mirror.%resource-id-table[id] := gadget;
  id
end method gadget->id-setter;

define sealed method id->gadget
    (sheet :: <sheet>, id :: <integer>)
 => (gadget :: false-or(<gadget>))
  let top-mirror = top-level-mirror(sheet);
  top-mirror & element(top-mirror.%resource-id-table, id, default: #f)
end method id->gadget;

//---*** How do we avoid clashing with all resource ids currently
//---*** chosen by the user?  Maybe we just advertise which ids we
//---*** reserve for DUIM's use.
define sealed method ensure-gadget-id
    (gadget :: <gadget>) => (id :: <integer>)
  let resource-id = sheet-resource-id(gadget);
  let id
    = select (resource-id by instance?)
	<integer> => resource-id;
	otherwise => generate-next-gadget-id(gadget);
      end;
  register-gadget-id(gadget, id)
end method ensure-gadget-id;

define sealed method generate-next-gadget-id
    (gadget :: <gadget>) => (id :: <integer>)
  let top-mirror = top-level-mirror(gadget, error?: #t);
  let id = top-mirror.%next-resource-id;
  register-gadget-id(gadget, id)
end method generate-next-gadget-id;

define sealed method register-gadget-id
    (gadget :: <gadget>, id :: <integer>) => (id :: <integer>)
  let top-mirror = top-level-mirror(gadget, error?: #t);
  top-mirror.%resource-id-table[id] := gadget;
  top-mirror.%next-resource-id := id + 1;
  id
end method register-gadget-id;


/// Exit, cancel, default button, etc.

define method handle-command-for-id
    (sheet :: <sheet>, id :: <integer>) => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  select (id)
    $IDOK => 
      duim-debug-message("Handling command IDOK for %=", sheet);
      activate-default-button(frame);
    $IDCANCEL =>
      duim-debug-message("Handling command IDCANCEL for %=", sheet);
      handle-cancel(frame);
    otherwise =>
      let gadget = id->gadget(sheet, id);
      if (gadget)
	when (sheet-mapped?(gadget) & gadget-enabled?(gadget))
	  handle-gadget-activation(gadget)
	end
      else
	handle-id-activation(frame, id)
      end;
  end
end method handle-command-for-id;

define sealed method handle-gadget-activation
    (gadget :: <gadget>) => (handled? :: <boolean>)
  duim-debug-message("Ignoring activation command for gadget %=", gadget);
  #f
end method handle-gadget-activation;

// This handles IDOK commands for more than just buttons...
define method activate-default-button
    (frame :: <frame>) => (activated? :: <boolean>)
  let gadget = win32-sheet-with-focus();
  duim-debug-message("  Handling IDOK: focus currently %=", gadget);
  let activated? = instance?(gadget, <action-gadget>)
		   & gadget-enabled?(gadget)
		   & activate-win32-gadget(gadget);
  // If we didn't activate the gadget, try to activate the default button
  unless (activated?)
    let button = frame-default-button(frame);
    // Don't activate an upmapped or disabled default button...
    when (button & sheet-mapped?(button) & gadget-enabled?(button))
      handle-gadget-activation(button)
    end
  end
end method activate-default-button;

define function win32-sheet-with-focus
    () => (sheet :: false-or(<sheet>))
  let handle = GetFocus();
  let sheet = handle-sheet(handle);
  if (instance?(sheet, <win32-subgadget-mixin>))
    subgadget-owner(sheet)
  else
    sheet
  end
end function win32-sheet-with-focus;


define function handle-cancel
    (frame :: <frame>) => (handled? :: <boolean>)
  let gadget = win32-sheet-with-focus();
  duim-debug-message("  Handling IDCANCEL: focus currently %=", gadget);
  if (instance?(gadget, <gadget>) & cancel-gadget(gadget))
    #t
  else
    cancel-frame(frame)
  end
end function handle-cancel;

define sealed method cancel-frame
    (frame :: <frame>) => (handled? :: <boolean>)
  //---*** We should handle ESCAPE as canceling popups by default,
  //---*** for example in combo boxes.
  #f
end method cancel-frame;

define sealed method cancel-gadget 
    (gadget :: <gadget>) => (handled? :: <boolean>)
  #f
end method cancel-gadget;


//---*** What do we do about setting the color and font of a gadget?


/// "Subgadgets"

// This is the class of gadgets that are created by Windows as a
// child of a gadget.  For example, a combo box has a child text field
// that we need to model, but that we don't create ourselves.
define abstract class <win32-subgadget-mixin> (<win32-gadget-mixin>)
  sealed constant slot subgadget-owner :: <gadget>,
    required-init-keyword: owner:;
end class <win32-subgadget-mixin>;

define sealed method initialize
    (gadget :: <win32-subgadget-mixin>, #key handle) => ()
  next-method();
  let mirror
    = make(<window-mirror>,
	   sheet: gadget, handle: handle,
	   region: make-bounding-box(0, 0, 100, 100));
  note-mirror-created(gadget, mirror)
end method initialize;

//--- Slight hack to allow people to find the frame etc. of this gadget
define sealed method top-level-sheet
    (gadget :: <win32-subgadget-mixin>)
 => (sheet :: false-or(<top-level-sheet>))
  top-level-sheet(subgadget-owner(gadget))
end method top-level-sheet;

define sealed method update-gadget-font
    (gadget :: <win32-subgadget-mixin>, mirror :: <window-mirror>) => ()
  //--- Use the font for the owner
  update-gadget-font(subgadget-owner(gadget), mirror)
end method update-gadget-font;

define sealed method gadget-documentation
    (gadget :: <win32-subgadget-mixin>) => (documentation)
  gadget-documentation(subgadget-owner(gadget))
end method gadget-documentation;


/// Subclassed windows gadgets

/// This is the class of gadgets that need to see the control's events
/// in order to grab some of them. For example, our text-field code
/// needs to see the return key press to do the value changed callback.

define abstract class <win32-subclassed-gadget-mixin> (<win32-gadget-mixin>)
  sealed slot %old-WndProc :: <WNDPROC>;
end class <win32-subclassed-gadget-mixin>;

// This function gets the first crack at WM_xxx messages...
define open generic handle-control-message
    (gadget :: <win32-subclassed-gadget-mixin>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>);

define sealed method subclassed-window-callback-function
    (handle :: <HWND>,		// window handle
     message :: <message-type>,	// type of message
     wParam  :: <wparam-type>,	// additional information
     lParam  :: <lparam-type>)	// additional information
  => (result :: <lresult-type>)
  let gadget = handle-sheet(handle);
  assert(instance?(gadget, <win32-subclassed-gadget-mixin>),
	 "Can't find sheet for subclassed mirror -- this can't happen!");
  if (handle-control-message(gadget, message, wParam, lParam))
    0
  else
    default-subclassed-window-callback(gadget, handle, message, wParam, lParam)
  end
end method subclassed-window-callback-function;

define sealed inline method default-subclassed-window-callback
    (gadget :: <win32-subclassed-gadget-mixin>,
     handle :: <HWND>,		// window handle
     message :: <message-type>,	// type of message
     wParam  :: <wparam-type>,	// additional information
     lParam  :: <lparam-type>)	// additional information
  => (result :: <lresult-type>)
  CallWindowProc(gadget.%old-WndProc, handle, message, wParam, lParam)
end method default-subclassed-window-callback;

define callback SubclassedWndProc :: <WNDPROC> = subclassed-window-callback-function;

define sealed method note-mirror-created
    (gadget :: <win32-subclassed-gadget-mixin>, mirror :: <window-mirror>) => ()
  next-method();
  let handle = window-handle(mirror);
  let old-wndproc
    = SetWindowLong(handle, $GWL-WNDPROC, pointer-address(SubclassedWndProc));
  gadget.%old-WndProc := make(<WNDPROC>, address: old-wndproc)
end method note-mirror-created;


/// Layout gadget mixins

// This is the class of gadgets that also behave as a layout of any
// children that they have.  In order to make tabbing between gadgets
// work, we make all of the mirrors as siblings parented into the main
// window, by specifying 'mirror-accepts-children?: #f'.
define class <win32-layout-gadget-mixin> (<win32-gadget-mixin>)
end class <win32-layout-gadget-mixin>;

define method initialize
    (gadget :: <win32-layout-gadget-mixin>, #key) => ()
  next-method();
  sheet-mirror-accepts-children?(gadget) := #f
end method initialize;


/// Labels

define sealed class <win32-label> 
    (<win32-gadget-mixin>,
     <label>,
     <leaf-pane>)
end class <win32-label>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-label>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-label>));
define sealed domain initialize (<win32-label>);

define sealed method make-gadget-control
    (gadget :: <win32-label>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  if (multi-line-label?(gadget) & instance?(gadget-label(gadget), <string>))
    let handle :: <HWND>
      = CreateWindowEx(gadget-extended-options(gadget),
		       "EDIT",
		       convert-to-windows-newlines(gadget-label(gadget)),
		       // Neither a group nor a tabstop be...
		       %logior(options, $ES-MULTILINE, $ES-READONLY),
		       x, y, width, height,
		       parent,
		       $null-hMenu,
		       application-instance-handle(),
		       $NULL-VOID);
    check-result("CreateWindowEx (EDIT)", handle);
    handle
  else
    let (text, image, mnemonic, index)
      = text-or-image-from-gadget-label(gadget);
    let image-style
      = select (image by instance?)
	  <win32-bitmap> => $SS-BITMAP;
	  <win32-icon>   => $SS-ICON;
	  otherwise      => $SS-LEFTNOWORDWRAP;
	end;
    let label = make-win32-mnemonic-label(text, mnemonic, index, #f);
    let handle :: <HWND>
      = CreateWindowEx(gadget-extended-options(gadget),
		       "STATIC",
		       label,
		       %logior(options, image-style),
		       x, y, width, height,
		       parent,
		       $null-hMenu,
		       application-instance-handle(),
		       $NULL-VOID);
    unless (label = $NULL-string) destroy(label) end;
    check-result("CreateWindowEx (STATIC)", handle);
    when (image)
      update-gadget-image(gadget, handle, image)
    end;
    handle
  end
end method make-gadget-control;

define method update-gadget-image
    (gadget :: <win32-label>, handle :: <HWND>, image :: <win32-bitmap>) => ()
  //---*** Apparently if this control is in a dialog box, we should be using
  //---*** SendDlgItemMessage instead of SendMessage.
  SendMessage(handle, $STM-SETIMAGE, $IMAGE-BITMAP, 
              pointer-address(image-handle(image)))
end method update-gadget-image;

define method update-gadget-image
    (gadget :: <win32-label>, handle :: <HWND>, image :: <win32-icon>) => ()
  //---*** Apparently if this control is in a dialog box, we should be using
  //---*** SendDlgItemMessage instead of SendMessage.
  SendMessage(handle, $STM-SETIMAGE, $IMAGE-ICON,   
              pointer-address(image-handle(image)))
end method update-gadget-image;

define sealed method do-compose-space 
    (gadget :: <win32-label>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  if (multi-line-label?(gadget) & instance?(gadget-label(gadget), <string>))
    let (width, height) = gadget-label-size(gadget, do-newlines?: #t);
    make(<space-requirement>,
	 width:  width  + $text-editor-extra-width,
	 height: height + $text-editor-extra-height)
  else
    let (width, height) = gadget-label-size(gadget);
    make(<space-requirement>,
	 width: width, height: height) 
  end
end method do-compose-space;


/// Separators

define sealed class <win32-separator>
    (<separator>,
     <drawing-pane>)
end class <win32-separator>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <separator>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-separator>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-separator>));
define sealed domain initialize (<win32-separator>);

define sealed method do-compose-space
    (pane :: <win32-separator>, #key width, height)
 => (space-requirement :: <space-requirement>)
  select (gadget-orientation(pane))
    #"horizontal" =>
      make(<space-requirement>,
	   min-width: 1, width: width | 1, max-width: $fill,
	   height: 2);
    #"vertical" =>
      make(<space-requirement>,
	   width: 2,
	   min-height: 1, height: height | 1, max-height: $fill);
  end
end method do-compose-space;

define sealed method handle-repaint
    (pane :: <win32-separator>, medium :: <win32-medium>, region :: <region>) => ()
  ignore(region);	// not worth checking
  let (left, top, right, bottom) = box-edges(pane);
  draw-separator(pane, medium, gadget-orientation(pane), left, top, right, bottom)
end method handle-repaint;

//--- Is there no Windows defined way to do this?
define sealed method draw-separator
    (sheet :: <sheet>, medium :: <win32-medium>,
     orientation :: <gadget-orientation>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>, #key type :: <border-type> = #f) => ()
  if (type == #"raised")
    draw-border(sheet, medium, type, left, top, right, bottom)
  else
    let highlight-color
      = native-color->color(GetSysColor($COLOR-3DHIGHLIGHT), medium);
    let shadow-color
      = native-color->color(GetSysColor($COLOR-3DSHADOW), medium);
    select (orientation)
      #"horizontal" =>
	let bottom = top + 1;
	with-drawing-options (medium, brush: shadow-color)
	  draw-line(medium, left, top, right, top)
	end;
	with-drawing-options (medium, brush: highlight-color)
	  draw-line(medium, left, bottom, right, bottom);
	  draw-line(medium, right, top, right, bottom)
	end;
      #"vertical" =>
	let right = left + 1;
	with-drawing-options (medium, brush: shadow-color)
	  draw-line(medium, left, top, left, bottom)
	end;
	with-drawing-options (medium, brush: highlight-color)
	  draw-line(medium, right, top, right, bottom);
	  draw-line(medium, left, bottom, right, bottom)
	end;
    end
  end
end method draw-separator;


/// Text gadgets

/// Note that some of the methods below are on written on <text-gadget>
/// so that <win32-combo-box> gets to use them
//--- This probably means we need another class to get text field and combo boxes!

define abstract class <win32-text-gadget-mixin>
    (<win32-gadget-mixin>,
     <text-field>)
  sealed slot %changed? :: <boolean> = #f;
  sealed constant slot %current-selection :: <simple-text-range>
    = make(<text-range>, start: -1, end: -1);
end class <win32-text-gadget-mixin>;

define sealed domain gadget-text (<win32-text-gadget-mixin>);
define sealed domain gadget-text-setter (<string>, <win32-text-gadget-mixin>);
define sealed domain gadget-text-buffer (<win32-text-gadget-mixin>);
define sealed domain gadget-text-buffer-setter (<string>, <win32-text-gadget-mixin>);

define sealed method make-gadget-control
    (gadget :: <win32-text-gadget-mixin>,
     parent :: <HWND>, 
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  // 'gadget-text-buffer' gets initialized from the 'text:' init keyword,
  // so it needs to be converted to the internal representation
  let text = gadget-convert-to-windows-newlines(gadget, gadget-text-buffer(gadget));
  gadget-text-buffer(gadget) := text;
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     "EDIT",
		     text,
		     win32-text-gadget-options(gadget, options),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (EDIT)", handle);
  handle
end method make-gadget-control;

//---*** We could add $DS-LOCALEDIT to make the text buffer ourselves
define sealed method win32-text-gadget-options
    (gadget :: <win32-text-gadget-mixin>, options :: <options-type>)
 => (gadget-options :: <options-type>)
  %logior(options,
	  if (sheet-tab-stop?(gadget)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
	  if (gadget-read-only?(gadget)) $ES-READONLY else 0 end)
end method win32-text-gadget-options;

define sealed method note-mirror-created
    (gadget :: <win32-text-gadget-mixin>, mirror :: <window-mirror>) => ()
  next-method();
  // Set the initial text selection
  text-selection(gadget) := gadget.%current-selection
end method note-mirror-created;

define method gadget-convert-to-windows-newlines
    (gadget :: <text-gadget>, string :: <byte-string>)
 => (string :: <byte-string>)
  string
end method gadget-convert-to-windows-newlines;

define method gadget-convert-from-windows-newlines
    (gadget :: <text-gadget>, string :: <byte-string>)
 => (string :: <byte-string>)
  string
end method gadget-convert-from-windows-newlines;

define sealed method note-gadget-text-changed 
    (gadget :: <win32-text-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

// Updates the Windows text field from the DUIM gadget
define sealed method update-gadget-text
    (gadget :: <text-gadget>, mirror :: <window-mirror>) => ()
  //---*** Use our own buffer to avoid needless consing in 'get-window-text'
  let handle   = window-handle(mirror);
  let old-text = get-window-text(handle);
  let new-text = gadget-text-buffer(gadget);
  when (old-text ~= new-text)
    unless (SetWindowText(handle, new-text))
      let message
	= format-to-string("Failed to change gadget text for %= to '%s'",
			   gadget, new-text);
      report-error(message)
    end
  end
end method update-gadget-text;

define sealed method handle-command
    (gadget :: <win32-text-gadget-mixin>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $EN-CHANGE    => handle-text-gadget-changing(gadget);
    $EN-KILLFOCUS => handle-text-gadget-changed(gadget);
    otherwise     => next-method();
  end
end method handle-command;

define sealed method handle-text-gadget-changing
    (gadget :: <text-gadget>) => (handled? :: <boolean>)
  //---*** Use our own buffer to avoid needless consing in 'get-window-text'
  let handle = window-handle(gadget);
  let text   = get-window-text(handle);
  unless (text = gadget-text-buffer(gadget))
    gadget.%changed? := #t;
    distribute-text-changing-callback(gadget, text)
  end;
  #t
end method handle-text-gadget-changing;

define sealed method handle-text-gadget-changed
    (gadget :: <text-gadget>) => (handled? :: <boolean>)
  //---*** Use our own buffer to avoid needless consing in 'get-window-text'
  when (gadget.%changed?)
    let handle = window-handle(gadget);
    let text   = get-window-text(handle);
    distribute-text-changed-callback(gadget, text);
    gadget.%changed? := #f
  end;
  #t
end method handle-text-gadget-changed;

define sealed method text-caret-position
    (gadget :: <win32-text-gadget-mixin>)
 => (position :: false-or(<integer>))
  let handle = window-handle(gadget);
  when (handle)
    let position = SendMessage(handle, $EM-GETSEL, 0, 0);
    let _end     = HIWORD(position);
    _end
  end
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <win32-text-gadget-mixin>)
 => (position :: false-or(<integer>))
  let handle = window-handle(gadget);
  when (handle & position)
    //--- What if position is too large?
    SendMessage(handle, $EM-SETSEL, position, position);
    SendMessage(handle, $EM-SCROLLCARET, 0, 0)
  end;
  position
end method text-caret-position-setter;

//---*** What about 'selected-text-setter'?
define sealed method selected-text
    (gadget :: <win32-text-gadget-mixin>) => (string :: false-or(<string>))
  let selection = #f;
  let handle = window-handle(gadget);
  when (handle)
    //--- This should use the extended form of EM_GETSEL to handle large text fields
    let position = SendMessage(handle, $EM-GETSEL, 0, 0);
    let _start   = LOWORD(position);
    let _end     = HIWORD(position);
    let _size    = _end - _start;
    when (_size > 0)
      let buffer-size = SendMessage(handle, $WM-GETTEXTLENGTH, 0, 0) + 1;
      with-stack-structure (buffer :: <C-string>, size: buffer-size)
	let actual-length = GetWindowText(handle, buffer, buffer-size);
	when (actual-length = 0) ensure-no-error("GetWindowText") end;
	let string :: <byte-string> = make(<byte-string>, size: _size);
        without-bounds-checks
	  for (i :: <integer> from _start below _end,
	       j :: <integer> from 0)
	    string[j] := buffer[i]
	  end
	end;
	selection := string
      end
    end
  end;
  selection
end method selected-text;

define sealed method text-selection
    (gadget :: <win32-text-gadget-mixin>)
 => (range :: type-union(<text-range>, one-of(#f)))
  let handle = window-handle(gadget);
  when (handle)
    //--- This should use the extended form of EM_GETSEL to handle large text fields
    let current  = gadget.%current-selection;
    let position = SendMessage(handle, $EM-GETSEL, 0, 0);
    let _start   = LOWORD(position);
    let _end     = HIWORD(position);
    text-range-start(current) := _start;
    text-range-end(current)   := _end;
    if (_start = _end) #f else current end
  end
end method text-selection;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)), gadget :: <win32-text-gadget-mixin>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  let handle  = window-handle(gadget);
  let current = gadget.%current-selection;
  let (_start, _end)
    = select (range)
	#t =>			// select all
	  values( 0, -1);
	#f =>			// select none
	  values(-1, -1);
	otherwise =>
	  values(text-range-start(range), text-range-end(range));
      end;
  text-range-start(current) := _start;
  text-range-end(current)   := _end;
  when (handle)
    SendMessage(handle, $EM-SETSEL, _start, _end);
    SendMessage(handle, $EM-SCROLLCARET, 0, 0)
  end;
  range
end method text-selection-setter;

//---*** Still need to implement 'text-field-modified?' and setter


/// Win32 text field implementation class

// The superclass of <win32-text-field> and <win32-password-field>.
// We use subclassing so that we can catch the 'return' key when it
// is pressed, so that we can then send an activate callback.
define abstract class <win32-text-field-mixin>
    (<win32-subclassed-gadget-mixin>,
     <win32-text-gadget-mixin>)
end class <win32-text-field-mixin>;

define sealed method do-compose-space
    (gadget :: <win32-text-field-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  //---*** How should we really calculate the min width/height?
  let min-width  = $minimum-visible-characters * font-width(text-style, _port)
		     + $text-field-extra-width;
  let max-width  = $fill;
  let min-height = font-height(text-style, _port)
		     + $text-field-extra-height;
  let max-height = if (instance?(gadget, <text-editor>)) $fill else min-height end;
  let width  = constrain-size(width  | min-width,  min-width,  max-width);
  let height = constrain-size(height | min-height, min-height, max-height);
  make(<space-requirement>,
       width:  width,  min-width:  min-width,  max-width:  max-width,
       height: height, min-height: min-height, max-height: max-height)
end method do-compose-space;

define sealed method handle-control-message
    (gadget :: <win32-text-field-mixin>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  maybe-handle-keyboard-interrupt(gadget, message, wParam);
  /*
  //--- This gets done by 'handle-button-activation' via an IDOK command
  when (message = $WM-CHAR)
    // If we got a Return in a text field, generate a text-changed callback,
    // then continue processing the control message
    let char   = as(<character>, wParam);
    let keysym = standard-char->keysym(char);
    when (keysym == #"return")
      handle-text-gadget-changed(gadget)
    end
  end;
  */
  #f
end method handle-control-message;


/// Text fields

define sealed class <win32-text-field>
    (<win32-text-field-mixin>,
     <text-field>,
     <leaf-pane>)
end class <win32-text-field>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <text-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-text-field>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-text-field>));
define sealed domain initialize (<win32-text-field>);

define sealed method win32-text-gadget-options
    (gadget :: <win32-text-field>, options :: <options-type>)
 => (gadget-options :: <options-type>)
  %logior($ES-AUTOHSCROLL, next-method())
end method win32-text-gadget-options;


/// Password fields

define sealed class <win32-password-field>
    (<win32-text-field-mixin>,
     <password-field>,
     <leaf-pane>)
end class <win32-password-field>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <password-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-password-field>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-password-field>));
define sealed domain initialize (<win32-password-field>);

define sealed method win32-text-gadget-options
    (gadget :: <win32-password-field>, options :: <options-type>)
 => (gadget-options :: <options-type>)
  %logior($ES-PASSWORD, $ES-AUTOHSCROLL, next-method())
end method win32-text-gadget-options;


/// Text editors

define sealed class <win32-text-editor>
    (<win32-text-gadget-mixin>,
     <text-editor>,
     <leaf-pane>)
  sealed slot %text :: false-or(<byte-string>) = #f;
end class <win32-text-editor>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <text-editor>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-text-editor>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-text-editor>));
define sealed domain initialize (<win32-text-editor>);

define sealed method win32-text-gadget-options
    (gadget :: <win32-text-editor>, options :: <options-type>)
 => (gadget-options :: <options-type>)
  %logior($ES-MULTILINE, $ES-WANTRETURN, next-method())
end method win32-text-gadget-options;

define method gadget-text
    (gadget :: <win32-text-editor>) => (text :: <string>)
  gadget.%text
  | begin
      let text = gadget-convert-from-windows-newlines(gadget, gadget-text-buffer(gadget));
      gadget.%text := text;
      text
    end
end method gadget-text;

define method gadget-text-setter
    (text :: <string>, gadget :: <win32-text-editor>, #key do-callback? = #f)
 => (text :: <string>)
  gadget-text-buffer(gadget) := gadget-convert-to-windows-newlines(gadget, text);
  when (do-callback?)
    execute-value-changed-callback(gadget, gadget-client(gadget), gadget-id(gadget))
  end;
  note-gadget-text-changed(gadget);
  note-gadget-value-changed(gadget);
  text
end method gadget-text-setter;

define method gadget-text-buffer-setter
    (text :: <string>, gadget :: <win32-text-editor>)
 => (text :: <string>)
  gadget.%text := #f;
  next-method()
end method gadget-text-buffer-setter;

define method gadget-convert-to-windows-newlines
    (gadget :: <win32-text-editor>, string :: <byte-string>)
 => (string :: <byte-string>)
  convert-to-windows-newlines(string)
end method gadget-convert-to-windows-newlines;

define method gadget-convert-from-windows-newlines
    (gadget :: <win32-text-editor>, string :: <byte-string>)
 => (string :: <byte-string>)
  convert-from-windows-newlines(string)
end method gadget-convert-from-windows-newlines;

define sealed method do-compose-space
    (gadget :: <win32-text-editor>, #key width, height)
 => (space-req :: <space-requirement>)
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let nlines = gadget-lines(gadget);
  let ncols  = gadget-columns(gadget);
  let line-height = font-height(text-style, _port);
  let char-width  = font-width(text-style, _port);
  let vsp         = $default-vertical-spacing;
  let hscroll-height
    = if (gadget-scrolling-horizontally?(gadget)) GetSystemMetrics($SM-CYHSCROLL)
      else 0 end;
  let vscroll-width
    = if (gadget-scrolling-vertically?(gadget))   GetSystemMetrics($SM-CXVSCROLL)
      else 0 end;
  when (nlines & ~height)
    height := nlines * line-height + (nlines - 1) * vsp + hscroll-height
		+ $gadget-border-thickness * 5
  end;
  when (ncols  & ~width)
    width  := ncols  * char-width  + vscroll-width
		+ $gadget-border-thickness * 4
  end;
  //---*** How should we really calculate the min width/height?
  let min-width  = $minimum-visible-characters * font-width(text-style, _port)
		     + $text-editor-extra-width  + vscroll-width;
  let max-width  = $fill;
  let min-height = $minimum-visible-lines * line-height + ($minimum-visible-lines - 1) * vsp
		     + $text-editor-extra-height + hscroll-height;
  let max-height = $fill;
  let width  = constrain-size(width  | min-width,  min-width,  max-width);
  let height = constrain-size(height | min-height, min-height, max-height);
  make(<space-requirement>,
       width:  width,  min-width:  min-width,  max-width:  max-width,
       height: height, min-height: min-height, max-height: max-height)
end method do-compose-space;


/// Buttons

define abstract class <win32-button-mixin>
    (<win32-subclassed-gadget-mixin>,
     <value-gadget>)
  sealed slot sheet-ignore-focus-change? :: <boolean> = #f;
end class <win32-button-mixin>;

// See WIG pg.388 for the description of this rule
define sealed method button-box-spacing
    (framem :: <win32-frame-manager>, box :: <button-box>)
 => (spacing :: <integer>)
  select (gadget-orientation(box))
    #"horizontal" => win32-dialog-x-pixels(framem, 3);
    #"vertical"   => win32-dialog-y-pixels(framem, 3);
  end
end method button-box-spacing;

define sealed method text-or-image-from-gadget-label
    (gadget :: <gadget>)
 => (text :: false-or(<string>), image :: false-or(<image>),
     mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>));
  let label = defaulted-gadget-label(gadget);
  let (label, mnemonic, index)
    = compute-mnemonic-from-label(gadget, label);
  let mnemonic = mnemonic & as-uppercase(gesture-character(mnemonic));
  select (label by instance?)
    <string> =>
      values(label, #f, mnemonic, index);
    <win32-bitmap>, <win32-icon> =>
      values(if (mnemonic) as(<string>, vector(mnemonic)) else "" end,
	     label, mnemonic, index);
    <image> =>
      //---*** Decode the image and return an <HBITMAP>
      values("<image>", #f, mnemonic, index);
  end
end method text-or-image-from-gadget-label;

define constant $mnemonic-escape = '&';

// Create a new label given a string label and a mnemonic
// Clients are responsible for destroying the C string when done
//--- This only works on <byte-string> since <C-string> is effectively a byte string...
define sealed method make-win32-mnemonic-label 
    (label :: <byte-string>, mnemonic :: false-or(<mnemonic>), 
     index :: false-or(<integer>), new-index :: false-or(<integer>),
     #key postfix :: <byte-string> = "")
 => (new-label :: <C-string>)
  let new-index :: false-or(<integer>)
    = case
	new-index => new-index;
	index     => #f;
	mnemonic  => position(label, gesture-character(mnemonic));
	otherwise => #f;
      end;
  case
    new-index =>
      let label-size = size(label);
      let postfix-size = size(postfix);
      let new-label = make(<C-string>, size: 1 + label-size + postfix-size);
      without-bounds-checks
	for (i :: <integer> from 0 below new-index)
	  new-label[i] := label[i]
	end;
        new-label[new-index] := $mnemonic-escape;
        let j :: <integer> = new-index + 1;
	for (i :: <integer> from new-index below label-size)
	  new-label[j] := label[i];
          j := j + 1
	end;
	for (i :: <integer> from 0 below postfix-size)
	  new-label[j] := postfix[i];
          j := j + 1
	end;
      end;
      new-label;
    postfix =>
      as(<C-string>, concatenate-as(<byte-string>, label, postfix));
    otherwise =>
      as(<C-string>, label);
  end
end method make-win32-mnemonic-label;

define sealed method make-gadget-control
    (gadget :: <win32-button-mixin>, parent :: <HWND>, 
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let selection-mode = gadget-selection-mode(gadget);
  let button-style
    = select (selection-mode)
	#"none" =>
	  case
	    gadget-default?(gadget) => $BS-DEFPUSHBUTTON;
	    otherwise               => $BS-PUSHBUTTON;
	  end;
	#"single" =>
	  %logior($BS-RADIOBUTTON,
		  if (push-button-like?(gadget)) $BS-PUSHLIKE else 0 end);
	#"multiple" =>
	  %logior($BS-CHECKBOX,
		  if (push-button-like?(gadget)) $BS-PUSHLIKE else 0 end);
      end;
  let group-style
    = case
	~sheet-tab-stop?(gadget) | button-in-tool-bar?(gadget) =>
	  0;
	button-first-in-group?(gadget) =>
	  %logior($WS-GROUP, $WS-TABSTOP);
	selection-mode ~== #"single" =>
	  $WS-TABSTOP;
	otherwise =>
	  0;
      end;
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  let image-style
    = select (image by instance?)
	<win32-bitmap> => $BS-BITMAP;
	<win32-icon>   => $BS-ICON;
	otherwise      => 0;
      end;
  // If this button is the child of a tool bar, make sure that
  // it won't hang on to the input focus
  when (button-in-tool-bar?(gadget))
    sheet-accepts-focus?(gadget) := #f
  end;
  // Generate an id for buttons with accelerators, but note that
  // we don't bind it because we don't actually need it.
  //--- Will 'defaulted-gadget-accelerator' be too expensive?
  when (defaulted-gadget-accelerator(frame-manager(gadget), gadget))
    ensure-gadget-id(gadget)
  end;
  let label = make-win32-mnemonic-label(text, mnemonic, index, #f);
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     "BUTTON",
		     label,
		     %logior(options, 
			     button-style, group-style, image-style),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  unless (label = $NULL-string) destroy(label) end;
  check-result("CreateWindowEx (BUTTON)", handle);
  when (image)
    update-gadget-image(gadget, handle, image)
  end;
  handle
end method make-gadget-control;

define sealed method update-gadget-image
    (gadget :: <win32-button-mixin>, handle :: <HWND>, image :: <win32-bitmap>) => ()
  //---*** Apparently if this control is in a dialog box, we should be using
  //---*** SendDlgItemMessage instead of SendMessage.
  SendMessage(handle, $BM-SETIMAGE, $IMAGE-BITMAP, 
              pointer-address(image-handle(image)))
end method update-gadget-image;

define sealed method update-gadget-image
    (gadget :: <win32-button-mixin>, handle :: <HWND>, image :: <win32-icon>) => ()
  //---*** Apparently if this control is in a dialog box, we should be using
  //---*** SendDlgItemMessage instead of SendMessage.
  SendMessage(handle, $BM-SETIMAGE, $IMAGE-ICON,   
              pointer-address(image-handle(image)))
end method update-gadget-image;

define sealed method update-button-selection
    (gadget :: <win32-button-mixin>) => ()
  let handle = window-handle(gadget);
  when (handle)
    let selected? = gadget-value(gadget);
    SendMessage(handle, $BM-SETCHECK,
		if (selected?) $true else $false end,
		0)
  end
end method update-button-selection;

define sealed method note-mirror-created
    (gadget :: <win32-button-mixin>, mirror :: <window-mirror>) => ()
  next-method();
  // If this is the last in a group of buttons, lower all of the buttons
  // in order so that Windows sees them all as a single tab group.
  // Then lower all of their non-device parents below the whole group
  // so that the buttons all appear on top of their parents.
  let box = button-gadget-box(gadget);
  duim-debug-message("Button box for %=: %=", gadget, box);
  when (instance?(box, <button-box>))
    let buttons = gadget-box-buttons(box);
    when (gadget == last(buttons))
      let _port = port(gadget);
      for (button :: <win32-button-mixin> in buttons)
	let mirror = sheet-direct-mirror(button);
	lower-mirror(_port, button, mirror)
      end;
      for (button :: <win32-button-mixin> in buttons)
	lower-non-device-parents(_port, button)
      end
    end
  end;
  update-button-selection(gadget)
end method note-mirror-created;

define sealed method note-gadget-value-changed 
    (gadget :: <win32-button-mixin>) => ()
  next-method();
  update-button-selection(gadget)
end method note-gadget-value-changed;

define sealed method handle-command
    (gadget :: <win32-button-mixin>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $BN-CLICKED => handle-gadget-activation(gadget);	// double-click?: #f
    $BN-DBLCLK  => handle-button-gadget-click(gadget, double-click?: #t);
    otherwise   => next-method();
  end
end method handle-command;

define sealed method handle-control-message
    (pane :: <win32-button-mixin>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  maybe-handle-keyboard-interrupt(pane, message, wParam);
  // This rigamarole is so that the code for automatically switching
  // the focus out of toolbar buttons doesn't occur while the button
  // is processing. Without this code, the focus is removed more often
  // than not before the button up, in which case Windows thinks that
  // the button up was received outside the button, so the operation
  // is cancelled.
  select (message)
    $WM-LBUTTONDOWN, $WM-LBUTTONDBLCLK =>
      unless (sheet-accepts-focus?(pane))
	sheet-ignore-focus-change?(pane) := #t
      end;
      #f;
    $WM-LBUTTONUP =>
      sheet-ignore-focus-change?(pane) := #f;
      #f;
    otherwise =>
      #f;
  end
end method handle-control-message;

define sealed method handle-gadget-activation
    (gadget :: <win32-button-mixin>) => (handled? :: <boolean>)
  handle-button-gadget-click(gadget, double-click?: #f)
end method handle-gadget-activation;

define sealed method initialize-sheet-from-resource
    (gadget :: <win32-button-mixin>, handle :: <HWND>) => ()
  next-method();
  gadget-label(gadget) := get-window-text(handle);
  unless (instance?(gadget, <push-button>))
    let check = SendMessage(handle, $BM-GETCHECK, 0, 0);
    let selected? = (check = $BST-CHECKED);
    gadget-value(gadget) := selected?
  end
end method initialize-sheet-from-resource;


/// Push buttons

define sealed class <win32-push-button>
    (<win32-button-mixin>,
     <push-button>,
     <leaf-pane>)
end class <win32-push-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-push-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-push-button>));
define sealed domain initialize (<win32-push-button>);

define sealed method do-compose-space
    (gadget :: <win32-push-button>, #key width, height)
 => (space-req :: <space-requirement>)
  compose-space-for-push-button(gadget, width: width, height: height)
end method do-compose-space;

define sealed method compose-space-for-push-button
    (gadget :: <win32-button-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let (width, height) = gadget-label-size(gadget);
  let (extra-width, extra-height)
    = select (gadget-label(gadget) by instance?)	
	<image> =>
	  values($push-button-extra-icon-width, $push-button-extra-icon-height);
	otherwise =>
	  values($push-button-extra-text-width, $push-button-extra-text-height);
      end;
  let border*2 = $gadget-border-thickness * 2;
  make(<space-requirement>, 
       width:  width  + border*2 + extra-width,
       height: height + border*2 + extra-height)
end method compose-space-for-push-button;

define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <win32-push-button>)
 => (default? :: <boolean>)
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    let handle = window-handle(mirror);
    SendMessage(handle, $BM-SETSTYLE,
		if (default?) $BS-DEFPUSHBUTTON else $BS-PUSHBUTTON end,
		$true)
  end;
  default?
end method gadget-default?-setter;


/// Radio buttons

define sealed class <win32-radio-button>
    (<win32-button-mixin>,
     <radio-button>,
     <leaf-pane>)
end class <win32-radio-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-radio-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-radio-button>));
define sealed domain initialize (<win32-radio-button>);

define sealed method do-compose-space
    (gadget :: <win32-radio-button>, #key width, height)
 => (space-req :: <space-requirement>)
  if (push-button-like?(gadget))
    compose-space-for-push-button(gadget, width: width, height: height)
  else
    let (width, height) = gadget-label-size(gadget);
    let border*2 = $gadget-border-thickness * 2;
    make(<space-requirement>,
	 width:  width + border*2 + $button-icon-width,
	 height: height)
  end
end method do-compose-space;


/// Check buttons

define sealed class <win32-check-button>
    (<win32-button-mixin>,
     <check-button>,
     <leaf-pane>)
end class <win32-check-button>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-check-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-check-button>));
define sealed domain initialize (<win32-check-button>);

define sealed method do-compose-space
    (gadget :: <win32-check-button>, #key width, height)
 => (space-req :: <space-requirement>)
  if (push-button-like?(gadget))
    compose-space-for-push-button(gadget, width: width, height: height)
  else
    let (width, height) = gadget-label-size(gadget);
    let border*2 = $gadget-border-thickness * 2;
    make(<space-requirement>, 
	 width:  width + border*2 + $button-icon-width,
	 height: height)
  end
end method do-compose-space;


/// Group boxes

// Can you believe that these are a class of button in Windows? :-)
define sealed class <win32-group-box>
    (<win32-layout-gadget-mixin>,
     <group-box>,
     <basic-sheet>)
end class <win32-group-box>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <group-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-group-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-group-box>));
define sealed domain initialize (<win32-group-box>);

define sealed method make-gadget-control
    (gadget :: <win32-group-box>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  ignore(image);
  let label = make-win32-mnemonic-label(text, mnemonic, index, #f);
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget, default-border?: #f),
		     "BUTTON",
		     label,
		     %logior(options, $BS-GROUPBOX),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  unless (label = $NULL-string) destroy(label) end;
  check-result("CreateWindowEx (group box)", handle);
  handle
end method make-gadget-control;

define sealed method note-mirror-created
    (gadget :: <win32-group-box>, mirror :: <window-mirror>) => ()
  next-method();
  // Make sure that group-boxes are always last in the Z-order
  lower-mirror(port(gadget), gadget, mirror)
end method note-mirror-created;

define function group-box-border
    (gadget :: <group-box>) => (border :: <integer>)
  let framem = frame-manager(gadget);
  win32-dialog-x-pixels(framem, 4) + $gadget-border-thickness
end function group-box-border;

define sealed method group-box-extra-size
    (gadget :: <group-box>)
 => (extra-width :: <integer>, extra-height :: <integer>,
     min-width :: <integer>)
  let (width, height) = gadget-label-size(gadget);
  let border*2 = group-box-border(gadget) * 2;
  values(border*2, border*2 + height, width + border*2 + 14)
end method group-box-extra-size;

define sealed method group-box-child-offset
    (gadget :: <group-box>)
 => (x :: <integer>, y :: <integer>)
  let border = group-box-border(gadget);
  let (width, height) = gadget-label-size(gadget);
  ignore(width);
  values(border, border + height - $gadget-border-thickness)
end method group-box-child-offset;

define sealed method do-compose-space
    (gadget :: <win32-group-box>, #key width, height)
 => (space-req :: <space-requirement>)
  let (extra-width, extra-height, min-width) = group-box-extra-size(gadget);
  let child = sheet-child(gadget);
  if (child)
    let space-req
      = compose-space(child,
		      width:  width  & (width  - extra-width),
		      height: height & (height - extra-height));
    let (w, w-, w+, h, h-, h+)
      = space-requirement-components(child, space-req);
    make(<space-requirement>,
	 width:      max(w  + extra-width, min-width),
	 min-width:  max(w- + extra-width, min-width),
	 max-width:  max(w+ + extra-width, min-width),
	 height:     h  + extra-height,
	 min-height: h- + extra-height,
	 max-height: h+ + extra-height)
  else
    make(<space-requirement>,
	 width:  min-width,    min-width: min-width, max-width:  $fill,
	 height: extra-height, min-height: extra-height, max-height: $fill)
  end
end method do-compose-space;

define sealed method do-allocate-space
    (gadget :: <win32-group-box>, width :: <integer>, height :: <integer>) => ()
  let child = sheet-child(gadget);
  let (left, top) = group-box-child-offset(gadget);
  let (extra-width, extra-height) = group-box-extra-size(gadget);
  let right  = left + width  - extra-width;
  let bottom = top  + height - extra-height;
  when (child)
    set-sheet-edges(child, left, top, right, bottom)
  end
end method do-allocate-space;


/// Scroll bars

define sealed class <win32-scroll-bar>
    (<win32-gadget-mixin>,
     <scroll-bar>,
     <leaf-pane>)
end class <win32-scroll-bar>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <scroll-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-scroll-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-scroll-bar>));
define sealed domain initialize (<win32-scroll-bar>);

define sealed domain gadget-value (<win32-scroll-bar>);
define sealed domain gadget-value-range (<win32-scroll-bar>);
define sealed domain gadget-slug-size (<win32-scroll-bar>);
define sealed domain scroll-to-position (<win32-scroll-bar>, <object>);
define sealed domain scroll-up-page (<win32-scroll-bar>);
define sealed domain scroll-down-page (<win32-scroll-bar>);
define sealed domain scroll-up-line (<win32-scroll-bar>);
define sealed domain scroll-down-line (<win32-scroll-bar>);

define sealed method make-gadget-control
    (gadget :: <win32-scroll-bar>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     "SCROLLBAR",
		     "",
		     %logior(options,
			     select (gadget-orientation(gadget))
			       #"horizontal" => $SBS-HORZ;
			       #"vertical"   => $SBS-VERT;
			     end),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (SCROLLBAR)", handle);
  handle
end method make-gadget-control;

define sealed method note-mirror-created
    (gadget :: <win32-scroll-bar>, mirror :: <window-mirror>) => ()
  next-method();
  update-scroll-bar-mirror(gadget)
end method note-mirror-created;

define sealed method do-compose-space
    (gadget :: <win32-scroll-bar>, #key width, height)
 => (space-requirement :: <space-requirement>)
  select (gadget-orientation(gadget))
    #"horizontal" =>
      let arrow-width  = GetSystemMetrics($SM-CXHSCROLL);
      let min-width    = arrow-width * 2 + $minimum-scroll-shaft-length;
      let min-height   = GetSystemMetrics($SM-CYHSCROLL);
      let width = constrain-size(width | min-width, min-width, $fill);
      make(<space-requirement>,
	   width:      width,      height:     min-height,
	   min-width:  min-width,  min-height: min-height,
	   max-width:  $fill,      max-height: min-height);
    #"vertical" =>
      let arrow-height = GetSystemMetrics($SM-CYVSCROLL);
      let min-width    = GetSystemMetrics($SM-CXVSCROLL);
      let min-height   = arrow-height * 2 + $minimum-scroll-shaft-length;
      let height = constrain-size(height | min-height, min-height, $fill);
      make(<space-requirement>,
	   width:      min-width,  height:     height,
	   min-width:  min-width,  min-height: min-height,
	   max-width:  min-width,  max-height: $fill);
  end
end method do-compose-space;


define sealed method note-gadget-slug-size-changed
    (gadget :: <win32-scroll-bar>) => ()
  next-method();
  update-scroll-bar-mirror(gadget)
end method note-gadget-slug-size-changed;

define sealed method note-gadget-value-changed
    (gadget :: <win32-scroll-bar>) => ()
  next-method();
  update-scroll-bar-mirror(gadget)
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <win32-scroll-bar>) => ()
  next-method();
  update-scroll-bar-mirror(gadget)
end method note-gadget-value-range-changed;

define sealed method note-scroll-bar-changed
    (gadget :: <win32-scroll-bar>) => ()
  update-scroll-bar-mirror(gadget)
end method note-scroll-bar-changed;

define sealed method update-scroll-bar-mirror
    (gadget :: <win32-scroll-bar>) => ()
  let handle :: false-or(<hWnd>) = window-handle(gadget);
  when (handle)
    let (pos, size, min, max) = scroll-bar-adjusted-contents(gadget);
    with-stack-structure (scroll-info :: <LPCSCROLLINFO>)
      scroll-info.cbSize-value := safe-size-of(<SCROLLINFO>);
      scroll-info.fMask-value  := $SIF-ALL;
      scroll-info.nMin-value   := min;
      scroll-info.nMax-value   := max;
      scroll-info.nPage-value  := size;
      scroll-info.nPos-value   := pos;
      SetScrollInfo(handle, $SB-CTL, scroll-info, #t)
    end
  end
end method update-scroll-bar-mirror;

define sealed method scroll-bar-adjusted-contents
    (gadget :: <win32-scroll-bar>)
 => (position :: <integer>, slug-size :: <integer>,
     min :: <integer>, max :: <integer>)
  let value = gadget-value(gadget);
  let scroll-range = gadget-value-range(gadget);
  let first-value  = scroll-range[0];
  let range-increment
    = if (size(scroll-range) <= 1) 1 else scroll-range[1] - first-value end;
  let slug-pos  = floor/(value - first-value, range-increment);
  let slug-size = floor/(gadget-slug-size(gadget), range-increment);
  let min = 0;
  let max = size(scroll-range);
  when (slug-pos = 0 & slug-size = max - min)
    // If the slug is supposed to be 100% full, ensure that it really happens
    //--- We could just lose the scroll bar altogether in this case!
    slug-size := slug-size + 1
  end;
  values(slug-pos, slug-size, min, max)
end method scroll-bar-adjusted-contents;


define sealed method handle-scrolling
    (gadget :: <win32-scroll-bar>,
     scroll-code :: <integer>, position :: <integer>)
 => (handled? :: <boolean>)
  block (return)
    // Windows uses different name for horizontal and vertical scrolling,
    // but luckily the left/top and right/bottom names have the same values,
    // so this code works for both.
    let value-range = gadget-value-range(gadget);
    select (scroll-code)
      $SB-THUMBTRACK =>
	//--- Was: 'distribute-value-changing-callback(gadget, value-range[position])'
	let (min-pos, max-pos) = values(0, size(value-range) - 1);
	when (position >= min-pos & position <= max-pos)
	  scroll-to-position(gadget, value-range[position])
	end;
      $SB-THUMBPOSITION =>
	let (min-pos, max-pos) = values(0, size(value-range) - 1);
	when (position >= min-pos & position <= max-pos)
	  scroll-to-position(gadget, value-range[position])
	end;
      $SB-PAGEUP =>			// and $SB-PAGELEFT
	scroll-up-page(gadget);
      $SB-PAGEDOWN =>			// and $SB-PAGERIGHT
	scroll-down-page(gadget);
      $SB-LINEUP =>			// and $SB-LINELEFT
	scroll-up-line(gadget);
      $SB-LINEDOWN =>			// and $SB-LINERIGHT
	scroll-down-line(gadget);
      $SB-BOTTOM =>			// and $SB-RIGHT
	scroll-to-position(gadget, #"bottom");
      $SB-TOP =>			// and $SB-LEFT
	scroll-to-position(gadget, #"top");
      $SB-ENDSCROLL =>
	//--- We don't seem to need this one... should we worry about that?
	return(#f);
      otherwise =>
	return(#f)
    end;
    #t
  end
end method handle-scrolling;


/// General collection gadget handling

define class <win32-collection-gadget-mixin>
    (<win32-gadget-mixin>)
end class <win32-collection-gadget-mixin>;

define sealed domain gadget-selection-mode (<win32-collection-gadget-mixin>);
define sealed domain gadget-items (<win32-collection-gadget-mixin>);
define sealed domain gadget-items-setter (<sequence>, <win32-collection-gadget-mixin>);
define sealed domain gadget-selection (<win32-collection-gadget-mixin>);
define sealed domain gadget-selection-setter (<sequence>, <win32-collection-gadget-mixin>);

define sealed method update-gadget-items
    (gadget :: <win32-collection-gadget-mixin>, 
     reset-message :: <integer>,
     add-message :: <integer>) => ()
  let handle = window-handle(gadget);
  when (handle)
    SendMessage(handle, reset-message, 0, 0);
    for (item in gadget-items(gadget))
      let label = collection-gadget-item-label(gadget, item);
      with-c-string (c-string = label)
        SendMessage(handle, add-message, 0, pointer-address(c-string))
      end
    end
  end
end method update-gadget-items;

define sealed method set-single-selection-gadget-selection
    (gadget :: <win32-collection-gadget-mixin>, message :: <integer>) => ()
  let handle = window-handle(gadget);
  when (handle)
    let selection = gadget-selection(gadget);
    SendMessage(handle, message,
		if (empty?(selection)) -1 else selection[0] end,
		0)
  end
end method set-single-selection-gadget-selection;


/// List boxes

define sealed class <win32-list-box> 
    (<win32-collection-gadget-mixin>,
     <list-box>,
     <leaf-pane>)
end class <win32-list-box>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-list-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-list-box>));
define sealed domain initialize (<win32-list-box>);

define sealed method make-gadget-control
    (gadget :: <win32-list-box>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let selection-mode = gadget-selection-mode(gadget);
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     "LISTBOX",
		     "",
		     %logior(options, 
			     if (sheet-tab-stop?(gadget)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
			     $LBS-DISABLENOSCROLL, $LBS-NOTIFY,
			     select (selection-mode)
			       #"none"     => 0; //--- Is this right?
			       #"single"   => 0;
			       #"multiple" => $LBS-EXTENDEDSEL;
			     end),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (LISTBOX)", handle);
  handle
end method make-gadget-control;

define sealed method gadget-default-options
    (gadget :: <win32-list-box>) => (options :: <options-type>)
  //--- Horizontal scroll bars just don't seem to work in list boxes
  %logand(next-method(), %lognot($WS-HSCROLL))
end method gadget-default-options;

define sealed method do-compose-space 
    (pane :: <win32-list-box>, #key width, height)
 => (space-req :: <space-requirement>)
  compose-space-for-list-box(pane,
			     width: width, height: height,
			     default-lines: $list-box-default-visible-lines,
			     minimum-lines: $list-box-minimum-visible-lines,
			     extra-height:  $list-box-extra-height)
end method do-compose-space;

//--- Are the heuristics here reasonable?
define sealed method compose-space-for-list-box
    (pane :: <win32-gadget-mixin>,
     #key width, height,
          extra-height  = $list-box-extra-height,
	  default-lines = $list-box-default-visible-lines,
	  minimum-lines = $list-box-minimum-visible-lines,
          extra-lines   = 0,
          icon-height   = 0)
 => (space-req :: <space-requirement>)
  let _port = port(pane);
  let text-style  = get-default-text-style(_port, pane);
  let line-height = max(font-height(text-style, _port), icon-height);
  let char-width  = font-width(text-style, _port);
  let vsp         = $default-vertical-spacing;
  let nlines      = gadget-lines(pane);
  let thickness*2 = $gadget-border-thickness * 2;
  let min-width   = $minimum-visible-characters * char-width + thickness*2;
  let max-width   = $fill;
  local method lines->height (nlines :: <integer>) => (height :: <integer>)
	  nlines * line-height + (nlines - 1) * vsp + thickness*2 + extra-height
	end method;
  //---*** This all needs to take into account the size of the scrollbars
  select (gadget-scroll-bars(pane))
    #f, #"none", #"horizontal" =>
      // If there are no vertical scroll bars, make the gadget just tall
      // enough to hold all the items
      let nlines = nlines | size(gadget-items(pane));
      let best-width  = min-width;	//--- compute this from the labels
      let best-height = lines->height(nlines + extra-lines);
      let min-height = best-height;
      let max-height = $fill;
      let width  = constrain-size(width  | best-width,  min-width,  max-width);
      let height = constrain-size(height | best-height, min-height, max-height);
      make(<space-requirement>,
	   width:     width,     height:     height,
	   min-width: min-width, min-height: min-height,
	   max-width: max-width, max-height: max-height);
    otherwise =>
      // If there is a vertical scroll bar, use the requested height
      let nlines     = nlines | default-lines;
      let min-lines  = minimum-lines;
      let min-height = lines->height(min-lines);
      let max-height = $fill;
      let best-width  = min-width;	//--- compute this from the labels
      let best-height = lines->height(nlines + extra-lines);
      let width  = constrain-size(width  | best-width,  min-width,  max-width);
      let height = constrain-size(height | best-height, min-height, max-height);
      make(<space-requirement>,
	   width:     width,     height:     height,
	   min-width: min-width, min-height: min-height,
	   max-width: max-width, max-height: max-height);
  end
end method compose-space-for-list-box;

define sealed method note-mirror-created
    (gadget :: <win32-list-box>, mirror :: <window-mirror>) => ()
  next-method();
  note-gadget-items-changed(gadget);
  update-gadget-selection(gadget)
end method note-mirror-created;

define sealed method note-gadget-items-changed
    (gadget :: <win32-list-box>) => ()
  next-method();
  update-gadget-items(gadget, $LB-RESETCONTENT, $LB-ADDSTRING);
  //--- The following now gets done in the front-end
  /* update-gadget-selection(gadget) */
end method note-gadget-items-changed;

define sealed method update-gadget-selection
    (gadget :: <win32-list-box>) => ()
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    #"single" =>
      set-single-selection-gadget-selection(gadget, $LB-SETCURSEL);
    #"multiple" =>
      // Get the indices of the items currently selected in the <list-box>.
      let handle = window-handle(gadget);
      when (handle)
	let n-items :: <integer> = size(gadget-items(gadget));
	with-stack-structure (selections :: <c-unsigned-int*>, element-count: n-items)
	  let n-selected :: <integer>
	    = SendMessage(handle, $LB-GETSELITEMS,
			  n-items,
			  pointer-address(selections));
	  let old-selection :: <simple-object-vector> = make(<simple-vector>, size: n-selected);
	  for (i :: <integer> from 0 below n-selected)
	    old-selection[i] := pointer-value(selections, index: i)
	  end;
	  // At this point, n-selected and old-selection reflect the current state
	  // of the Windows gadget.  Now what we want to do is update the Windows
	  // gadget to reflect the state of 'gadget-selection', but avoid flickering
	  let new-selection = gadget-selection(gadget);
	  for (i :: <integer> from 0 below n-items)
	    let old-state = member?(i, old-selection);
	    let new-state = member?(i, new-selection);
	    when (old-state ~== new-state)
	      SendMessage(handle, $LB-SETSEL, if (new-state) 1 else 0 end, i)
	    end
	  end
	end
      end;
  end
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <win32-list-box>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

define sealed method handle-selection-changed
    (gadget :: <win32-list-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    #"single" =>
      let selection = SendMessage(handle, $LB-GETCURSEL, 0, 0);
      distribute-selection-changed-callback
	(gadget,
	 if (selection = $LB-ERR) #[] else vector(selection) end);
    #"multiple" =>
      let n-items :: <integer> = size(gadget-items(gadget));
      with-stack-structure (selections :: <c-unsigned-int*>, element-count: n-items)
	let n-selected :: <integer>
	  = SendMessage(handle, $LB-GETSELITEMS,
			n-items,
			pointer-address(selections));
	let selection :: <simple-object-vector> = make(<simple-vector>, size: n-selected);
	for (i :: <integer> from 0 below n-selected)
	  selection[i] := pointer-value(selections, index: i)
	end;
	distribute-selection-changed-callback(gadget, selection)
      end;
  end;
  #t
end method handle-selection-changed;

define sealed method handle-command
    (gadget :: <win32-list-box>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $LBN-SELCHANGE, $LBN-SELCANCEL =>
      handle-selection-changed(gadget);
    $LBN-DBLCLK =>
      activate-win32-gadget(gadget);
      #t;
    otherwise => 
      next-method();
  end
end method handle-command;


/// Option boxes

define sealed class <win32-option-box> 
    (<win32-collection-gadget-mixin>,
     <option-box>,
     <leaf-pane>)
end class <win32-option-box>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <option-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-option-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-option-box>));
define sealed domain initialize (<win32-option-box>);

define sealed method make-gadget-control
    (gadget :: <win32-option-box>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     "COMBOBOX",
		     "",
		     %logior(options, 
			     if (sheet-tab-stop?(gadget)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
			     $CBS-DROPDOWNLIST),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (OPTIONBOX)", handle);
  handle
end method make-gadget-control;

define sealed method do-compose-space 
    (gadget :: <win32-option-box>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(height);
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let min-width = $minimum-visible-characters * font-width(text-style, _port);
  let width = constrain-size(width | min-width, min-width, $fill);
  //---*** How should we really calculate the constant below?
  let height = font-height(text-style, _port) + $option-box-extra-height;
  make(<space-requirement>,
       width:  max(width, min-width), min-width: min-width, max-width: $fill,
       height: height)
end method do-compose-space;

// Windows option boxes take their size to mean their visible size plus
// the size of their pull-down.  Hence we make the DUIM size always be
// the size of the visible size, but make sure we set the size that
// Windows expects when we set the mirror's size.
define sealed method set-mirror-edges
    (_port :: <win32-port>, gadget :: <win32-option-box>, mirror :: <window-mirror>,
     left  :: <integer>, top    :: <integer>, 
     right :: <integer>, bottom :: <integer>) => ()
  let height = win32-option-box-height(gadget);
  next-method(_port, gadget, mirror,
	      left, top, right, top + height)
end method set-mirror-edges;

define sealed method win32-option-box-height
    (gadget :: <win32-option-box>) => (height :: <integer>)
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let n-items :: <integer> = size(gadget-items(gadget));
  let line-height = font-height(text-style, _port);
  let vsp         = $default-vertical-spacing;
  let nlines      = max(n-items, 1);
  //---*** How can we compute this for real?
  line-height + $option-box-extra-height + 4
    + min($option-box-maximum-popup-height,
	  nlines * line-height + (nlines - 1) * vsp)
end method win32-option-box-height;

define sealed method note-gadget-items-changed
    (gadget :: <win32-option-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-gadget-items(gadget, $CB-RESETCONTENT, $CB-ADDSTRING);
    //--- The following now gets done in the front-end
    /* update-gadget-selection(gadget); */
    // Call 'set-mirror-edges' to make sure that the drop-down menu
    // is the correct size.
    let _port = port(gadget);
    let (left, top, right, bottom) = mirror-edges(_port, gadget, mirror);
    set-mirror-edges(_port, gadget, mirror, left, top, right, bottom)
  end
end method note-gadget-items-changed;

define sealed method update-gadget-selection
    (gadget :: <win32-option-box>) => ()
  set-single-selection-gadget-selection(gadget, $CB-SETCURSEL)
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <win32-option-box>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

define sealed method note-mirror-created
    (gadget :: <win32-option-box>, mirror :: <window-mirror>) => ()
  next-method();
  note-gadget-items-changed(gadget);
  update-gadget-selection(gadget)
end method note-mirror-created;

define sealed method handle-selection-changed
    (gadget :: <win32-option-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  let selection = SendMessage(handle, $CB-GETCURSEL, 0, 0);
  unless (selection = $CB-ERR)
    distribute-selection-changed-callback(gadget, vector(selection));
    #t
  end
end method handle-selection-changed;

define sealed method handle-command
    (gadget :: <win32-option-box>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $CBN-SELENDOK =>
      handle-selection-changed(gadget);
    otherwise =>
      next-method();
  end
end method handle-command;

define sealed method cancel-gadget 
    (gadget :: <win32-option-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  when (handle & (SendMessage(handle, $CB-GETDROPPEDSTATE, 0, 0) = $true))
    SendMessage(handle, $CB-SHOWDROPDOWN, $false, 0);
    #t
  end
end method cancel-gadget;


/// Combo boxes

define sealed class <win32-combo-box> 
    (<win32-collection-gadget-mixin>,
     <combo-box>,
     <leaf-pane>)
  sealed slot %changed? :: <boolean> = #f;
  sealed slot %text-field :: false-or(<win32-text-field>) = #f;
end class <win32-combo-box>;

//--- If <win32-combo-box> was a <text-field>, we would not need this
define sealed method activate-win32-gadget
    (gadget :: <combo-box>) => (activated? :: <boolean>)
  handle-text-gadget-changed(gadget);
  next-method()
end method activate-win32-gadget;

define sealed domain make (singleton(<win32-combo-box>));
define sealed domain initialize (<win32-combo-box>);

define sealed class <win32-combo-box-text-field>
    (<win32-subgadget-mixin>,
     <win32-text-field>)
end class <win32-combo-box-text-field>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <combo-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-combo-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-combo-box-text-field>));
define sealed domain initialize (<win32-combo-box-text-field>);

define sealed method make-gadget-control
    (gadget :: <win32-combo-box>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(gadget-extended-options(gadget),
		     "COMBOBOX",
		     "",
		     %logior(options, 
			     if (sheet-tab-stop?(gadget)) %logior($WS-GROUP, $WS-TABSTOP) else 0 end,
			     $CBS-DROPDOWN, $CBS-AUTOHSCROLL),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (COMBOBOX)", handle);
  subclass-combo-box-text-field(gadget, handle);
  handle
end method make-gadget-control;

// This is a bizarre hack to subclass the text field which is
// a child of the combo box.
define function subclass-combo-box-text-field
    (gadget :: <win32-combo-box>, handle :: <HWND>) => ()
  let edit-control = GetWindow(handle, $GW-CHILD);
  check-result("Finding the combo box's edit control", edit-control);
  // This is odd, but making this gadget actually does all the work
  // to mirror and attach everything correctly.
  let text-field = make(<win32-combo-box-text-field>,
			owner: gadget, handle: edit-control);
  gadget.%text-field := text-field
end function subclass-combo-box-text-field;

define sealed method do-compose-space 
    (gadget :: <win32-combo-box>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(height);
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let min-width = $minimum-visible-characters * font-width(text-style, _port);
  let width = constrain-size(width | min-width, min-width, $fill);
  //---*** How should we really calculate the constant below?
  let height = font-height(text-style, _port) + $option-box-extra-height;
  make(<space-requirement>,
       width:  max(width, min-width), min-width: min-width, max-width: $fill,
       height: height)
end method do-compose-space;

// Windows combo boxes take their size to mean their visible size plus
// the size of their pull-down. Hence we make the DUIM size always be
// the size of the visible size, but make sure we set the size that
// Windows expects when we set the mirror's size.
define sealed method set-mirror-edges
    (_port :: <win32-port>, gadget :: <win32-combo-box>, mirror :: <window-mirror>,
     left  :: <integer>, top    :: <integer>, 
     right :: <integer>, bottom :: <integer>) => ()
  let height = win32-combo-box-height(gadget);
  next-method(_port, gadget, mirror,
	      left, top, right, top + height)
end method set-mirror-edges;

define sealed method win32-combo-box-height
    (gadget :: <win32-combo-box>) => (height :: <integer>)
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let n-items :: <integer> = size(gadget-items(gadget));
  let line-height = font-height(text-style, _port);
  let vsp         = $default-vertical-spacing;
  let nlines      = max(n-items, 1);
  //---*** How can we compute this for real?
  line-height + $option-box-extra-height + 4
    + min($option-box-maximum-popup-height,
	  nlines * line-height + (nlines - 1) * vsp)
end method win32-combo-box-height;

define sealed method note-gadget-items-changed
    (gadget :: <win32-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-gadget-items(gadget, $CB-RESETCONTENT, $CB-ADDSTRING);
    update-gadget-text(gadget, mirror);
    // Call 'set-mirror-edges' to make sure that the drop-down menu
    // is the correct size.
    let _port = port(gadget);
    let (left, top, right, bottom) = mirror-edges(_port, gadget, mirror);
    set-mirror-edges(_port, gadget, mirror, left, top, right, bottom)
  end
end method note-gadget-items-changed;

define sealed method note-gadget-text-changed 
    (gadget :: <win32-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

define sealed method note-gadget-value-changed
    (gadget :: <win32-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-value-changed;

define sealed method note-mirror-created
    (gadget :: <win32-combo-box>, mirror :: <window-mirror>) => ()
  next-method();
  note-gadget-items-changed(gadget)
end method note-mirror-created;

define sealed method handle-selection-changed
    (gadget :: <win32-combo-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  let selection = SendMessage(handle, $CB-GETCURSEL, 0, 0);
  unless (selection = $CB-ERR)
    let item = gadget-items(gadget)[selection];
    let text = collection-gadget-item-label(gadget, item);
    distribute-text-changed-callback(gadget, text);
    #t
  end
end method handle-selection-changed;

define sealed method handle-command
    (gadget :: <win32-combo-box>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $CBN-EDITCHANGE => handle-text-gadget-changing(gadget);
    $CBN-SELENDOK   => handle-selection-changed(gadget);
//---*** This doesn't seem to work, and also messes up
//---*** the SELENDOK so I've taken it out for now.
//  $EN-KILLFOCUS   => handle-text-gadget-changed(gadget);
    otherwise       => next-method();
  end
end method handle-command;

//--- This is a hack to wrestle the magic keys from the combo-box so
//--- that we can correctly handle hitting return, escape or tab.
define sealed method handle-control-message
    (text-field :: <win32-combo-box-text-field>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  maybe-handle-keyboard-interrupt(text-field, message, wParam);
  let gadget = subgadget-owner(text-field);
  duim-debug-message("Handling message #x%x for subclassed %=",
		     message, gadget);
  when (message = $WM-KEYUP | message = $WM-CHAR | message = $WM-KEYDOWN)
    let key-name = virtual-key->keysym(wParam);
    duim-debug-message("Handling key-name %= for subclassed %=",
		       key-name, gadget);
    select (key-name)
      #"return", #"escape" =>
	when (message = $WM-KEYDOWN)
	  handle-command-for-id(gadget, $IDOK)
	end;
	#t;
      #"tab" =>
	//---*** We need to handle Tab and shift-Tab somehow, since
	//---*** combo boxes won't do it for us.
	duim-debug-message("Dropping Tab on the floor for %=!", gadget);
	#t;
      otherwise =>
	#f;
    end
  end
end method handle-control-message;

define sealed method cancel-gadget 
    (gadget :: <win32-combo-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  when (handle & (SendMessage(handle, $CB-GETDROPPEDSTATE, 0, 0) = $true))
    SendMessage(handle, $CB-SHOWDROPDOWN, $false, 0);
    #t
  end
end method cancel-gadget;


define method gadget-text-buffer
    (gadget :: <win32-combo-box-text-field>)
 => (text :: <string>)
  gadget-text-buffer(subgadget-owner(gadget))
end method gadget-text-buffer;

define method gadget-text-buffer-setter
    (text :: <string>, gadget :: <win32-combo-box-text-field>)
 => (text :: <string>)
  gadget-text-buffer-setter(text, subgadget-owner(gadget))
end method gadget-text-buffer-setter;

define sealed method text-caret-position
    (gadget :: <win32-combo-box>)
 => (position :: false-or(<integer>))
  text-caret-position(gadget.%text-field)
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <win32-combo-box>)
 => (position :: false-or(<integer>))
  text-caret-position-setter(position, gadget.%text-field)
end method text-caret-position-setter;

//---*** What about 'selected-text-setter'?
define sealed method selected-text
    (gadget :: <win32-combo-box>) => (string :: false-or(<string>))
  selected-text(gadget.%text-field)
end method selected-text;

define sealed method text-selection
    (gadget :: <win32-combo-box>)
 => (range :: type-union(<text-range>, one-of(#f)))
  text-selection(gadget.%text-field)
end method text-selection;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)), gadget :: <win32-combo-box>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  text-selection-setter(range, gadget.%text-field)
end method text-selection-setter;


/// Viewports

define sealed class <win32-viewport>
    (<viewport>,
     <win32-pane-mixin>,
     <permanent-medium-mixin>,
     <mirrored-sheet-mixin>,
     <single-child-composite-pane>)
end class <win32-viewport>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-viewport>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-viewport>));
define sealed domain initialize (<win32-viewport>);

/*---*** Omit this method, since it causes very odd behavior
define sealed method repaint-mirror
    (viewport :: <win32-viewport>, mirror :: <window-mirror>) => ()
  let sheet = sheet-child(viewport);
  when (sheet & ~sheet-direct-mirror(sheet))
    next-method()
  end
end method repaint-mirror;
*/

//---*** Kludge to stop double redisplay events for mirrored children
//---*** of viewports. Should really address the problem in viewports...
define sideways method handle-event
    (viewport :: <win32-viewport>, event :: <window-repaint-event>) => ()
  let child = sheet-child(viewport);
  when (child & ~sheet-direct-mirror(child))
    handle-event(event-handler(child), event)
  end
end method handle-event;

// Things are weird for our mirrored viewports!
// We only want to erase the background of a viewport when nothing
// else will do it for us, and we also want to erase in the background
// color of the child.  We erase a viewport when:
//  - the viewport doesn't have a child yet, or
//  - the child of the viewport is not mirrored (and won't get repaint events), or
//  - the viewport is not completely filled with the child
define method erase-background
    (viewport :: <win32-viewport>, mirror :: <window-mirror>, hDC :: <HDC>) => ()
  let sheet = sheet-child(viewport);
  if (~sheet)
    next-method()		// no kid, just erase the viewport
  else
    let sheet-mirror = sheet-direct-mirror(sheet);
    when (~sheet-mirror		// unmirrored kid, erase the viewport
	  | begin		// any of the viewport visible? if so, erase it
	      let (vl, vt, vr, vb) = box-edges(sheet-region(viewport));
	      let (sl, st, sr, sb) = sheet-edges(sheet);
	      ~ltrb-contains-ltrb?(sl, st, sr, sb,
				   vl, vt, vr, vb)
	    end)
      let brush = if (sheet-mirror)
		    // Erase in the color of the child
		    mirror-background-brush(sheet, sheet-mirror)
		  else
		    //--- Well, we should be using the sheet's background anyway
		    //--- but 'mirror-background-brush' is gonna hose us...
		    mirror-background-brush(viewport, mirror)
		  end;
      let (width, height) = sheet-size(viewport);
      let pen :: <HPEN> = $null-hpen;
      //---*** Do we need to restore the DC afterwards?
      check-result("SelectObject (null pen)", SelectObject(hDC, pen));
      check-result("SelectObject (brush)",    SelectObject(hDC, brush));
      //--- '+ 1' because Windows doesn't draw the lower-right of rectangles
      Rectangle(hDC, 0, 0, width + 1, height + 1)
    end
  end
end method erase-background;


/// Borders

define sealed class <win32-border>
    (<standard-repainting-mixin>,
     <border>,
     <basic-sheet>)
  sealed slot %pen   :: false-or(<standard-pen>) = #f;
  sealed slot %brush :: false-or(type-union(<standard-brush>, <ink>)) = #f;
end class <win32-border>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <border>, #key label)
 => (class :: <class>, options :: false-or(<sequence>))
  let border-class = if (label) <win32-group-box> else <win32-border> end;
  values(border-class, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-border>));
define sealed domain initialize (<win32-border>);

define sealed method port-default-text-style
    (_port :: <win32-port>, gadget :: <win32-border>)
 => (text-style :: false-or(<text-style>))
  $win32-default-gadget-text-style
end method port-default-text-style;

define sealed method do-compose-space
    (pane :: <win32-border>, #key width, height)
 => (space-req :: <space-requirement>)
  let thickness*2 = $gadget-border-thickness * 2;
  space-requirement+(pane,
		     next-method(pane,
				 width:  width  & width  - thickness*2,
				 height: height & height - thickness*2),
		     width: thickness*2, height: thickness*2)
end method do-compose-space;

define sealed method do-allocate-space
    (pane :: <win32-border>, width :: <integer>, height :: <integer>) => ()
  let child = sheet-child(pane);
  let thickness = $gadget-border-thickness;
  when (child)
    set-sheet-edges(child,
                    thickness, thickness,
                    width - thickness, height - thickness)
  end
end method do-allocate-space;

define sealed method handle-repaint
    (pane :: <win32-border>, medium :: <win32-medium>, region :: <region>) => ()
  ignore(region);	// not worth checking
  let (left, top, right, bottom) = box-edges(pane);
  draw-border(pane, medium, border-type(pane), left, top, right, bottom)
end method handle-repaint;

define sealed method draw-border
    (pane :: <sheet>, medium :: <win32-medium>,
     type :: <border-type>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  let hDC :: <HDC> = get-DC(medium);
  let transform = medium-device-transform(medium);
  with-device-coordinates (transform, left, top, right, bottom)
    with-stack-structure (rect :: <LPRECT>)
      rect.left-value   := left;
      rect.top-value    := top;
      rect.right-value  := right;
      rect.bottom-value := bottom;
      let (edges, flags)
        = select (type)
	    #"flat", #"none" =>
	      values(0, %logior($BF-FLAT, $BF-RECT));
	    #f, #"sunken", #"input", #"output" =>
	      values($EDGE-SUNKEN, $BF-RECT);
	    #"raised" =>
	      values($EDGE-RAISED, $BF-RECT);
	    #"ridge" =>
	      values($EDGE-BUMP,   $BF-RECT);
	    #"groove" =>
	      values($EDGE-ETCHED, $BF-RECT);
          end;
      check-result("DrawEdge", DrawEdge(hdc, rect, edges, flags))
    end
  end
end method draw-border;
