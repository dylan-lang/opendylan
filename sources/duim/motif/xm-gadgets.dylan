Module:    motif-duim
Synopsis:  Motif basic gadget implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif gadgets

define open abstract class <motif-gadget-mixin>
    (<gadget>,
     <motif-pane-mixin>)
  slot %initial-width  = #f;
  slot %initial-height = #f;
end class <motif-gadget-mixin>;

define open abstract class <motif-scrolled-gadget-mixin>
    (<motif-gadget-mixin>)
end class <motif-scrolled-gadget-mixin>;

define open abstract class <motif-buttons-gadget-mixin>
    (<motif-gadget-mixin>)
end class <motif-buttons-gadget-mixin>;

define protocol <<motif-gadget-protocol>> ()
  function make-motif-widget
    (_port :: <motif-port>, gadget :: <abstract-gadget>)
 => (widget, scroll-widget);
  function initialize-gadget-mirror
    (gadget :: <abstract-gadget>, mirror :: <motif-mirror>) => ();
  function initialize-gadget-callbacks
    (gadget :: <abstract-gadget>, mirror :: <motif-mirror>, widget) => ();
end protocol <<motif-gadget-protocol>>;

define sealed method do-make-mirror
    (_port :: <motif-port>, gadget :: <motif-gadget-mixin>)
 => (mirror :: <window-mirror>)
  let (widget, scroll-widget)
    = make-motif-widget(_port, gadget);
  assert(~scroll-widget,
	 "There shouldn't be a scroll widget for non-scrolling gadgets!");
  let mirror
    = make(<window-mirror>, sheet: gadget, widget: widget);
  install-gadget-callbacks(gadget, mirror, widget);
  xt/add-widget-destroy-callback(widget, destroy-mirror-callback, mirror);
  initialize-gadget-mirror(gadget, mirror);
  xt/XtRealizeWidget(widget);
  record-initial-widget-sizes(gadget, widget);
  //---*** Do we really need this?  Won't the DUIM core do the right thing
  //---*** during mirroring, creating it unmapped then mapping it if necessary?
  when (sheet-enabled?(gadget))
    xt/XtMapWidget(widget)
  end
end method do-make-mirror;

define sealed method do-make-mirror
    (_port :: <motif-port>, gadget :: <motif-scrolled-gadget-mixin>)
 => (mirror :: <window-mirror>)
  let (widget, scroll-widget)
    = make-motif-widget(_port, gadget);
  let mirror
    = make(<scrolled-mirror>, sheet: gadget, widget: scroll-widget, work-widget: widget);
  install-gadget-callbacks(gadget, mirror, widget);
  xt/add-widget-destroy-callback(widget, destroy-mirror-callback, mirror);
  initialize-gadget-mirror(gadget, mirror);
  if (gadget-scroll-bars(gadget))
    xt/XtSetValues(widget, mapped-when-managed: #t);
    xt/XtRealizeWidget(scroll-widget);
    xt/XtRealizeWidget(widget);
    xt/XtManageChild(widget);
    xt/XtManageChild(scroll-widget)
  else
    xt/XtRealizeWidget(widget);
    xt/XtManageChild(widget)
  end;
  record-initial-widget-sizes(gadget, widget);
  //---*** Do we really need this?  Won't the DUIM core do the right thing
  //---*** during mirroring, creating it unmapped then mapping it if necessary?
  when (sheet-enabled?(gadget))
    xt/XtMapWidget(widget)
  end
end method do-make-mirror;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-gadget-mixin>, mirror :: <window-mirror>) => ()
  #f
end method initialize-gadget-mirror;

//--- Width and height for scrolled windows are a little shy of the mark,
//--- by about 3 or 4 pixels, so we have to kludge it
define method record-initial-widget-sizes
    (gadget :: <motif-gadget-mixin>, widget :: xt/<Widget>)
  let parent = xt/XtParent(widget);
  let (xwidth, xheight) = xt/XtGetValues(widget, #"width", #"height");
  if (xm/XmIsScrolledWindow(parent))
    gadget.%initial-width
      := xwidth  + scroll-dimension(parent, #"vertical",   #"width")  + 4;
    gadget.%initial-height
      := xheight + scroll-dimension(parent, #"horizontal", #"height") + 4
  else
    gadget.%initial-width  := xwidth
    gadget.%initial-height := xheight
  end
end method record-initial-widget-sizes;

define method scroll-dimension
    (scrolled-widget, orientation, dimension) => (size :: <integer>)
  let orientation = select (orientation)
		      #"horizontal" => xm/$XmHORIZONTAL;
		      #"vertical"   => xm/$XmVERTICAL;
		    end;
  block (return)
    for (child in xt/XtGetValues(scrolled-widget, #"children"))
      when (xm/XmIsScrollBar(child)
	    & xt/XtGetValues(widget, #"orientation") = orientation)
	return(xt/XtGetValues(child, dimension))
      end
    end;
    0
  end
end method scroll-dimension;


define method widget-attributes
    (_port :: <motif-port>, gadget :: <motif-gadget-mixin>)
 => (foreground, background, font)
  let foreground :: <color> = get-default-foreground(_port, gadget);
  let background :: <color> = get-default-background(_port, gadget);
  let text-style = get-default-text-style(_port, gadget);
  let foreground
    = if (default-foreground(gadget))
	vector(foreground:, allocate-color(foreground, port-default-palette(_port)))
      else
	#[]
      end;
  let background
    = if (default-background(gadget))
	vector(background:, allocate-color(background, port-default-palette(_port)))
      else
	#[]
      end;
  let font
    = if (default-text-style(gadget))
	vector(text-style:, text-style-mapping(_port, text-style).%font-name)
      else
	#[]
      end;
  value(foreground, background, font)
end method widget-attributes;


// Not unreasonable as a default method
// We take the values suggested by Motif as the default sizes
define method do-compose-space
    (pane :: <motif-gadget-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let mirror  = sheet-mirror(pane);
  let widget  = mirror-widget(mirror);
  let (xwidth, xheight) = xt/XtGetValues(widget, #"width", #"height");
  let min-width  = pane.%initial-width  | xwidth;
  let min-height = pane.%initial-height | xheight;
  make(<space-requirement>,
       width:      if (width)  max(min-width,  width)  else min-width  end, 
       min-width:  min-width,
       max-width:  $fill,
       height:     if (height) max(min-height, height) else min-height end,
       min-height: min-height,
       max-height: $fill)
end method do-compose-space;


define sealed method defaulted-gadget-label
    (gadget :: <gadget>) => (label)
  gadget-label(gadget) | $default-label
end method defaulted-gadget-label;

define sealed method note-gadget-label-changed
    (gadget :: <motif-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-label(gadget, mirror)
end method note-gadget-label-changed;

define sealed method update-mirror-label
    (gadget :: <motif-gadget-mixin>, mirror :: <window-mirror>) => ()
  let widget = mirror-widget(sheet-direct-mirror(gadget));
  let label  = defaulted-gadget-label(gadget);
  let label :: <string> = if (instance?(label, <string>)) label else "" end;
  xt/XtSetValues(widget, label-string: label)
end method update-mirror-label;


define sealed method note-gadget-enabled
    (client, gadget :: <motif-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let widget = mirror-widget(sheet-direct-mirror(gadget));
  xt/XtSetSensitive(widget, #t)
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <motif-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let widget = mirror-widget(sheet-direct-mirror(gadget));
  xt/XtSetSensitive(widget, #f)
end method note-gadget-disabled;

//---*** DO WE NEED THIS?
define sealed method activate-motif-gadget
    (gadget :: <action-gadget>) => (activated? :: <boolean>)
  when (gadget-activate-callback(gadget))
    distribute-activate-callback(gadget);
    #t
  end
end method activate-motif-gadget;

//---*** DO WE NEED THIS?
define sealed method activate-motif-gadget
    (gadget :: <text-field>) => (activated? :: <boolean>)
  handle-text-gadget-changed(gadget);
  next-method()
end method activate-motif-gadget;


/// Callback suppression

// Motif's gadgets are inconsistent about whether they allow the suppression
// of the value-changed callback when the gadget-value is set from code.
// (E.g. XmToggleButtonSetState has a third argument, XmTextFieldSetString doesn't.)
// Since DUIM sometimes calls its own callbacks from Silica code, we use this
// mixin to suppress the callback from within the Motif callback when the
// Motif gadget doesn't do so.
define open abstract class <suppress-value-changed-callback-mixin>
    (<value-gadget>)
  sealed slot %suppress-callback? :: <boolean> = #f;
end class <suppress-value-changed-callback-mixin>;

define method gadget-value-setter
    (value, gadget :: <suppress-value-changed-callback-mixin>, #key do-callback?)
 => (value)
  ignore(do-callback?);
  block ()
    gadget.%suppress-callback? := #t;
    next-method()
  cleanup
    gadget.%suppress-callback? := #f;
  end
end method gadget-value-setter;

define macro suppressing-value-changed-callback
  { suppressing-value-changed-callback (?gadget:expression) ?:body end }
    => { unless (?gadget.%suppress-callback?)
	   ?body
	 end }
end macro suppressing-value-changed-callback;


/// Exit, cancel, default button, etc.

//---*** DO WE NEED THIS?
define method handle-command-for-id
    (sheet :: <sheet>, id :: <integer>) => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  select (id)
    $IDOK => 
      duim-debug-message("Handling IDOK for %=", sheet);
      activate-default-button(frame);
    $IDCANCEL =>
      duim-debug-message("Handling IDCANCEL for %=", sheet);
      handle-cancel(frame);
    otherwise =>
      let gadget = id->gadget(sheet, id);
      if (gadget)
	handle-gadget-activation(gadget)
      else
	handle-id-activation(frame, id)
      end;
  end
end method handle-command-for-id;

//---*** DO WE NEED THIS?
define sealed method handle-gadget-activation
    (gadget :: <gadget>) => (handled? :: <boolean>)
  duim-debug-message("Ignoring activation command for gadget %=", gadget);
  #f
end method handle-gadget-activation;

//---*** DO WE NEED THIS?
// This handles IDOK commands for more than just buttons...
define method activate-default-button
    (frame :: <frame>) => (activated? :: <boolean>)
  let gadget = motif-sheet-with-focus();
  duim-debug-message("  Handling IDOK: focus currently %=", gadget);
  let activated? = instance?(gadget, <action-gadget>)
		   & gadget-enabled?(gadget)
		   & activate-motif-gadget(gadget);
  // If we didn't activate the gadget, try to activate the default button
  unless (activated?)
    let button = frame-default-button(frame);
    // Don't activate an upmapped or disabled default button...
    when (button & sheet-mapped?(button) & gadget-enabled?(button))
      handle-gadget-activation(button)
    end
  end
end method activate-default-button;

//---*** DO WE NEED THIS?
define function motif-sheet-with-focus
    () => (sheet :: false-or(<sheet>))
  let handle = GetFocus();
  let mirror = window-mirror(handle);
  when (mirror)
    let sheet = mirror-sheet(mirror);
    if (instance?(sheet, <motif-subgadget-mixin>))
      subgadget-owner(sheet)
    else
      sheet
    end
  end
end function motif-sheet-with-focus;


//---*** DO WE NEED THIS?
define function handle-cancel
    (frame :: <frame>) => (handled? :: <boolean>)
  let gadget = motif-sheet-with-focus();
  duim-debug-message("  Handling IDCANCEL: focus currently %=", gadget);
  if (instance?(gadget, <gadget>) & cancel-gadget(gadget))
    #t
  else
    cancel-frame(frame)
  end
end function handle-cancel;

//---*** DO WE NEED THIS?
define sealed method cancel-frame
    (frame :: <frame>) => (handled? :: <boolean>)
  //---*** We should handle ESCAPE as canceling popups by default,
  //---*** for example in combo boxes.
  #f
end method cancel-frame;

//---*** DO WE NEED THIS?
define sealed method cancel-gadget 
    (gadget :: <gadget>) => (handled? :: <boolean>)
  #f
end method cancel-gadget;

//---*** What do we do about setting the color and font of a gadget?


/// Labels

define sealed class <motif-label> 
    (<motif-gadget-mixin>,
     <label>,
     <leaf-pane>)
end class <motif-label>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-label>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-label>));
define sealed domain initialize (<motif-label>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-label>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let resources
    = vector(mapped-when-managed:, #f,
	     label-string:, defaulted-gadget-label(gadget));
  let widget
    = xt/XtCreateManagedWidget("DUIMLabel", xm/<XmLabelGadget>, parent-widget,
			       resources:
				 concatenate(resources, foreground, background, font));
  values(widget, #f)
end method make-motif-widget;


/// Separators

define sealed class <motif-separator>
    (<motif-gadget-mixin>
     <separator>,
     <leaf-pane>)
end class <motif-separator>;

define sealed method class-for-make-pane
    (framem :: <motif-frame-manager>, class == <separator>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-separator>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-separator>));
define sealed domain initialize (<motif-separator>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-separator>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  ignore(font);
  let resources
    = vector(mapped-when-managed:, #f);
  let widget
    = xt/XtCreateManagedWidget("DUIMSeparator", xm/<XmSeparatorGadget>, parent-widget,
			       resources:
				 concatenate(resources, foreground, background));
  values(widget, #f)
end method make-motif-widget;

define sealed method do-compose-space
    (pane :: <motif-separator>, #key width, height)
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


/// Text gadgets

// Mixin class for text fields, password fields, and text editors
define open abstract class <motif-text-gadget-mixin>
    (<suppress-value-changed-callback-mixin>,
     <motif-scrolled-gadget-mixin>,
     <text-field>)
  sealed slot %changed? :: <boolean> = #f;
  sealed constant slot %current-selection :: <simple-text-range>
    = make(<text-range>, start: -1, end: -1);
end class <motif-text-gadget-mixin>;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-text-gadget-mixin>, mirror :: <window-mirror>) => ()
  next-method();
  // Set the initial text selection
  text-selection(gadget) := gadget.%current-selection
end method initialize-gadget-mirror;

define method gadget-convert-to-native-newlines
    (gadget :: <text-gadget>, string :: <byte-string>)
 => (string :: <byte-string>)
  string
end method gadget-convert-to-native-newlines;

define method gadget-convert-from-native-newlines
    (gadget :: <text-gadget>, string :: <byte-string>)
 => (string :: <byte-string>)
  string
end method gadget-convert-from-native-newlines;

define sealed method note-gadget-text-changed 
    (gadget :: <motif-text-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

// Updates the Motif text field from the DUIM gadget
define sealed method update-gadget-text
    (gadget :: <text-gadget>, mirror :: <window-mirror>) => ()
  let (old-text, widget) = get-widget-text(gadget);
  let new-text = gadget-text-buffer(gadget);
  when (old-text ~= new-text)
    // Preserve cursor position as much as possible
    let cursorpos = xm/XmTextFieldGetInsertionPosition(widget);
    xm/XmTextFieldSetString(widget, new-text);
    xm/XmTextFieldSetInsertionPosition(widget, min(cursorpos, size(new-text)))
  end
end method update-gadget-text;

define sealed method handle-text-gadget-changing
    (gadget :: <text-gadget>) => ()
  let (text, widget) = get-widget-text(gadget);
  when (text ~= gadget-text-buffer(gadget))
    gadget.%changed? := #t;
    distribute-text-changing-callback(gadget, text)
  end
end method handle-text-gadget-changing;

define sealed method handle-text-gadget-changed
    (gadget :: <text-gadget>) => ()
  when (gadget.%changed?)
    let (text, widget) = get-widget-text(gadget);
    distribute-text-changed-callback(gadget, text);
    gadget.%changed? := #f
  end
end method handle-text-gadget-changed;


/// Text and password fields

define open abstract class <motif-text-field-mixin>
    (<motif-text-gadget-mixin>)
end class <motif-text-field-mixin>;

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-text-field-mixin>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let text = gadget-convert-to-windows-newlines(gadget, gadget-text-buffer(gadget));
  gadget-text-buffer(gadget) := text;
  let resources
    = vector(mapped-when-managed:, #f,
	     value:, text,
	     cursor-position-visible:, #t,
	     sensitive:, gadget-enabled?(gadget));
  let widget
    = xm/XmCreateTextField("DUIMTextField", parent-widget,
			   resources:
			     concatenate(resources, foreground, background, font));
  values(widget, #f)
end method make-motif-widget;

define sealed method install-gadget-callbacks
    (gadget :: <motif-text-field-mixin>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "valueChangedCallback", text-field-value-changed-callback, mirror);
  xt/XtAddCallback(widget, "activateCallback",     text-field-activate-callback,      mirror);
  xt/XtAddCallback(widget, "focusCallback",        text-field-focus-in-callback,      mirror);
  xt/XtAddCallback(widget, "losingFocuscallback",  text-field-focus-out-callback,     mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function text-field-value-changed-callback
    (widget, client-data, call-data :: xm/<XmTextFieldCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  suppressing-value-changed-callback (gadget)
    handle-text-gadget-changing(gadget)
  end
end xm/xm-callback-function text-field-value-changed-callback;

define xm/xm-callback-function text-field-activate-callback
    (widget, client-data, call-data :: xm/<XmTextFieldCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  suppressing-value-changed-callback (gadget)
    handle-text-gadget-changed(gadget);
    distribute-activate-callback(gadget)
  end
end xm/xm-callback-function text-field-activate-callback;

define xm/xm-callback-function text-field-focus-in-callback
    (widget, client-data, call-data :: xm/<XmTextFieldCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  xt/XtSetValues(widget, cursor-position-visible: #t)
  distribute-focus-in-callback(gadget)
end xm/xm-callback-function text-field-focus-in-callback;

define xm/xm-callback-function text-field-focus-out-callback
    (widget, client-data, call-data :: xm/<XmTextFieldCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  xt/XtSetValues(widget, cursor-position-visible: #f)
  distribute-focus-out-callback(gadget)
end xm/xm-callback-function text-field-focus-out-callback;

define method get-widget-text
    (gadget :: <motif-text-field-mixin>)
 => (text :: <string>, widget)
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  let text   = xm/XmTextFieldGetString(widget);
  values(text, widget)
end method get-widget-text;

define sealed method do-compose-space
    (gadget :: <motif-text-field-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let initial-width  = gadget.%initial-width;
  let initial-height = gadget.%initial-height;
  make(<space-requirement>,
       width:      if (width)  max(width,  initial-width)  else initial-width  end, 
       min-width:  initial-width,
       max-width:  $fill,
       height:     if (height) max(height, initial-height) else initial-height end, 
       min-height: initial-height,
       max-height: $fill)
end method do-compose-space;

define sealed method text-caret-position
    (gadget :: <motif-text-field-mixin>)
 => (position :: <integer>)
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  xm/XmTextFieldGetInsertionPosition(widget)
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <motif-text-field-mixin>)
 => (position :: false-or(<integer>))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  let last   = xm/XmTextFieldGetLastPosition(widget);
  xm/XmTextFieldSetInsertionPosition(widget, max(0, min(position, last)));
  position
end method text-caret-position-setter;

//---*** What about 'selected-text-setter'?
define sealed method selected-text
    (gadget :: <motif-text-field-mixin>) => (string :: false-or(<string>))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  //---*** DO THIS
end method selected-text;

define sealed method text-selection
    (gadget :: <motif-text-field-mixin>)
 => (range :: type-union(<text-range>, one-of(#f)))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  //---*** DO THIS
end method text-selection;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)), gadget :: <motif-text-field-mixin>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  //---*** DO THIS
end method text-selection-setter;


/// Text fields

define sealed class <motif-text-field>
    (<motif-text-field-mixin>,
     <text-field>,
     <leaf-pane>)
end class <motif-text-field>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <text-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-text-field>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-text-field>));
define sealed domain initialize (<motif-text-field>);


/// Password fields

define sealed class <motif-password-field>
    (<motif-text-field-mixin>,
     <password-field>,
     <leaf-pane>)
end class <motif-password-field>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <password-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-password-field>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-password-field>));
define sealed domain initialize (<motif-password-field>);


/// Text editors

define sealed class <motif-text-editor>
    (<motif-text-gadget-mixin>,
     <text-editor>,
     <leaf-pane>)
  slot %text :: false-or(<byte-string>) = #f;
end class <motif-text-editor>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <text-editor>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-text-editor>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-text-editor>));
define sealed domain initialize (<motif-text-editor>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-text-editor>)
 => (widget :: xt/<Widget>, scroll-widget :: xt/<Widget>)
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let text = gadget-convert-to-windows-newlines(gadget, gadget-text-buffer(gadget));
  gadget-text-buffer(gadget) := text;
  let resources
    = vector(mapped-when-managed:, #f,
	     value:, text,
	     cursor-position-visible:, #t,
	     editable:, ~gadget-read-only?(gadget),
	     edit-mode:, xm/$XmMULTI-LINE-EDIT,
	     columns:, gadget-columns(gadget),
	     rows:, gadget-lines(gadget),
	     sensitive:, gadget-enabled?(gadget));
  let text-widget
    = if (scroll-bars)
	xm/XmCreateScrolledText("DUIMTextEditor", parent-widget,
				resources:
				  concatenate(resources,
					      //---*** Not quite...
					      vector(scroll-horizontal:, #t,
						     :scroll-vertical:, #t),
					      foreground, background, font))
      else
	xm/XmCreateText("DUIMTextEditor", parent-widget,
			resources:
			  concatenate(resources, foreground, background, font))
      end;
  let scroll-widget
    = if (scroll-bars) xt/XtParent(text-widget) else text-widget end;
  values(text-widget, scroll-widget)
end method make-motif-widget;

define sealed method install-gadget-callbacks
    (gadget :: <motif-text-editor>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "valueChangedCallback", text-editor-value-changed-callback, mirror);
  xt/XtAddCallback(widget, "focusCallback",        text-editor-focus-in-callback,      mirror);
  xt/XtAddCallback(widget, "losingFocuscallback",  text-editor-focus-out-callback,     mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function text-editor-value-changed-callback
    (widget, client-data, call-data :: xm/<XmTextCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  suppressing-value-changed-callback (gadget)
    handle-text-gadget-changing(gadget)
  end
end xm/xm-callback-function text-editor-value-changed-callback;

define xm/xm-callback-function text-editor-focus-in-callback
    (widget, client-data, call-data :: xm/<XmTextCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  xt/XtSetValues(widget, cursor-position-visible: #t)
  distribute-focus-in-callback(gadget)	//---*** DO NOTHING?
end xm/xm-callback-function text-editor-focus-in-callback;

define xm/xm-callback-function text-editor-focus-out-callback
    (widget, client-data, call-data :: xm/<XmTextCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  xt/XtSetValues(widget, cursor-position-visible: #f)
  distribute-focus-in-callback(gadget)	//---*** DO THE VALUE-CHANGED CALLBACK?
end xm/xm-callback-function text-editor-focus-out-callback;

//---*** DO THIS
define sealed method do-compose-space
    (gadget :: <motif-text-editor>, #key width, height)
 => (space-req :: <space-requirement>)
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let nlines = gadget-lines(gadget);
  let ncols  = gadget-columns(gadget);
  let line-height = font-height(text-style, _port);
  let char-width  = font-width(text-style, _port);
  let vsp         = $default-vertical-spacing;
  when (nlines & ~height)
    height := nlines * line-height + (nlines - 1) * vsp
  end;
  when (ncols  & ~width)
    width  := ncols  * char-width
  end;
  //---*** How should we really calculate the min width/height?
  let hscroll-height
    = if (gadget-scrolling-horizontally?(gadget)) GetSystemMetrics($SM-CYHSCROLL)
      else 0 end;
  let vscroll-width
    = if (gadget-scrolling-vertically?(gadget))   GetSystemMetrics($SM-CXVSCROLL)
      else 0 end;
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

define method get-widget-text
    (gadget :: <motif-text-editor>)
 => (text :: <string>, widget)
  let mirror = sheet-mirror(gadget);
  let widget = mirror-work-widget(mirror);
  let text   = xm/XmTextGetString(widget);
  values(text, widget)
end method get-widget-text;

// Updates the Motif text field from the DUIM gadget
define sealed method update-gadget-text
    (gadget :: <motif-text-editor>, mirror :: <window-mirror>) => ()
  let (old-text, widget) = get-widget-text(gadget);
  let new-text = gadget-text-buffer(gadget);
  when (old-text ~= new-text)
    // Preserve cursor position as much as possible
    let cursorpos = xm/XmTextGetCursorPosition(widget);
    xm/XmTextSetString(widget, new-text);
    xm/XmTextSetCursorPosition(widget, min(cursorpos, size(new-text)));
  end
end method update-gadget-text;

define sealed method text-caret-position
    (gadget :: <motif-text-editor>)
 => (position :: <integer>)
  let mirror = sheet-mirror(gadget);
  let widget = mirror-work-widget(mirror);
  xm/XmTextGetCursorPosition(widget)
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <motif-text-editor>)
 => (position :: false-or(<integer>))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-work-widget(mirror);
  let last   = xm/XmTextGetLastPosition(widget);
  xm/XmTextSetCursorPosition(widget, max(0, min(position, last)));
  position
end method text-caret-position-setter;

//---*** What about 'selected-text-setter'?
define sealed method selected-text
    (gadget :: <motif-text-editor>) => (string :: false-or(<string>))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-work-widget(mirror);
  //---*** DO THIS
end method selected-text;

define sealed method text-selection
    (gadget :: <motif-text-editor>)
 => (range :: type-union(<text-range>, one-of(#f)))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-work-widget(mirror);
  //---*** DO THIS
end method text-selection;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)), gadget :: <motif-text-editor>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  let mirror = sheet-mirror(gadget);
  let widget = mirror-work-widget(mirror);
  //---*** DO THIS
end method text-selection-setter;

define method gadget-text
    (gadget :: <motif-text-editor>) => (text :: <string>)
  gadget.%text
  | begin
      let text = gadget-convert-from-native-newlines(gadget, gadget-text-buffer(gadget));
      gadget.%text := text;
      text
    end
end method gadget-text;

define method gadget-text-setter
    (text :: <string>, gadget :: <motif-text-editor>, #key do-callback? = #f)
 => (text :: <string>)
  gadget-text-buffer(gadget) := gadget-convert-to-native-newlines(gadget, text);
  when (do-callback?)
    execute-value-changed-callback(gadget, gadget-client(gadget), gadget-id(gadget))
  end;
  note-gadget-text-changed(gadget);
  note-gadget-value-changed(gadget);
  text
end method gadget-text-setter;

define method gadget-text-buffer-setter
    (text :: <string>, gadget :: <motif-text-editor>)
 => (text :: <string>)
  gadget.%text := #f;
  next-method()
end method gadget-text-buffer-setter;

define method gadget-convert-to-native-newlines
    (gadget :: <motif-text-editor>, string :: <byte-string>)
 => (string :: <byte-string>)
  convert-to-native-newlines(string)
end method gadget-convert-to-native-newlines;

define method gadget-convert-from-native-newlines
    (gadget :: <motif-text-editor>, string :: <byte-string>)
 => (string :: <byte-string>)
  convert-from-native-newlines(string)
end method gadget-convert-from-native-newlines;


/// Buttons

// See WIG pg.388 for the description of this rule
define sealed method button-box-spacing
    (framem :: <motif-frame-manager>, box :: <button-box>)
 => (spacing :: <integer>)
  select (gadget-orientation(box))
    #"horizontal" => 3;
    #"vertical"   => 3;
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
      values(add-gadget-label-postfix(gadget, label),
	     #f, mnemonic, index);
    <motif-bitmap>, <motif-icon> =>
      values(if (mnemonic) as(<string>, vector(mnemonic)) else "" end,
	     label, mnemonic, index);
    <image> =>
      //---*** Decode the image and return a pixmap or something
      values("<image>",
	     #f, mnemonic, index);
  end
end method text-or-image-from-gadget-label;

//---*** WHAT DO WE DO ABOUT THIS?
// Create a new label given a string label and a mnemonic
// Clients are responsible for destroying the C string when done
//--- This only works on <byte-string> since <C-string> is effectively a byte string...
define sealed method make-motif-mnemonic-label 
    (label :: <byte-string>, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
 => (new-label :: <C-string>)
  if (~mnemonic | index)
    // If there's no mnemonic, or 'text-or-image-from-gadget-label' or
    // 'compute-mnemonic-from-label' already found a mnemonic, just
    // leave the label alone
    as(<C-string>, label)
  else
    let mnemonic  = gesture-character(mnemonic);
    let new-label = make(<C-string>, size: 1 + size(label));
    let added-marker? = #f;
    without-bounds-checks
      let j :: <integer> = 0;
      for (i :: <integer> from 0 below size(label))
	let char = label[i];
	if (~added-marker? & char-equal?(char, mnemonic))
	  added-marker? := #t;
	  new-label[j] := '&';
	  new-label[j + 1] := char;
	  j := j + 2
	else
	  new-label[j] := char;
	  j := j + 1
	end
      end
    end;
    new-label
  end
end method make-motif-mnemonic-label;


/// Push buttons

define sealed class <motif-push-button>
    (<motif-gadget-mixin>,
     <push-button>,
     <leaf-pane>)
end class <motif-push-button>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-push-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-push-button>));
define sealed domain initialize (<motif-push-button>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-push-button>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let resources
    = vector(mapped-when-managed:, #f,
	     label-string:, defaulted-gadget-label(gadget),
	     sensitive:, gadget-enabled?(gadget));
  let widget
    = xt/XtCreateManagedWidget("DUIMPushButton", xm/<XmPushButton>, parent-widget,
			       resources:
				 concatenate(resources, foreground, background, font));
  values(widget, #f)
end method make-motif-widget;

define sealed method install-gadget-callbacks
    (gadget :: <motif-push-button>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "activateCallback", push-button-callback, mirror);
  xt/XtAddCallback(widget, "armCallback",      push-button-callback, mirror);
  xt/XtAddCallback(widget, "disarmCallback",   push-button-callback, mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function push-button-callback
    (widget, client-data, call-data :: xm/<XmPushButtonCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  select (call-data.xm/reason-value)
    xm/$XmCR-ACTIVATE =>
      when (gadget-activate-callback(gadget))
	distribute-activate-callback(gadget);
      end;
    xm/$XmCR-ARM    => #f;
    xm/$XmCR-DISARM => #f;
    otherwise       => #f;
  end
end xm/xm-callback-function push-button-callback;

//---*** WHAT DO WE DO ABOUT THIS?
define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <motif-push-button>)
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

define sealed class <motif-radio-button>
    (<motif-gadget-mixin>,
     <radio-button>,
     <leaf-pane>)
end class <motif-radio-button>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-radio-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-radio-button>));
define sealed domain initialize (<motif-radio-button>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-radio-button>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let indicator     = xm/$XmONE-OF-MANY;
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let resources
    = vector(mapped-when-managed:, #f,
	     label-string:, defaulted-gadget-label(gadget),
	     sensitive:, gadget-enabled?(gadget),
	     indicator-type:, indicator,
	     set:, gadget-value(gadget));
  let widget
    = xt/XtCreateManagedWidget("DUIMToggleButton", xm/<XmToggleButton>, parent-widget,
			       resources:
				 concatenate(resources, foreground, background, font));
  values(widget, #f)
end method make-motif-widget;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-radio-button>, mirror :: <window-mirror>) => ()
  next-method();
  let widget  = mirror-widget(mirror);
  xm/XmToggleButtonSetState(widget, gadget-value(gadget), #f)
end method initialize-gadget-mirror;

define sealed method install-gadget-callbacks
    (gadget :: <motif-radio-button>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "activateCallback",     radio-button-callback, mirror);
  xt/XtAddCallback(widget, "valueChangedCallback", radio-button-callback, mirror);
  xt/XtAddCallback(widget, "armCallback",          radio-button-callback, mirror);
  xt/XtAddCallback(widget, "disarmCallback",       radio-button-callback, mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function radio-button-callback
    (widget, client-data, call-data :: xm/<XmToggleButtonCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  select (call-data.xm/reason-value)
    xm/$XmCR-ACTIVATE     =>
      when (gadget-activate-callback(gadget))
	distribute-activate-callback(gadget);
      end;
    xm/$XmCR-VALUE-CHANGED =>
      when (gadget-value-changed-callback(gadget))
	let value = (call-data.xm/set-value > 0);
	distribute-value-changed-callback(gadget, value);
      end;
    xm/$XmCR-ARM    => #f;
    xm/$XmCR-DISARM => #f;
    otherwise       => #f;
  end
end xm/xm-callback-function radio-button-callback;

define sealed method note-gadget-value-changed
    (gadget :: <motif-radio-button>) => ()
  next-method();
  let mirror  = sheet-mirror(gadget);
  let widget  = mirror-widget(mirror);
  xm/XmToggleButtonSetState(widget, gadget-value(gadget), #f)
end method note-gadget-value-changed;


/// Check buttons

define sealed class <motif-check-button>
    (<motif-gadget-mixin>,
     <check-button>,
     <leaf-pane>)
end class <motif-check-button>;

define sealed method class-for-make-pane
    (framem :: <motif-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-check-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-check-button>));
define sealed domain initialize (<motif-check-button>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-check-button>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let indicator     = xm/$XmN-OF-MANY;
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let resources
    = vector(mapped-when-managed:, #f,
	     label-string:, defaulted-gadget-label(gadget),
	     sensitive:, gadget-enabled?(gadget),
	     indicator-type:, indicator,
	     set:, gadget-value(gadget));
  let widget
    = xt/XtCreateManagedWidget("DUIMCheckButton", xm/<XmToggleButton>, parent-widget,
			       resources:
				 concatenate(resources, foreground, background, font));
  values(widget, #f)  
end method make-motif-widget;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-check-button>, mirror :: <window-mirror>) => ()
  next-method();
  let widget  = mirror-widget(mirror);
  xm/XmToggleButtonSetState(widget, gadget-value(gadget), #f)
end method initialize-gadget-mirror;

define sealed method install-gadget-callbacks
    (gadget :: <motif-check-button>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "activateCallback",     check-button-callback, mirror);
  xt/XtAddCallback(widget, "valueChangedCallback", check-button-callback, mirror);
  xt/XtAddCallback(widget, "armCallback",          check-button-callback, mirror);
  xt/XtAddCallback(widget, "disarmCallback",       check-button-callback, mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function check-button-callback
    (widget, client-data, call-data :: xm/<XmToggleButtonCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  select (call-data.xm/reason-value)
    xm/$XmCR-ACTIVATE      =>
      when (gadget-activate-callback(gadget))
	distribute-activate-callback(gadget);
      end;
    xm/$XmCR-VALUE-CHANGED =>
      when (gadget-value-changed-callback(gadget))
	let value = (call-data.xm/set-value > 0);
	distribute-value-changed-callback(gadget, value);
      end;
    xm/$XmCR-ARM    => #f;
    xm/$XmCR-DISARM => #f;
    otherwise       => #f;
  end
end xm/xm-callback-function check-button-callback;

define sealed method note-gadget-value-changed
    (gadget :: <motif-check-button>) => ()
  next-method();
  let mirror  = sheet-mirror(gadget);
  let widget  = mirror-widget(mirror);
  xm/XmToggleButtonSetState(widget, gadget-value(gadget), #f)
end method note-gadget-value-changed;


/// Scroll bars

define sealed class <motif-scroll-bar>
    (<motif-gadget-mixin>,
     <scroll-bar>,
     <leaf-pane>)
end class <motif-scroll-bar>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <scroll-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-scroll-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-scroll-bar>));
define sealed domain initialize (<motif-scroll-bar>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-scroll-bar>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  ignore(font);
  let orientation = select (gadget-orientation(gadget))
		      #"horizontal" => xm/$XmHORIZONTAL;
		      #"vertical"   => xm/$XmVERTICAL;
		    end;
  let resources
    = vector(mapped-when-managed:, #f,
	     orientation:, orientation,
	     sensitive:, gadget-enabled?(gadget));
  let widget
    = xm/XmCreateScrollBar("DUIMScrollBar", parent-widget,
			   resources:
			     concatenate(resources, foreground, background));
  values(widget, #f)
end method make-motif-widget;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-scroll-bar>, mirror :: <window-mirror>) => ()
  next-method();
  update-scroll-bar-mirror(gadget)
end method initialize-gadget-mirror;

define sealed method install-gadget-callbacks
    (gadget :: <motif-scroll-bar>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "dragCallback",          scroll-bar-callback, mirror);
  xt/XtAddCallback(widget, "valueChangedCallback",  scroll-bar-callback, mirror);
  xt/XtAddCallback(widget, "incrementCallback",     scroll-bar-callback, mirror);
  xt/XtAddCallback(widget, "decrementCallback",     scroll-bar-callback, mirror);
  xt/XtAddCallback(widget, "pageIncrementCallback", scroll-bar-callback, mirror);
  xt/XtAddCallback(widget, "pageDecrementCallback", scroll-bar-callback, mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function scroll-bar-callback
    (widget, client-data, call-data :: xm/<XmScrollBarCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  //---*** DO THIS
end xm/xm-callback-function scroll-bar-callback;

define sealed method do-compose-space
    (gadget :: <motif-scroll-bar>, #key width, height)
 => (space-requirement :: <space-requirement>)
  let initial-width  = gadget.%initial-width;
  let initial-height = gadget.%initial-height;
  select (gadget-orientation(gadget))
    #"horizontal" =>
    make(<space-requirement>,
	 width:      if (width)  max(width,  initial-width)  else initial-width  end, 
	 min-width:  initial-width,
	 max-width:  $fill,
	 height:     if (height) max(height, initial-height) else initial-height end, 
	 min-height: initial-height,
	 max-height: initial-height);
    #"vertical" =>
    make(<space-requirement>,
	 width:      if (width)  max(width,  initial-width)  else initial-width  end, 
	 min-width:  initial-width,
	 max-width:  initial-width,
	 height:     if (height) max(height, initial-height) else initial-height end, 
	 min-height: initial-height,
	 max-height: $fill);
  end
end method do-compose-space;

// This hack takes advantage of the fact that 'do-compose-space' for these
// gadgets uses initial-width and initial-height as minimums.
//--- In the future maybe we could calculate based on some property of
//--- sheet being scrolled.  For example, for a vertical scroll bar, at
//--- least as high as text style height plus the spacing.
define method record-initial-widget-sizes
    (gadget :: <motif-scroll-bar>, widget :: xt/<Widget>)
  next-method();
  select (gadget-orientation(gadget))
    #"horizontal" =>
      gadget.%initial-width  = gadget.%initial-height * 2 + 10;
    #"vertical" =>
      gadget.%initial-height = gadget.%initial-width  * 2 + 10;
  end
end method record-initial-widget-sizes;

define sealed method note-gadget-slug-size-changed
    (gadget :: <motif-scroll-bar>) => ()
  update-scroll-bar-mirror(gadget)
end method note-gadget-slug-size-changed;

define sealed method note-gadget-value-changed
    (gadget :: <motif-scroll-bar>) => ()
  next-method();
  update-scroll-bar-mirror(gadget)
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <motif-scroll-bar>) => ()
  next-method();
  update-scroll-bar-mirror(gadget)
end method note-gadget-value-range-changed;

define sealed method note-scroll-bar-changed
    (gadget :: <motif-scroll-bar>) => ()
  update-scroll-bar-mirror(gadget)
end method note-scroll-bar-changed;

//---*** WHAT DO WE DO ABOUT THIS?
define sealed method update-scroll-bar-mirror
    (gadget :: <motif-scroll-bar>) => ()
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
    (gadget :: <motif-scroll-bar>)
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

//---*** The MOTIF CALLBACK CODE NEEDS TO DO THIS
define sealed method handle-scrolling
    (gadget :: <motif-scroll-bar>,
     scroll-code :: <integer>, position :: <integer>)
 => (handled? :: <boolean>)
  block (return)
    // Motif uses different name for horizontal and vertical scrolling,
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


/// List boxes

define sealed class <motif-list-box> 
    (<motif-scrolled-gadget-mixin>,
     <list-box>,
     <leaf-pane>)
end class <motif-list-box>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-list-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-list-box>));
define sealed domain initialize (<motif-list-box>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-list-box>)
 => (widget :: xt/<Widget>, scroll-widget :: xt/<Widget>)
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let selection-mode = gadget-selection-mode(gadget);
  let items     = gadget-items(gadget);
  let selection = gadget-selection(gadget);
  let label-key = gadget-label-key(gadget);
  let value-key = gadget-label-key(gadget);
  let resources
    = vector(mapped-when-managed:, #f,
	     scroll-bar-display-policy:, xm/$XmAS-NEEDED,
	     list-size-policy:, #"resize-if-possible",
	     items:, map-as(<vector>, label-key, items),
	     item-count:, size(items),
	     visible-item-count:, size(selection),
	     selected-items:, map-as(<vector>, method (i) label-key(items[i]) end, items),
	     selected-item-count: size(selection),
	     selection-policy:, select (selection-mode)
				  #"single"   => xm/$XmCR-BROWSE-SELECT;
				  #"multiple" => xm/$XmCR-EXTENDED-SELECT;
				end);
  let list-widget
    = if (scroll-bars)
	xm/XmCreateScrolledList(parent-width, "DUIMListPane",
				resources:
				  concatenate(resources, foreground, background, font))
      else
	xm/XmCreateList(parent-width, "DUIMListPane",
			resources:
			  concatenate(resources, foreground, background, font))
      end;
  let scroll-widget
    = if (scroll-bars) xt/XtParent(list-widget) else list-widget end;
  values(list-widget, scroll-widget)
end method make-motif-widget;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-list-box>, mirror :: <window-mirror>) => ()
  next-method();
  note-gadget-items-changed(gadget)
end method initialize-gadget-mirror;

define sealed method install-gadget-callbacks
    (gadget :: <motif-list-box>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "browseSelectionCallback",   list-box-callback, mirror);
  xt/XtAddCallback(widget, "defaultActionCallback",     list-box-callback, mirror);
  xt/XtAddCallback(widget, "extendedSelectionCallback", list-box-callback, mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function list-box-callback
    (widget, client-data, call-data :: xm/<XmListCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  //---*** DO THIS
end xm/xm-callback-function list-box-callback;

define sealed method do-compose-space 
    (pane :: <motif-list-box>, #key width, height)
 => (space-req :: <space-requirement>)
  compose-space-for-list-box(pane,
			     width: width, height: height,
			     default-lines: $list-box-default-visible-lines,
			     minimum-lines: $list-box-minimum-visible-lines,
			     extra-height:  $list-box-extra-height)
end method do-compose-space;

//---*** DO THIS RIGHT
//--- Are the heuristics here reasonable?
define sealed method compose-space-for-list-box
    (pane :: <motif-gadget-mixin>,
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

//---*** DO THIS RIGHT
define sealed method note-gadget-items-changed
    (gadget :: <motif-list-box>) => ()
  next-method();
  update-gadget-items(gadget, $LB-RESETCONTENT, $LB-ADDSTRING);
  update-gadget-selection(gadget)
end method note-gadget-items-changed;

//---*** DO THIS RIGHT
define sealed method update-gadget-selection
    (gadget :: <motif-list-box>) => ()
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
	  // of the Motif gadget.  Now what we want to do is update the Motif
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
    (gadget :: <motif-list-box>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

//---*** DO THIS RIGHT
define sealed method handle-selection-changed
    (gadget :: <motif-list-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    #"single" =>
      let selection = SendMessage(handle, $LB-GETCURSEL, 0, 0);
      distribute-selection-changed-callback
	(gadget,
	 if (selection = $LB-ERR) #() else vector(selection) end);
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

//---*** WHAT ABOUT THIS?
define sealed method handle-command
    (gadget :: <motif-list-box>, mirror :: <window-mirror>,
     id :: <integer>, event :: <integer>)
 => (handled? :: <boolean>)
  ignore(mirror, id);
  select (event)
    $LBN-SELCHANGE, $LBN-SELCANCEL =>
      handle-selection-changed(gadget);
    $LBN-DBLCLK =>
      activate-motif-gadget(gadget);
      #t;
    otherwise => 
      next-method();
  end
end method handle-command;


/// Option boxes

define sealed class <motif-option-box> 
    (<motif-buttons-gadget-mixin>,
     <option-box>,
     <leaf-pane>)
end class <motif-option-box>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <option-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-option-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-option-box>));
define sealed domain initialize (<motif-option-box>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-option-box>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  let resources
    = vector(mapped-when-managed:, #f,
	     sensitive:, gadget-enabled?(gadget),
	     sub-menu-id:, menu-widget,
	     label-string:, defaulted-gadget-label(gadget));
  let menu-widget
    = xm/XmCreatePulldownMenu(parent-widget, "DUIMPulldownMenu",
			      resources:
				concatenate(foreground, background));
  let option-widget
    = xm/XmCreateOptionMenu(parent-widget "DUIMOptionPane",
			    resources:
			      concatenate(resources, foreground, background, font));
  when (~empty?(font))
    xt/XtSetValues(xm/XmOptionLabelGadget(option-widget),
		   font-list: font)
  end;
  //---*** SO WHO GLUES THE MENU AND OPTIONS WIDGETS TOGETHER?
  values(option-widget, menu-widget)
end method make-motif-widget;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-option-box>, mirror :: <window-mirror>) => ()
  next-method();
  let (foreground, background, font) = widget-attributes(port(gadget), gadget);
  let items     = gadget-items(gadget);
  let selection = gadget-selection(gadget);
  let label-key = gadget-label-key(gadget);
  let value-key = gadget-label-key(gadget);
  // Make the buttons for the option box
  //---*** This should be done by a 'note-gadget-items-changed' method
  for (item in items)
    let resources
      = vector(label-string:, label-key(item));
    let button
      = xt/XtCreateManagedWidget("DUIMPushButton", menu-widget,
				 resources:
				   concatenate(resources, foreground, background, font));
    let button-value = value-key(item);
    xt/XtAddCallback(button, "activateCallback", option-box-button-callback,
		     vector(mirror, button-value));
    when (button-value = gadget-value(gadget))
      xt/XtSetValues(mirror-widget(mirror), menu-history: button)
    end;
    value->button(mirror, button-value) := button
  end
end method initialize-gadget-mirror;

define sealed method install-gadget-callbacks
    (gadget :: <motif-option-box>, mirror :: <window-mirror>, widget) => ()
  // The callbacks are all on the buttons...
  #f
end method install-gadget-callbacks;

define xm/xm-callback-function option-box-button-callback
    (widget, client-data, call-data :: xm/<XmPushButtonCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data[0]);
  let value  = client-data[1];
  select (call-data.xm/reason-value)
    xm/$XmCR-ACTIVATE =>
      distribute-value-changed-callback(gadget, value);
    otherwise         => #f;
  end
end xm/xm-callback-function option-box-button-callback;

define sealed method do-compose-space 
    (gadget :: <motif-option-box>, #key width, height)
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

//---*** DO THIS RIGHT
define sealed method motif-option-box-height
    (gadget :: <motif-option-box>) => (height :: <integer>)
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
end method motif-option-box-height;

//---*** DO THIS RIGHT
define sealed method note-gadget-items-changed
    (gadget :: <motif-option-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-gadget-items(gadget, $CB-RESETCONTENT, $CB-ADDSTRING);
    update-gadget-selection(gadget);
    // Call 'set-mirror-edges' to make sure that the drop-down menu
    // is the correct size.
    let _port = port(gadget);
    let (left, top, right, bottom) = mirror-edges(_port, gadget, mirror);
    set-mirror-edges(_port, gadget, mirror, left, top, right, bottom)
  end
end method note-gadget-items-changed;

//---*** DO THIS RIGHT
define sealed method update-gadget-selection
    (gadget :: <motif-option-box>) => ()
  set-single-selection-gadget-selection(gadget, $CB-SETCURSEL)
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <motif-option-box>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

//---*** DO THIS RIGHT
define sealed method handle-selection-changed
    (gadget :: <motif-option-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  let selection = SendMessage(handle, $CB-GETCURSEL, 0, 0);
  unless (selection = $CB-ERR)
    distribute-selection-changed-callback(gadget, vector(selection));
    #t
  end
end method handle-selection-changed;

//---*** WHAT ABOUT THIS?
define sealed method handle-command
    (gadget :: <motif-option-box>, mirror :: <window-mirror>,
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

//---*** DO WE NEED THIS?
define sealed method cancel-gadget 
    (gadget :: <motif-option-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  when (handle & (SendMessage(handle, $CB-GETDROPPEDSTATE, 0, 0) = $true))
    SendMessage(handle, $CB-SHOWDROPDOWN, $false, 0);
    #t
  end
end method cancel-gadget;


/// Combo boxes

///---*** WHAT ABOUT THIS?  MOTIF DOESN'T SEEM TO HAVE COMBO BOXES

define sealed class <motif-combo-box> 
    (<motif-scrolled-gadget-mixin>,
     <combo-box>,
     <leaf-pane>)
  sealed slot %changed? :: <boolean> = #f;
end class <motif-combo-box>;

//--- If <motif-combo-box> was a <text-field>, we would not need this
define sealed method activate-motif-gadget
    (gadget :: <combo-box>) => (activated? :: <boolean>)
  handle-text-gadget-changed(gadget);
  next-method()
end method activate-motif-gadget;

define sealed class <motif-combo-box-text-field>
    (<motif-subgadget-mixin>,
     <motif-text-field>)
end class <motif-combo-box-text-field>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <combo-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-combo-box>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-combo-box>));
define sealed domain initialize (<motif-combo-box>);

define sealed method make-gadget-control
    (gadget :: <motif-combo-box>, parent :: <HWND>, options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let ext-style = if (border-type(gadget) == #"none") 0 else $WS-EX-CLIENTEDGE end;
  let handle :: <HWND>
    = CreateWindowEx(ext-style,
		     "COMBOBOX",
		     "",
		     %logior(options, 
			     $WS-GROUP, $WS-TABSTOP,
			     $CBS-AUTOHSCROLL, $CBS-HASSTRINGS,
			     $CBS-DROPDOWN),
		     x, y, width, height,
		     parent,
		     $null-hMenu,
		     application-instance-handle(),
		     $NULL-VOID);
  check-result("CreateWindowEx (COMBOBOX)", handle);
  subclass-combo-box-text-field(gadget, handle);
  handle
end method make-gadget-control;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-combo-box>, mirror :: <window-mirror>) => ()
  next-method();
  note-gadget-items-changed(gadget)
end method initialize-gadget-mirror;

// This is a bizarre hack to subclass the text field which is
// a child of the combo box.
define function subclass-combo-box-text-field
    (gadget :: <motif-combo-box>, handle :: <HWND>) => ()
  let edit-control = GetWindow(handle, $GW-CHILD);
  check-result("Finding the combo box's edit control", edit-control);
  // This is odd, but making this gadget actually does all the work
  // to mirror and attach everything correctly.
  make(<motif-combo-box-text-field>,
       owner: gadget, handle: edit-control);
end function subclass-combo-box-text-field;

define sealed method do-compose-space 
    (gadget :: <motif-combo-box>, #key width, height)
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

define sealed method motif-combo-box-height
    (gadget :: <motif-combo-box>) => (height :: <integer>)
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
end method motif-combo-box-height;

define sealed method note-gadget-items-changed
    (gadget :: <motif-combo-box>) => ()
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
    (gadget :: <motif-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

define sealed method note-gadget-value-changed
    (gadget :: <motif-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-value-changed;

define sealed method handle-selection-changed
    (gadget :: <motif-combo-box>) => (handled? :: <boolean>)
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
    (gadget :: <motif-combo-box>, mirror :: <window-mirror>,
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
    (text-field :: <motif-combo-box-text-field>, message :: <message-type>,
     wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let gadget = subgadget-owner(text-field);
  duim-debug-message("Handling message #x%x for subclassed %=",
		     message, gadget);
  when (message = $WM-KEYUP | message = $WM-CHAR | message = $WM-KEYDOWN)
    let key-name = virtual-key->keysym(wParam);
    duim-debug-message("Handling key-name %= for subclassed %=",
		       key-name, gadget);
    select (key-name)
      #"return", #"escape" =>
	message = $WM-KEYDOWN & handle-command-for-id(gadget, $IDOK);
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
    (gadget :: <motif-combo-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  when (handle & (SendMessage(handle, $CB-GETDROPPEDSTATE, 0, 0) = $true))
    SendMessage(handle, $CB-SHOWDROPDOWN, $false, 0);
    #t
  end
end method cancel-gadget;


/// Viewports

define sealed class <motif-viewport>
    (<viewport>,
     <motif-pane-mixin>,
     <permanent-medium-mixin>,
     <motif-mirrored-sheet-mixin>,
     <single-child-composite-pane>)
end class <motif-viewport>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-viewport>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-viewport>));
define sealed domain initialize (<motif-viewport>);

/*---*** Omit this method, since it causes very odd behavior
define sealed method repaint-mirror
    (viewport :: <motif-viewport>, mirror :: <window-mirror>) => ()
  let sheet = sheet-child(viewport);
  when (sheet & ~sheet-direct-mirror(sheet))
    next-method()
  end
end method repaint-mirror;
*/

// Things are wierd for our mirrored viewports!
// We only want to erase the background of a viewport when nothing
// else will do it for us, and we also want to erase in the background
// color of the child.  We erase a viewport when:
//  - the viewport doesn't have a child yet, or
//  - the child of the viewport is not mirrored (and won't get repaint events), or
//  - the viewport is not completely filled with the child
define method erase-background
    (viewport :: <motif-viewport>, mirror :: <window-mirror>, hDC :: <HDC>) => ()
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
      let (width, height) = sheet-size(viewport);
      //---*** Now clear the rectangle (0,0, width,height)
      //---*** presumably by drawing in the background color
      //---*** Use the background of 'sheet-mirror' (if not #f), else background of viewport
      //---*** Do we need to restore the DC afterwards?
    end
  end
end method erase-background;


/// Borders

define sealed class <motif-border>
    (<standard-repainting-mixin>,
     <border>,
     <basic-sheet>)
  sealed slot %pen   :: false-or(<standard-pen>) = #f;
  sealed slot %brush :: false-or(type-union(<standard-brush>, <ink>)) = #f;
end class <motif-border>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <border>, #key label)
 => (class :: <class>, options :: false-or(<sequence>))
  let border-class = if (label) <motif-group-box> else <motif-border> end;
  values(border-class, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-border>));
define sealed domain initialize (<motif-border>);

define constant $gadget-border-thickness :: <integer> = 2;

define sealed method do-compose-space
    (pane :: <motif-border>, #key width, height)
 => (space-req :: <space-requirement>)
  let thickness*2 = $gadget-border-thickness * 2;
  space-requirement+(pane,
		     next-method(pane,
				 width:  width  & width  - thickness*2,
				 height: height & height - thickness*2),
		     width: thickness*2, height: thickness*2)
end method do-compose-space;

define sealed method do-allocate-space
    (pane :: <motif-border>, width :: <integer>, height :: <integer>) => ()
  let child = sheet-child(pane);
  let thickness = $gadget-border-thickness;
  when (child)
    set-sheet-edges(child,
                    thickness, thickness,
                    width - thickness, height - thickness)
  end
end method do-allocate-space;

define sealed method handle-repaint
    (pane :: <motif-border>, medium :: <motif-medium>, region :: <region>) => ()
  ignore(region);	// not worth checking
  let (left, top, right, bottom) = box-edges(pane);
  draw-border(pane, medium, border-type(pane), left, top, right, bottom)
end method handle-repaint;

//---*** DO THE RIGHT THING
define sealed method draw-border
    (pane :: <sheet>, medium :: <motif-medium>, type :: <border-type>,
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


/// Sliders

define sealed class <motif-slider>
    (<suppress-value-changed-callback-mixin>,
     <motif-gadget-mixin>,
     <slider>,
     <leaf-pane>)
end class <motif-slider>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <slider>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-slider>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-slider>));
define sealed domain initialize (<motif-slider>);

define sealed method make-motif-widget
    (_port :: <win32-port>, gadget :: <motif-slider>)
 => (widget :: xt/<Widget>, scroll-widget :: singleton(#f))
  let parent-mirror = sheet-mirror(sheet-device-parent(gadget));
  let parent-widget = mirror-widget(parent-mirror);
  let (foreground, background, font) = widget-attributes(_port, gadget);
  ignore(font);
  let (min-value, max-value, shift) = motif-slider-range-values(gadget);
  let orientation = select (gadget-orientation(gadget))
		      #"horizontal" => xm/$XmHORIZONTAL;
		      #"vertical"   => xm/$XmVERTICAL;
		    end;
  let resources
    = vector(mapped-when-managed:, #f,
	     orientation:, orientation,
	     sensitive:, gadget-enabled?(gadget),
	     minimum:, min-value,
	     maximum:, max-value,
	     title-string:, defaulted-gadget-label(gadget));
  let show-value
    = if (slider-show-value?(gadget))
	vector(show-value:, #t,
	       decimal-points:, shift)
      else
	#[]
      end;
  let widget
    = xm/XmCreateScale("DUIMSlider", parent-widget,
		       resources:
			 concatenate(resources, show-value, foreground, background, font));
  values(widget, #f)
end method make-motif-widget;

define sealed method initialize-gadget-mirror
    (gadget :: <motif-slider>, mirror :: <window-mirror>) => ()
  next-method();
  let widget = mirror-widget(mirror);
  xm/XmScaleSetValue(widget, value->motif-slider-value(gadget, gadget-value(gadget)))
end method initialize-gadget-mirror;

// Motif sliders use integer values.  So given a float range by DUIM, we
// have to convert that to an integer range by multiplying by some number of
// powers of ten, then telling Motif to shift the decimal point back by the 
// same number of places
define method motif-slider-range-values
    (gadget :: <range-gadget-mixin>)
 => (min :: <integer>, max :: <integer>, shift :: <integer>)
  let _min  = gadget-start-value(gadget);
  let _max  = gadget-end-value(gadget);
  let range = max - min;
  let tens  = round(log(range, 10));
  let shift = max(3 - decades, 0);
  let scale = expt(10, shift);
  values(floor(_min * scale), ceiling(_max * scale), shift)
end method motif-slider-values;

define method value->motif-slider-value
    (gadget :: <range-gadget-mixin>, value) => (x-value :: <integer>)
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  let dmin   = gadget-start-value(gadget);
  let dmax   = gadget-end-value(gadget);
  let (xmin, xmax) = xt/XtGetValues(widget, #"minimum", #"maximum");
  round/((value - dmin) * xmax + (dmax - value) * xmin,
	 dmax - dmin)
end method value->motif-slider-value;

define method motif-slider-value->value
    (gadget :: <range-gadget-mixin>, x-value :: <integer>) => (value)
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  let dmin   = gadget-start-value(gadget);
  let dmax   = gadget-end-value(gadget);
  let (xmin, xmax) = xt/XtGetValues(widget, #"minimum", #"maximum");
  as(<single-float>, x-value - xmin) * dmax + as(<single-float>, xmax - x-value) * dmin
    / as(<single-float>, xmax - xmin)
end method motif-slider-value->value;

define sealed method install-gadget-callbacks
    (gadget :: <motif-slider>, mirror :: <window-mirror>, widget) => ()
  xt/XtAddCallback(widget, "dragCallback",          slider-callback, mirror);
  xt/XtAddCallback(widget, "valueChangedCallback",  slider-callback, mirror);
end method install-gadget-callbacks;

define xm/xm-callback-function slider-callback
    (widget, client-data, call-data :: xm/<XmSliderCallbackStruct>)
  ignore(widget);
  let gadget = mirror-sheet(client-data);
  let xval   = call-data.xm/value-value;
  let value  = motif-slider-value->value(gadget, x-val);
  select (call-data.xm/reason-value)
    xm/$XmCR-DRAG          =>
      distribute-value-changing-callback(gadget, value);
    xm/$XmCR-VALUE-CHANGED =>
      distribute-value-changed-callback(gadget, value);
    otherwise              => #f;
  end
end xm/xm-callback-function slider-callback;

define sealed method do-compose-space
    (gadget :: <motif-slider>, #key width, height)
 => (space-requirement :: <space-requirement>)
  let initial-width  = gadget.%initial-width;
  let initial-height = gadget.%initial-height;
  select (gadget-orientation(gadget))
    #"horizontal" =>
    make(<space-requirement>,
	 width:      if (width)  max(width,  initial-width)  else initial-width  end, 
	 min-width:  initial-width,
	 max-width:  $fill,
	 height:     if (height) max(height, initial-height) else initial-height end, 
	 min-height: initial-height,
	 max-height: initial-height);
    #"vertical" =>
    make(<space-requirement>,
	 width:      if (width)  max(width,  initial-width)  else initial-width  end, 
	 min-width:  initial-width,
	 max-width:  initial-width,
	 height:     if (height) max(height, initial-height) else initial-height end, 
	 min-height: initial-height,
	 max-height: $fill);
  end
end method do-compose-space;

define sealed method note-gadget-value-changed
    (gadget :: <motif-slider>) => ()
  next-method();
  let mirror = sheet-mirror(gadget);
  let widget = mirror-widget(mirror);
  xm/XmScaleSetValue(widget, value->motif-slider-value(gadget, gadget-value(gadget)))
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <motif-slider>) => ()
  next-method();
  //---*** WHAT DO WE DO ABOUT THIS?
end method note-gadget-value-range-changed;


/// Tool bars

//---*** Someday we should do these for real!

define sealed class <motif-tool-bar>
    (<tool-bar>, <single-child-wrapping-pane>)
  //--- The way we do this separator stuff is just loathsome...
  slot tool-bar-decoration :: <sheet>;
  slot %separator :: false-or(<separator>) = #f;
end class <motif-tool-bar>;

define method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <tool-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-tool-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-tool-bar>));
define sealed domain initialize (<motif-tool-bar>);

define method initialize
    (gadget :: <motif-tool-bar>, #key frame-manager: framem) => ()
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
define method note-sheet-mapped (gadget :: <motif-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #t
  end
end method note-sheet-mapped;

// Ditto, for unmapping
define method note-sheet-unmapped (gadget :: <motif-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #f
  end
end method note-sheet-unmapped;


/// Status bars

define sealed class <motif-status-bar>
    (<motif-gadget-mixin>,
     <status-bar>,
     <row-layout>)
  slot status-bar-simple? :: <boolean> = #f,
    setter: %simple?-setter;
  slot status-bar-simple-text :: <string> = "";
  keyword border:      = $status-bar-border;
  keyword spacing:     = $status-bar-spacing;
  keyword y-alignment: = #"center";
end class <motif-status-bar>;

define sealed method class-for-make-pane
    (framem :: <motif-frame-manager>, class == <status-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-status-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<motif-status-bar>));
define sealed domain initialize (<motif-status-bar>);

define sealed method make-gadget-control
    (gadget :: <motif-status-bar>, 
     parent :: <HWND>, 
     options :: <options-type>,
     #key x, y, width, height)
 => (handle :: <HWND>)
  let handle :: <HWND>
    = CreateWindowEx(0,
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
    (gadget :: <motif-status-bar>, #key width, height)
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
    (gadget :: <motif-status-bar>, width :: <integer>, height :: <integer>) => ()
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

define class <status-label-mirror> (<motif-mirror>)
  sealed slot status-label-status-bar :: <status-bar>,
    required-init-keyword: status-bar:;
  sealed slot status-label-part-number :: <integer>,
    required-init-keyword: part-number:;
end class <status-label-mirror>;

define sealed method make-gadget-mirror
    (status-bar :: <motif-status-bar>, gadget :: <motif-label>)
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
end method make-gadget-mirror;

define sealed method update-mirror-label
    (gadget :: <motif-label>, mirror :: <status-label-mirror>) => ()
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
    (simple? :: <boolean>, gadget :: <motif-status-bar>)
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
    (label :: <string>, gadget :: <motif-status-bar>)
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
    (gadget :: <motif-status-bar>) => ()
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
