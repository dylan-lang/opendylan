Module:       gtk-duim
Synopsis:     GTK gadget implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $default-label = "";
define constant $button-box-x-spacing = 3;
define constant $button-box-y-spacing = 3;


/// GTK gadgets

define class <gadget-mirror> (<widget-mirror>)
end class <gadget-mirror>;


define open abstract class <gtk-gadget-mixin>
    (<gadget>,
     <gtk-pane-mixin>)
  // sealed constant each-subclass slot %gtk-fixed-width? :: <boolean> = #f,
  //   init-keyword: gtk-fixed-width?:;
  // sealed constant each-subclass slot %gtk-fixed-height? :: <boolean> = #f,
  //   init-keyword: gtk-fixed-height?:;
end class <gtk-gadget-mixin>;

define generic %gtk-fixed-width?
    (gadget :: <gtk-gadget-mixin>)
 => (fixed? :: <boolean>);

define method %gtk-fixed-width?
    (gadget :: <gtk-gadget-mixin>)
 => (fixed? :: <boolean>);
  #f;
end method;

define generic %gtk-fixed-height?
    (gadget :: <gtk-gadget-mixin>)
 => (fixed? :: <boolean>);

define method %gtk-fixed-height?
    (gadget :: <gtk-gadget-mixin>)
 => (fixed? :: <boolean>);
  #f;
end method;

define method gadget-widget (sheet :: <gtk-gadget-mixin>)
 => (widget)
  let mirror = sheet-mirror(sheet);
  let widget = mirror & mirror-widget(mirror);
  widget
end;

// /*---*** Not used yet!
define method widget-attributes
    (_port :: <gtk-port>, gadget :: <gtk-gadget-mixin>)
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
  values(foreground, background, font)
end method widget-attributes;
// */


define method do-compose-space
    (gadget :: <gtk-gadget-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  duim-debug-message("do-compose-space(%= , %=, %=)", gadget, width, height);
  let mirror = sheet-direct-mirror(gadget);
  if (mirror)
    let widget = mirror-widget(mirror);
    gtk-space-requirements(gadget, widget)
  else
    gtk-debug("Composing space on an unmirrored gadget!");
    default-space-requirement(gadget, width: width, height: height)
  end
end method do-compose-space;

// We take the values suggested by GTK as the default sizes
define method gtk-space-requirements
    (gadget :: <gtk-gadget-mixin>, widget :: <GtkWidget>)
 => (space-req :: <space-requirement>)
  let (width, height) = widget-size(widget);
  let max-width  = if (gadget.%gtk-fixed-width?)  width  else $fill end;
  let max-height = if (gadget.%gtk-fixed-height?) height else $fill end;
  make(<space-requirement>,
       min-width:  width,  width:  width,  max-width:  max-width,
       min-height: height, height: height, max-height: max-height)
end method gtk-space-requirements;

define method widget-size
    (widget :: <GtkWidget>)
 => (width :: <integer>, height :: <integer>)
  with-stack-structure (request :: <GtkRequisition>)
    with-gdk-lock
      gtk-widget-get-preferred-size(widget, null-pointer(<GtkRequisition>), request);
    end;
    duim-debug-message("widget-size for %= is %=x%=",
                       widget,
                       request.gtk-requisition-width,
                       request.gtk-requisition-height);
    values(request.gtk-requisition-width, request.gtk-requisition-height)
  end
end method widget-size;


define sealed method defaulted-gadget-label
    (gadget :: <gadget>) => (label)
  gadget-label(gadget) | $default-label
end method defaulted-gadget-label;

define sealed method note-gadget-label-changed
    (gadget :: <gtk-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-label(gadget, mirror)
end method note-gadget-label-changed;

define sealed method update-mirror-label
    (gadget :: <gtk-gadget-mixin>, mirror :: <gadget-mirror>) => ()
  let widget = mirror-widget(sheet-direct-mirror(gadget));
  let label  = defaulted-gadget-label(gadget);
  let label :: <string> = if (instance?(label, <string>)) label else "" end;
  // xt/XtSetValues(widget, label-string: label)
  ignoring("update-mirror-label")
end method update-mirror-label;

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
/*---*** Not ready yet!
    <gtk-bitmap>, <gtk-icon> =>
      values(if (mnemonic) as(<string>, vector(mnemonic)) else "" end,
             label, mnemonic, index);
*/
    <image> =>
      //---*** Decode the image and return a pixmap or something
      values("<image>",
             #f, mnemonic, index);
  end
end method text-or-image-from-gadget-label;


define sealed method note-gadget-enabled
    (client, gadget :: <gtk-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let widget = gadget-widget(gadget);
  widget & with-gdk-lock
    gtk-widget-set-sensitive(widget, #t)
  end
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <gtk-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let widget = gadget-widget(gadget);
  widget & with-gdk-lock
    gtk-widget-set-sensitive(widget, #f)
  end
end method note-gadget-disabled;

//---*** DO WE NEED THIS?
define sealed method activate-gtk-gadget
    (gadget :: <action-gadget>) => (activated? :: <boolean>)
  when (gadget-activate-callback(gadget))
    distribute-activate-callback(gadget);
    #t
  end
end method activate-gtk-gadget;

//---*** DO WE NEED THIS?
define sealed method activate-gtk-gadget
    (gadget :: <text-field>) => (activated? :: <boolean>)
  handle-text-gadget-changed(gadget);
  next-method()
end method activate-gtk-gadget;


/// Exit, cancel, default button, etc.

/*---*** Do we need any of this?
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

define sealed method handle-gadget-activation
    (gadget :: <gadget>) => (handled? :: <boolean>)
  duim-debug-message("Ignoring activation command for gadget %=", gadget);
  #f
end method handle-gadget-activation;

// This handles IDOK commands for more than just buttons...
define method activate-default-button
    (frame :: <frame>) => (activated? :: <boolean>)
  let gadget = gtk-sheet-with-focus();
  duim-debug-message("  Handling IDOK: focus currently %=", gadget);
  let activated? = instance?(gadget, <action-gadget>)
                   & gadget-enabled?(gadget)
                   & activate-gtk-gadget(gadget);
  // If we didn't activate the gadget, try to activate the default button
  unless (activated?)
    let button = frame-default-button(frame);
    // Don't activate an upmapped or disabled default button...
    when (button & sheet-mapped?(button) & gadget-enabled?(button))
      handle-gadget-activation(button)
    end
  end
end method activate-default-button;

define function gtk-sheet-with-focus
    () => (sheet :: false-or(<sheet>))
  let handle = GetFocus();
  let mirror = gadget-mirror(handle);
  when (mirror)
    let sheet = mirror-sheet(mirror);
    if (instance?(sheet, <gtk-subgadget-mixin>))
      subgadget-owner(sheet)
    else
      sheet
    end
  end
end function gtk-sheet-with-focus;


define function handle-cancel
    (frame :: <frame>) => (handled? :: <boolean>)
  let gadget = gtk-sheet-with-focus();
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
*/

//---*** What do we do about setting the color and font of a gadget?


/// Labels

define sealed class <gtk-label>
    (<gtk-gadget-mixin>,
     <label>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword gtk-fixed-width?:  = #t;
  keyword gtk-fixed-height?: = #t;
end class <gtk-label>;

define method %gtk-fixed-width?
    (gadget :: <gtk-label>)
 => (fixed? :: <boolean>)
  #t;
end method;

define method %gtk-fixed-height?
    (gadget :: <gtk-label>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-label>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-label>)
 => (mirror :: <gadget-mirror>)
  with-c-string (c-string = defaulted-gadget-label(gadget))
    with-gdk-lock
      let widget = gtk-label-new(c-string);
      assert(~null-pointer?(widget), "gtk-label-new failed");
      make(<gadget-mirror>,
           widget: widget,
           sheet:  gadget)
    end
  end
end method make-gtk-mirror;

define sealed method update-mirror-label
    (gadget :: <gtk-label>, mirror :: <gadget-mirror>) => ()
  let widget = mirror-widget(mirror);
  with-gdk-lock
    gtk-label-set-text(widget, defaulted-gadget-label(gadget))
  end
end method update-mirror-label;


/// Separators
define sealed class <gtk-separator>
    (<gtk-gadget-mixin>,
     <separator>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <gtk-separator>;

define method %gtk-fixed-width?
    (gadget :: <gtk-separator>)
 => (fixed? :: <boolean>)
  select(gadget-orientation(gadget))
    #"horizontal" => #f;
    #"vertical"   => #t;
  end;
end method;

define method %gtk-fixed-height?
    (gadget :: <gtk-separator>)
 => (fixed? :: <boolean>)
  select(gadget-orientation(gadget))
    #"horizontal" => #t;
    #"vertical"   => #f;
  end;
end method;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <separator>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-separator>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-separator>)
 => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let widget = select(gadget-orientation(gadget))
                   #"horizontal" => gtk-hseparator-new();
                   #"vertical"   => gtk-vseparator-new();
                 end;
      make(<gadget-mirror>,
           widget: widget,
           sheet:  gadget)
  end
end method make-gtk-mirror;


/// Buttons

define class <gtk-button-mixin> (<gtk-gadget-mixin>, <button>)
end class <gtk-button-mixin>;

define method install-event-handlers
    (sheet :: <gtk-button-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  duim-g-signal-connect(sheet, #"clicked") (#rest args) handle-button-gadget-click(sheet) end;
end method install-event-handlers;

define sealed method handle-gtk-clicked-event
    (gadget :: <gtk-button-mixin>, widget :: <GtkWidget>,
     event :: <GdkEventAny>)
 => (handled? :: <boolean>)
  gtk-debug("Clicked on button %=", gadget-label(gadget));
  handle-button-gadget-click(gadget)
end method handle-gtk-clicked-event;

define sealed method button-box-spacing
    (framem :: <gtk-frame-manager>, box :: <button-box>)
 => (spacing :: <integer>)
  select (gadget-orientation(box))
    #"horizontal" => $button-box-x-spacing;
    #"vertical"   => $button-box-y-spacing;
  end
end method button-box-spacing;


/// Push buttons

define sealed class <gtk-push-button>
    (<gtk-button-mixin>,
     <push-button>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword gtk-fixed-width?:  = #t;
  keyword gtk-fixed-height?: = #t;
end class <gtk-push-button>;

define method %gtk-fixed-width?
    (gadget :: <gtk-push-button>)
 => (fixed? :: <boolean>)
  #t;
end method;

define method %gtk-fixed-height?
    (gadget :: <gtk-push-button>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-push-button>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-push-button>)
 => (mirror :: <gadget-mirror>)
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("image label")
  end;
  with-c-string (c-string = text)
    with-gdk-lock
      let widget = gtk-button-new-with-label(c-string);
      assert(~null-pointer?(widget), "gtk-button-new-with-label failed");
      make(<gadget-mirror>,
           widget: widget,
           sheet:  gadget)
    end
  end
end method make-gtk-mirror;

define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <gtk-push-button>)
 => (default? :: <boolean>)
  next-method();
  ignoring("gadget-default?-setter");
  default?
end method gadget-default?-setter;


/// Radio buttons

define sealed class <gtk-radio-button>
    (<gtk-button-mixin>,
     <radio-button>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword gtk-fixed-width?:  = #t;
  keyword gtk-fixed-height?: = #t;
end class <gtk-radio-button>;

define method %gtk-fixed-width?
    (gadget :: <gtk-radio-button>)
 => (fixed? :: <boolean>)
  #t;
end method;

define method %gtk-fixed-height?
    (gadget :: <gtk-radio-button>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-radio-button>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-radio-button>)
 => (mirror :: <gadget-mirror>)
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("image label")
  end;
  with-c-string (c-string = text)
    with-gdk-lock
      let widget
        = if (push-button-like?(gadget))
            gtk-toggle-button-new-with-label(c-string)
          else
            gtk-radio-button-new-with-label(null-pointer(<GSList>), c-string)
          end;
      assert(~null-pointer?(widget), "gtk-toggle/radio-button-new-with-label failed");
      make(<gadget-mirror>,
           widget: widget,
           sheet:  gadget)
    end
  end
end method make-gtk-mirror;

define method update-mirror-attributes
    (gadget :: <gtk-radio-button>, mirror :: <gadget-mirror>) => ()
  next-method();
  let selected? = gadget-value(gadget);
  let widget = mirror-widget(mirror);
  with-gdk-lock
    with-disabled-event-handler (mirror, #"clicked")
      gtk-toggle-button-set-active(widget, selected?)
    end
  end
end method update-mirror-attributes;

define sealed method note-gadget-value-changed
    (gadget :: <gtk-radio-button>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-attributes(gadget, mirror)
end method note-gadget-value-changed;


/// Check buttons

define sealed class <gtk-check-button>
    (<gtk-button-mixin>,
     <check-button>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword gtk-fixed-width?:  = #t;
  keyword gtk-fixed-height?: = #t;
end class <gtk-check-button>;

define method %gtk-fixed-width?
    (gadget :: <gtk-check-button>)
 => (fixed? :: <boolean>)
  #t;
end method;

define method %gtk-fixed-height?
    (gadget :: <gtk-check-button>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-check-button>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-check-button>)
 => (mirror :: <gadget-mirror>)
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("image label")
  end;
  with-c-string (c-string = text)
    with-gdk-lock
      let widget
        = if (push-button-like?(gadget))
            gtk-toggle-button-new-with-label(c-string)
          else
            gtk-check-button-new-with-label(c-string)
          end;
      assert(~null-pointer?(widget), "gtk-toggle/radio-button-new-with-label failed");
      make(<gadget-mirror>,
           widget: widget,
           sheet:  gadget)
    end
  end
end method make-gtk-mirror;

define method update-mirror-attributes
    (gadget :: <gtk-check-button>, mirror :: <gadget-mirror>) => ()
  next-method();
  let selected? = gadget-value(gadget);
  let widget = mirror-widget(mirror);
  with-gdk-lock
    with-disabled-event-handler (mirror, #"clicked")
      gtk-toggle-button-set-active(widget, selected?)
    end
  end
end method update-mirror-attributes;

define sealed method note-gadget-value-changed
    (gadget :: <gtk-check-button>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-attributes(gadget, mirror)
end method note-gadget-value-changed;


/// Text gadgets

define method gtk-copy-text (text :: <c-string>) => (str :: <string>)
  // Convert to gc'able string.
  as(<byte-string>, text)
end;

// Mixin class for text fields, password fields, and text editors, i.e.
// all GtkEditable objects.
define open abstract class <gtk-text-gadget-mixin>
    (<gtk-gadget-mixin>,
     <text-field>)
  sealed slot %changed? :: <boolean> = #f;
  sealed constant slot %current-selection :: <simple-text-range>
    = make(<text-range>, start: -1, end: -1);
end class <gtk-text-gadget-mixin>;

define method install-event-handlers
    (sheet :: <gtk-text-gadget-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  duim-g-signal-connect(sheet, #"activate") (#rest args) activate-gtk-gadget(sheet) end;
  duim-g-signal-connect(sheet, #"changed") (#rest args) handle-text-gadget-changing(sheet) end;
end method install-event-handlers;

define sealed method update-mirror-attributes
    (gadget :: <gtk-text-gadget-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  // Set the initial text selection
  text-selection(gadget) := gadget.%current-selection
end method update-mirror-attributes;

// This is called right after gadget buffer text changes in DUIM
define sealed method note-gadget-text-changed
    (gadget :: <gtk-text-gadget-mixin>) => ()
  gtk-debug("note-gadget-text-changed");
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

define sealed method handle-text-gadget-changing
    (gadget :: <gtk-text-gadget-mixin>) => ()
  gtk-debug("handle-text-gadget-changing");
  let old-text = gadget.gadget-text-buffer;
  let widget = gadget-widget(gadget);
  // --- TODO: use a stretchy buffer to avoid copying on each character?
  with-gdk-lock
    let chars = gtk-editable-get-chars(widget, 0, -1);
    let new-text = unless (old-text = chars)
                     gadget.gadget-text-buffer := gtk-copy-text(chars);
                   end;
    g-free(chars);
    when (new-text)
      gadget.%changed? := #t;
      distribute-text-changing-callback(gadget, new-text)
    end;
  end;
end method handle-text-gadget-changing;

define sealed method handle-text-gadget-changed
    (gadget :: <gtk-text-gadget-mixin>) => ()
  gtk-debug("handle-text-gadget-changed");
  when (gadget.%changed?)
    let text = gadget-text-buffer(gadget);
    distribute-text-changed-callback(gadget, text);
    gadget.%changed? := #f
  end
end method handle-text-gadget-changed;

/* ---*** Implement me
define sealed method text-selection
    (gadget :: <gtk-text-gadget-mixin>) => (range :: type-union(<text-range>, one-of(#f)))
  let widget = gadget-widget(gadget);
  let start-pos = widget.selection-start-pos-value;
  let end-pos = widget.selection-end-pos-value;
  when (start-pos < end-pos)
    make(<text-range>, start: start-pos, end: end-pos)
  end;
end method text-selection;

define sealed method selected-text
    (gadget :: <gtk-text-gadget-mixin>) => (string :: false-or(<string>))
  let widget = gadget-widget(gadget);
  let start-pos = widget.selection-start-pos-value;
  let end-pos = widget.selection-end-pos-value;
  if (start-pos >= end-pos)
    #f
  elseif (start-pos = 0 & end-pos = gadget.gadget-text-buffer.size)
    gadget.gadget-text-buffer
  else
    with-gdk-lock
      let chars = gtk-editable-get-chars(widget, start-pos, end-pos);
      let string = gtk-copy-text(chars);
      g-free(chars);
      string
    end
  end;
end method selected-text;

*/

define method widget-range-bounds (widget, range == #t)
 => (start-pos :: <integer>, end-pos :: <integer>)
  values(0, -1)
end method widget-range-bounds;

define method widget-range-bounds (widget, range == #f)
 => (start-pos :: <integer>, end-pos :: <integer>)
  let pos = with-gdk-lock gtk-editable-get-position(widget) end;
  values(pos, pos)
end method widget-range-bounds;

define method widget-range-bounds (widget, range :: <text-range>)
 => (start-pos :: <integer>, end-pos :: <integer>)
  let start-pos = range.text-range-start;
  let end-pos = range.text-range-end;
  if (start-pos < end-pos)
    values(start-pos, end-pos)
  else
    widget-range-bounds(widget, #f)
  end;
end method widget-range-bounds;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)),
     gadget :: <gtk-text-gadget-mixin>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  let widget = gadget-widget(gadget);
  let (start-pos, end-pos) = widget-range-bounds(widget, range);
  with-gdk-lock
    gtk-editable-select-region(widget, start-pos, end-pos);
  end;
  range
end method text-selection-setter;

define sealed method text-caret-position
    (gadget :: <gtk-text-gadget-mixin>)
 => (position :: <integer>)
  let widget = gadget-widget(gadget);
  with-gdk-lock
    gtk-editable-get-position(widget);
  end
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <gtk-text-gadget-mixin>)
 => (position :: false-or(<integer>))
  if (position)
    let widget = gadget-widget(gadget);
    with-gdk-lock
      gtk-editable-set-position(widget, position);
    end;
    position
  end;
end method text-caret-position-setter;


/// Text and password fields

/// Text fields

define abstract class <gtk-text-field-mixin>
    (<gtk-text-gadget-mixin>,
     <text-field>)
  // sealed constant each-subclass slot %gtk-text-visibility,
  //  required-init-keyword: gtk-text-visibility:;
  keyword gtk-fixed-height?: = #t;
end class <gtk-text-field-mixin>;

define generic %gtk-text-visibility
    (gadget :: <gtk-text-field-mixin>)
 => (fixed? :: <boolean>);

define method %gtk-fixed-height?
    (gadget :: <gtk-text-field-mixin>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed method make-gtk-mirror
    (gadget :: <gtk-text-field-mixin>)
 => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let max = text-field-maximum-size(gadget);
    let text = gadget-text-buffer(gadget);
    let visibility = %gtk-text-visibility(gadget);
    let widget = gtk-entry-new();
    assert(~null-pointer?(widget), "gtk-entry-new failed");
    if (max)
      gtk-entry-set-max-length(widget, max)
    end;
    // Note that this is happening before install-event-handlers, so don't
    // need to disable events.
    gtk-entry-set-visibility(widget, visibility);
    unless (empty?(text))
      with-c-string (c-text = text)
        gtk-entry-set-text(widget, c-text);
      end;
    end;
    make(<gadget-mirror>,
         widget: widget,
         sheet:  gadget)
  end
end method make-gtk-mirror;

// Updates the GTK text field from the DUIM gadget
define sealed method update-gadget-text
    (gadget :: <gtk-text-field-mixin>, mirror :: <gadget-mirror>) => ()
  ignore(mirror);
  let widget = gadget-widget(gadget);
  let new-text = gadget-text-buffer(gadget);
  with-gdk-lock
    with-disabled-event-handler (mirror, #"changed")
      with-c-string (c-text = new-text)
        gtk-entry-set-text(widget, c-text);
      end;
    end
  end;
end method update-gadget-text;

/// Text fields

define sealed class <gtk-text-field>
    (<gtk-text-field-mixin>,
     <text-field>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword gtk-text-visibility: = #t;
end class <gtk-text-field>;

define method %gtk-text-visibility
    (gadget :: <gtk-text-field>)
 => (fixed? :: <boolean>);
  #t
end method %gtk-text-visibility;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <text-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-text-field>, #f)
end method class-for-make-pane;

/// Password fields

define sealed class <gtk-password-field>
    (<gtk-text-field-mixin>,
     <password-field>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword gtk-text-visibility: = #f;
end class <gtk-password-field>;

define method %gtk-text-visibility
    (gadget :: <gtk-password-field>)
 => (fixed? :: <boolean>)
  #f
end method %gtk-text-visibility;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <password-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-password-field>, #f)
end method class-for-make-pane;


/// Text editors

define sealed class <gtk-text-editor>
    (<gtk-gadget-mixin>,
     <text-editor>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <gtk-text-editor>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <text-editor>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-text-editor>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-text-editor>)
 => (mirror :: <gadget-mirror>)
  let lines = gadget-lines(gadget);
  let columns = gadget-columns(gadget);
  let word-wrap? = text-field-word-wrap?(gadget);
  let text = gadget-text-buffer(gadget);
  with-gdk-lock
    let widget = gtk-text-view-new();
    assert(~null-pointer?(widget), "gtk-text-new failed");
    // Note that this is happening before install-event-handlers, so don't
    // need to disable events.
    when (lines | columns)
      ignoring("lines:/columns:")
    end;
    if (word-wrap?)
      widget.@wrap-mode := $GTK-WRAP-WORD-CHAR;
    end;
    let buffer = gtk-text-view-get-buffer(widget);
    //duim-debug-message("Setting text to %=", text);
    gtk-text-buffer-set-text(buffer, text, -1);
    make(<gadget-mirror>,
         widget: widget,
         sheet:  gadget)
  end
end method make-gtk-mirror;

define sealed method update-gadget-text
    (gadget :: <gtk-text-editor>, mirror :: <gadget-mirror>) => ()
  //duim-debug-message("Updating text-editors text");
  ignore(mirror);
  note-gadget-text-changed(gadget);
end method update-gadget-text;

define method note-gadget-text-changed
    (gadget :: <gtk-text-editor>) => ()
  let widget = gadget-widget(gadget);
  when (widget)
    with-gdk-lock
      let buffer = gtk-text-view-get-buffer(widget);
      let new-text = gadget-text-buffer(gadget);
      gtk-text-buffer-set-text(buffer, new-text, size(new-text));
    end
  end;
end;

define method gadget-text-setter
    (text :: <string>, gadget :: <gtk-text-editor>, #key do-callback? = #f)
 => (text :: <string>)
  gadget-text-buffer(gadget) := text;
  note-gadget-text-changed(gadget);
  text;
end;


/// Scroll bars

define sealed class <gtk-scroll-bar>
    (<gtk-gadget-mixin>,
     <scroll-bar>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <gtk-scroll-bar>;

define sealed class <gtk-horizontal-scroll-bar> (<gtk-scroll-bar>)
  keyword gtk-fixed-height?: = #t;
end class <gtk-horizontal-scroll-bar>;

define method %gtk-fixed-height?
    (gadget :: <gtk-horizontal-scroll-bar>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed class <gtk-vertical-scroll-bar> (<gtk-scroll-bar>)
  keyword gtk-fixed-width?: = #t;
end class <gtk-vertical-scroll-bar>;

define method %gtk-fixed-width?
    (gadget :: <gtk-vertical-scroll-bar>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <scroll-bar>,
     #key orientation = #"horizontal")
 => (class :: <class>, options :: false-or(<sequence>))
  values(select (orientation)
           #"horizontal" => <gtk-horizontal-scroll-bar>;
           #"vertical"   => <gtk-vertical-scroll-bar>;
         end,
         #f)
end method class-for-make-pane;

define function gadget-range-values
    (gadget :: <range-gadget-mixin>)
 => (start-value :: <real>, end-value :: <real>, increment :: <real>)
  let range = gadget-value-range(gadget);
  let n = range.size;
  select (n)
    0 =>
      values(0, 0, 0);
    1 =>
      let start = range[0];
      values(start, start, 0);
    otherwise =>
      let start = range[0];
      values(start, range[n - 1], range[1] - start)
  end;
end gadget-range-values;

define method scroll-bar-adjusted-contents
    (gadget :: <gtk-scroll-bar>)
 => (value :: <double-float>,
     lower :: <double-float>, upper :: <double-float>,
     step-increment :: <double-float>, page-increment :: <double-float>,
     page-size :: <double-float>)
  let range-value = gadget-value(gadget);
  let (range-start, range-end, range-step) = gadget-range-values(gadget);
  let slug-size = gadget-slug-size(gadget);

  let lower = as(<double-float>, range-start);
  let page-size = as(<double-float>, slug-size);
  let step-increment = as(<double-float>, range-step);
  let page-increment = max(page-size, step-increment);
  let upper = as(<double-float>, range-end); // this inclues page size.
  let value = as(<double-float>, range-value);

  values(value, lower, upper, step-increment, page-increment, page-size)
end scroll-bar-adjusted-contents;

define sealed method make-gtk-mirror
    (gadget :: <gtk-scroll-bar>)
 => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let (value, lower, upper, step-inc, page-inc, page-size)
      = scroll-bar-adjusted-contents(gadget);
    let adj = gtk-adjustment-new(value,
                                 lower,
                                 upper,
                                 step-inc,
                                 page-inc,
                                 page-size);
    let widget = select(gadget-orientation(gadget))
                   #"horizontal" => gtk-hscrollbar-new(adj);
                   #"vertical"   => gtk-vscrollbar-new(adj);
                 end;
    assert(~null-pointer?(widget), "gtk-h/vscrollbar-new failed");
    // --- Does DUIM have anything to select/deselect smooth scrolling?
    // gtk-range-set-update-policy(widget, $gtk-update-discontinuous);
    make(<gadget-mirror>,
         widget: widget,
         sheet:  gadget)
  end
end method make-gtk-mirror;

define method install-event-handlers
    (sheet :: <gtk-scroll-bar>, mirror :: <gadget-mirror>) => ()
  next-method();
  let widget = mirror-widget(mirror);
  duim-g-signal-connect(sheet, #"value-changed") (adjustment, #rest args) gtk-adjustment-value-changed-signal-handler(sheet, widget) end;
end method install-event-handlers;

define method gtk-adjustment-value-changed-signal-handler
    (gadget :: <gtk-scroll-bar>, widget :: <GtkWidget>) => ()
  let adj = gtk-range-get-adjustment(widget);
  let value = adj.gtk-adjustment-get-value;
  scroll-to-position(gadget, value);
end;

define sealed method update-mirror-attributes
    (gadget :: <gtk-scroll-bar>, mirror :: <gadget-mirror>) => ()
  next-method();
//  ignoring("update-mirror-attributes on <scroll-bar>")
end method update-mirror-attributes;

define sealed method note-gadget-slug-size-changed
    (gadget :: <gtk-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-slug-size-changed;

define sealed method note-gadget-value-changed
    (gadget :: <gtk-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <gtk-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-value-range-changed;

define sealed method note-scroll-bar-changed
    (gadget :: <gtk-scroll-bar>) => ()
  let widget = gadget-widget(gadget);
  when (widget)
    with-gdk-lock
      let (value, lower, upper, step-inc, page-inc, page-size)
        = scroll-bar-adjusted-contents(gadget);
      let adjustment :: <GtkAdjustment> = gtk-range-get-adjustment(widget);
      adjustment.@lower := lower;
      adjustment.@upper := upper;
      adjustment.@value := value;
      adjustment.@step-increment := step-inc;
      adjustment.@page-increment := page-inc;
      adjustment.@page-size := page-size;
    end
  end;
end method note-scroll-bar-changed;


/// List gadgets

define sealed class <gtk-list-control-mixin>
    (<gtk-gadget-mixin>,
     <collection-gadget>,
     <sealed-constructor-mixin>)
  slot store-model :: false-or(<GtkTreeModel>) = #f;
end class <gtk-list-control-mixin>;

define sealed class <gtk-tree-view-control-mixin>
    (<gtk-list-control-mixin>)
end;

define method update-mirror-attributes
    (gadget :: <gtk-tree-view-control-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  with-gdk-lock
    let widget = mirror.mirror-widget;
    let selection = gtk-tree-view-get-selection(widget);
    gtk-tree-selection-set-mode
      (selection,
       select (gadget-selection-mode(gadget))
         #"none"     => $GTK-SELECTION-NONE;
         #"single"   => $GTK-SELECTION-BROWSE;
         #"multiple" => $GTK-SELECTION-MULTIPLE;
       end);
    if (instance?(gadget, <table-control>))
      widget.@headers-visible := #t;
    else
      widget.@headers-visible := #f;
    end;
  end;
end method update-mirror-attributes;

define method install-event-handlers
    (sheet :: <gtk-tree-view-control-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  let widget = mirror-widget(mirror);
  let selection = with-gdk-lock gtk-tree-view-get-selection(widget) end;
  g-signal-connect(selection, "changed", method (#rest args) handle-gtk-select-row-event(sheet) end);
  duim-g-signal-connect(sheet, #"button-press-event") (widget, event, #rest args) handle-gtk-button-press-event(sheet, event) end;
  with-gdk-lock
    gtk-widget-add-events(widget, $GDK-BUTTON-PRESS-MASK);
  end
end method install-event-handlers;

define sealed method handle-gtk-select-row-event
    (gadget :: <gtk-tree-view-control-mixin>)
 => (handled? :: <boolean>)
  gtk-debug("Selected list control item!");
  let mirror = gadget.sheet-direct-mirror;
  let widget = mirror-widget(mirror);
  let selection = gtk-tree-view-get-selection(widget);
  let new-selection = make(<stretchy-vector>);

  let (selected-path-list, model)
    = gtk-tree-selection-get-selected-rows(selection);
  let selected-paths
    = glist-to-vector(selected-path-list, <GtkTreePath>);

  for (path in selected-paths)
    with-stack-structure (iter :: <GtkTreeIter>)
      gtk-tree-model-get-iter(model, iter, path);
      with-stack-structure (value :: <GValue>)
        g-value-nullify(value);
        gtk-tree-model-get-value(model, iter, 0, value);
        add!(new-selection, g-value-to-dylan(value));
      end;
    end;
  end;

  gtk-debug("  Selection now %=", new-selection);
  distribute-selection-changed-callback(gadget, new-selection);
  #t
end method handle-gtk-select-row-event;

define sealed method handle-gtk-button-press-event
    (gadget :: <gtk-list-control-mixin>, event :: <GdkEventButton>)
 => (handled? :: <boolean>)
  gtk-debug("Pressed button %=, type %=",
            event.gdk-event-button-button,
            select (event.gdk-event-button-type)
              $GDK-BUTTON-PRESS  => "button press";
              $GDK-2BUTTON-PRESS => "double click";
              $GDK-3BUTTON-PRESS => "treble click";
              otherwise => event.gdk-event-button-type;
            end);
  if (event.gdk-event-button-type == $GDK-2BUTTON-PRESS)
    gtk-debug("Double clicked on list control!");
    when (gadget-activate-callback(gadget))
      distribute-activate-callback(gadget);
    end;
    #t
  elseif ((event.gdk-event-button-type == $GDK-BUTTON-PRESS) & (event.gdk-event-button-button == 3)) //right click
    gtk-debug("right clicked on list control!");
    when (gadget-popup-menu-callback(gadget))
      gtk-set-button-time(event);
      handle-event(gadget,
                   make(<popup-menu-gadget-event>,
                        gadget: gadget,
                        target: 0,
                        x: round(event.gdk-event-button-x),
                        y: round(event.gdk-event-button-y)));
      //XXX: fix this when there is some spare time
      //distribute-popup-menu-callback(gadget, 0,
      //                               x: round(event.gdk-event-button-x),
      //                               y: round(event.gdk-event-button-y));
    end;
    #t
  end
end method handle-gtk-button-press-event;

define method glist-to-vector
    (GList :: <GList>, type :: <type>)
 => (vector :: <stretchy-object-vector>)
  let vector = make(<stretchy-object-vector>);
  local method process-list
            (GList :: <GList>)
          case
            null-pointer?(GList) =>
              #f;
            otherwise =>
              add!(vector, c-type-cast(type, glist.g-list-data));
              process-list(glist.g-list-next);
          end
        end;
  process-list(GList);
  vector
end method glist-to-vector;

define sealed method note-gadget-items-changed
    (gadget :: <gtk-list-control-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-list-control-items(gadget, mirror)
end method note-gadget-items-changed;

define method update-gadget
    (gadget :: <gtk-list-control-mixin>) => ()
  // No, we don't call 'next-method' here!
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-list-control-items(gadget, mirror)
end method update-gadget;

define function generate-list-model () => (res :: <GtkTreeModel>)
  with-gdk-lock
    let type-vector = make(<GType*>, element-count: 2);
    type-vector[0] := $G-TYPE-INT;
    type-vector[1] := $G-TYPE-STRING;
    gtk-list-store-newv(2, type-vector);
  end;
end;

define sealed method update-list-control-items
    (gadget :: <gtk-list-control-mixin>, mirror :: <gadget-mirror>)
 => ()
  let widget = mirror.mirror-widget;
  let items = gadget-items(gadget);
  let label-function = gadget-label-key(gadget);
  let model = gadget.store-model
    | begin
        gadget.store-model := generate-list-model();
        with-gdk-lock widget.@model := gadget.store-model end;
        gadget.store-model;
      end;
  with-gdk-lock
    gtk-list-store-clear(model);
    with-stack-structure(iter :: <GtkTreeIter>)
      for (item in items, i from 0)
        let label = label-function(item);
        gtk-list-store-append(model, iter);
        with-stack-structure (gvalue :: <GValue>)
          g-value-nullify(gvalue);
          g-value-set-value(gvalue, i);
          gtk-list-store-set-value(model, iter, 0, gvalue);
          g-value-nullify(gvalue);
          g-value-set-value(gvalue, label);
          gtk-list-store-set-value(model, iter, 1, gvalue);
        end;
      end
    end;
  end
end method update-list-control-items;

define method update-mirror-attributes
    (gadget :: <gtk-list-control-mixin>, mirror :: <gadget-mirror>) => ()
  next-method();
  update-list-control-items(gadget, mirror);
end;

define sealed method update-gadget-selection
    (gadget :: <gtk-list-control-mixin>) => ()
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    #"single" =>
      ignoring("update-gadget-selection");
    #"multiple" =>
      ignoring("update-gadget-selection");
  end
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <gtk-list-control-mixin>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

define sealed method handle-selection-changed
    (gadget :: <gtk-list-control-mixin>) => (handled? :: <boolean>)
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    #"single" =>
      ignoring("handle-selection-changed for <gtk-list-control-mixin>");
    #"multiple" =>
      ignoring("handle-selection-changed for <gtk-list-control-mixin>");
  end;
  #t
end method handle-selection-changed;


// List boxes

define sealed class <gtk-list-box>
    (<gtk-tree-view-control-mixin>,
     <list-box>,
     <leaf-pane>)
end class <gtk-list-box>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-list-box>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-list-box>)
 => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let widget = gtk-tree-view-new();
    let renderer = gtk-cell-renderer-text-new();
    let column = gtk-tree-view-column-new();
    gtk-tree-view-column-pack-start(column, renderer, #f);
    gtk-tree-view-column-add-attribute(column, renderer, "text", 1);
    gtk-tree-view-append-column(widget, column);
    make(<gadget-mirror>,
         widget: widget,
         sheet:  gadget)
  end
end method make-gtk-mirror;


// List controls

//---*** Need to implement add-item etc...
define sealed class <gtk-list-control>
    (<gtk-tree-view-control-mixin>,
     <list-control>,
     <leaf-pane>)
end class <gtk-list-control>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <list-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-list-control>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-list-control>)
 => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let widget = gtk-tree-view-new();
    let renderer = gtk-cell-renderer-text-new();
    let column = gtk-tree-view-column-new();
    gtk-tree-view-column-pack-start(column, renderer, #f);
    gtk-tree-view-column-add-attribute(column, renderer, "text", 1);
    gtk-tree-view-append-column(widget, column);
    make(<gadget-mirror>,
         widget: widget,
         sheet:  gadget)
  end
end method make-gtk-mirror;

// Scrolled mixins
define sealed class <scrolled-mixin>
    (<gtk-gadget-mixin>)
end;

define sealed class <scrolled-mirror> (<gadget-mirror>)
  constant slot scrolled-window, required-init-keyword: scrolled-window:;
end;

define method set-mirror-parent
    (child :: <scrolled-mirror>, parent :: <widget-mirror>)
 => ()
  with-gdk-lock
    gtk-container-add(parent.mirror-widget, child.scrolled-window);
  end;
end;

define method set-mirror-size
    (mirror :: <scrolled-mirror>, width :: <integer>, height :: <integer>)
 => ()
  set-widget-size(mirror, mirror.scrolled-window, width, height);
end;

define method do-compose-space
    (gadget :: <scrolled-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  duim-debug-message("do-compose-space(%= , %=, %=)", gadget, width, height);
  let mirror = sheet-direct-mirror(gadget);
  if (mirror)
    let widget = scrolled-window(mirror);
    gtk-space-requirements(gadget, widget)
  else
    gtk-debug("Composing space on an unmirrored gadget!");
    default-space-requirement(gadget, width: width, height: height)
  end
end method do-compose-space;

define method init-scrolled-window
    (widget :: <GtkWidget>, gadget :: <scrolled-mixin>)
  with-gdk-lock
    let scrolled-win
      = gtk-scrolled-window-new(null-pointer(<GtkAdjustment>),
                                null-pointer(<GtkAdjustment>));
    gtk-container-add(scrolled-win, widget);
    let (#rest policies)
      = select (gadget-scroll-bars(gadget))
          #f, #"none" => values($GTK-POLICY-NEVER, $GTK-POLICY-NEVER);
          #t, #"dynamic" => values($GTK-POLICY-AUTOMATIC, $GTK-POLICY-AUTOMATIC);
          #"both" => values($GTK-POLICY-ALWAYS, $GTK-POLICY-ALWAYS);
          //#"both" => values($GTK-POLICY-AUTOMATIC, $GTK-POLICY-AUTOMATIC);
          #"horizontal" => values($GTK-POLICY-ALWAYS, $GTK-POLICY-AUTOMATIC);
          #"vertical" => values($GTK-POLICY-AUTOMATIC, $GTK-POLICY-ALWAYS);
        end;
    //duim-debug-message("scroll-bar %=", gadget-scroll-bars(gadget));
    apply(gtk-scrolled-window-set-policy, scrolled-win, policies);
    gtk-widget-show(scrolled-win);
    scrolled-win;
  end;
end;


// Table controls

define sealed class <gtk-table-control>
    (<scrolled-mixin>,
     <gtk-tree-view-control-mixin>,
     <table-control>,
     <leaf-pane>)
end class <gtk-table-control>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <table-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-table-control>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-table-control>)
 => (mirror :: <gadget-mirror>)
  let columns = table-control-columns(gadget);
  with-gdk-lock
    let widget = gtk-tree-view-new();
    let columns = table-control-columns(gadget);
    for (c in columns, i from 1)
      let renderer = gtk-cell-renderer-text-new();
      let column = gtk-tree-view-column-new();
      gtk-tree-view-column-pack-start(column, renderer, #f);
      gtk-tree-view-column-add-attribute(column, renderer, "text", i);
      //gtk-tree-view-column-set-resizable(column, #t);
      gtk-tree-view-append-column(widget, column);
    end;
    gtk-tree-view-set-fixed-height-mode(widget, #t);
    let scrolled-win
      = init-scrolled-window(widget, gadget);
    make(<scrolled-mirror>,
         widget: widget,
         scrolled-window: scrolled-win,
         sheet:  gadget);
  end;
end method make-gtk-mirror;

define method update-mirror-attributes
    (gadget :: <gtk-table-control>, mirror :: <gadget-mirror>) => ()
  next-method();
  let widget = mirror.mirror-widget;
  let columns = table-control-columns(gadget);
  with-gdk-lock
    for (c in columns, i from 0)
      let column = gtk-tree-view-get-column(widget, i);
      column.@title := c.table-column-heading;
      column.@alignment
        := select (c.table-column-alignment)
             #"left"      => $GTK-JUSTIFY-LEFT;
             #"right"     => $GTK-JUSTIFY-RIGHT;
             #"center"    => $GTK-JUSTIFY-CENTER;
           end;
      column.@fixed-width := c.table-column-width;
    end;
  end;
  //gtk-clist-column-titles-active(widget);
end method update-mirror-attributes;

define sealed class <gtk-table-item> (<table-item>)
  sealed slot %table :: false-or(<table-control>) = #f;
end;

define sealed domain make (singleton(<gtk-table-item>));
define sealed domain initialize(<gtk-table-item>);

define sealed method do-make-item
    (pane :: <gtk-table-control>, class == <table-item>, #key object)
 => (item :: <gtk-table-item>)
  make(<gtk-table-item>, object: object);
end;

define sealed method do-add-item
    (gadget :: <gtk-table-control>, item :: <gtk-table-item>, #key after) => ()
  let mirror = sheet-direct-mirror(gadget);
  let columns = table-control-columns(gadget);
  let model = gadget.store-model;
  let item = item-object(item);
  mirror & with-gdk-lock
    with-stack-structure (iter :: <GtkTreeIter>)
      gtk-list-store-append(model, iter);
      with-stack-structure (gvalue :: <GValue>)
        g-value-nullify(gvalue);
        g-value-set-value(gvalue, gadget-items(gadget).size - 1);
        gtk-list-store-set-value(model, iter, 0, gvalue);
        for (c in columns, j from 1)
          let generator = table-column-generator(c);
          let label = gadget-item-label(gadget, generator(item));
          unless (instance?(label, <string>))
            label := format-to-string("%=", label);
          end;
          g-value-nullify(gvalue);
          g-value-set-value(gvalue, label);
          gtk-list-store-set-value(model, iter, j, gvalue);
        end;
      end;
    end;
  end;
end;

/*
define method install-event-handlers
    (sheet :: <gtk-table-control>, mirror :: <gadget-mirror>) => ()
  next-method();
  install-named-handlers(mirror, #[#"click_column", #"resize_column"])
end method install-event-handlers;

define sealed method handle-gtk-click-column-event
    (gadget :: <gtk-table-control>, widget :: <GtkWidget>,
     event :: <GdkEventAny>)
 => (handled? :: <boolean>)
  gtk-debug("Clicked on column!");
  #t
end method handle-gtk-click-column-event;

define sealed method handle-gtk-resize-column-event
    (gadget :: <gtk-list-control-mixin>, widget :: <GtkWidget>,
     event :: <GdkEventAny>)
 => (handled? :: <boolean>)
  gtk-debug("Resized column!");
  #t
end method handle-gtk-resize-column-event;
*/

define function generate-table-model (no-of-columns :: <integer>)
 => (res :: <GtkTreeModel>)
  with-gdk-lock
    let type-vector = make(<GType*>, element-count: 1 + no-of-columns);
    type-vector[0] := $G-TYPE-INT;
    for (i from 1 to no-of-columns)
      type-vector[i] := $G-TYPE-STRING;
    end;
    gtk-list-store-newv(no-of-columns + 1, type-vector);
  end;
end;

define sealed method update-list-control-items
    (gadget :: <gtk-table-control>, mirror :: <gadget-mirror>)
 => ()
  let widget = mirror.mirror-widget;
  let items = gadget-items(gadget);
  let columns = table-control-columns(gadget);
  let no-of-columns = columns.size;
  let model = gadget.store-model |
    begin
      gadget.store-model := generate-table-model(no-of-columns);
      widget.@model := with-gdk-lock gadget.store-model end;
      gadget.store-model;
    end;
  with-gdk-lock
    gtk-list-store-clear(model);
    with-stack-structure(iter :: <GtkTreeIter>)
      for (item in items, i from 0)
        gtk-list-store-append(model, iter);
        with-stack-structure (gvalue :: <GValue>)
          g-value-nullify(gvalue);
          g-value-set-value(gvalue, i);
          gtk-list-store-set-value(model, iter, 0, gvalue);
          for (c in columns, j from 1)
            let generator = table-column-generator(c);
            let label = gadget-item-label(gadget, generator(item));
            unless (instance?(label, <string>))
              label := format-to-string("%=", label);
            end;
            g-value-nullify(gvalue);
            g-value-set-value(gvalue, label);
            gtk-list-store-set-value(model, iter, j, gvalue);
          end;
        end;
      end;
    end;
    //gtk-tree-view-columns-autosize(widget);
  end;
end method update-list-control-items;

// Tree control

define sealed class <gtk-tree-control>
    (<scrolled-mixin>,
     <gtk-tree-view-control-mixin>,
     <tree-control>,
     <leaf-pane>)
end;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <tree-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-tree-control>, #f);
end;

define sealed method make-gtk-mirror
    (gadget :: <gtk-tree-control>) => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let widget = gtk-tree-view-new();
    let renderer = gtk-cell-renderer-text-new();
    let column = gtk-tree-view-column-new();
    gtk-tree-view-column-pack-start(column, renderer, #f);
    gtk-tree-view-column-add-attribute(column, renderer, "text", 1);
    gtk-tree-view-append-column(widget, column);
    let type-vector = make(<GType*>, element-count: 2);
    type-vector[0] := $G-TYPE-INT;
    type-vector[1] := $G-TYPE-STRING;
    let model = gtk-tree-store-newv(2, type-vector);
    gadget.store-model := model;
    widget.@model := model;
    let scrolled-win = init-scrolled-window(widget, gadget);
    make(<scrolled-mirror>,
         widget: widget,
         scrolled-window: scrolled-win,
         sheet: gadget);
  end;
end;

define sealed class <gtk-tree-node> (<tree-node>)
  sealed slot gtk-iter :: false-or(<GtkTreeIter>) = #f,
    init-keyword: gtk-iter:;
end;

define sealed domain make (singleton(<gtk-tree-node>));
define sealed domain initialize (<gtk-tree-node>);

define sealed method do-make-node
    (pane :: <gtk-tree-control>, class == <tree-node>, #rest rest, #key, #all-keys )
 => (item :: <gtk-tree-node>)
  apply(make, <gtk-tree-node>, rest);
end;

define sealed method do-add-node
    (pane :: <gtk-tree-control>, parent, item :: <gtk-tree-node>, #key after) => ()
  let mirror = sheet-direct-mirror(pane);
  when (mirror)
    //insert into tree control and gadget-items
    //remember index into gadget-items (for selection)
    let index = gadget-items(pane).size - 1;
    let np = null-pointer(<GtkTreeIter>);
    let gtk-parent = ((node-generation(item) > 0) & gtk-iter(parent)) | np;
    let children? = tree-control-children-predicate(pane);
    with-gdk-lock
      with-stack-structure (iter :: <GtkTreeIter>)
        let model = pane.store-model;
        gtk-tree-store-insert-before(model, iter, gtk-parent, np);
        with-stack-structure (data :: <GValue>)
          g-value-nullify(data);
          g-value-set-value(data, index);
          gtk-tree-store-set-value(model, iter, 0, data);
          let label = pane.gadget-label-key(item.node-object);
          unless (instance?(label, <string>))
            label := format-to-string("%=", label);
          end;
          g-value-nullify(data);
          g-value-set-value(data, label);
          gtk-tree-store-set-value(model, iter, 1, data);
          if (children?(item.node-object))
            with-stack-structure (dummy :: <GtkTreeIter>)
              with-stack-structure (dummy-value :: <GValue>)
                gtk-tree-store-insert-before(model, dummy, iter, np);
                g-value-nullify(dummy-value);
                g-value-set-value(dummy-value, "this is just a dummy");
                gtk-tree-store-set-value(model, dummy, 1, dummy-value);
                g-value-nullify(dummy-value);
                g-value-set-value(dummy-value, -1);
                gtk-tree-store-set-value(model, dummy, 0, dummy-value);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

define sealed method do-add-nodes
    (pane :: <gtk-tree-control>, parent, nodes :: <sequence>, #key after) => ()
  //let selected-nodes = gadget-selected-nodes(pane);
  //gadget-selection(pane) := #[];
  for (node in nodes)
    add-node(pane, parent, node, after: after)
  end;
  //gadget-selection(pane) := compute-gadget-selection(pane, selected-nodes)
end method do-add-nodes;

define sealed method do-expand-node
    (pane :: <gtk-tree-control>, node :: <gtk-tree-node>) => ()
  with-gdk-lock
    let path = gtk-tree-model-get-path(pane.store-model, node.gtk-iter);
    gtk-tree-view-expand-row(pane.sheet-direct-mirror.mirror-widget , path, #f)
  end;
end method do-expand-node;

define sealed method update-list-control-items
    (gadget :: <gtk-tree-control>, mirror :: <gadget-mirror>)
 => ()
  let model = gadget.store-model;
  let roots = tree-control-roots(gadget);
  with-gdk-lock
    gtk-tree-store-clear(model);
  end;
  gadget-selection(gadget) := #[];
  gadget-items(gadget).size := 0;
  tree-control-root-nodes(gadget) := make(<stretchy-vector>);
  for (tln in roots)
    let node = make-node(gadget, tln);
    add-node(gadget, gadget, node, setting-roots?: #t);
  end;
end;

define method install-event-handlers
    (sheet :: <gtk-tree-control>, mirror :: <gadget-mirror>) => ()
  next-method();
  duim-g-signal-connect(sheet, #"row-expanded")
    (treeview, treeiter, treepath, #rest args)
    handle-row-expanded(sheet, treeiter, treepath) end;
end;

define function find-node-list
    (gadget :: <gtk-tree-control>, indices :: <collection>)
 => (res :: false-or(<tree-node>))
  let roots = tree-control-root-nodes(gadget);
  let node = roots[indices[0]];
  for (i from 1 below indices.size)
    node := node.node-children[indices[i]];
  end;
  node;
end;

define sealed method note-tree-control-roots-changed
     (pane :: <gtk-tree-control>, #key value = $unsupplied) => ()
  //change root nodes!
  //clear store and gadget-items
  update-list-control-items(pane, sheet-direct-mirror(pane))
end;

define method handle-row-expanded
  (sheet :: <gtk-tree-control>, iter :: <GtkTreeIter>, path :: <GtkTreePath>)
  //duim-debug-message("handling row expansion signal");
  let model = sheet.store-model;
  let path = map(string-to-integer,
                 split(as(<byte-string>, gtk-tree-path-to-string(path)),
                       ':'));
  let node = find-node-list(sheet, path);
  node.gtk-iter := iter;
  let tree = sheet;
  unless (node-state(node))
    with-busy-cursor (tree)
      // If no items have ever been added, do it now
      let children-predicate = tree-control-children-predicate(tree);
      when (children-predicate(node-object(node)))
        let children-generator = tree-control-children-generator(tree);
        let objects = children-generator(node-object(node));
        let nodes = map-as(<simple-vector>,
                           method (object) make-node(tree, object) end, objects);
        do-add-nodes(tree, node, nodes)
      end;
      node-state(node) := #"expanded"
    end
  end;

  with-stack-structure (iter2 :: <GtkTreeIter>)
    //remove the dummy entry
    let res = gtk-tree-model-iter-children(model, iter2, iter);
    with-stack-structure (value :: <GValue>)
      g-value-nullify(value);
      gtk-tree-model-get-value(model, iter2, 0, value);
      if (g-value-to-dylan(value) == -1)
        gtk-tree-store-remove(model, iter2);
      end;
    end;
  end;
  #t;
end;


/// Option boxes

define sealed class <gtk-option-box>
    (<gtk-list-control-mixin>,
     <option-box>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <gtk-option-box>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <option-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-option-box>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-option-box>)
 => (mirror :: <gadget-mirror>)
  let widget = with-gdk-lock gtk-combo-box-new() end;
  assert(~null-pointer?(widget), "gtk-combo-box-new failed");
  make(<gadget-mirror>,
       widget: widget,
       sheet:  gadget)
end method make-gtk-mirror;

define sealed method note-gadget-items-changed
    (gadget :: <gtk-option-box>) => ()
  next-method();
  ignoring("note-gadget-items-changed on <option-box>")
end method note-gadget-items-changed;

define sealed method update-gadget-selection
    (gadget :: <gtk-option-box>) => ()
  ignoring("update-gadget-selection on <option-box>")
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <gtk-option-box>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;


/// Combo boxes

define sealed class <gtk-combo-box>
    (<gtk-list-control-mixin>,
     <combo-box>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <gtk-combo-box>;

define method %gtk-fixed-height? (obj :: <gtk-combo-box>) => (res :: <boolean>)
  #t;
end;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <combo-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-combo-box>, #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-combo-box>)
 => (mirror :: <gadget-mirror>)
  with-gdk-lock
    let widget = gtk-combo-box-text-new-with-entry();
    make(<gadget-mirror>,
         widget: widget,
         sheet:  gadget)
  end
end method make-gtk-mirror;

define method update-mirror-attributes
    (gadget :: <gtk-combo-box>, mirror :: <gadget-mirror>) => ()
  next-method();
  with-gdk-lock
    mirror.mirror-widget.@entry-text-column := 1;
  end
end;

define method install-event-handlers
    (sheet :: <gtk-combo-box>, mirror :: <gadget-mirror>) => ()
  next-method();
  duim-g-signal-connect (sheet, #"changed") (#rest args) handle-changing-selection(sheet) end;
  let gtkentry = with-gdk-lock gtk-bin-get-child(mirror.mirror-widget) end;
  g-signal-connect(gtkentry, "activate", method(#rest args) handle-changed-selection(sheet) end);
end;

define method handle-changed-selection (gadget :: <gtk-combo-box>) => (handled? :: <boolean>)
  let widget = mirror-widget(sheet-direct-mirror(gadget));
  let text = as(<byte-string>, gtk-combo-box-text-get-active-text(widget));
  distribute-text-changed-callback(gadget, text);
  #t
end;

define method handle-changing-selection (gadget :: <gtk-combo-box>) => (handled? :: <boolean>)
  let widget = mirror-widget(sheet-direct-mirror(gadget));
  let row = gtk-combo-box-get-active(widget);
  let text =
    if (row = -1)
      as(<byte-string>, gtk-combo-box-text-get-active-text(widget));
    else
      let gtkentry = gtk-bin-get-child(widget);
      gtk-entry-set-text(gtkentry, gadget-items(gadget)[row]);
      gtk-widget-grab-focus(gtkentry);
      gadget-items(gadget)[row]
    end;
  distribute-text-changing-callback(gadget, text);
  #t
end;

define sealed method note-gadget-items-changed
    (gadget :: <gtk-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-list-control-items(gadget, mirror);
  end
end method note-gadget-items-changed;


/// Viewports

define sealed class <gtk-viewport>
    (<viewport>,
     <gtk-pane-mixin>,
     <permanent-medium-mixin>,
     <single-child-composite-pane>,
     <sealed-constructor-mixin>)
end class <gtk-viewport>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-viewport>, #f)
end method class-for-make-pane;

define method make-gtk-mirror
    (sheet :: <gtk-viewport>)
 => (mirror :: <widget-mirror>)
  with-gdk-lock
   let widget = gtk-viewport-new(gtk-adjustment-new(0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0),
                                 gtk-adjustment-new(0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0));
   let layout = gtk-fixed-new();
   make(<drawing-area-mirror>,
        widget: widget,
        sheet:  sheet,
        layout: layout);
  end
end method;


/// Borders

/*---*** Use the fake border for now...
define sealed class <gtk-border>
    (<standard-repainting-mixin>,
     <border>,
     <basic-sheet>,
     <sealed-constructor-mixin>)
  sealed slot %pen   :: false-or(<standard-pen>) = #f;
  sealed slot %brush :: false-or(type-union(<standard-brush>, <ink>)) = #f;
end class <gtk-border>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <border>, #key label)
 => (class :: <class>, options :: false-or(<sequence>))
  let border-class = if (label) <gtk-group-box> else <gtk-border> end;
  values(border-class, #f)
end method class-for-make-pane;

define constant $gadget-border-thickness :: <integer> = 2;

define sealed method do-compose-space
    (pane :: <gtk-border>, #key width, height)
 => (space-req :: <space-requirement>)
  let thickness*2 = $gadget-border-thickness * 2;
  space-requirement+(pane,
                     next-method(pane,
                                 width:  width  & width  - thickness*2,
                                 height: height & height - thickness*2),
                     width: thickness*2, height: thickness*2)
end method do-compose-space;

define sealed method do-allocate-space
    (pane :: <gtk-border>, width :: <integer>, height :: <integer>) => ()
  let child = sheet-child(pane);
  let thickness = $gadget-border-thickness;
  when (child)
    set-sheet-edges(child,
                    thickness, thickness,
                    width - thickness, height - thickness)
  end
end method do-allocate-space;

define sealed method handle-repaint
    (pane :: <gtk-border>, medium :: <gtk-medium>, region :: <region>) => ()
  ignore(region);        // not worth checking
  let (left, top, right, bottom) = box-edges(pane);
  draw-border(pane, medium, border-type(pane), left, top, right, bottom)
end method handle-repaint;

//---*** DO THE RIGHT THING
define sealed method draw-border
    (pane :: <sheet>, medium :: <gtk-medium>, type :: <border-type>,
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
*/


/// Sliders

define sealed class <gtk-slider>
    (<gtk-gadget-mixin>,
     <slider>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <gtk-slider>;

define sealed class <gtk-horizontal-slider> (<gtk-slider>)
  keyword gtk-fixed-height?: = #t;
end class <gtk-horizontal-slider>;

define method %gtk-fixed-height?
    (gadget :: <gtk-horizontal-slider>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed class <gtk-vertical-slider> (<gtk-slider>)
  keyword gtk-fixed-width?: = #t;
end class <gtk-vertical-slider>;

define method %gtk-fixed-width?
    (gadget :: <gtk-vertical-slider>)
 => (fixed? :: <boolean>)
  #t;
end method;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <slider>,
     #key orientation = #"horizontal")
 => (class :: <class>, options :: false-or(<sequence>))
  values(select (orientation)
           #"horizontal" => <gtk-horizontal-slider>;
           #"vertical"   => <gtk-vertical-slider>;
         end,
         #f)
end method class-for-make-pane;

define sealed method make-gtk-mirror
    (gadget :: <gtk-horizontal-slider>)
 => (mirror :: <gadget-mirror>)
  let widget = with-gdk-lock gtk-hscale-new(null-pointer(<GtkAdjustment>)) end;
  assert(~null-pointer?(widget), "gtk-hscale-new failed");
  make(<gadget-mirror>,
       widget: widget,
       sheet:  gadget)
end method make-gtk-mirror;

define sealed method make-gtk-mirror
    (gadget :: <gtk-vertical-slider>)
 => (mirror :: <gadget-mirror>)
  let widget = with-gdk-lock gtk-vscale-new(null-pointer(<GtkAdjustment>)) end;
  assert(~null-pointer?(widget), "gtk-vscale-new failed");
  make(<gadget-mirror>,
       widget: widget,
       sheet:  gadget)
end method make-gtk-mirror;

define sealed method note-gadget-value-changed
    (gadget :: <gtk-slider>) => ()
  next-method();
  ignoring("note-gadget-value-changed on <slider>")
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <gtk-slider>) => ()
  next-method();
  ignoring("note-gadget-value-range-changed on <slider>")
end method note-gadget-value-range-changed;


/// Tool bars

//---*** Someday we should do these for real!

define sealed class <gtk-tool-bar>
    (<tool-bar>, <single-child-wrapping-pane>, <sealed-constructor-mixin>)
  //--- The way we do this separator stuff is just loathsome...
  slot tool-bar-decoration :: <sheet>;
  slot %separator :: false-or(<separator>) = #f;
end class <gtk-tool-bar>;

define method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <tool-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-tool-bar>, #f)
end method class-for-make-pane;

define method initialize
    (gadget :: <gtk-tool-bar>, #key frame-manager: framem) => ()
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
define method note-sheet-mapped (gadget :: <gtk-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #t
  end
end method note-sheet-mapped;

// Ditto, for unmapping
define method note-sheet-unmapped (gadget :: <gtk-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #f
  end
end method note-sheet-unmapped;


/// Status bars


/*---*** No status bar for now...
define sealed class <gtk-status-bar>
    (<gtk-gadget-mixin>,
     <status-bar>,
     <row-layout>,
     <sealed-constructor-mixin>)
  slot status-bar-simple? :: <boolean> = #f,
    setter: %simple?-setter;
  slot status-bar-simple-text :: <string> = "";
  keyword border:      = $status-bar-border;
  keyword spacing:     = $status-bar-spacing;
  keyword y-alignment: = #"center";
end class <gtk-status-bar>;

define sealed method class-for-make-pane
    (framem :: <gtk-frame-manager>, class == <status-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<gtk-status-bar>, #f)
end method class-for-make-pane;

define sealed method make-gadget-control
    (gadget :: <gtk-status-bar>,
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
    (gadget :: <gtk-status-bar>, #key width, height)
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
    (gadget :: <gtk-status-bar>, width :: <integer>, height :: <integer>) => ()
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
      duim-debug-message("  part %= has right edge at %=", i, right);
      pointer-value(widths, index: i) := right
    end;
    // Allocate a little extra space so the size grip doesn't
    // overlap the final part
    pointer-value(widths, index: n-parts - 1) := final-x + extra-width;
    inc!(min-height, $status-bar-border * 2);
    duim-debug-message("  fixed height %=", min-height);
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

define class <status-label-mirror> (<gtk-mirror>)
  sealed slot status-label-status-bar :: <status-bar>,
    required-init-keyword: status-bar:;
  sealed slot status-label-part-number :: <integer>,
    required-init-keyword: part-number:;
end class <status-label-mirror>;

define sealed method make-gadget-mirror
    (status-bar :: <gtk-status-bar>, gadget :: <gtk-label>)
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
    (gadget :: <gtk-label>, mirror :: <status-label-mirror>) => ()
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
    (simple? :: <boolean>, gadget :: <gtk-status-bar>)
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
    (label :: <string>, gadget :: <gtk-status-bar>)
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
    (gadget :: <gtk-status-bar>) => ()
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
*/
