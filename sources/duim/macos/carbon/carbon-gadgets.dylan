Module:       carbon-duim
Synopsis:     Macintosh gadget implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $default-label = "";

// From the Macintosh Human Interface Guidelines
define constant $default-control-height = 20;
define constant $button-box-x-spacing   = 12;
define constant $button-box-y-spacing   = 10;


/// Macintosh controls

define open abstract class <carbon-gadget-mixin>
    (<gadget>,
     <carbon-pane-mixin>)
  sealed constant each-subclass slot %carbon-fixed-width? :: <boolean> = #f,
    init-keyword: carbon-fixed-width?:;
  sealed constant each-subclass slot %carbon-fixed-height? :: <boolean> = #f,
    init-keyword: carbon-fixed-height?:;
end class <carbon-gadget-mixin>;

define class <control-mirror> (<carbon-mirror>)
  sealed slot mirror-control :: <ControlHandle>,
     required-init-keyword: control:;
end class <control-mirror>;

define sealed method make-control-mirror
    (gadget :: <carbon-gadget-mixin>, procID :: <integer>,
     #key title :: false-or(<string>) = #f,
          value :: <integer>,
          min-value :: <integer>,
          max-value :: <integer>)
 => (mirror :: <control-mirror>)
  let parent = sheet-device-parent(gadget);
  with-stack-structure (Rect :: <Rect*>)
    with-pascal-string (title = title | $default-label)
      let (left, top, right, bottom) = sheet-native-edges(gadget);
      SetRect(Rect, left, top, right, bottom);
      let user-data = 0;	//--- Should we use this?
      let control
	= ensure-no-error
	    ("NewControl",
	     NewControl
	       (parent,
		Rect,
		title,
		$false,
		value, min-value, max-value,
		procID,
		user-data));
      make(<control-mirror>,
	   control: control,
	   sheet:   gadget)
    end
  end
end method make-control-mirror;

define sealed method map-mirror
    (_port :: <carbon-port>, gadget :: <carbon-gadget-mixin>, mirror :: <carbon-mirror>) => ()
  ShowControl(mirror.mirror-control)
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <carbon-port>, gadget :: <carbon-gadget-mixin>, mirror :: <carbon-mirror>) => ()
  HideControl(mirror.mirror-control)
end method unmap-mirror;

define sealed method destroy-mirror
    (_port :: <carbon-port>, gadget :: <carbon-gadget-mixin>, mirror :: <control-mirror>) => ()
  DisposeControl(mirror.mirror-control)
end method destroy-mirror;

define method gadget-control
    (gadget :: <carbon-gadget-mixin>) => (control /* :: <what?> */)
  let mirror = sheet-direct-mirror(gadget);
  mirror & mirror.mirror-control
end method gadget-control;

/*---*** Not used yet!
define method control-attributes
    (_port :: <carbon-port>, gadget :: <carbon-gadget-mixin>)
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
end method control-attributes;
*/

define method do-compose-space
    (gadget :: <carbon-gadget-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let mirror = sheet-direct-mirror(gadget);
  if (mirror)
    let control = mirror-control(mirror);
    with-stack-structure (BaseLineOffset :: <SInt16*>)
      with-stack-structure (Rect :: <Rect*>)
	SetRect(Rect, 0, 0, 0, 0);
	ensure-no-error
	  ("GetBestControlRect",
	   GetBestControlRect
	     (control,
	      Rect,
	      BaseLineOffset));
	//---*** It would be *really* nice to handle base lines too...
	ignore(BaseLineOffset);
	let width  = Rect.right-value  - Rect.left-value;
	let height = Rect.bottom-value - Rect.top-value;
	let max-width  = if (gadget.%carbon-fixed-width?)  width  else $fill end;
	let max-height = if (gadget.%carbon-fixed-height?) height else $fill end;
	make(<space-requirement>,
	     min-width:  width,  width:  width,  max-width:  max-width,
	     min-height: height, height: height, max-height: max-height)
      end
    end
  else
    carbon-debug("Composing space on an unmirrored gadget!");
    default-space-requirement(gadget, width: width, height: height)
  end
end method do-compose-space;


define sealed method defaulted-gadget-label
    (gadget :: <gadget>) => (label)
  gadget-label(gadget) | $default-label
end method defaulted-gadget-label;

define sealed method note-gadget-label-changed
    (gadget :: <carbon-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-label(gadget, mirror)
end method note-gadget-label-changed;

define sealed method update-mirror-label
    (gadget :: <carbon-gadget-mixin>, mirror :: <control-mirror>) => ()
  let control = mirror-control(sheet-direct-mirror(gadget));
  let label  = defaulted-gadget-label(gadget);
  let label :: <string> = if (instance?(label, <string>)) label else "" end;
  // xt/XtSetValues(control, label-string: label)
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
    <carbon-bitmap>, <carbon-icon> =>
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
    (client, gadget :: <carbon-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let control = gadget-control(gadget);
  ensure-no-error
    ("ActivateControl",
     ActivateControl(control))
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <carbon-gadget-mixin>) => ()
  ignore(client);
  next-method();
  let control = gadget-control(gadget);
  ensure-no-error
    ("DeactivateControl",
     DeactivateControl(control))
end method note-gadget-disabled;

//---*** DO WE NEED THIS?
define sealed method activate-carbon-gadget
    (gadget :: <action-gadget>) => (activated? :: <boolean>)
  when (gadget-activate-callback(gadget))
    distribute-activate-callback(gadget);
    #t
  end
end method activate-carbon-gadget;

//---*** DO WE NEED THIS?
define sealed method activate-carbon-gadget
    (gadget :: <text-field>) => (activated? :: <boolean>)
  handle-text-gadget-changed(gadget);
  next-method()
end method activate-carbon-gadget;


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
  let gadget = carbon-sheet-with-focus();
  duim-debug-message("  Handling IDOK: focus currently %=", gadget);
  let activated? = instance?(gadget, <action-gadget>)
		   & gadget-enabled?(gadget)
		   & activate-carbon-gadget(gadget);
  // If we didn't activate the gadget, try to activate the default button
  unless (activated?)
    let button = frame-default-button(frame);
    // Don't activate an upmapped or disabled default button...
    when (button & sheet-mapped?(button) & gadget-enabled?(button))
      handle-gadget-activation(button)
    end
  end
end method activate-default-button;

define function carbon-sheet-with-focus
    () => (sheet :: false-or(<sheet>))
  let handle = GetFocus();
  let mirror = gadget-mirror(handle);
  when (mirror)
    let sheet = mirror-sheet(mirror);
    if (instance?(sheet, <carbon-subgadget-mixin>))
      subgadget-owner(sheet)
    else
      sheet
    end
  end
end function carbon-sheet-with-focus;


define function handle-cancel
    (frame :: <frame>) => (handled? :: <boolean>)
  let gadget = carbon-sheet-with-focus();
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

/*---*** Not yet implemented
define sealed class <carbon-label> 
    (<carbon-gadget-mixin>,
     <label>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-fixed-width?:  = #t;
  keyword carbon-fixed-height?: = #t;
end class <carbon-label>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-label>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-label>)
 => (mirror :: <control-mirror>)
  not-yet-implemented("make-carbon-mirror");
  /*---*** Not yet implemented
  with-c-string (c-string = defaulted-gadget-label(gadget))
    let control = carbon-label-new(c-string);
    assert(~null-pointer?(control), "carbon-label-new failed");
    make(<control-mirror>,
	 control: control,
	 sheet:  gadget)
  end
  */
end method make-carbon-mirror;

define sealed method update-mirror-label
    (gadget :: <carbon-label>, mirror :: <control-mirror>) => ()
  with-c-string (c-string = defaulted-gadget-label(gadget))
    let control = mirror-control(mirror);
    carbon-label-set-text(control, c-string)
  end
end method update-mirror-label;
*/


/// Separators
/*---*** Use the default separators
define sealed class <carbon-separator>
    (<carbon-gadget-mixin>,
     <separator>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-separator>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <separator>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-separator>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-separator>)
 => (mirror :: <control-mirror>)
  let parent = sheet-device-parent(gadget);
  let parent-control = gadget-control(parent);
  let (foreground, background, font) = control-attributes(_port, gadget);
  ignore(font);
  let resources
    = vector(mapped-when-managed:, #f);
  let control
    = xt/XtCreateManagedControl("DUIMSeparator", xm/<XmSeparatorGadget>, parent-control,
			       resources:
				 concatenate(resources, foreground, background));
  values(control, #f)
end method make-carbon-mirror;

define sealed method do-compose-space
    (pane :: <carbon-separator>, #key width, height)
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
*/


/// Buttons

define class <carbon-button-mixin> (<carbon-gadget-mixin>, <button>)
end class <carbon-button-mixin>;

define sealed method button-box-spacing
    (framem :: <carbon-frame-manager>, box :: <button-box>)
 => (spacing :: <integer>)
  select (gadget-orientation(box))
    #"horizontal" => $button-box-x-spacing;
    #"vertical"   => $button-box-y-spacing;
  end
end method button-box-spacing;


/// Push buttons

define sealed class <carbon-push-button>
    (<carbon-button-mixin>,
     <push-button>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-fixed-width?:  = #t;
  keyword carbon-fixed-height?: = #t;
end class <carbon-push-button>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-push-button>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-push-button>)
 => (mirror :: <control-mirror>)
  let (text, image, mnemonic, index) 
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("image label")
  end;
  make-control-mirror
    (gadget, $kControlPushButtonProc, 
     title: text,
     value: 0, min-value: 0, max-value: 1)
end method make-carbon-mirror;

define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <carbon-push-button>)
 => (default? :: <boolean>)
  next-method();
  ignoring("gadget-default?-setter");
  default?
end method gadget-default?-setter;


/// Radio buttons

define sealed class <carbon-radio-button>
    (<carbon-button-mixin>,
     <radio-button>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-fixed-width?:  = #t;
  keyword carbon-fixed-height?: = #t;
end class <carbon-radio-button>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-radio-button>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-radio-button>)
 => (mirror :: <control-mirror>)
  let (text, image, mnemonic, index) 
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("image label")
  end;
  make-control-mirror
    (gadget, $kControlRadioButtonProc, 
     title: text,
     value: $kControlRadioButtonUncheckedValue,
     min-value: $kControlRadioButtonUncheckedValue,
     max-value: $kControlRadioButtonCheckedValue)
end method make-carbon-mirror;

define method update-mirror-attributes
    (gadget :: <carbon-radio-button>, mirror :: <control-mirror>) => ()
  next-method();
  ignoring("update-mirror-attributes")
end method update-mirror-attributes;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-radio-button>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-attributes(gadget, mirror)
end method note-gadget-value-changed;


/// Check buttons

define sealed class <carbon-check-button>
    (<carbon-button-mixin>,
     <check-button>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-fixed-width?:  = #t;
  keyword carbon-fixed-height?: = #t;
end class <carbon-check-button>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-check-button>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-check-button>)
 => (mirror :: <control-mirror>)
  let (text, image, mnemonic, index) 
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("image label")
  end;
  make-control-mirror
    (gadget, $kControlCheckBoxProc, 
     title: text,
     value: $kControlCheckBoxUncheckedValue,
     min-value: $kControlCheckBoxUncheckedValue,
     max-value: $kControlCheckBoxCheckedValue)
end method make-carbon-mirror;

define method update-mirror-attributes
    (gadget :: <carbon-check-button>, mirror :: <control-mirror>) => ()
  next-method();
  ignoring("update-mirror-attributes")
end method update-mirror-attributes;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-check-button>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-mirror-attributes(gadget, mirror)
end method note-gadget-value-changed;


/// Text gadgets

/*---*** Not yet implemented
// Mixin class for text fields, password fields, and text editors, i.e.
// all MacEditable objects.
define open abstract class <carbon-text-gadget-mixin>
    (<carbon-gadget-mixin>,
     <text-field>)
  sealed slot %changed? :: <boolean> = #f;
  sealed constant slot %current-selection :: <simple-text-range>
    = make(<text-range>, start: -1, end: -1);
end class <carbon-text-gadget-mixin>;

define sealed method update-mirror-attributes
    (gadget :: <carbon-text-gadget-mixin>, mirror :: <control-mirror>) => ()
  next-method();
  // Set the initial text selection
  text-selection(gadget) := gadget.%current-selection
end method update-mirror-attributes;

// This is called right after gadget buffer text changes in DUIM
define sealed method note-gadget-text-changed 
    (gadget :: <carbon-text-gadget-mixin>) => ()
  carbon-debug("note-gadget-text-changed");
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

define sealed method handle-text-gadget-changing
    (gadget :: <carbon-text-gadget-mixin>) => ()
  ignoring("handle-text-gadget-changing")
end method handle-text-gadget-changing;

define sealed method handle-text-gadget-changed
    (gadget :: <carbon-text-gadget-mixin>) => ()
  ignoring("handle-text-gadget-changing")
end method handle-text-gadget-changed;

define sealed method text-selection
    (gadget :: <carbon-text-gadget-mixin>) => (range :: false-or(<text-range>))
  let control = CARBON-EDITABLE(gadget-control(gadget));
  let start-pos = control.selection-start-pos-value;
  let end-pos = control.selection-end-pos-value;
  when (start-pos < end-pos)
    make(<text-range>, start: start-pos, end: end-pos)
  end;
end method text-selection;

define sealed method selected-text
    (gadget :: <carbon-text-gadget-mixin>) => (string :: false-or(<string>))
  let control = CARBON-EDITABLE(gadget-control(gadget));
  let start-pos = control.selection-start-pos-value;
  let end-pos = control.selection-end-pos-value;
  if (start-pos >= end-pos)
    #f
  elseif (start-pos = 0 & end-pos = gadget.gadget-text-buffer.size)
    gadget.gadget-text-buffer
  else
    let chars = CARBON-STRING(carbon-editable-get-chars(control, start-pos, end-pos));
    let string = carbon-copy-text(chars);
    g-free(chars);
    string
  end;
end method selected-text;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)),
     gadget :: <carbon-text-gadget-mixin>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  ignoring("text-selection-setter");
  range
end method text-selection-setter;

define sealed method text-caret-position
    (gadget :: <carbon-text-gadget-mixin>)
 => (position :: <integer>)
  ignoring("text-caret-position");
  0
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <carbon-text-gadget-mixin>)
 => (position :: false-or(<integer>))
  if (position)
    ignoring("text-caret-position-setter");
  end;
  position
end method text-caret-position-setter;
*/


/// Text and password fields

/// Text fields

/*---*** Not yet implemented
define abstract class <carbon-text-field-mixin>
    (<carbon-text-gadget-mixin>,
     <text-field>)
  sealed constant each-subclass slot %carbon-text-visibility,
    required-init-keyword: carbon-text-visibility:;
  keyword carbon-fixed-height?: = #t;
end class <carbon-text-field-mixin>;

define sealed method make-carbon-mirror
    (gadget :: <carbon-text-field-mixin>)
 => (mirror :: <control-mirror>)
  let max = text-field-maximum-size(gadget);
  let text = gadget-text-buffer(gadget);
  let visibility = %carbon-text-visibility(gadget);
  let control = if (max)
		 carbon-entry-new-with-max-length(max)
	       else
		 carbon-entry-new();
	       end;
  assert(~null-pointer?(control), "carbon-entry-new failed");
  // Note that this is happening before install-event-handlers, so don't
  // need to disable events.
  carbon-entry-set-visibility(control, visibility);
  unless (empty?(text))
    with-c-string (c-text = text)
      carbon-entry-set-text(control, c-text);
    end;
  end;
  make(<control-mirror>,
       control: control,
       sheet:  gadget)
end method make-carbon-mirror;

// Updates the MAC text field from the DUIM gadget
define sealed method update-gadget-text
    (gadget :: <carbon-text-field-mixin>, mirror :: <control-mirror>) => ()
  ignore(mirror);
  let control = gadget-control(gadget);
  let new-text = gadget-text-buffer(gadget);
  with-disabled-event-handler (control, #"changed")
    with-c-string (c-text = new-text)
      carbon-entry-set-text(control, c-text);
    end;
  end;
end method update-gadget-text;

/// Text fields

define sealed class <carbon-text-field>
    (<carbon-text-field-mixin>,
     <text-field>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-text-visibility: = $true;
end class <carbon-text-field>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <text-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-text-field>, #f)
end method class-for-make-pane;

/// Password fields

define sealed class <carbon-password-field>
    (<carbon-text-field-mixin>,
     <password-field>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  keyword carbon-text-visibility: = $false;
end class <carbon-password-field>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <password-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-password-field>, #f)
end method class-for-make-pane;
*/


/// Text editors

/*---*** Not yet implemented
define sealed class <carbon-text-editor>
    (<carbon-text-gadget-mixin>,
     <text-editor>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-text-editor>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <text-editor>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-text-editor>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-text-editor>)
 => (mirror :: <control-mirror>)
  /*---*** Not yet implemented
  let lines = gadget-lines(gadget);
  let columns = gadget-columns(gadget);
  let word-wrap? = text-field-word-wrap?(gadget);
  let text = gadget-text-buffer(gadget);
  let control = carbon-text-new(null-pointer(<MacAdjustment*>),
			    null-pointer(<MacAdjustment*>));
  assert(~null-pointer?(control), "carbon-text-new failed");
  // Note that this is happening before install-event-handlers, so don't
  // need to disable events.
  when (lines | columns)
    ignoring("lines:/columns:")
  end;
  carbon-text-set-word-wrap(control, if (word-wrap?) $true else $false end);
  set-text-control-text(control, text);
  make(<control-mirror>,
       control: control,
       sheet:  gadget)
  */
end method make-carbon-mirror;

define sealed method update-gadget-text
    (gadget :: <carbon-text-editor>, mirror :: <control-mirror>) => ()
  ignore(mirror);
  let control = gadget-control(gadget);
  when (control)
    ignoring("update-gadget-text")
  end;
end method update-gadget-text;

define method set-text-control-text (control, text :: <string>)
  with-c-string (c-text = text)
    with-stack-structure (position :: <c-int*>)
      carbon-editable-delete-text(control, 0, -1);
      pointer-value(position) := 0;
      carbon-editable-insert-text(control, c-text, text.size, position);
    end;
  end;
end set-text-control-text;
*/


/// Scroll bars

define sealed class <carbon-scroll-bar>
    (<carbon-gadget-mixin>,
     <scroll-bar>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-scroll-bar>;

define sealed class <carbon-horizontal-scroll-bar> (<carbon-scroll-bar>)
  keyword carbon-fixed-height?: = #t;
end class <carbon-horizontal-scroll-bar>;

define sealed class <carbon-vertical-scroll-bar> (<carbon-scroll-bar>)
  keyword carbon-fixed-width?: = #t;
end class <carbon-vertical-scroll-bar>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <scroll-bar>, 
     #key orientation = #"horizontal")
 => (class :: <class>, options :: false-or(<sequence>))
  values(select (orientation)
	   #"horizontal" => <carbon-horizontal-scroll-bar>;
	   #"vertical"   => <carbon-vertical-scroll-bar>;
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
end function gadget-range-values;

// define method carbon-gadget-range 

define method range-gadget-adjusted-contents
    (gadget :: <carbon-scroll-bar>)
 => (value :: <integer>, min-value :: <integer>, max-value :: <integer>)
  let progress-range = gadget-value-range(gadget);
  let first-value    = progress-range[0];
  let range-increment
    = if (size(progress-range) <= 1) 1 else progress-range[1] - first-value end;
  let pos = floor/(value - first-value, range-increment);
  let min-value = 0;
  let max-value = max(min-value, size(progress-range) - 1);
  values(pos, min-value, max-value)
end method range-gadget-adjusted-contents;

define sealed method make-carbon-mirror
    (gadget :: <carbon-scroll-bar>)
 => (mirror :: <control-mirror>)
  let (value, min-value, max-value)
    = range-gadget-adjusted-contents(gadget);
  make-control-mirror
    (gadget, $kControlProgressBarProc, 
     value: value,
     min-value: min-value,
     max-value: max-value)
end method make-carbon-mirror;

define sealed method update-mirror-attributes
    (gadget :: <carbon-scroll-bar>, mirror :: <control-mirror>) => ()
  next-method();
  ignoring("update-mirror-attributes on <scroll-bar>")
end method update-mirror-attributes;

define sealed method note-gadget-slug-size-changed
    (gadget :: <carbon-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-slug-size-changed;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <carbon-scroll-bar>) => ()
  next-method();
  note-scroll-bar-changed(gadget);
end method note-gadget-value-range-changed;

define sealed method note-scroll-bar-changed
    (gadget :: <carbon-scroll-bar>) => ()
  let control = gadget-control(gadget);
  when (control)
    ignoring("note-scroll-bar-changed")
  end
end method note-scroll-bar-changed;


/// List gadgets

/*---*** Not yet implemented
define sealed class <carbon-list-control-mixin> 
    (<carbon-gadget-mixin>,
     <collection-gadget>,
     <sealed-constructor-mixin>)
end class <carbon-list-control-mixin>;

define method update-mirror-attributes
    (gadget :: <carbon-list-control-mixin>, mirror :: <control-mirror>) => ()
  next-method();
  let control = mirror.mirror-control;
  carbon-clist-set-selection-mode
    (control,
     select (gadget-selection-mode(gadget))
       #"none"     => $CARBON-SELECTION-BROWSE;
       #"single"   => $CARBON-SELECTION-SINGLE;
       #"multiple" => $CARBON-SELECTION-EXTENDED;
     end);
  carbon-clist-set-shadow-type(control, $CARBON-SHADOW-IN);
  if (instance?(gadget, <table-control>))
    carbon-clist-column-titles-show(control)
  else
    carbon-clist-column-titles-hide(control);
    //---*** How should we decide this?
    carbon-clist-set-column-width(control, 0, 500)
  end;
  update-list-control-items(gadget, mirror)
end method update-mirror-attributes;

/*
define sealed method handle-carbon-select-row-event
    (gadget :: <carbon-list-control-mixin>, control :: <MacControl*>,
     event :: <GdkEventAny*>)
 => (handled? :: <boolean>)
  carbon-debug("Clicked on list control!");
  let selection = list-selection(gadget, sheet-direct-mirror(gadget));
  carbon-debug("  Selection now %=", selection);
  distribute-selection-changed-callback(gadget, selection);
  #t
end method handle-carbon-select-row-event;

define sealed method handle-carbon-button-press-event
    (gadget :: <carbon-list-control-mixin>, control :: <MacControl*>,
     event :: <GdkEventButton*>)
 => (handled? :: <boolean>)
  carbon-debug("Pressed button %=, type %=",
		event.button-value,
		select (event.type-value)
		  $GDK-BUTTON-PRESS  => "button press";
		  $GDK-2BUTTON-PRESS => "double click";
		  $GDK-3BUTTON-PRESS => "treble click";
		  otherwise => event.type-value;
		end);
  if (event.type-value == $GDK-2BUTTON-PRESS)
    carbon-debug("Double clicked on list control!");
    when (gadget-activate-callback(gadget))
      distribute-activate-callback(gadget);
    end;
    #t
  end
end method handle-carbon-button-press-event;

define method list-selection
    (gadget :: <carbon-list-control-mixin>, mirror :: <control-mirror>)
 => (vector :: <vector>)
  let control = pointer-cast(<MACCList*>, mirror.mirror-control);
  let selection = control.selection-value;
  glist-to-vector(selection, <integer>)
end method list-selection;

define method glist-to-vector
    (GList :: <GList*>, type :: <type>)
 => (vector :: <stretchy-object-vector>)
  let vector = make(<stretchy-object-vector>);
  local method process-list
	    (GList :: <GList*>)
	  case
	    null-pointer?(GList) =>
	      #f;
	    otherwise =>
	      add!(vector, c-type-cast(type, glist.data-value));
	      process-list(glist.next-value);
	  end
	end;
  process-list(GList);
  vector
end method glist-to-vector;
*/

define sealed method note-gadget-items-changed
    (gadget :: <carbon-list-control-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-list-control-items(gadget, mirror)
end method note-gadget-items-changed;

define method update-gadget
    (gadget :: <carbon-list-control-mixin>) => ()
  // No, we don't call 'next-method' here!
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-list-control-items(gadget, mirror)
end method update-gadget;

define sealed method update-list-control-items
    (gadget :: <carbon-list-control-mixin>, mirror :: <control-mirror>)
 => ()
  ignoring("update-list-control-items")
end method update-list-control-items;

define sealed method update-gadget-selection
    (gadget :: <carbon-list-control-mixin>) => ()
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
    (gadget :: <carbon-list-control-mixin>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;

define sealed method handle-selection-changed
    (gadget :: <carbon-list-control-mixin>) => (handled? :: <boolean>)
  select (gadget-selection-mode(gadget))
    #"none" =>
      #f;
    #"single" =>
      ignoring("handle-selection-changed for <carbon-list-control-mixin>");
    #"multiple" =>
      ignoring("handle-selection-changed for <carbon-list-control-mixin>");
  end;
  #t
end method handle-selection-changed;
*/

// List boxes

/*---*** Not yet implemented
define sealed class <carbon-list-box> 
    (<carbon-list-control-mixin>,
     <list-box>,
     <leaf-pane>)
end class <carbon-list-box>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-list-box>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-list-box>)
 => (mirror :: <control-mirror>)
  let control = carbon-clist-new(1);
  assert(~null-pointer?(control), "carbon-clist-new failed");
  make(<control-mirror>,
       control: control,
       sheet:  gadget)
end method make-carbon-mirror;
*/

// List controls

//---*** Need to implement add-item etc...
/*---*** Not yet implemented!
define sealed class <carbon-list-control> 
    (<carbon-list-control-mixin>,
     <list-control>,
     <leaf-pane>)
end class <carbon-list-control>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <list-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-list-control>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-list-control>)
 => (mirror :: <control-mirror>)
  let control = carbon-clist-new(1);
  assert(~null-pointer?(control), "carbon-clist-new failed");
  make(<control-mirror>,
       control: control,
       sheet:  gadget)
end method make-carbon-mirror;
*/

// Table controls

/*---*** Use the fake ones for now...
define sealed class <carbon-table-control> 
    (<carbon-list-control-mixin>,
     <table-control>,
     <leaf-pane>)
end class <carbon-table-control>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <table-control>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-table-control>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-table-control>)
 => (mirror :: <control-mirror>)
  let columns = table-control-columns(gadget);
  let control = carbon-clist-new(columns.size);
  assert(~null-pointer?(control), "carbon-clist-new failed");
  make(<control-mirror>,
       control: control,
       sheet:  gadget)
end method make-carbon-mirror;

define method update-mirror-attributes
    (gadget :: <carbon-table-control>, mirror :: <control-mirror>) => ()
  next-method();
  let control = mirror.mirror-control;
  carbon-clist-column-titles-active(control);
  for (i :: <integer> from 0,
       column :: <table-column> in table-control-columns(gadget))
    let heading   = table-column-heading(column);
    let width     = table-column-width(column);
    let alignment = table-column-alignment(column);
    with-c-string (c-string = heading)
      carbon-clist-set-column-title(control, i, c-string)
    end;
    carbon-clist-set-column-width(control, i, width);
    carbon-clist-set-column-justification
      (control, i,
       select (alignment)
	 #"left"      => $CARBON-JUSTIFY-LEFT;
	 #"right"     => $CARBON-JUSTIFY-RIGHT;
	 #"center"    => $CARBON-JUSTIFY-CENTER;
       end)
  end
end method update-mirror-attributes;

define sealed method update-list-control-items
    (gadget :: <carbon-table-control>, mirror :: <control-mirror>)
 => ()
  ignoring("update-list-control-items");
  /*
  let control = mirror.mirror-control;
  let items = gadget-items(gadget);
  let label-function = gadget-label-key(gadget);
  let columns = table-control-columns(gadget);
  let no-of-columns = columns.size;
  carbon-clist-clear(control);
  for (item in items)
    let label = label-function(item);
    let object = item-object(item);
    let string* = make(<C-string*>, element-count: no-of-columns);
    for (index :: <integer> from 0 below no-of-columns,
	 column :: <table-column> in columns)
      let generator = table-column-generator(column);
      let label  = label-function(generator(object));
      string*[index] := as(<C-string>, label)
    end;
    block ()
      carbon-clist-append(control, string*);
    cleanup
      map(destroy, string*)
    end
  end
  */
end method update-list-control-items;
*/


/// Option boxes

/*---*** Not yet implemented
define sealed class <carbon-option-box> 
    (<carbon-list-control-mixin>,
     <option-box>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-option-box>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <option-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-option-box>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-option-box>)
 => (mirror :: <control-mirror>)
  let control = carbon-clist-new(1);
  assert(~null-pointer?(control), "carbon-clist-new failed");
  make(<control-mirror>,
       control: control,
       sheet:  gadget)
end method make-carbon-mirror;

define sealed method note-gadget-items-changed
    (gadget :: <carbon-option-box>) => ()
  next-method();
  ignoring("note-gadget-items-changed on <option-box>")
end method note-gadget-items-changed;

define sealed method update-gadget-selection
    (gadget :: <carbon-option-box>) => ()
  ignoring("update-gadget-selection on <option-box>")
end method update-gadget-selection;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-option-box>) => ()
  next-method();
  update-gadget-selection(gadget)
end method note-gadget-value-changed;
*/


/// Combo boxes

/*---*** Not yet implemented
define sealed class <carbon-combo-box> 
    (<carbon-gadget-mixin>,
     <combo-box>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
  sealed slot %changed? :: <boolean> = #f;
end class <carbon-combo-box>;

//--- If <carbon-combo-box> was a <text-field>, we would not need this
define sealed method activate-carbon-gadget
    (gadget :: <combo-box>) => (activated? :: <boolean>)
  handle-text-gadget-changed(gadget);
  next-method()
end method activate-carbon-gadget;

define sealed class <carbon-combo-box-text-field>
    (<carbon-subgadget-mixin>,
     <carbon-text-field>)
end class <carbon-combo-box-text-field>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <combo-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-combo-box>, #f)
end method class-for-make-pane;

define sealed method make-gadget-control
    (gadget :: <carbon-combo-box>, parent :: <HWND>, options :: <options-type>,
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

define sealed method update-mirror-attributes
    (gadget :: <carbon-combo-box>, mirror :: <control-mirror>) => ()
  next-method();
  note-gadget-items-changed(gadget)
end method update-mirror-attributes;

// This is a bizarre hack to subclass the text field which is
// a child of the combo box.
define function subclass-combo-box-text-field
    (gadget :: <carbon-combo-box>, handle :: <HWND>) => ()
  let edit-control = GetWindow(handle, $GW-CHILD);
  check-result("Finding the combo box's edit control", edit-control);
  // This is odd, but making this gadget actually does all the work
  // to mirror and attach everything correctly.
  make(<carbon-combo-box-text-field>,
       owner: gadget, handle: edit-control);
end function subclass-combo-box-text-field;

define sealed method do-compose-space 
    (gadget :: <carbon-combo-box>, #key width, height)
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

define sealed method carbon-combo-box-height
    (gadget :: <carbon-combo-box>) => (height :: <integer>)
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
end method carbon-combo-box-height;

define sealed method note-gadget-items-changed
    (gadget :: <carbon-combo-box>) => ()
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
    (gadget :: <carbon-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-text-changed;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-combo-box>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & update-gadget-text(gadget, mirror)
end method note-gadget-value-changed;

define sealed method handle-selection-changed
    (gadget :: <carbon-combo-box>) => (handled? :: <boolean>)
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
    (gadget :: <carbon-combo-box>, mirror :: <control-mirror>,
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
    (text-field :: <carbon-combo-box-text-field>, message :: <message-type>,
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
    (gadget :: <carbon-combo-box>) => (handled? :: <boolean>)
  let handle = window-handle(gadget);
  when (handle & (SendMessage(handle, $CB-GETDROPPEDSTATE, 0, 0) = $true))
    SendMessage(handle, $CB-SHOWDROPDOWN, $false, 0);
    #t
  end
end method cancel-gadget;
*/


/// Viewports

define sealed class <carbon-viewport>
    (<viewport>,
     <carbon-pane-mixin>,
     <permanent-medium-mixin>,
     <single-child-composite-pane>,
     <sealed-constructor-mixin>)
end class <carbon-viewport>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-viewport>, #f)
end method class-for-make-pane;


/// Borders

/*---*** Use the fake border for now...
define sealed class <carbon-border>
    (<standard-repainting-mixin>,
     <border>,
     <basic-sheet>,
     <sealed-constructor-mixin>)
  sealed slot %pen   :: false-or(<standard-pen>) = #f;
  sealed slot %brush :: false-or(type-union(<standard-brush>, <ink>)) = #f;
end class <carbon-border>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <border>, #key label)
 => (class :: <class>, options :: false-or(<sequence>))
  let border-class = if (label) <carbon-group-box> else <carbon-border> end;
  values(border-class, #f)
end method class-for-make-pane;

define constant $gadget-border-thickness :: <integer> = 2;

define sealed method do-compose-space
    (pane :: <carbon-border>, #key width, height)
 => (space-req :: <space-requirement>)
  let thickness*2 = $gadget-border-thickness * 2;
  space-requirement+(pane,
		     next-method(pane,
				 width:  width  & width  - thickness*2,
				 height: height & height - thickness*2),
		     width: thickness*2, height: thickness*2)
end method do-compose-space;

define sealed method do-allocate-space
    (pane :: <carbon-border>, width :: <integer>, height :: <integer>) => ()
  let child = sheet-child(pane);
  let thickness = $gadget-border-thickness;
  when (child)
    set-sheet-edges(child,
                    thickness, thickness,
                    width - thickness, height - thickness)
  end
end method do-allocate-space;

define sealed method handle-repaint
    (pane :: <carbon-border>, medium :: <carbon-medium>, region :: <region>) => ()
  ignore(region);	// not worth checking
  let (left, top, right, bottom) = box-edges(pane);
  draw-border(pane, medium, border-type(pane), left, top, right, bottom)
end method handle-repaint;

//---*** DO THE RIGHT THING
define sealed method draw-border
    (pane :: <sheet>, medium :: <carbon-medium>, type :: <border-type>,
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

define sealed class <carbon-slider>
    (<carbon-gadget-mixin>,
     <slider>,
     <leaf-pane>,
     <sealed-constructor-mixin>)
end class <carbon-slider>;

define sealed class <carbon-horizontal-slider> (<carbon-slider>)
  keyword carbon-fixed-height?: = #t;
end class <carbon-horizontal-slider>;

define sealed class <carbon-vertical-slider> (<carbon-slider>)
  keyword carbon-fixed-width?: = #t;
end class <carbon-vertical-slider>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <slider>, 
     #key orientation = #"horizontal")
 => (class :: <class>, options :: false-or(<sequence>))
  values(select (orientation)
	   #"horizontal" => <carbon-horizontal-slider>;
	   #"vertical"   => <carbon-vertical-slider>;
	 end,
	 #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-horizontal-slider>)
 => (mirror :: <control-mirror>)
  let control = carbon-hscale-new(null-pointer(<MacAdjustment*>));
  assert(~null-pointer?(control), "carbon-hscale-new failed");
  make(<control-mirror>,
       control: control,
       sheet:   gadget)
end method make-carbon-mirror;

define sealed method make-carbon-mirror
    (gadget :: <carbon-vertical-slider>)
 => (mirror :: <control-mirror>)
  let control = carbon-vscale-new(null-pointer(<MacAdjustment*>));
  assert(~null-pointer?(control), "carbon-vscale-new failed");
  make(<control-mirror>,
       control: control,
       sheet:   gadget)
end method make-carbon-mirror;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-slider>) => ()
  next-method();
  ignoring("note-gadget-value-changed on <slider>")
end method note-gadget-value-changed;

define sealed method note-gadget-value-range-changed
    (gadget :: <carbon-slider>) => ()
  next-method();
  ignoring("note-gadget-value-range-changed on <slider>")
end method note-gadget-value-range-changed;


/// Tool bars

//---*** Someday we should do these for real!

define sealed class <carbon-tool-bar>
    (<tool-bar>, <single-child-wrapping-pane>, <sealed-constructor-mixin>)
  //--- The way we do this separator stuff is just loathsome...
  slot tool-bar-decoration :: <sheet>;
  slot %separator :: false-or(<separator>) = #f;
end class <carbon-tool-bar>;

define method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <tool-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-tool-bar>, #f)
end method class-for-make-pane;

define method initialize
    (gadget :: <carbon-tool-bar>, #key frame-manager: framem) => ()
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
define method note-sheet-mapped (gadget :: <carbon-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #t
  end
end method note-sheet-mapped;

// Ditto, for unmapping
define method note-sheet-unmapped (gadget :: <carbon-tool-bar>) => ()
  next-method();
  when (sheet-direct-mirror(gadget.%separator))
    sheet-mapped?(tool-bar-decoration(gadget)) := #f
  end
end method note-sheet-unmapped;


/// Status bars


/*---*** No status bar for now...
define sealed class <carbon-status-bar>
    (<carbon-gadget-mixin>,
     <status-bar>,
     <row-layout>,
     <sealed-constructor-mixin>)
  slot status-bar-simple? :: <boolean> = #f,
    setter: %simple?-setter;
  slot status-bar-simple-text :: <string> = "";
  keyword border:      = $status-bar-border;
  keyword spacing:     = $status-bar-spacing;
  keyword y-alignment: = #"center";
end class <carbon-status-bar>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <status-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-status-bar>, #f)
end method class-for-make-pane;

define sealed method make-gadget-control
    (gadget :: <carbon-status-bar>, 
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
    (gadget :: <carbon-status-bar>, #key width, height)
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
    (gadget :: <carbon-status-bar>, width :: <integer>, height :: <integer>) => ()
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

define class <status-label-mirror> (<carbon-mirror>)
  sealed slot status-label-status-bar :: <status-bar>,
    required-init-keyword: status-bar:;
  sealed slot status-label-part-number :: <integer>,
    required-init-keyword: part-number:;
end class <status-label-mirror>;

define sealed method make-gadget-mirror
    (status-bar :: <carbon-status-bar>, gadget :: <carbon-label>)
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
    (gadget :: <carbon-label>, mirror :: <status-label-mirror>) => ()
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
    (simple? :: <boolean>, gadget :: <carbon-status-bar>)
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
    (label :: <string>, gadget :: <carbon-status-bar>)
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
    (gadget :: <carbon-status-bar>) => ()
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



/// Progress controls

/*---*** Not yet implemented
define sealed class <carbon-progress-bar>
    (<carbon-gadget-mixin>,
     <progress-bar>,
     <leaf-pane>)
end class <carbon-progress-bar>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <progress-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-progress-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<carbon-progress-bar>));
define sealed domain initialize (<carbon-progress-bar>);

define sealed method make-carbon-mirror
    (gadget :: <carbon-progress-bar>)
 => (mirror :: <control-mirror>)
  let (value, min-value, max-value)
    = range-gadget-adjusted-contents(gadget);
  make-control-mirror
    (gadget, $kControlProgressBarProc, 
     value: value,
     min-value: min-value,
     max-value: max-value)
end method make-carbon-mirror;

define sealed method note-mirror-created
    (gadget :: <carbon-progress-bar>, mirror :: <window-mirror>) => ()
  next-method();
  update-progress-bar(gadget)
end method note-mirror-created;

define sealed method do-compose-space
    (gadget :: <carbon-progress-bar>, #key width, height)
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
    (gadget :: <carbon-progress-bar>, #key width, height)
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
    (gadget :: <carbon-progress-bar>) => ()
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    let handle = window-handle(mirror);
    let _port = port(gadget);
    let value = gadget-value(gadget);
    if (value)
      //---*** We shouldn't really overload the progress note value this way...
      when (~sheet-withdrawn?(gadget) & sheet-layed-out?(gadget))
	map-mirror(_port, gadget, mirror)
      end;
      let progress-range = gadget-value-range(gadget);
      let first-value    = progress-range[0];
      let range-increment
	= if (size(progress-range) <= 1) 1 else progress-range[1] - first-value end;
      let pos = floor/(value - first-value, range-increment);
      let min-value = 0;
      let max-value = max(min-value, size(progress-range) - 1);
      SendMessage(handle, $PBM-SETRANGE, 0, MAKELPARAM(min-value, max-value));
      SendMessage(handle, $PBM-SETPOS, pos, 0)
    else
      unmap-mirror(_port, gadget, mirror)
    end
  end
end method update-progress-bar;

define sealed method note-gadget-value-changed
    (gadget :: <carbon-progress-bar>) => ()
  next-method();
  update-progress-bar(gadget)
end method note-gadget-value-changed;
*/
