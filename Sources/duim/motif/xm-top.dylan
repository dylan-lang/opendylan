Module:    motif-duim
Synopsis:  Motif top level window handling
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Frame protocols

define protocol <<motif-frame-protocol>> ()
  function note-motif-frame-destroyed (frame :: <abstract-frame>) => ();
end protocol <<motif-frame-protocol>>;


/// Top level mirrors

define sealed class <top-level-mirror> (<window-mirror>)
  sealed slot mirror-shell-widget = #f,
    init-keyword: shell-widget:;
end class <top-level-mirror>;

define sealed method top-level-mirror
    (sheet :: <sheet>, #key error? = #f)
 => (mirror :: false-or(<top-level-mirror>))
  let sheet  = top-level-sheet(sheet);
  let mirror = sheet & sheet-direct-mirror(sheet);
  mirror
    | (error? & error("Failed to find top-level mirror for %=", sheet))
end method top-level-mirror;

define sealed method top-level-mirror
    (frame :: <frame>, #key error? = #f)
 => (mirror :: false-or(<top-level-mirror>))
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  mirror
    | (error? & error("Failed to find top-level mirror for %=", sheet))
end method top-level-mirror;


/// Accelerator handling

define function make-keyboard-gesture
    (keysym :: <symbol>, #rest modifiers)
 => (gesture :: <keyboard-gesture>)
  make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
end function make-keyboard-gesture;

define function gesture-modifiers
    (gesture :: <keyboard-gesture>)
 => (shift? :: <boolean>, control? :: <boolean>, alt? :: <boolean>)
  let modifier-state = gesture-modifier-state(gesture);
  values(~zero?(logand(modifier-state, $shift-key)),
	 ~zero?(logand(modifier-state, $control-key)),
	 ~zero?(logand(modifier-state, $alt-key)))
end function gesture-modifiers;

//---*** WHAT ABOUT ALL THIS ACCELERATOR STUFF?
define table $accelerator-table :: <object-table>
  = { // This is the set defined by WIG, Appendix B, Table B.2, page 438
      #"Copy"        => make-keyboard-gesture(#"c", #"control"),
      #"Cut"         => make-keyboard-gesture(#"x", #"control"),
      #"Help"        => make-keyboard-gesture(#"f1"),
      #"Open"        => make-keyboard-gesture(#"o", #"control"),
      #"Open..."     => make-keyboard-gesture(#"o", #"control"),
      #"Paste"       => make-keyboard-gesture(#"v", #"control"),
      #"Print"       => make-keyboard-gesture(#"p", #"control"),
      #"Print..."    => make-keyboard-gesture(#"p", #"control"),
      #"Save"        => make-keyboard-gesture(#"s", #"control"),
      #"Undo"        => make-keyboard-gesture(#"z", #"control"),

      // The same set with the mnemonics already in (a bit of a hack!)
      #"&Copy"       => make-keyboard-gesture(#"c", #"control"),
      #"Cu&t"        => make-keyboard-gesture(#"x", #"control"),
      #"&Help"       => make-keyboard-gesture(#"f1"),
      #"&Open"       => make-keyboard-gesture(#"o", #"control"),
      #"&Open..."    => make-keyboard-gesture(#"o", #"control"),
      #"&Paste"      => make-keyboard-gesture(#"v", #"control"),
      #"&Print"      => make-keyboard-gesture(#"p", #"control"),
      #"&Print..."   => make-keyboard-gesture(#"p", #"control"),
      #"&Save"       => make-keyboard-gesture(#"s", #"control"),
      #"&Undo"       => make-keyboard-gesture(#"z", #"control"),

      // Some extras that seemed to be missing
      #"Delete"      => make-keyboard-gesture(#"delete"),
      #"Find"        => make-keyboard-gesture(#"f", #"control"),
      #"Find..."     => make-keyboard-gesture(#"f", #"control"),
      #"New"         => make-keyboard-gesture(#"n", #"control"),
      #"New..."      => make-keyboard-gesture(#"n", #"control"),
      #"Redo"        => make-keyboard-gesture(#"y", #"control"),
      #"Select All"  => make-keyboard-gesture(#"a", #"control"),

      // The same set with the mnemonics already in (a bit of a hack!)
      #"&Delete"     => make-keyboard-gesture(#"delete"),
      #"&Find"       => make-keyboard-gesture(#"f", #"control"),
      #"&Find..."    => make-keyboard-gesture(#"f", #"control"),
      #"&New"        => make-keyboard-gesture(#"n", #"control"),
      #"&New..."     => make-keyboard-gesture(#"n", #"control"),
      #"&Redo"       => make-keyboard-gesture(#"y", #"control"),
      #"&Select All" => make-keyboard-gesture(#"a", #"control")
      };

define sealed method defaulted-gadget-accelerator
    (framem :: <motif-frame-manager>, gadget :: <accelerator-mixin>)
 => (accelerator :: false-or(<accelerator>))
  let accelerator = gadget-accelerator(gadget);
  if (unsupplied?(accelerator))
    let label = gadget-label(gadget);
    let key   = instance?(label, <string>) & as(<symbol>, label);
    element($accelerator-table, key, default: #f)
  else
    accelerator
  end
end method defaulted-gadget-accelerator;


define sealed method add-gadget-label-postfix
    (gadget :: <motif-gadget-mixin>, label :: <string>) => (label :: <string>)
  label
end method add-gadget-label-postfix;

define sealed method add-gadget-label-postfix
    (gadget :: <accelerator-mixin>, label :: <string>) => (label :: <string>)
  let framem  = frame-manager(gadget);
  let gesture = defaulted-gadget-accelerator(framem, gadget);
  if (gesture)
    let keysym = gesture-keysym(gesture);
    let (shift?, control?, alt?) = gesture-modifiers(gesture);
    concatenate-as(<string>, 
		   label,
		   "\t",
		   if (shift?)   "Shift+" else "" end,
		   if (control?) "Ctrl+"  else "" end,
		   if (alt?)     "Alt+"   else "" end,
		   keysym->key-name(keysym))
  else
    label
  end
end method add-gadget-label-postfix;

// Map keysyms to their labels on a typical keyboard
define table $keysym->key-name :: <object-table>
  = { #"return"     => "Enter",
      #"newline"    => "Shift+Enter",
      #"linefeed"   => "Line Feed",
      #"up"	    => "Up Arrow",
      #"down"	    => "Down Arrow",
      #"left"	    => "Left Arrow",
      #"right"	    => "Right Arrow",
      #"prior"	    => "Page Up",
      #"next"	    => "Page Down",
      #"lwin"	    => "Left Windows",
      #"rwin"	    => "Right Windows",
      #"numpad0"    => "Num 0",
      #"numpad1"    => "Num 1",
      #"numpad2"    => "Num 2",
      #"numpad3"    => "Num 3",
      #"numpad4"    => "Num 4",
      #"numpad5"    => "Num 5",
      #"numpad6"    => "Num 6",
      #"numpad7"    => "Num 7",
      #"numpad8"    => "Num 8",
      #"numpad9"    => "Num 9",
      #"num-lock"   => "Num Lock",
      #"caps-lock"  => "Caps Lock" };

define function keysym->key-name
    (keysym) => (name :: <string>)
  element($keysym->key-name, keysym, default: #f)
  | string-capitalize(as(<string>, keysym))
end function keysym->key-name;


define sealed method accelerator-table
    (sheet :: <top-level-sheet>) => (accelerators :: false-or(<HACCEL>))
  let mirror = sheet-direct-mirror(sheet);
  // Ensure that we don't build the accelerator table too early (i.e.,
  // before all of the resource ids have been created).  This isn't as bad
  // as it seems, since users won't have been able to use an accelerator
  // before the top-level sheet is mapped anyway...
  when (sheet-mapped?(sheet))
    mirror.%accelerator-table
    | (mirror.%accelerator-table := make-accelerator-table(sheet))
  end
end method accelerator-table;

define sealed method accelerator-table
    (sheet :: <sheet>) => (accelerators :: false-or(<HACCEL>))
  let top-sheet = top-level-sheet(sheet);
  top-sheet & accelerator-table(top-sheet)
end method accelerator-table;

define method make-accelerator-table
    (sheet :: <top-level-sheet>) => (accelerators :: <HACCEL>)
  local method fill-accelerator-entry
	    (gadget :: <accelerator-mixin>, accelerator :: <accelerator>,
	     entry :: <LPACCEL>) => ()
	  let keysym    = gesture-keysym(accelerator);
	  let modifiers = gesture-modifier-state(accelerator);
	  let char      = gesture-character(accelerator);
	  let (vkey :: <integer>, fVirt :: <integer>)
	    = case
		char
		& zero?(logand(modifiers, logior($control-key, $meta-key)))
		& character->virtual-key(char) =>
		  values(character->virtual-key(char), 0);
		keysym->virtual-key(keysym) =>
		  values(keysym->virtual-key(keysym),
			 logior($ACCEL-FVIRTKEY,
				if (zero?(logand(modifiers, $shift-key)))   0 else $ACCEL-FSHIFT end,
				if (zero?(logand(modifiers, $control-key))) 0 else $ACCEL-FCONTROL end,
				if (zero?(logand(modifiers, $alt-key)))     0 else $ACCEL-FALT end));
		otherwise =>
		  error("Can't decode the gesture with keysym %=, modifiers #o%o",
			keysym, modifiers);
	      end;
	  let cmd :: <integer>
	    = sheet-resource-id(gadget) | gadget->id(gadget);
	  entry.fVirt-value := fVirt;
	  entry.key-value   := vkey;
	  entry.cmd-value   := cmd;
	end method;
  let accelerators   = frame-accelerators(sheet-frame(sheet));
  let n :: <integer> = size(accelerators);
  if (n > 0)
    with-stack-structure (entries :: <LPACCEL>, element-count: n)
      for (i :: <integer> from 0 below n)
	let entry  = accelerators[i];
	let gadget = entry[0];
	let accel  = entry[1];
	let entry  = pointer-value-address(entries, index: i);
	fill-accelerator-entry(gadget, accel, entry)
      end;
      check-result("CreateAcceleratorTable", CreateAcceleratorTable(entries, n))
    end
  else
    $null-HACCEL
  end
end method make-accelerator-table;

define sealed method destroy-accelerator-table
    (sheet :: <top-level-sheet>) => ()
  let accelerator-table = accelerator-table(sheet);
  when (accelerator-table & ~null-handle?(accelerator-table))
    DestroyAcceleratorTable(accelerator-table)
  end;
  let mirror = sheet-direct-mirror(sheet);
  mirror.%accelerator-table := #f
end method destroy-accelerator-table;

define method note-accelerators-changed
    (framem :: <motif-frame-manager>, frame :: <basic-frame>) => ()
  // Force the accelerators to be recomputed
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    destroy-accelerator-table(top-sheet)
  end
end method note-accelerators-changed;


/// Dialog handling

define method mirror-registered-dialogs
    (mirror :: <top-level-mirror>) => (dialogs :: <sequence>)
  mirror.%dialog-mirrors
end method mirror-registered-dialogs;

define method register-dialog-mirror
    (frame :: <simple-frame>, dialog-mirror :: <dialog-mirror>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-mirror = sheet-direct-mirror(top-sheet);
    add!(top-mirror.%dialog-mirrors, dialog-mirror)
  end
end method register-dialog-mirror;

define method unregister-dialog-mirror
    (frame :: <simple-frame>, dialog-mirror :: <dialog-mirror>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let top-mirror = sheet-direct-mirror(top-sheet);
    remove!(top-mirror.%dialog-mirrors, dialog-mirror)
  end
end method unregister-dialog-mirror;


/// Top level sheets

define open abstract class <motif-top-level-sheet-mixin>
    (<standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <motif-pane-mixin>)
end class <motif-top-level-sheet-mixin>;

define sealed class <motif-top-level-sheet>
    (<motif-top-level-sheet-mixin>,
     <top-level-sheet>)
end class <motif-top-level-sheet>;

define sealed method class-for-make-pane
    (framem :: <motif-frame-manager>, class == <top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-top-level-sheet>, #f)
end method class-for-make-pane;

// Like a top-level sheet, but for embedded apps such as OLE parts
define sealed class <motif-embedded-top-level-sheet>
    (<motif-top-level-sheet-mixin>,
     <embedded-top-level-sheet>)
end class <motif-embedded-top-level-sheet>;

define sealed method class-for-make-pane
    (framem :: <motif-frame-manager>, class == <embedded-top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-embedded-top-level-sheet>, #f)
end method class-for-make-pane;

// For discussion of input-focus, see:
//  - O'Reilly, Vol 4, 5.7 Kbd Traversal and Focus, p171
//    which suggests that using the keyboard-focus-policy is the wrong way
//    to control focus behaviour.
//  - O'Reilly, Vol 4, 11.1.4, Input Model, p.347-8
//    suggests that :input is the right resource to control focus behavior
//    (further discussion in O'Reilly, Vol 4, 14.4, p473)
// Finally, note that a value of #t or the default of #f doesn't seem to
// affect the behaviour we want in twm.
define sealed method do-make-mirror
    (_port :: <motif-port>, sheet :: <motif-top-level-sheet-mixin>)
 => (mirror :: <top-level-mirror>)
  let frame    = sheet-frame(sheet);
  let dmirror  = sheet-direct-mirror(sheet-device-parent(sheet));
  let dwidget  = mirror-widget(dmirror);
  let resource = make-resource-name(frame-name(frame));
  let modal?   = frame & frame-mode(frame) == #"modal";
  let client   = frame & get-property(frame-properties(frame), #"dialog-for");
  if (modal? | client)
    make-modal-top-level-mirror(sheet, frame, gwidget, resource, dialog-for)
  else
    make-modeless-top-level-mirror(sheet, frame, gwidget, resource)
  end
end method do-make-mirror;

define function make-resource-name (name) => (name :: <string>)
  let string = as(<string>, name);
  let string = string-capitalize(string);
  remove!(string, ' ')
end function make-resource-name;

// The Richard Billington theory follows...
// 
// If the shell widget is 'mapped-when-managed: #t', then when its child
// (the DUIMtopsheet widget, in this case) is managed, it also gets managed
// (doesn't mention this upward propagation of management in this case, but
// that's what seems to happen) and the shell gets mapped, and so do it's
// children, and the mirror pops up ahead of time.
//
// However, the DUIMtopsheet widget needs to be managed so that its size
// and position can be set prior to mapping.
//
// Therefore, we create the DUIMframeshell with 'mapped-when-managed: #f',
// manage the DUIMtopsheet, and that at the last moment (in 'enable-mirror'
// on top level sheets) we change the DUIMframeShell's mapped-when-managed
// attribute to #t.  Then we map the DUIMtopsheet child and everything's OK.
//
// A couple of wierd notes: If you leave 'mapped-when-managed: #f' and you
// map the window by hand, everything is OK, except the window comes up
// ignoring all input; you need to maintain the size and location of the
// shell widget -= at least during initial management and mapping (see
// 'set-mirror-edges' on top level sheets) or windows try to be too small,
// etc.

define sealed method make-modal-top-level-mirror
    (sheet :: <top-level-sheet>, frame :: <basic-frame>,
     graft-widget, resource-name :: <string>, dialog-for :: false-or(<basic-frame>))
 => (mirror :: <top-level-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let title = frame-title(frame) | "DUIM Window";
  let x = frame-geometry(frame)[0];
  let y = frame-geometry(frame)[1];
  let width  = frame-geometry(frame)[2];
  let height = frame-geometry(frame)[3];
  let transient-for-widget
    = dialog-for & top-level-shell-width(sheet-direct-mirror(top-level-sheet(dialog-for)));
  let shell-widget
    = xt/XtCreatePopupShell(resource-name,	// DUIMframeShell
			    xm/<dialog-shell>,
			    transient-for-widget | graft-widget,
			    resources:
			      vector(title:, title,
				     mapped-when-managed:, #f,
				     x:, x, y:, y,
				     width: width, height: height));
  let widget
    = xt/XtCreateWidget("DUIMTopSheet",
			xm/<bulletin-board>,
			shell-widget,
			resources:
			  vector(dialog-style:, xm/$XmDIALOG-PRIMARY-APPLICATION-MODAL,
				 auto-unmanage:, #f,
				 mapped-when-managed:, #f,
				 resize-policy:, xm/$XmRESIZE-NONE,
				 x:, x, y:, y,
				 width: width, height: height,
				 margin-width: 0, margin-height: 0,
				 border-width: 0, shadow-thickness: 0));
  let mirror
    = make(<top-level-mirror>,
	   shell-widget: shell-widget,
	   widget: widget,
	   sheet:  sheet);
  install-event-handlers(mirror);
  install-frame-event-handlers(mirror);
  xt/add-widget-destroy-callback(widget, top-level-mirror-destroy-callback, mirror);
  xt/add-editres-handler(shell-widget);
  xt/XtRealizeWidget(widget);
  xt/XtManageChild(widget);
  mirror
end method make-modal-top-level-mirror;

define sealed method make-modeless-top-level-mirror
    (sheet :: <top-level-sheet>, frame :: <basic-frame>,
     graft-widget, resource-name :: <string>)
 => (mirror :: <top-level-mirror>)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let title = frame-title(frame) | "DUIM Window";
  let icon  = frame-icon(frame);
  let x = frame-geometry(frame)[0];
  let y = frame-geometry(frame)[1];
  let width  = frame-geometry(frame)[2];
  let height = frame-geometry(frame)[3];
  let geometry-spec
    = format-to-string("%dx%d", width, height);
  let shell-widget
    = xt/XtCreatePopupShell(resource-name,	// DUIMframeShell
			    xt/<top-level-shell>,
			    graft-widget,
			    resources:
			      vector(title:, title,
				     icon:, icon,
				     icon-name:, title,
				     x:, x, y:, y,
				     width: width, height: height));
  let widget
    = xt/XtCreateManagedWidget("DUIMTopSheet",
			       xm/<bulletin-board>,
			       shell-widget,
			       resources:
				 vector(resize-policy:, xm/$XmRESIZE-NONE,
					// x:, x, y:, y,
					// width: width, height: height,
					margin-width: 0, margin-height: 0,
					border-width: 0));
  let mirror
    = make(<top-level-mirror>,
	   shell-widget: shell-widget,
	   widget: widget,
	   sheet:  sheet);
  xt/XtSetValues(shell-widget, geometry: geometry);
  install-event-handlers(mirror);
  install-frame-event-handlers(mirror);
  xt/add-widget-destroy-callback(widget, destroy-top-level-mirror-callback, mirror);
  xt/add-editres-handler(shell-widget);
  xt/XtRealizeWidget(shell-widget);
  xt/XtRealizeWidget(widget);
  xt/XtManageChild(widget);
  mirror
end method make-modeless-top-level-mirror;

define function destroy-top-level-mirror-callback (mirror) => ()
  mirror-widget(mirror) := #f;
  mirror-shell-widget(mirror) := #f
end function destroy-mirror-callback;


define sealed method map-mirror
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  let widget = mirror-widget(mirror);
  let shell  = mirror-shell-widget(mirror);
  if (xm/XmIsDialogShell(shell))
    xt/XtSetValues(shell, mapped-when-managed: #t);
    xt/XtMapWidget(widget)
  else
    xt/XtPopup(shell, #"none")
  end
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  let widget = mirror-widget(mirror);
  let shell  = mirror-shell-widget(mirror);
  if (xm/XmIsDialogShell(shell))
    xt/XtUnmanageChild(widget)
  else
    xt/XtPopdown(shell)
  end
end method unmap-mirror;

define sealed method raise-mirror 
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  let shell = mirror-shell-widget(mirror);
  x/XRaiseWindow(_port.%display, x/XtWindow(widget))
end method raise-mirror;

define sealed method lower-mirror
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  let shell = mirror-shell-widget(mirror);
  x/XLowerWindow(_port.%display, x/XtWindow(widget))
end method lower-mirror;


define sealed method mirror-edges
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <top-level-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let widget = mirror-widget(mirror);
  let shell  = mirror-shell-widget(mirror);
  let (x, y)          = xt/XtGetValues(shell, #"x", #"y");
  let (width, height) = xt/XtGetValues(shell, #"width", #"height");
  values(x, y, x + width, y + height)
end method mirror-edges;

define sealed method set-mirror-edges
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <top-level-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  let widget = mirror-widget(mirror);
  let shell  = mirror-shell-widget(mirror);
  let width  = right - left;
  let height = bottom - top;
  let (ol, ot, or, ob) = mirror-edges(_port, sheet, mirror);
  let (ow, oh) = values(or - ol, ob - ot);
  unless (left = ol & top = ot)
    xt/XtMoveWidget(shell, left, top)
  end;
  unless (width = ow & height = oh)
    xt/XtSetValues(widget, width: width, height: height);
    xt/XtConfigureWidget(shell, left, top, width, height, 0)
  end
end method set-mirror-edges;

define sealed method destroy-mirror 
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <top-level-mirror>) => ()
  next-method();
  let shell = mirror-shell-widget(mirror);
  mirror-shell-widget(mirror) := #f;
  xt/XtDestroyWidget(shell)
end method destroy-mirror;

define method note-motif-frame-destroyed
    (frame :: <simple-frame>) => ()
  unless (frame-owner(frame))
    duim-debug-message("Quitting frame %=", frame);
    PostQuitMessage(0)
  end
end method note-motif-frame-destroyed;


/// Top level layout

define class <top-level-layout> (<layout-pane>)
  sealed slot top-level-client-layout,
    init-keyword: client-layout:;
end class <top-level-layout>;

define sealed method do-compose-space
    (layout :: <top-level-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  let frame = sheet-frame(layout);
  let menu-width = frame-menu-bar-size(frame);
  let client-layout = top-level-client-layout(layout);
  let (extra-width, extra-height) = window-frame-extra-size(frame);
  if (client-layout)
    let client-width  = width  & (width  - extra-width);
    let client-height = height & (height - extra-width);
    let child-space
      = compose-space(client-layout, 
		      width: client-width, height: client-height);
    let (w, w-, w+, h, h-, h+)
      = space-requirement-components(client-layout, child-space);
    let best-width  = max(w,  menu-width) + extra-width;
    let min-width   = max(w-, menu-width) + extra-width;
    let max-width   = max(w+, menu-width) + extra-height;
    let best-height = h  + extra-height;
    let min-height  = h- + extra-height;
    let max-height  = h+ + extra-height;
    make(<space-requirement>,
	 width:  best-width,  min-width:  min-width,  max-width:  max-width,
	 height: best-height, min-height: min-height, max-height: max-height)
  else
    let min-width   = extra-width;
    let min-height  = extra-height;
    let best-width  = max(width  | 0, min-width);
    let best-height = max(height | 0, min-height);
    make(<space-requirement>,
	 width:  best-width,  min-width: min-width, max-width: $fill,
	 height: best-height, min-height: min-height, max-height: $fill)
  end
end method do-compose-space;

define sealed method do-allocate-space
    (layout :: <top-level-layout>, width :: <integer>, height :: <integer>) => ()
  let frame = sheet-frame(layout);
  let (extra-width, extra-height) = window-frame-extra-size(frame);
  let client-layout = top-level-client-layout(layout);
  set-sheet-edges(client-layout,
		  0, 0, 
		  width - extra-width, height - extra-height)
end method do-allocate-space;

define sealed method frame-menu-bar-size
    (frame :: <basic-frame>)
 => (width  :: <integer>, height  :: <integer>)
  let menu-bar = frame-menu-bar(frame);
  // Menu bars aren't part of the DUIM sheet hierarchy in Motif,
  // so we have to call 'compose-space' ourselves to measure it
  if (menu-bar)
    let space-req = compose-space(menu-bar);
    let (w, w-, w+, h, h-, h+) 
      = space-requirement-components(menu-bar, space-req);
    ignore(w-, w+, h-, h+);
    values(w, h)
  else
    values(0, 0)
  end
end method frame-menu-bar-size;

define method frame-client-area-offset
    (frame :: <basic-frame>)
 => (x :: <integer>, y :: <integer>)
  let (x, y)          = values(100, 100);
  let (width, height) = frame-size(frame);
  let width  = width  | $default-sheet-size;
  let height = height | $default-sheet-size;
  frame-non-client-geometry(frame, x, y, x + width, y + height)
end method frame-client-area-offset;

define method window-frame-extra-size
    (frame :: <basic-frame>)
 => (width :: <integer>, height :: <integer>)
  let (x, y)          = values(100, 100);
  let (width, height) = frame-size(frame);
  let width  = width  | $default-sheet-size;
  let height = height | $default-sheet-size;
  let (left, top, right, bottom)
    = client->frame-edges(frame, x, y, x + width, y + height);
  values(right - left - width, bottom - top - height)
end method window-frame-extra-size;

define method frame-non-client-geometry
    (frame :: <basic-frame>, 
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => (x :: <integer>, y :: <integer>, width :: <integer>, height :: <integer>)
  let (width, height) = values(right - left, bottom - top);
  let (l, t, r, b) = client->frame-edges(frame, left, top, right, bottom);
  let (w, h) = values(r - l, b - t);
  let x-offset     = left   - l;
  let y-offset     = top    - t;
  let extra-width  = width  - w;
  let extra-height = height - h;
  values(x-offset, y-offset, extra-width, extra-height)
end method frame-non-client-geometry;

define method client->frame-edges
    (frame :: <basic-frame>, 
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => (l :: <integer>, t :: <integer>, r :: <integer>, b :: <integer>)
  with-stack-structure (rect :: <LPRECT>)
    rect.left-value   := left;
    rect.top-value    := top;
    rect.right-value  := right;
    rect.bottom-value := bottom;
    let menu-bar? = frame-menu-bar(frame) & #t;
    let (style, extended-style) = frame-window-styles(frame);
    check-result("AdjustWindowRectEx",
		 AdjustWindowRectEx(rect, style, menu-bar?, extended-style));
    let frame-left   = rect.left-value;
    let frame-top    = rect.top-value;
    let frame-right  = rect.right-value;
    let frame-bottom = rect.bottom-value;
    duim-debug-message
      ("Adjusted [%=,%=,%=,%=] => [%=,%=,%=,%=]: diff [%=,%=,%=,%=]",
       left, top, right, bottom,
       frame-left, frame-top, frame-right, frame-bottom,
       frame-left - left, frame-top - top,
       frame-right - right, frame-bottom - bottom);
    values(frame-left, frame-top, frame-right, frame-bottom)
  end
end method client->frame-edges;


/// Frame wrapper

define method frame-wrapper
    (framem :: <motif-frame-manager>, 
     frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (wrapper :: false-or(<sheet>))
  let menu-bar      = frame-menu-bar(frame);
  let client-layout = make-client-layout(framem, frame, layout);
  make(<top-level-layout>,
       client-layout: client-layout,
       children: make-children(menu-bar, client-layout))
end method frame-wrapper;

define sealed method make-client-layout
    (framem :: <motif-frame-manager>, 
     frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (client-layout :: <sheet>)
  let tool-bar   = frame-tool-bar(frame);
  let status-bar = frame-status-bar(frame);
  with-frame-manager (framem)
    let indented-children
      = make-children(tool-bar & tool-bar-decoration(tool-bar), layout);
    let indented-children-layout
      = unless (empty?(indented-children))
	  with-spacing (spacing: motif-dialog-x-pixels(framem, 1))
	    make(<column-layout>,
		 children: indented-children,
		 y-spacing: $top-level-spacing)
          end
        end;
    make(<column-layout>,
	 children: make-children(indented-children-layout, status-bar),
	 y-spacing: $top-level-spacing)
  end
end method make-client-layout;

define function make-children
    (#rest maybe-children)
 => (children :: <sequence>)
  let children :: <stretchy-object-vector> = make(<stretchy-vector>);
  for (child in maybe-children)
    when (child)
      add!(children, child)
    end
  end;
  children
end function make-children;


define method update-frame-layout
    (framem :: <motif-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let old-layout = *old-layout*;
    let new-layout = frame-layout(frame);
    let layout-parent = sheet-parent(old-layout);
    if (old-layout)
      if (new-layout)
	replace-child(layout-parent, old-layout, new-layout);
	relayout-children(layout-parent);
	relayout-parent(new-layout);
	sheet-mapped?(new-layout, clear?: #t, repaint?: #t) := sheet-mapped?(layout-parent)
      else
	remove-child(layout-parent, old-layout);
	relayout-parent(layout-parent)
      end
    else
      not-yet-implemented("Adding a new layout into a frame")
    end;
    let wrapper = sheet-child(top-sheet);
    top-level-client-layout(wrapper) := new-layout
  end
end method update-frame-layout;

define sealed method update-frame-wrapper
    (framem :: <motif-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    not-yet-implemented("Updating of the frame wrapper")
  end
end method update-frame-wrapper;


/// Geometry updating

define sealed method handle-move
    (sheet :: <top-level-sheet>, mirror :: <top-level-mirror>,
     x :: <integer>, y :: <integer>)
 => (handled? :: <boolean>)
  let (old-x, old-y) = sheet-position(sheet);
  unless (x = old-x & y = old-y)
    let frame = sheet-frame(sheet);
    let (x-offset, y-offset) = frame-client-area-offset(frame);
    let x :: <integer> = x - x-offset;
    let y :: <integer> = y - y-offset;
    duim-debug-message("Sheet %= moved to %=, %= (from %=, %=)",
		       sheet, x, y, old-x, old-y);
    set-sheet-position(sheet, x, y)
  end;
  #t
end method handle-move;

define sealed method handle-resize
    (sheet :: <top-level-sheet>, mirror :: <top-level-mirror>,
     width :: <integer>, height :: <integer>)
 => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  let (left, top, right, bottom) 
    = client->frame-edges(frame, 0, 0, width, height);
  let (width, height) = values(right - left, bottom - top);
  let (left, top) = box-position(mirror.%region);
  let region = make-bounding-box(left, top, left + width, top + height);
  //--- This hack is to avoid doing anything on the first WM_SIZE
  //--- which comes in before the children are attached.  Can we
  //--- do something more interesting?
  if (sheet-mapped?(sheet))
    let (old-width, old-height) = box-size(sheet-region(sheet));
    duim-debug-message("Resizing %= to %dx%d -- was %dx%d",
		       sheet, width, height, old-width, old-height);
    distribute-event(port(sheet),
		     make(<window-configuration-event>,
			  sheet: sheet,
			  region: region))
  else
    duim-debug-message("Ignoring WM_SIZE event for %= to size %dx%d",
		       sheet, width, height)
  end;
  #t
end method handle-resize;
