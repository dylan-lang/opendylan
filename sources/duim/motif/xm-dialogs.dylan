Module:    motif-duim
Synopsis:  Motif dialog implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM dialogs

define sealed class <dialog-mirror> (<top-level-mirror>)
  sealed constant slot %owner :: false-or(<frame>),
    required-init-keyword: owner:;
end class <dialog-mirror>;

define sealed domain make (singleton(<dialog-mirror>));
define sealed domain initialize (<dialog-mirror>);

//---*** WRONG
define sealed method make-top-level-window
    (frame :: <dialog-frame>, sheet :: <motif-top-level-sheet-mixin>,
     resource-id == #f)
 => (handle :: <HWND>, resource :: singleton(#f),
     mirror-class :: <class>, mirror-initargs)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let title = frame-title(frame);
  let x = frame-geometry(frame)[0];
  let y = frame-geometry(frame)[1];
  let owner = frame-owner(frame);
  let owner-top-level = owner & top-level-sheet(owner);
  let owner-handle
    = if (owner-top-level & sheet-mapped?(owner-top-level))
	window-handle(owner-top-level)
      else
	$NULL-HWND
      end;
  let (style, extended-style) = frame-window-styles(frame);
  //--- Call compute-default-foreground/background/text-style to
  //--- figure out what characteristics the mirror should have
  let handle :: <HWND>
    = CreateWindowEx
        (extended-style,
	 $dialog-class-name,		// See RegisterClass call
	 title | "",			// Text for window title bar
	 style,
	 x | $CW-USEDEFAULT,		// x position
	 y | $CW-USEDEFAULT,		// y position
	 right - left,			// width
	 bottom - top,			// height
	 owner-handle,			// dialog's owner
	 $null-hMenu,			// Use the window class menu
	 application-instance-handle(),
	 $NULL-VOID);			// No data in our WM_CREATE
  check-result("CreateWindow (dialog)", handle);
  values(handle, #f, <dialog-mirror>, vector(owner:, owner))
end method make-top-level-window;

define sealed method cancel-frame
    (dialog :: <dialog-frame>) => (handled? :: <boolean>)
  let button = dialog-cancel-button(dialog);
  when (button & gadget-enabled?(button))
    handle-gadget-activation(button)
  end
end method cancel-frame;

define sealed method map-mirror
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <dialog-mirror>) => ()
  ensure-dialog-position(sheet-frame(sheet), mirror);
  next-method();
  let dialog = sheet-frame(sheet);
  let owner  = frame-owner(dialog);
  owner & register-dialog-mirror(owner, mirror)
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <motif-port>,
     sheet :: <motif-top-level-sheet-mixin>, mirror :: <dialog-mirror>) => ()
  next-method();
  let dialog = sheet-frame(sheet);
  let owner  = frame-owner(dialog);
  owner & unregister-dialog-mirror(owner, mirror)
end method unmap-mirror;

//---*** We should try and ensure that dialogs are never bigger than
//---*** a standard Motif dialog should be.  Find the reference in
//---*** WIG (or wherever) that describes this.
define sealed method ensure-dialog-position
    (frame :: <dialog-frame>, mirror :: <dialog-mirror>) => ()
  let handle = window-handle(mirror);
  let (x, y) = compute-dialog-position(frame);
  duim-debug-message("Setting position for %= to %d x %d", frame, x, y);
  check-result("SetWindowPos",
	       SetWindowPos(handle, $NULL-HWND, x, y, 0, 0,
			    %logior($SWP-NOACTIVATE, $SWP-NOZORDER, 
				    $SWP-NOSIZE)))
end method ensure-dialog-position;

define sealed method compute-dialog-position
    (frame :: <dialog-frame>)
 => (x :: <integer>, y :: <integer>)
  //--- Is there a better way to get this?  'frame-position' always
  //--- gets the info from the top level sheet which isn't what
  //--- we want here.
  let sheet = top-level-sheet(frame);
  let (width, height) = sheet-size(sheet);
  let geometry = frame-geometry(frame);
  let frame-x = geometry[0];
  let frame-y = geometry[1];
  let frame-width = geometry[2];
  let frame-height = geometry[3];
  let width = frame-width | width;
  let height = frame-height | height;
  if (frame-x & frame-y)
    values(frame-x, frame-y)
  else
    let _display = display(sheet);
    let owner = frame-owner(frame);
    let owner-top-sheet = owner & top-level-sheet(owner);
    let owner-mirror = owner-top-sheet & sheet-direct-mirror(owner-top-sheet);
    let (screen-width, screen-height) = sheet-size(_display);
    if (owner-mirror)
      // Center the dialog over the client area
      let (owner-x, owner-y) = client-to-screen-position(owner-mirror, 0, 0);
      let owner-handle = window-handle(owner-mirror);
      let (owner-width, owner-height) = get-client-size(owner-handle);
      let (x-offset, y-offset) = frame-client-area-offset(owner);
      duim-debug-message("  Owner currently %d x %d, at %d, %d [offset %d x %d]",
			 owner-width, owner-height, owner-x, owner-y,
			 x-offset, y-offset);
      duim-debug-message("  Dialog currently %d x %d",
			 width, height);
      let x
	= max(min(screen-width - width,
		  owner-x + floor/(owner-width  - width, 2)),
	      0);
      let y
	= max(min(screen-height - height,
		  owner-y + max(floor/(owner-height - height, 2), 
				//---*** andrewa: why do I need this 20?
				y-offset - 20)),
	      0);
      values(x, y)
    else
      // Center the dialog on the screen
      values(max(floor/(screen-width  - width, 2),  0),
	     max(floor/(screen-height - height, 2), 0))
    end
  end
end method compute-dialog-position;


/// Piggy-back on the default dialogs from gadget-panes for now

define sealed method frame-wrapper
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>,
     layout :: false-or(<sheet>))
 => (sheet :: false-or(<sheet>))
  default-dialog-frame-wrapper(framem, dialog, layout)
end method frame-wrapper;

define sealed method update-frame-layout
    (framem :: <motif-frame-manager>, frame :: <dialog-frame>) => ()
  update-default-dialog-layout(framem, frame)
end method update-frame-layout;

define method make-exit-button
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>,
     callback :: false-or(<callback-type>), label :: <string>,
     #rest initargs, 
     #key enabled? = (callback ~= #f), #all-keys)
 => (button :: false-or(<push-button>))
  when (callback)
    with-frame-manager (framem)
      apply(make, <push-button>,
	    activate-callback: method (button)
				 let dialog = sheet-frame(button);
				 execute-callback(dialog, callback, dialog)
			       end,
	    label: label,
	    enabled?: enabled?,
	    min-width: $exit-button-min-width,
	    initargs)
    end
  end
end method make-exit-button;

define sealed method default-dialog-border
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  motif-dialog-x-pixels(framem, $dialog-border-base-units)
end method default-dialog-border;

define sealed method default-dialog-spacing
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  motif-dialog-y-pixels(framem, 4)
end method default-dialog-spacing;

define sealed method default-dialog-button-spacing
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  select (dialog-exit-buttons-position(dialog))
    #"left", #"right" => motif-dialog-y-pixels(framem, 3);
    #"top", #"bottom" => motif-dialog-x-pixels(framem, 3);
    otherwise         => motif-dialog-x-pixels(framem, 3);
  end
end method default-dialog-button-spacing;

define sealed method default-dialog-extra-size
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>)
 => (width :: <integer>, height :: <integer>)
  ignore(framem);
  window-frame-extra-size(dialog);
end method default-dialog-extra-size;


/// Dialog flow control

// Generate an ordinary exit event
define sealed method do-exit-dialog
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
  // On Motif, we need to re-enable the owner before we dismiss
  // the dialog so that the focus gets returned to the right place
  let owner  = frame-owner(dialog);
  let modal? = (frame-mode(dialog) == #"modal");
  when (owner & modal?)
    frame-enabled?(owner) := #t
  end;
  frame-mapped?(dialog) := #f;
  distribute-event(port(dialog),
		   make(<dialog-exit-event>,
			frame: dialog,
			destroy-frame?: destroy?))
end method do-exit-dialog;

// Generate an "error" exit event
define sealed method do-cancel-dialog 
    (framem :: <motif-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
  let owner  = frame-owner(dialog);
  let modal? = (frame-mode(dialog) == #"modal");
  when (owner & modal?)
    frame-enabled?(owner) := #t
  end;
  frame-mapped?(dialog) := #f;
  distribute-event(port(dialog),
		   make(<dialog-cancel-event>,
			frame: dialog,
			destroy-frame?: destroy?))
end method do-cancel-dialog;


/// Utilities for the built-in dialogs

define sealed method dialog-owner-handle
    (owner :: <sheet>) => (handle :: <HWND>)
  window-handle(owner)
end method dialog-owner-handle;

define sealed method dialog-owner-handle
    (owner :: <display>) => (handle :: <HWND>)
  $NULL-HWND
end method dialog-owner-handle;


/// Notify user

define sealed method do-notify-user
    (framem :: <motif-frame-manager>, owner :: <sheet>,
     message :: <string>, style :: <notification-style>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), name, 
	  exit-style :: false-or(<notification-exit-style>) = #f,
     #all-keys)
 => (ok? :: <boolean>, exit-type)
  let _port     = port(owner);
  let x-display = _port.%display;
  let parent-widget = mirror-widget(sheet-mirror(owner));
  let (visual, colormap, depth) = xt/widget-visual-specs(parent-widget);
  let title
    = title | select (style)
		#"information"   => "Note";
		#"question"      => "Note";
		#"warning"       => "Warning";
		#"error"         => "Error";
		#"serious-error" => "Error";
		#"fatal-error"   => "Error";
	      end;
  let dialog-type
    = select (style)
	#"information"   => $XmDIALOG-INFORMATION;
	#"question"      => $XmDIALOG-QUESTION;
	#"warning"       => $XmDIALOG-WARNING;
	#"error"         => $XmDIALOG-ERROR;
	#"serious-error" => $XmDIALOG-ERROR;
	#"fatal-error"   => $XmDIALOG-ERROR;
      end;
  let exit-style
    = exit-style | if (style == #"question") #"yes-no" else #"ok" end;
  let modality
    = select (style)
	#"serious-error" => xm/$XmDIALOG-FULL-APPLICATION-MODAL;
	#"fatal-error"   => xm/$XmDIALOG-SYSTEM-MODAL;
	otherwise        => xm/$XmDIALOG-APPLICATION-MODAL;
      end;
  let shell-resources
    = vector(visual:, visual,
	     colormap:, colormap,
	     depth:, depth);
  let resources
    = vector(dialog-type:, dialog-type,
	     dialog-style:, modality,
	     dialog-title:, title, 
	     message-string:, message,
	     default-position:, #f);
  select (exit-style)
    #"ok"            =>
      resources
	:= concatenate!(resources, vector(ok-label-string:,     "OK",
					  default-button-type:, xm/$XmDIALOG-OK-BUTTON));
    #"ok-cancel"     =>
      resources
	:= concatenate!(resources, vector(ok-label-string:,     "OK",
					  cancel-label-string:, "Cancel",
					  default-button-type:, xm/$XmDIALOG-OK-BUTTON));
    #"yes-no"        =>
      resources
	:= concatenate!(resources, vector(ok-label-string:,     "Yes",
					  cancel-label-string:, "No",
					  default-button-type:, xm/$XmDIALOG-OK-BUTTON));
    #"yes-no-cancel" =>
      resources
	:= concatenate!(resources, vector(ok-label-string:,     "Yes",
					  cancel-label-string:, "No",
					  default-button-type:, xm/$XmDIALOG-OK-BUTTON));
  end;
  let (x, y)
    = begin
	let (x, y) = sheet-size(owner);
	let (x, y) = values(floor/(x, 2), floor/(y, 2));
	with-device-coordinates (sheet-device-transform(owner), x, y)
	  values(x, y)
	end
      end;
  let shell  = #f;
  let dialog = #f;
  let result = #f;
  let client-data  = #f;
  block ()
    local method waiter () result end method,
	  method setter (value) result := value end method;
    shell  := xt/XtCreatePopupShell("NotifyUserShell", xm/<dialog-shell>, parent-widget,
				   resources: shell-resources);
    dialog := xm/XmCreateMessageBox(shell, "NotifyUser",
				    resources: resources);
    client-data := make(<callback-client-data>,
			owner-widget: parent-widget,
			x-display: x-display,
			pointer-x: x,
			pointer-y: y,
			setter: setter);
    xt/XtAddCallback(dialog, "okCallback", notifier-button-press-callback, client-data);
    if (exit-style == #"question")
      xt/XtUnmanageChild(xm/XmMessageBoxGetChild(dialog, xm/$XmDIALOG-CANCEL-BUTTON))
    else
      xt/XtAddCallback(dialog, "cancelCallback", notifier-button-press-callback, client-data)
    end;
    xt/XtUnmanageChild(xm/XmMessageBoxGetChild(dialog, xm/$XmDIALOG-HELP-BUTTON))
    xt/XtAddCallback(dialog, "mapCallback", notifier-map-callback, client-data);
    xm/XmAddWmProtocolCallback(xt/XtParent(dialog), "wmDeleteWindow", notifier-delete-window-callback, setter);
    xt/XtManageChild(dialog);
    //---*** CLIM does this: '(mp:process-wait "Waiting for CLIM:NOTIFY-USER" #'waiter)'
    select (result)
      #"yes"    => values(#t, #"yes");
      #"no"     => values(#f, #"no");
      #"ok"     => values(#t, #"ok");
      #"cancel" => values(#f, #"cancel");
      otherwise => error("Unexpected return code %= from MessageBox", result);
    end;
  cleanup
    when (dialog)
      x/XSync(x-display, #f);
      xt/XtUnmanageChild(dialog);
      xt/XtDestroyWidget(shell)
    end;
  end
end method do-notify-user;

define xm/xm-callback-function notifier-button-press-callback
    (widget, client-data :: <callback-client-data>, call-data :: xm/<XmPushButtonCallbackStruct>)
  ignore(widget);
  client-data.%setter(call-data.xm/reason-value)
end xm/xm-callback-function notifier-button-press-callback;

define xm/xm-callback-function notifier-delete-window-callback
    (widget, value-setter, call-data)
  ignore(widget, call-data);
  value-setter(#"cancel")
end xm/xm-callback-function notifier-delete-window-callback;

define xm/xm-callback-function notifier-map-callback
    (widget, client-data :: <callback-client-data>, call-data)
  ignore(call-data);
  let owner-widget = client-data.%owner-widget;
  let lx = client-data.%x;
  let ly = client-data.%y;
  let (width, height) = xt/XtGetValues(widget, #"width", #"height");
  let (rx, ry) = xt/XtTranslateCoords(owner-widget, lx, ly);
  let (rwidth, rheight)
    = values(x/XWidthOfScreen(xt/XtScreen(widget)), x/XHeightOfScreen(xt/XtScreen(widget)));
  rx := rx - round/(width, 2);
  ry := ry - round/(height, 2);
  xt/XtSetValues(widget,
		 x: max(0, min(rx, rwidth  - width)),
		 y: max(0, min(ry, rheight - height)))
end xm/xm-callback-function notifier-map-callback;


/// Choose file

define sealed method do-choose-file
    (framem :: <motif-frame-manager>, owner :: <sheet>, 
     direction :: one-of(#"input", #"output"),
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  if-exists, if-does-not-exist = #"ask",
	  default :: false-or(<string>), default-type = $unsupplied,
	  filters, default-filter,
     #all-keys)
 => (locator :: false-or(<string>), filter :: false-or(<integer>))
  ignore(if-exists);
  let _port     = port(owner);
  let x-display = _port.%display;
  let parent-widget = mirror-widget(sheet-mirror(owner));
  let (visual, colormap, depth) = xt/widget-visual-specs(parent-widget);
  let (directory, pattern) = motif-directory-and-pattern(default, default-type);
  let shell-resources
    = vector(visual:, visual,
	     colormap:, colormap,
	     depth:, depth);
  let resources
    = vector(directory:, directory,
	     pattern:, pattern,
	     dialog-title:, title, 
	     dialog-style:, xm/$XmDIALOG-FULL-APPLICATION-MODAL,
	     default-position:, #f);

  when (file-label)
    resources
      := concatenate!(resources, vector(file-list-label-string:, file-label))
  end;
  when (directory-label)
    resources
      := concatenate!(resources, vector(dir-list-label-string:, directory-label))
  end;
  let (x, y)
    = begin
	let (x, y) = sheet-size(owner);
	let (x, y) = values(floor/(x, 2), floor/(y, 2));
	with-device-coordinates (sheet-device-transform(owner), x, y)
	  values(x, y)
	end
      end;
  let shell  = #f;
  let dialog = #f;
  let result = #f;
  let client-data  = #f;
  block ()
    local method waiter () result end method,
	  method setter (value) result := value end method;
    shell  := xt/XtCreatePopupShell("ChooseFileShell", xm/<dialog-shell>, parent-widget,
				   resources: shell-resources);
    dialog := xm/XmCreateFileSelectionBox(shell, "ChooseFile",
					  resources: resources);
    client-data := make(<callback-client-data>,
			owner-widget: parent-widget,
			x-display: x-display,
			pointer-x: x,
			pointer-y: y,
			setter: setter);
    xt/XtAddCallback(dialog, "okCallback",     choose-file-button-press-callback, client-data);
    xt/XtAddCallback(dialog, "cancelCallback", choose-file-button-press-callback, client-data)
    xt/XtUnmanageChild(xm/XmFileSelectionBoxGetChild(dialog, xm/$XmDIALOG-HELP-BUTTON))
    xt/XtAddCallback(dialog, "mapCallback", notifier-map-callback, client-data);
    xm/XmAddWmProtocolCallback(xt/XtParent(dialog), "wmDeleteWindow", notifier-delete-window-callback, setter);
    xt/XtManageChild(dialog);
    //---*** CLIM does this: '(mp:process-wait "Waiting for CLIM:SELECT-FILE" #'waiter)'
    if (result == #"cancel")
      values(#f, #f)
    else
      values(result, #f)
    end
  cleanup
    when (dialog)
      x/XSync(x-display, #f);
      xt/XtUnmanageChild(dialog);
      xt/XtDestroyWidget(shell)
    end;
  end
end method do-choose-file;

define xm/xm-callback-function choose-file-button-press-callback
    (widget, client-data :: <callback-client-data>, call-data :: xm/<XmFileSelectionBoxCallbackStruct>)
  ignore(widget);
  select (call-data.xm/reason-value)
    $XmCR-OK  =>
      let filename = xm/XmStringGetLToR(call-data.xm/value-value);
      client-data.%setter(filename);
    otherwise =>
      client-data.%setter(#"cancel");
  end
end xm/xm-callback-function choose-file-button-press-callback;

define function motif-directory-and-pattern
    (default :: false-or(<string>), default-type)
 => (directory :: false-or(<string>), pattern :: false-or(<string>))
  if (~default)
    values(#f, #f)
  else
    let type
      = select (type by instance?)
	  <string> =>
	    type;
	  <symbol> =>
	    gethash($file-type-table, type) | as-lowercase(as(<string>, type));
	  otherwise =>
	    "";
	end;
    let default    = as(<file-locator>, default);
    let directory  = as(<string>, locator-directory(default));
    let wild-name? = (locator-base(default) = "*");
    let wild-type? = (locator-extension(default) = "*");
    let pattern    = when (wild-name? ~== wild-type?)
		       as(<string>, make(<file-locator>, 
					 base: locator-base(default),
					 extension: locator-extension(default)))
		     end;
    values(directory, pattern)
  end
end function motif-directory-and-pattern;

//---*** This should be in Locators or File-System
define table $file-type-table :: <table>
  = { #"dylan"       => "dylan",
      #"c"           => "c",
      #"c-include"   => "h",
      #"cpp"         => "cpp",
      #"cpp-include" => "hpp",
      #"text"        => "text",
      #"project"     => "hdp",
      #"lid"         => "lid",
      #"executable"  => "exe",
      #"resource"    => "res",
      #"library"     => "lib" };

 
/// Choose directory

define variable $Shell-IMalloc :: false-or(<C-Interface>) = #f;

define function get-shell-IMalloc () => (IMalloc :: false-or(<C-Interface>))
  unless ($Shell-IMalloc)
    let (result, IMalloc) = SHGetMalloc();
    when (result = $NOERROR)
      $Shell-IMalloc := IMalloc
    end
  end;
  $Shell-IMalloc
end function get-shell-IMalloc;

define sealed method do-choose-directory
    (framem :: <motif-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default :: false-or(<string>),
     #all-keys)
 => (locator :: false-or(<string>))
  let locator = #f;
  let shell-IMalloc = get-shell-IMalloc();
  when (shell-IMalloc)
    let handle = dialog-owner-handle(owner);
    // "C:\" and "C:\WINDOWS" are valid paths, but "C:\WINDOWS\"
    // is not.  If we pass in an invalid path, the dialog silently
    // ignores us.  For simplicity, just strip off any trailing '\\'.
    when (default)
      let _size = size(default);
      let _end  = if (element(default, _size - 1, default: #f) = '\\') _size - 1
		  else _size end;
      default := copy-sequence(default, end: _end)
    end;
    with-stack-structure (bi :: <LPBROWSEINFO>)
      with-stack-structure (buffer :: <C-string>, size: $MAX-PATH)
	title   := if (title) as(<C-string>, copy-sequence(title))
		   else $NULL-string end;
        default := if (default) as(<C-string>, default)
		   else $NULL-string end;
	bi.hwndOwner-value := handle;
	bi.pidlRoot-value  := null-pointer(<LPCITEMIDLIST>);
	bi.pszDisplayName-value := buffer;
	bi.lpszTitle-value := title;
	bi.ulFlags-value   := $BIF-RETURNONLYFSDIRS;
	bi.lpfn-value      := browse-for-folder;	// see below
	bi.lParam-value    := pointer-address(default);
	bi.iImage2-value   := 0;
	let pidlBrowse = SHBrowseForFolder(bi);
	when (SHGetPathFromIDList(pidlBrowse, buffer))
	  locator := as(<byte-string>, buffer)
	end;
	IMalloc/Free(shell-IMalloc, pidlBrowse); 
        unless (default = $NULL-string) destroy(default) end;
        unless (title   = $NULL-string) destroy(title)   end;
      end
    end
  end;
  locator
end method do-choose-directory;

// This callback allows the dialog to open with its selection set to
// the 'default:' passed in to 'do-choose-directory' 
define sealed method browse-for-folder-function
    (handle :: <HWND>,		// window handle
     message :: <message-type>,	// type of message
     lParam  :: <wparam-type>,	// additional information
     lpData  :: <lparam-type>)	// additional information
 => (result :: <lresult-type>)
  ignore(lParam);
  when (message = $BFFM-INITIALIZED)
    PostMessage(handle, $BFFM-SETSELECTION, 1, lpData)
  end;
  0
end method browse-for-folder-function;

define callback browse-for-folder :: <LPBFFCALLBACK> = browse-for-folder-function;

 
/// Color chooser

define variable *custom-colors* :: <LPCOLORREF>
    = make(<LPCOLORREF>, element-count: 16);

define sealed method do-choose-color
    (framem :: <motif-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default :: false-or(<color>),
     #all-keys)
 => (color :: false-or(<color>));
  let handle = dialog-owner-handle(owner);
  with-stack-structure (color :: <LPCHOOSECOLOR>)
    color.lStructSize-value  := safe-size-of(<CHOOSECOLOR>);
    color.hwndOwner-value    := handle;
    color.hInstance-value    := application-instance-handle();
    color.rgbResult-value    := if (default) %color->native-color(default)
				else $native-black end;
    color.Flags-value        := %logior($CC-ANYCOLOR,
					if (default) $CC-RGBINIT else 0 end,
					$CC-SHOWHELP);
    color.lpCustColors-value := *custom-colors*;
    if (ChooseColor(color))
      let colorref = color.rgbResult-value;
      %native-color->color(colorref)
    else
      ensure-no-dialog-error("ChooseColor")
    end
  end
end method do-choose-color;


/// Text style chooser

define sealed method do-choose-text-style
    (framem :: <motif-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default :: false-or(<text-style>),
     #all-keys)
 => (text-style :: false-or(<text-style>));
  let _port = port(framem);
  let hDC :: <hDC> = _port.%memory-hDC;
  let handle = dialog-owner-handle(owner);
  with-stack-structure (logfont :: <LPLOGFONT>)
    with-stack-structure (cf :: <LPCHOOSEFONT>)
      cf.lStructSize-value := safe-size-of(<CHOOSEFONT>);
      cf.hwndOwner-value   := handle;
      cf.hInstance-value   := application-instance-handle();
      cf.hDC-value         := hDC;
      cf.lpLogFont-value   := logfont;
      cf.Flags-value       := %logior($CF-SHOWHELP,
				      $CF-FORCEFONTEXIST,
				      $CF-SCREENFONTS,
				      $CF-EFFECTS);
      cf.lpszStyle-value   := $NULL-string;
      if (ChooseFont(cf))
	make-text-style-from-font(_port, logfont)
      else
	ensure-no-dialog-error("ChooseFont")
      end
    end
  end
end method do-choose-text-style;
