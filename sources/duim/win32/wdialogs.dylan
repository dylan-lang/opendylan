Module:    win32-duim
Synopsis:  Win32 dialog implementation
Author:    Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic Win32 constants

//---*** These should be computed
// See WIG, pg. 388
define constant $dialog-border-base-units :: <integer> =  3;

define constant $exit-button-min-width    :: <integer> = 75;


/// DUIM dialogs

define sealed class <dialog-mirror> (<top-level-mirror>)
  sealed constant slot %owner :: false-or(<frame>),
    required-init-keyword: owner:;
end class <dialog-mirror>;

define sealed domain make (singleton(<dialog-mirror>));
define sealed domain initialize (<dialog-mirror>);

define method frame-window-styles
    (frame :: <dialog-frame>)
 => (style :: <unsigned-int>, extended-style :: <unsigned-int>)
  //---*** For some reason making a dialog with no title doesn't work,
  //---*** but then screws up the layout calculations. So until it works
  //---*** we'll just always supply a title.
  let title? = #t;	//--- was 'frame-title(frame)';
  let style
    = %logior($WS-OVERLAPPED,
	      if (frame-minimize-box?(frame)) %logior($WS-SYSMENU, $WS-MINIMIZEBOX) else 0 end,
	      if (frame-maximize-box?(frame)) %logior($WS-SYSMENU, $WS-MAXIMIZEBOX) else 0 end,
	      if (title?) $WS-CAPTION else 0 end,
	      if (frame-resizable?(frame)) $WS-SIZEBOX else 0 end);
  let extended-style
    = %logior($WS-EX-DLGMODALFRAME,
	      if (frame-always-on-top?(frame)) $WS-EX-TOPMOST else 0 end);
  values(style, extended-style)
end method frame-window-styles;

define sealed method make-top-level-window
    (frame :: <dialog-frame>, sheet :: <win32-top-level-sheet>,
     resource-id == #f)
 => (handle :: <HWND>, resource :: singleton(#f),
     mirror-class :: <class>, mirror-initargs)
  make-dialog-top-level-window(frame, sheet)
end method make-top-level-window;

// Disambiguating method
define sealed method make-top-level-window
    (frame :: <dialog-frame>, sheet :: <win32-embedded-top-level-sheet>,
     resource-id == #f)
 => (handle :: <HWND>, resource :: singleton(#f),
     mirror-class :: <class>, mirror-initargs)
  make-dialog-top-level-window(frame, sheet)
end method make-top-level-window;

define sealed method make-dialog-top-level-window
    (frame :: <dialog-frame>, sheet :: <win32-top-level-sheet-mixin>)
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
end method make-dialog-top-level-window;

define sealed method update-dialog-resource-ids
    (dialog :: <dialog-frame>) => ()
/*---*** Should be handled more directly now
  let exit-button = dialog-exit-button(dialog);
  exit-button & (gadget->id(exit-button) := $IDOK);
  let cancel-button = dialog-cancel-button(dialog);
  cancel-button & (gadget->id(cancel-button) := $IDCANCEL);
*/
  let help-button = dialog-help-button(dialog);
  help-button & (gadget->id(help-button) := $IDHELP);
end method update-dialog-resource-ids;

define sealed method cancel-frame
    (dialog :: <dialog-frame>) => (handled? :: <boolean>)
  let button = dialog-cancel-button(dialog);
  when (button & gadget-enabled?(button))
    handle-gadget-activation(button)
  end
end method cancel-frame;

define sealed method map-mirror
    (_port :: <win32-port>,
     sheet :: <win32-top-level-sheet-mixin>, mirror :: <dialog-mirror>) => ()
  ensure-dialog-position(sheet-frame(sheet), mirror);
  next-method();
  let dialog = sheet-frame(sheet);
  let owner = frame-owner(dialog);
  update-dialog-resource-ids(dialog);
  owner & register-dialog-mirror(owner, mirror)
end method map-mirror;

define sealed method unmap-mirror
    (_port :: <win32-port>,
     sheet :: <win32-top-level-sheet-mixin>, mirror :: <dialog-mirror>) => ()
  next-method();
  let dialog = sheet-frame(sheet);
  let owner = frame-owner(dialog);
  owner & unregister-dialog-mirror(owner, mirror)
end method unmap-mirror;

//---*** We should try and ensure that dialogs are never bigger than
//---*** a standard Windows dialog should be.  Find the reference in
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
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>,
     layout :: false-or(<sheet>))
 => (sheet :: false-or(<sheet>))
  default-dialog-frame-wrapper(framem, dialog, layout)
end method frame-wrapper;

define sealed method update-frame-layout
    (framem :: <win32-frame-manager>, frame :: <dialog-frame>) => ()
  update-default-dialog-layout(framem, frame)
end method update-frame-layout;

define method make-exit-button
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>,
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
            fixed-width?: #t,
	    width: $exit-button-min-width,
	    initargs)
    end
  end
end method make-exit-button;

define sealed method default-dialog-border
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  win32-dialog-x-pixels(framem, $dialog-border-base-units)
end method default-dialog-border;

define sealed method default-dialog-spacing
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  win32-dialog-y-pixels(framem, 4)
end method default-dialog-spacing;

define sealed method default-dialog-button-spacing
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>)
 => (border :: <integer>)
  select (dialog-exit-buttons-position(dialog))
    #"left", #"right" => win32-dialog-y-pixels(framem, 3);
    #"top", #"bottom" => win32-dialog-x-pixels(framem, 3);
    otherwise         => win32-dialog-x-pixels(framem, 3);
  end
end method default-dialog-button-spacing;

define sealed method default-dialog-extra-size
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>)
 => (width :: <integer>, height :: <integer>)
  ignore(framem);
  window-frame-extra-size(dialog)
end method default-dialog-extra-size;


/// Dialog flow control

// Generate an ordinary exit event
define sealed method do-exit-dialog
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
  // On Windows, we need to re-enable the owner before we dismiss
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
    (framem :: <win32-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
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
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     message :: <string>, style :: <notification-style>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), name, 
	  exit-style :: false-or(<notification-exit-style>) = #f,
     #all-keys)
 => (ok? :: <boolean>, exit-type)
  let handle = dialog-owner-handle(owner);
  let title
    = title | select (style)
		#"information"   => "Note";
		#"question"      => "Note";
		#"warning"       => "Warning";
		#"error"         => "Error";
		#"serious-error" => "Error";
		#"fatal-error"   => "Error";
	      end;
  let style-flag
    = select (style)
	#"information"   => $MB-ICONINFORMATION;
	#"question"      => $MB-ICONQUESTION;
	#"warning"       => $MB-ICONWARNING;
	#"error"         => $MB-ICONERROR;
	#"serious-error" => $MB-ICONERROR;
	#"fatal-error"   => $MB-ICONERROR;
      end;
  let button-flag
    = if (exit-style)
	select (exit-style)
	  #"ok"            => $MB-OK;
	  #"ok-cancel"     => $MB-OKCANCEL;
	  #"yes-no"        => $MB-YESNO;
	  #"yes-no-cancel" => $MB-YESNOCANCEL;
	end
      else
	select (style)
	  #"question" => $MB-YESNO;
	  otherwise   => $MB-OK;
	end
      end;
  let modality
    = select (style)
	#"serious-error" => $MB-TASKMODAL;
	#"fatal-error"   => $MB-SYSTEMMODAL;
	otherwise        => $MB-APPLMODAL;
      end;
  let flags  = %logior(style-flag, button-flag, modality, $MB-SETFOREGROUND);
  let result = MessageBox(handle, message, title, flags);
  select (result)
    $IDYES    => values(#t, #"yes");
    $IDNO     => values(#f, #"no");
    $IDOK     => values(#t, #"ok");
    $IDCANCEL => values(#f, #"cancel");
    otherwise => error("Unexpected return code %= from MessageBox", result);
  end;
end method do-notify-user;


/// Choose file

define constant $max-file-name-length :: <integer> = 1000;

define sealed method do-choose-file
    (framem :: <win32-frame-manager>, owner :: <sheet>, 
     direction == #"input",
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  if-exists, if-does-not-exist = #"ask",
	  default :: false-or(<string>), default-type = $unsupplied,
	  filters, default-filter, selection-mode = #"single",
     #all-keys)
 => (locator :: false-or(type-union(<string>, <sequence>)),
     filter :: false-or(<integer>))
  ignore(if-exists, exit-boxes);
  let handle = dialog-owner-handle(owner);
  let bufsiz = $max-file-name-length * (if (selection-mode == #"multiple") 2 else 1 end);
  with-stack-structure (buffer :: <C-string>, size: bufsiz)
    with-stack-structure (file :: <LPOPENFILENAME>)
      init-open-file-name(file, handle, buffer, bufsiz,
			  direction: direction,
			  selection-mode: selection-mode,
			  if-does-not-exist: if-does-not-exist,
			  default: default,
			  default-type: default-type,
			  filters: filters,
			  default-filter: default-filter,
			  title: title);
      let result = GetOpenFileName(file);
      deinit-open-file-name(file);
      if (result)
	values(parse-file-name-buffer(buffer, bufsiz, file.nFileOffset-value, selection-mode),
	       file.nFilterIndex-value - 1)
      else
	values(ensure-no-dialog-error("GetOpenFileName"),
	       file.nFilterIndex-value - 1)
      end
    end
  end
end method do-choose-file;

define sealed method do-choose-file
    (framem :: <win32-frame-manager>, owner :: <sheet>, 
     direction == #"output",
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  if-exists = #"ask", if-does-not-exist,
	  default :: false-or(<string>), default-type = $unsupplied,
	  filters, default-filter, selection-mode = #"single",
     #all-keys)
 => (locator :: false-or(type-union(<string>, <sequence>)),
     filter :: false-or(<integer>))
  ignore(if-does-not-exist, exit-boxes);
  let handle = dialog-owner-handle(owner);
  let bufsiz = $max-file-name-length * (if (selection-mode == #"multiple") 2 else 1 end);
  with-stack-structure (buffer :: <C-string>, size: bufsiz)
    with-stack-structure (file :: <LPOPENFILENAME>)
      init-open-file-name(file, handle, buffer, bufsiz,
			  direction: direction,
			  selection-mode: selection-mode,
			  if-exists: if-exists,
			  default: default,
			  default-type: default-type,
			  filters: filters,
			  default-filter: default-filter,
			  title: title);
      let result = GetSaveFileName(file);
      deinit-open-file-name(file);
      if (result)
	values(parse-file-name-buffer(buffer, bufsiz, file.nFileOffset-value, selection-mode),
	       file.nFilterIndex-value - 1)
      else
	values(ensure-no-dialog-error("GetSaveFileName"),
	       file.nFilterIndex-value - 1)
      end
    end
  end
end method do-choose-file;

define function parse-file-name-buffer
    (buffer :: <C-string>, buffer-size :: <integer>, offset :: <integer>, selection-mode)
 => (locator :: type-union(<string>, <sequence>))
  select (selection-mode)
    #"single" =>
      as(<byte-string>, buffer);
    #"multiple" =>
      local method copy-substring	// like 'copy-sequence-as'...
		(buffer :: <C-string>, _start :: <integer>, _end :: <integer>)
	     => (string :: <byte-string>)
	      let string :: <byte-string> = make(<byte-string>, size: _end - _start);
	      without-bounds-checks
		for (i :: <integer> from _start below _end,
		     j :: <integer> from 0)
		  string[j] := buffer[i]
		end
	      end;
	      string
	    end method,
	    method find-null		// like 'position'...
		(buffer :: <C-string>, _start :: <integer>, _end :: <integer>)
	     => (index :: false-or(<integer>))
	      block (return)
		without-bounds-checks
		  for (i :: <integer> = _start then i + 1,
		       until: i = _end)
		    when (buffer[i] == '\0')
		      return(i)
		    end
		  end
	        end;
	        #f
	      end
	    end method;
      let locators  = make(<stretchy-vector>);
      let directory = copy-substring(buffer, 0, offset);
      directory[size(directory) - 1] := '\\';
      let i :: <integer> = offset;
      block (break)
	while (i < buffer-size)
	  let j = find-null(buffer, i, buffer-size);
	  when (~j | j = i + 1) break() end;	// two nulls means we're done
	  let name = copy-substring(buffer, i, j);
	  add!(locators, concatenate(directory, name));
	  i := j + 1
	end
      end;
      locators;
  end
end function parse-file-name-buffer;

define sealed method init-open-file-name
    (file :: <LPOPENFILENAME>, handle :: <HWND>,
     buffer :: <C-string>, buffer-size :: <integer>,
     #key direction = #"input", title :: false-or(<string>),
	  if-exists = #"ask", if-does-not-exist = #"ask",
	  default :: false-or(<string>), default-type = $unsupplied,
	  filters, default-filter, selection-mode = #"single") => ()
  file.lStructSize-value := safe-size-of(<OPENFILENAME>);
  file.hwndOwner-value   := handle;
  file.hInstance-value   := application-instance-handle();
  // Process the filters, if any
  let filter-value :: <C-string>
    = as(<C-string>, "All Files (*.*)\0*.*\0");
  when (filters)
    let n :: <integer> = 0;
    for (filter in filters)
      inc!(n, reduce(method(x, elt) x + size(elt) + 1 end, 0, filter))
    end;
    filter-value := make(<C-string>, size: n);
    without-bounds-checks
      let i :: <integer> = 0;
      for (filter in filters)
	for (string :: <byte-string> in filter,
	     name? = #t then #f)
	  for (j :: <integer> from 0 below size(string))
	    filter-value[i] := string[j];
	    inc!(i)
	  end;
	  filter-value[i] := if (name?) '\0' else ';' end;
	  inc!(i)
	end;
	filter-value[i - 1] := '\0';
      end
    end
  end;
  // Process default name and type
  // 'deinit-open-file-name' will take care of releasing these
  let (default-dir :: <C-string>, default-name :: <C-string>)
    = process-default-name(default);
  let default-type :: <C-string>
    = process-default-type(default-type);
  // Set up filters, defaults, etc
  let flags
    = %logior($OFN-HIDEREADONLY,	//---*** use $OFN-SHOWHELP someday...
	      $OFN-EXPLORER,
	      if (selection-mode == #"multiple") $OFN-ALLOWMULTISELECT else 0 end);
  let direction-flags
    = select (direction)
	#"input"  => if (if-exists == #"ask")         $OFN-FILEMUSTEXIST   else 0 end;
	#"output" => if (if-does-not-exist == #"ask") $OFN-OVERWRITEPROMPT else 0 end;
      end;
  file.Flags-value             := %logior(flags, direction-flags);
  file.lpstrFilter-value       := filter-value;
  file.nFilterIndex-value      := if (default-filter) default-filter + 1 else 1 end;
  file.lpstrCustomFilter-value := $NULL-string;
  file.nMaxCustFilter-value    := 0;
  file.lpstrFile-value         := buffer;
  file.lpstrFile-value[0]      := as(<character>, 0);
  file.nMaxFile-value          := buffer-size;
  file.lpstrFileTitle-value    := $NULL-string;
  file.nMaxFileTitle-value     := 0;
  file.lpstrInitialDir-value   := default-dir;
  file.lpstrDefExt-value       := default-type;
  file.lpstrTitle-value        := if (title) as(<C-string>, title) else $NULL-string end;
  file.nFileOffset-value       := 0;
  file.nFileExtension-value    := 0;
  when (default-name ~= $NULL-string)
    without-bounds-checks
      for (i :: <integer> from 0 below size(default-name))
	file.lpstrFile-value[i] := default-name[i]
      end;
      file.lpstrFile-value[size(default-name)] := as(<character>, 0)
    end;
    destroy(default-name)	// all done with this now
  end
end method init-open-file-name;

// Destroy helper C strings...
define sealed method deinit-open-file-name
    (file :: <LPOPENFILENAME>) => ()
  let filter = file.lpstrFilter-value;
  let dir    = file.lpstrInitialDir-value;
  let type   = file.lpstrDefExt-value;
  let title  = file.lpstrTitle-value;
  file.lpstrFilter-value     := $NULL-string;
  file.lpstrInitialDir-value := $NULL-string;
  file.lpstrDefExt-value     := $NULL-string;
  file.lpstrTitle-value      := $NULL-string;
  unless (filter = $NULL-string) destroy(filter) end;
  unless (dir    = $NULL-string) destroy(dir)    end;
  unless (type   = $NULL-string) destroy(type)   end;
  unless (title  = $NULL-string) destroy(title)  end;
end method deinit-open-file-name;

define function process-default-name
    (name :: false-or(<string>))
 => (name :: <C-string>, dir :: <C-string>)
  if (name)
    let backslash = #f;
    without-bounds-checks
      for (i :: <integer> from size(name) - 1 to 0 by -1,
	   until: name[i] = '\\')
      finally backslash := i;
      end
    end;
    if (backslash)
      values(as(<C-string>, copy-sequence(name, end: backslash + 1)),
	     as(<C-string>, copy-sequence(name, start: backslash + 1)))
    else
      values($NULL-string, as(<C-string>, name))
    end
  else
    values($NULL-string, $NULL-string)
  end
end function process-default-name;

define function process-default-type
    (type) => (type :: <C-string>)
  select (type by instance?)
    <string> =>
      as(<C-string>, type);
    <symbol> =>
      let type = gethash($file-type-table, type) | as-lowercase(as(<string>, type));
      as(<C-string>, type);
    singleton($unsupplied) =>
      as(<C-string>, "\0");
    otherwise =>
      $NULL-string;
  end
end function process-default-type;

//---*** This should be in Locators or File-System
define table $file-type-table :: <object-table>
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
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default :: false-or(<string>),
     #all-keys)
 => (locator :: false-or(<string>))
  ignore(exit-boxes);
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
	title   := as(<C-string>, copy-sequence(title | ""));
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
    (handle :: <HWND>,			// window handle
     message :: <message-type>,		// type of message
     lParam  :: <wparam-type>,		// additional information
     lpData  :: <lparam-type>)		// additional information
 => (result :: <lresult-type>)
  ignore(lParam);
  when (message = $BFFM-INITIALIZED & ~zero?(lpData))
    PostMessage(handle, $BFFM-SETSELECTION, 1, lpData)
  end;
  0
end method browse-for-folder-function;

define callback browse-for-folder :: <LPBFFCALLBACK> = browse-for-folder-function;


/// Printer chooser

define variable *printer-device-mode*  :: <LPDEVMODE>  = make(<LPDEVMODE>);

// Values are (printer, n-copies :: <integer>, print-to-file? :: <boolean>)
// when choosing a printer, or no values when doing print setup
//---*** This doesn't seem right!
define sealed method do-choose-printer
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default, setup?,
     #all-keys)
 => (#rest values);
  ignore(exit-boxes);
  let handle = dialog-owner-handle(owner);
  with-stack-structure (print :: <LPPRINTDLG>)
    print.lStructSize-value  := safe-size-of(<PRINTDLG>);
    print.hwndOwner-value    := handle;
    print.hInstance-value    := application-instance-handle();
    print.hDevMode-value     := *printer-device-mode*;	//--- use 'null-pointer(<HGLOBAL>)'?
    print.hDevNames-value    := null-pointer(<HGLOBAL>);
    print.Flags-value        := %logior($PD-ALLPAGES,
					$PD-COLLATE,
					if (setup?) $PD-PRINTSETUP else 0 end,
					$PD-USEDEVMODECOPIES,
					$PD-SHOWHELP);
    print.nCopies-value      := 1;
    if (PrintDlg(print))
      if (setup?)
	values()
      else
	if (~zero?(logand(print.Flags-value, $PD-PRINTTOFILE)))
	  values(#f, print.nCopies-value, #t)
	else
	  values(#f, print.nCopies-value, #f)
	end	
      end
    else
      ensure-no-dialog-error("PrintDlg");
      values(#f, 0, #f)
    end
  end
end method do-choose-printer;

 
/// Color chooser

define variable *custom-colors* :: <LPCOLORREF>
    = make(<LPCOLORREF>, element-count: 16);

define sealed method do-choose-color
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), exit-boxes,
	  default :: false-or(<color>),
     #all-keys)
 => (color :: false-or(<color>));
  ignore(exit-boxes);
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

// hughg, 1999/08/02: References to "($LF-FACESIZE - 1)" in this function
// really are correct: that constant refers to the number of characters
// allowed for the "face name" *including* the null terminator.
define sealed method do-choose-text-style
    (framem :: <win32-frame-manager>, owner :: <sheet>,
     #key title :: false-or(<string>), documentation :: false-or(<string>),
	  exit-boxes, default :: false-or(<text-style>),
	
	  fixed-width-only? :: <boolean>,
	  show-help? :: <boolean>, show-apply? :: <boolean>,
	  choose-character-set? :: <boolean>, choose-effects? :: <boolean>,
     #all-keys)
 => (text-style :: false-or(<text-style>));
  ignore(exit-boxes, show-apply?);
  let _port = port(framem);
  let hDC :: <hDC> = _port.%memory-hDC;
  let handle = dialog-owner-handle(owner);
  with-stack-structure (logfont :: <LPLOGFONT>)
    when (default)
      // ---*** hughg, 1999/08/02: Note that I've hardwired the charcter-set
      // in the following call to #"ANSI", which should get reasonable results
      // (for English-speaking setups) but probably isn't the Right Thing.
      let (height :: <integer>,
	   width :: <integer>,
	   escapement :: <integer>,
	   orientation :: <integer>,
	   weight :: <integer>,
	   italic :: <integer>,
	   underline :: <integer>,
	   strikeout :: <integer>,
	   charset :: <integer>,
	   output-precision :: <integer>,
	   clip-precision :: <integer>,
	   quality :: <integer>,
	   pitch-and-family :: <integer>,
	   face-name :: limited(<string>, size: ($LF-FACESIZE - 1)))
	= font-components-from-text-style(_port, default, #"ANSI");
      logfont.lfHeight-value := height;
      logfont.lfWidth-value := width;
      logfont.lfEscapement-value := escapement;
      logfont.lfOrientation-value := orientation;
      logfont.lfWeight-value := weight;
      logfont.lfItalic-value := italic;
      logfont.lfUnderline-value	:= underline;
      logfont.lfStrikeOut-value	:= strikeout;
      logfont.lfCharSet-value := charset;
      logfont.lfOutPrecision-value := output-precision;
      logfont.lfClipPrecision-value := clip-precision;
      logfont.lfQuality-value := quality;
      logfont.lfPitchAndFamily-value := pitch-and-family;
      for (char in face-name,
	   i :: <integer> from 0 below ($LF-FACESIZE - 1))
	lfFaceName-array(logfont, i) := char;
      finally
	lfFaceName-array(logfont, i) := '\0';
      end;
    end;
    with-stack-structure (cf :: <LPCHOOSEFONT>)
      cf.lStructSize-value := safe-size-of(<CHOOSEFONT>);
      cf.hwndOwner-value   := handle;
      //--- hughg, 1999/08/12: This is only needed if we're using a
      // custom dialog template, which we're not.
      // cf.hInstance-value   := application-instance-handle();
      //--- hughg 1999/08/12: This is only needed for printer fonts, yet
      // the options below imply this function only shows screen fonts.
      // cf.hDC-value         := hDC;
      cf.lpLogFont-value   := logfont;
      cf.Flags-value
	:= %logior(if (fixed-width-only?) $CF-FIXEDPITCHONLY else 0 end,
		   if (show-apply?) $CF-APPLY else 0 end,
		   if (show-help?) $CF-SHOWHELP else 0 end,
		   if (choose-effects?) $CF-EFFECTS else 0 end,
		   if (choose-character-set?) 0 else $CF-NOSCRIPTSEL end,
		   if (default) $CF-INITTOLOGFONTSTRUCT else 0 end,
		   $CF-FORCEFONTEXIST,
		   $CF-SCREENFONTS);
      cf.lpszStyle-value   := $NULL-string;
      if (ChooseFont(cf))
	make-text-style-from-font(_port, logfont)
      else
	ensure-no-dialog-error("ChooseFont")
      end
    end
  end
end method do-choose-text-style;


///---*** Some not yet implemented dialogs
/*
/// Find and replace dialogs

define sealed method make-find-options
    (window :: <HWND>, #key find, replace) => (options :: <LPFINDREPLACE>)
  let options :: <LPFINDREPLACE> = make(<LPFINDREPLACE>);
  lStructSize-value(options) := safe-size-of(<FINDREPLACE>);
  hwndOwner-value(options) := window;
  hInstance-value(options) := application-instance-handle();
  Flags-value(options) := $FR-NOMATCHCASE + $FR-NOWHOLEWORD;
  lpstrFindWhat-value(options) := find;
  lpstrReplaceWith-value(options) := replace | $NULL-string;
  wFindWhatLen-value(options) := size(find);
  wReplaceWithLen-value(options) := if (replace) size(replace) else 0 end;
  options
end method make-find-options;

define sealed method display-find-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let options :: <LPFINDREPLACE> = make-find-options(window, find: "Hello");
  FindText(options)
end method display-find-dialog;

define sealed method display-replace-dialog
    (window :: <HWND>) => (ok? :: <boolean>)
  let options :: <LPFINDREPLACE>
    = make-find-options(window, find: "Hello", replace: "Goodbye");
  ReplaceText(options)
end method display-replace-dialog;
*/
