Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Frame Managers

//--- Should these be added to the display instead of the port?
define open abstract primary class <basic-frame-manager> (<frame-manager>)
  sealed slot port :: false-or(<port>) = #f,
    init-keyword: port:,
    setter: %port-setter;
  sealed slot frame-manager-frames  :: <stretchy-object-vector> = make(<stretchy-vector>);
  sealed slot frame-manager-palette :: false-or(<palette>) = #f,
    init-keyword: palette:;
end class <basic-frame-manager>;

define sealed class <portable-frame-manager> (<basic-frame-manager>)
end class <portable-frame-manager>;

//---*** This is most likely wrong, since it forces a 1-to-1 mapping
//---*** between ports and displays
define method display (framem :: <basic-frame-manager>) => (display :: false-or(<display>))
  let displays = port-displays(port(framem));
  ~empty?(displays) & displays[0]
end method display;

define method initialize (framem :: <frame-manager>, #key)
  next-method();
  unless (frame-manager-palette(framem))
    frame-manager-palette(framem) := port-default-palette(port(framem))
  end
end method initialize;


define method find-color
    (name, framem :: <basic-frame-manager>, #key error? = #t) => (color :: <color>)
  find-color(name, frame-manager-palette(framem), error?: error?)
end method find-color;


/// Frame manager creation

define method find-frame-manager
    (#rest options,
     #key port: _port, server-path, class, palette, #all-keys)
 => (framem :: <frame-manager>)
  dynamic-extent(options);
  block (return)
    with-keywords-removed (new-options = options, #[port:, server-path:, palette:])
      unless (_port)
	_port := default-port(server-path: server-path)
      end;
      unless (palette)
        palette := port-default-palette(_port)
      end;
      // Don't call 'port-default-frame-manager', because that can
      // get us into a loop...
      let default-framem
        = begin
	    let framems = port-frame-managers(_port);
	    case
	      empty?(framems) => #f;
	      ~class => framems[0];
	      otherwise => find-value(framems, method (f) object-class(f) == class end);
	    end
	  end;
      case
        // 'find-frame-manager' -> default one
	default-framem & empty?(options) =>
	  default-framem;
	// We specified a port, so make sure the default framem matches it
	default-framem
	& apply(frame-manager-matches-options?, default-framem, _port, new-options) =>
	  default-framem;
        // No default, look for one in the port, or create a new one;
        otherwise =>
          for (framem in port-frame-managers(_port))
            when (apply(frame-manager-matches-options?, framem, _port,
                        palette: palette, new-options))
              return(framem)
            end;
          end;
          let framem
            = apply(make-frame-manager, _port,
		    palette: palette, new-options);
          add!(port-frame-managers(_port), framem);
          framem
      end
    end
  end
end method find-frame-manager;

define open generic make-frame-manager
    (port :: <port>, #key class, palette, #all-keys)
 => (framem :: <frame-manager>);

define method make-frame-manager
    (_port :: <port>,
     #key palette, class = <portable-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;

define sealed inline method make
    (class == <frame-manager>, #rest initargs, #key port: _port, #all-keys)
 => (framem :: <frame-manager>)
  dynamic-extent(initargs);
  apply(make-frame-manager, _port, initargs)
end method make;

define method frame-manager-matches-options?
    (framem :: <frame-manager>, _port, #key palette, class, #all-keys)
 => (true? :: <boolean>)
  ignore(palette);
  port(framem) == _port
  & (~class | object-class(framem) == class)
end method frame-manager-matches-options?;

define method destroy-frame-manager (framem :: <frame-manager>) => ()
  while (~empty?(frame-manager-frames(framem)))
    destroy-frame(frame-manager-frames(framem)[0])
  end;
  let _port = port(framem);
  remove!(port-frame-managers(_port), framem)
end method destroy-frame-manager;


/// Frame protocol

//--- These are all forward references into 'duim-frames'

// The current application frame in this thread
define thread variable *current-frame* = #f;

define inline function current-frame () *current-frame* end;

define open generic destroy-frame (frame :: <abstract-frame>) => ();

define open generic frame-input-focus
    (frame :: <abstract-frame>) => (sheet :: false-or(<abstract-sheet>));

define open generic frame-input-focus-setter
    (sheet :: false-or(<abstract-sheet>), frame :: <abstract-frame>)
 => (sheet :: false-or(<abstract-sheet>));

define open generic frame-cursor-override
    (frame :: <abstract-frame>) => (cursor :: false-or(<cursor>));

define open generic frame-cursor-override-setter
    (cursor :: false-or(<cursor>), frame :: <abstract-frame>)
 => (cursor :: false-or(<cursor>));

define function do-frames
    (function :: <function>,
     #key port: _port, frame-manager: framem, z-order :: <z-order> = #f) => ()
  dynamic-extent(function);
  local method do-port-frames (_port :: <port>) => ()
	  for (framem in port-frame-managers(_port))
	    frame-manager-do-frames(function, framem, z-order: z-order)
	  end
	end method;
  case
    framem =>
      // Frame manager specified, so map over all the frames for
      // just this frame manager
      frame-manager-do-frames(function, framem, z-order: z-order);
    _port =>
      // Port specified, map over all of the frames for all of the
      // frame managers on the port
      do-port-frames(_port);
    otherwise =>
      // Map over all of the port...
      for (_port in *ports*)
	do-port-frames(_port)
      end;
  end
end function do-frames;

define open generic frame-manager-do-frames
    (function :: <function>, framem :: <frame-manager>, #key z-order :: <z-order>) => ();

define method frame-manager-do-frames
    (function :: <function>, framem :: <frame-manager>,
     #key z-order :: <z-order> = #f) => ()
  ignore(z-order);
  // Copy the sequence of frame manager's frames in case the function
  // modifies the sequence, e.g., 'do-frames(exit-frame, port: _port)'
  let frames = copy-sequence(frame-manager-frames(framem));
  do(function, frames)
end method frame-manager-do-frames;


/// Pane creation

// The current application frame in this thread
define thread variable *current-frame-manager* :: false-or(<frame-manager>) = #f;

define inline function current-frame-manager () *current-frame-manager* end;

// Here for compatibility and self-documentation...
define method make-pane 
    (pane-class :: <class>, #rest pane-options, #key, #all-keys)
 => (pane :: <sheet>)
  dynamic-extent(pane-options);
  apply(make, pane-class, pane-options)
end method make-pane;

//--- If you change this method, change the one in gadgets/gadget-mixins
define method make
    (pane-class :: subclass(<sheet>),
     #rest pane-options,
     #key port, frame-manager: framem, #all-keys)
 => (pane :: <sheet>)
  dynamic-extent(pane-options);
  let framem = framem
               | *current-frame-manager*
               | port-default-frame-manager(port | default-port())
               | error("Can't find a frame manager to use with 'make-pane'");
  let (concrete-class, concrete-options)
    = apply(class-for-make-pane, framem, pane-class, pane-options);
  // If there's a mapping from the abstract pane class to a concrete pane
  // class, then use it.  Otherwise just try to create a class named by the
  // abstract pane class.
  if (concrete-class == pane-class)
    apply(next-method, pane-class,
	  frame-manager: framem, pane-options)
  else
    //---*** Unfortunately, this recursive call to make will call
    //---*** 'class-for-make-pane' again.  How to speed this up?
    apply(make, concrete-class,
	  frame-manager: framem,
	  concrete-options | pane-options)
  end
end method make;

// Platform-specific back-ends must supply methods for:
//  - <top-level-sheet>
//  - <viewport>
//  - <scroll-bar>
//  - <push-button>
//  - <radio-button>
//  - <check-button>
//  - <menu>
//  - <menu-bar>
//  - <push-menu-button>
//  - <radio-menu-button>
//  - <check-menu-button>
//  - <slider>
//  - <list-box>
//  - <option-box>
//  - <text-field>
//  - <password-field>
//  - <text-editor>
//  - <splitter>
// The DUIM gadgets library provides methods for:
//  - <label>
//  - <separator>
//  - <tool-bar>
//  - <status-bar>
//  - <scroller>
//  - <push-box>
//  - <radio-box>
//  - <check-box>
//  - <push-menu-box>
//  - <radio-menu-box>
//  - <check-menu-box>
//  - <property-page>
// The DUIM layouts library provides methods for:
//  - <row-layout>	(default method supplied by DUIM core)
//  - <column-layout>	(default method supplied by DUIM core)
//  - <table-layout>	(default method supplied by DUIM core)
//  - <grid-layout>	(default method supplied by DUIM core)
//  - <pinboard-layout>	(default method supplied by DUIM core)
// The DUIM gadget panes library optionally provides methods for:
//  - <border>
//  - <spacing>
//  - <group-box>
//  - <progress-bar>
//  - <spin-box>
//  - <tab-control>
//  - <list-control>
//  - <table-control>
//  - <tree-control>
define open generic class-for-make-pane
    (framem :: <abstract-frame-manager>, pane-class,
     #rest pane-options, #key, #all-keys)
 => (class :: <class>, options :: false-or(<sequence>));

// Default method just uses the abstract pane class as the concrete pane class,
// e.g., <row-layout-pane> -> <row-layout-pane>
define method class-for-make-pane
    (framem :: <frame-manager>, pane-class :: <class>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(pane-class, #f)
end method class-for-make-pane;


/// Some high level "chooser" functions

define constant <notification-style>
    = one-of(#"information", #"question", #"warning",
	     #"error", #"serious-error", #"fatal-error");

define constant <notification-exit-style>
    = one-of(#"ok", #"ok-cancel",
	     #"yes-no", #"yes-no-cancel");

// Returns #t on normal exit, or #f on "cancel"
// STYLE is one of #"error", #"warning", #"information", #"question", etc.
define method notify-user
    (message-string :: <string>, #rest options,
     #key frame, owner, title, documentation, exit-boxes, name,
          style :: <notification-style> = #"information",
          exit-style :: false-or(<notification-exit-style>) = #f,
          foreground, background, text-style)
 => (value :: <boolean>, exit-type)
  dynamic-extent(options);
  ignore(title, documentation, exit-boxes, name, exit-style,
	 foreground, background, text-style);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:])
    apply(do-notify-user, framem, owner, message-string, style, options)
  end
end method notify-user;

define open generic do-notify-user
    (framem :: <abstract-frame-manager>, owner :: <sheet>,
     message-string :: <string>, style :: <notification-style>,
     #key title, documentation, name, exit-style,
     #all-keys)
 => (value :: <boolean>, exit-type);


// ITEMS can be a sequence or a <menu>
define method choose-from-menu
    (items, #rest options,
     #key frame, owner,
	  title, value, default-item, label-key, value-key,
          width, height, foreground, background, text-style,
          multiple-sets?)
 => (value, success? :: <boolean>)
  dynamic-extent(options);
  ignore(title, label-key, value-key,
	 width, height, foreground, background, text-style, multiple-sets?);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:, default-item:, value:])
    apply(do-choose-from-menu, framem, owner, items,
	  value: value | default-item,
	  options)
  end
end method choose-from-menu;

define open generic do-choose-from-menu
    (framem :: <abstract-frame-manager>, owner :: <sheet>, items,
     #key title, value, label-key, value-key, 
          width, height, foreground, background, text-style,
	  multiple-sets?,
     #all-keys)
 => (value, success? :: <boolean>);


// ITEMS can be a sequence or a <menu>
define method choose-from-dialog
    (items, #rest options,
     #key frame, owner,
	  title, value, default-item, label-key, value-key,
          selection-mode = #"single", gadget-class, gadget-options,
          width, height, foreground, background, text-style)
 => (value, success? :: <boolean>,
     width :: false-or(<integer>), height :: false-or(<integer>))
  dynamic-extent(options);
  ignore(title, label-key, value-key,
	 selection-mode, gadget-class, gadget-options,
	 width, height, foreground, background, text-style);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:, default-item:, value:])
    apply(do-choose-from-dialog, framem, owner, items,
	  value: value | default-item,
	  options)
  end
end method choose-from-dialog;

define open generic do-choose-from-dialog
    (framem :: <abstract-frame-manager>, owner :: <sheet>, items,
     #key title, value, label-key, value-key,
          selection-mode, gadget-class, gadget-options,
          width, height, foreground, background, text-style,
     #all-keys)
 => (value, success? :: <boolean>,
     width :: false-or(<integer>), height :: false-or(<integer>));


// Returns the pathname (or pathnames!) on normal exit, or #f on "cancel"
// The 'filters' argument is a bit weird; it takes the form:
//  #[#["Text files", "*.txt", "*.text"],
//    #["Dylan files", "*.dylan"],
//    ...]
// 'filter-index' is an index into the set of filters
define method choose-file
    (#rest options,
     #key frame, owner, title, documentation, exit-boxes,
          direction = #"input", if-exists = #"ask", if-does-not-exist = #"ask",
	  default, default-type, filters, default-filter, selection-mode = #"single")
 => (locator :: false-or(type-union(<string>, <sequence>)),
     filter :: false-or(<integer>))
  dynamic-extent(options);
  ignore(title, documentation, exit-boxes,
	 default, if-exists, if-does-not-exist,
	 default-type, filters, default-filter, selection-mode);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:])
    apply(do-choose-file, framem, owner, direction, options)
  end
end method choose-file;

define open generic do-choose-file
    (framem :: <abstract-frame-manager>, owner :: <sheet>,
     direction :: one-of(#"input", #"output"),
     #key title, documentation, exit-boxes,
	  if-exists, if-does-not-exist,
	  default, default-type, filters, default-filter, selection-mode,
     #all-keys)
 => (locator :: false-or(type-union(<string>, <sequence>)),
     filter :: false-or(<integer>));


// Returns the pathname on normal exit, or #f on "cancel"
define method choose-directory
    (#rest options,
     #key frame, owner, title, documentation, exit-boxes, default)
 => (locator)
  dynamic-extent(options);
  ignore(title, documentation, exit-boxes, default);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:])
    apply(do-choose-directory, framem, owner, options)
  end
end method choose-directory;

define open generic do-choose-directory
    (framem :: <abstract-frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, default,
     #all-keys)
 => (locator);


// Values are (printer, n-copies :: <integer>, print-to-file? :: <boolean>)
// when choosing a printer, or no values when doing print setup
define method choose-printer
    (#rest options,
     #key frame, owner, title, documentation, exit-boxes, default, setup?)
 => (#rest values);
  dynamic-extent(options);
  ignore(title, documentation, exit-boxes, default, setup?);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:])
    apply(do-choose-printer, framem, owner, options)
  end
end method choose-printer;

define open generic do-choose-printer
    (framem :: <abstract-frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, default, setup?,
     #all-keys)
 => (#rest values);


// Returns the color on normal exit, or #f on "cancel"
define method choose-color
    (#rest options,
     #key frame, owner, title, documentation, exit-boxes, default)
 => (color :: false-or(<color>))
  dynamic-extent(options);
  ignore(title, documentation, exit-boxes, default);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:])
    apply(do-choose-color, framem, owner, options)
  end
end method choose-color;

define open generic do-choose-color
    (framem :: <abstract-frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, default,
     #all-keys)
 => (color :: false-or(<color>));


// Returns the text-style on normal exit, or #f on "cancel"
//---*** hughg, 1999/08/12: The following keywords have been added for
// controlling the Win32 backend, but may not be portable to other GUI
// toolkit backends, in which case a redesign to achieve a unified
// interface might be a good idea!
//   fixed-width-only?, show-help?, show-apply?,
//   choose-character-set?, choose-effects?
define method choose-text-style
    (#rest options,
     #key frame, owner, title, documentation, exit-boxes, default,
	  fixed-width-only? :: <boolean> = #f,
	  show-help? :: <boolean> = #f, show-apply? :: <boolean> = #f,
	  choose-character-set? :: <boolean> = #f,
	  choose-effects? :: <boolean> = #f)
 => (text-style :: false-or(<text-style>))
  dynamic-extent(options);
  ignore(title, documentation, exit-boxes, default);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  with-keywords-removed (options = options, #[frame:, owner:])
    apply(do-choose-text-style, framem, owner, options)
  end
end method choose-text-style;

define open generic do-choose-text-style
    (framem :: <abstract-frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, default,
     #all-keys)
 => (text-style :: false-or(<text-style>));


define method get-frame-manager-and-owner
    (frame :: false-or(<frame>), owner)
 => (framem :: <frame-manager>, owner :: <sheet>)
  let frame = frame
              | (sheet?(owner) & sheet-frame(owner))
              | (frame?(owner) & owner)
              | current-frame();
  let owner = owner | frame;
  if (owner)
    let framem = frame & frame-manager(frame);
    values(framem, if (frame?(owner)) top-level-sheet(owner) else owner end)
  else
    let _port = default-port();
    let framem = port-default-frame-manager(_port);
    values(framem, find-display(port: _port))
  end
end method get-frame-manager-and-owner;
