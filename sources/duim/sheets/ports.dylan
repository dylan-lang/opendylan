Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Ports

define constant <focus-policy>
    = one-of(#"sheet-under-pointer", #"click-to-select");

define constant <event-processor-type>
    = one-of(#"n", #"n+1", #"2n");

define protocol <<port-protocol>> ()
  // Making and destroying ports
  function class-for-make-port
    (type-name, #rest initargs, #key, #all-keys)
 => (class :: <class>, initargs :: false-or(<sequence>));
  function destroy-port (port :: <abstract-port>) => ();
  function port-matches-server-path?
    (port :: <abstract-port>, server-path) => (true? :: <boolean>);
  function restart-port (port :: <abstract-port>) => ();
  // The contract of 'process-next-event' is to handle a single "raw"
  // event at the back-end.  Note that this will not necessarily result
  // in the distribution of a DUIM <event> object.
  function process-next-event
      (port :: <abstract-port>, #key timeout)
   => (timed-out? :: <boolean>);
  // Accessors
  getter port-modifier-state
    (port :: <abstract-port>) => (state :: <integer>);
  getter port-pointer
    (port :: <abstract-port>) => (pointer :: <pointer>);
  setter port-pointer-setter
    (pointer :: <pointer>, port :: <abstract-port>) => (pointer :: <pointer>);
  getter port-input-focus
    (port :: <abstract-port>) => (focus :: false-or(<abstract-sheet>));
  setter port-input-focus-setter
    (focus :: false-or(<abstract-sheet>), port :: <abstract-port>)
 => (focus :: false-or(<abstract-sheet>));
  getter port-focus-policy
    (port :: <abstract-port>) => (policy :: <focus-policy>);
  function note-focus-in
    (port :: <abstract-port>, sheet :: <abstract-sheet>) => ();
  function note-focus-out
    (port :: <abstract-port>, sheet :: <abstract-sheet>) => ();
  getter port-frame-managers
    (port :: <abstract-port>) => (framems :: <stretchy-vector>);
  getter port-default-frame-manager
    (port :: <abstract-port>) => (framem :: false-or(<abstract-frame-manager>));
  setter port-default-frame-manager-setter
    (framem :: false-or(<abstract-frame-manager>), port :: <abstract-port>)
 => (framem :: false-or(<abstract-frame-manager>));
  getter port-default-palette
    (port :: <abstract-port>) => (palette :: false-or(<palette>));
  setter port-default-palette-setter
    (palette :: false-or(<palette>), port :: <abstract-port>)
 => (palette :: false-or(<palette>));
  getter port-name
    (port :: <abstract-port>) => (name :: false-or(<string>));
  getter port-type
    (port :: <abstract-port>) => (type :: <symbol>);
  getter port-server-path
    (port :: <abstract-port>) => (server-path);
  getter port-properties
    (port :: <abstract-port>) => (properties :: <sequence>);
  setter port-properties-setter
    (properties :: <sequence>, port :: <abstract-port>) => (properties :: <sequence>);
  getter port-displays
    (port :: <abstract-port>) => (displays :: <stretchy-vector>);
  setter port-displays-setter
    (displays :: <stretchy-vector>, port :: <abstract-port>) => (displays :: <stretchy-vector>);
  // Text style mapping
  getter port-font-mapping-table
    (port :: <abstract-port>) => (table :: <table>);
  getter port-font-mapping-cache
    (port :: <abstract-port>) => (pair :: <pair>);
  getter port-undefined-text-style
    (port :: <abstract-port>) => (text-style :: <text-style>);
  setter port-undefined-text-style-setter
    (text-style :: <text-style>, port :: <abstract-port>)
 => (text-style :: <text-style>);
  // Ports can specialize these to compute foreground and background
  // brush based on X resource files or whatever
  function get-default-foreground
    (port :: <abstract-port>, sheet :: <abstract-sheet>, #key foreground, default)
 => (foreground :: false-or(<ink>));
  function port-default-foreground
    (port :: <abstract-port>, sheet :: <abstract-sheet>)
 => (foreground :: false-or(<ink>));
  function get-default-background
    (port :: <abstract-port>, sheet :: <abstract-sheet>, #key background, default)
 => (background :: false-or(<ink>));
  function port-default-background
    (port :: <abstract-port>, sheet :: <abstract-sheet>)
 => (background :: false-or(<ink>));
  function get-default-text-style
    (port :: <abstract-port>, sheet :: <abstract-sheet>, #key text-style, default)
 => (text-style :: false-or(<text-style>));
  function port-default-text-style
    (port :: <abstract-port>, sheet :: <abstract-sheet>)
 => (text-style :: false-or(<text-style>));
end protocol <<port-protocol>>;


define open abstract primary class <basic-port> (<port>)
  sealed slot port-server-path;
  sealed constant slot port-lock :: <simple-lock> = make(<simple-lock>);
  sealed slot port-properties :: <stretchy-object-vector> = make(<stretchy-vector>);
  sealed slot port-displays :: <stretchy-object-vector> = make(<stretchy-vector>);
  // This tells us what policy we use for event processing:
  //  - #"n" means event processing happens in each user thread
  //  - #"n+1" means there's a single event processing thread that
  //    distributes events to each user thread
  //  - #"2n" means there's an event processing thread for each and
  //    every user thread
  sealed constant slot port-event-processor-type :: <event-processor-type> = #"n",
    init-keyword: event-processor-type:;
  sealed slot port-event-thread = #f;
  sealed slot port-frame-managers :: <stretchy-object-vector> = make(<stretchy-vector>);
  sealed slot port-input-focus :: false-or(<sheet>) = #f,
    setter: %input-focus-setter;
  sealed constant slot port-focus-policy :: <focus-policy> = #"sheet-under-pointer",
    init-keyword: focus-policy:;
  sealed slot %pointer = #f;
  // The modifier state gets maintained by each back-end
  sealed slot port-modifier-state :: <integer> = make-modifier-state();
  sealed constant slot %trace-stack    :: <stretchy-object-vector> = make(<stretchy-vector>);
  sealed constant slot %ancestor-stack :: <stretchy-object-vector> = make(<stretchy-vector>);
  // The next two are for when the port is generating double click
  // events itself.  When the interval is #f, the underlying platform
  // may still generate double click events
  sealed slot %double-click-interval = #f,
    init-keyword: double-click-interval:;
  sealed slot %last-button-press-time = 0;
  //--- The next two should really be in the display, no?
  //--- Fix this when you change sheet.%port to sheet.%display
  sealed slot port-default-palette :: false-or(<palette>) = #f;
  sealed slot %medium-cache :: <object-deque> = make(<object-deque>);
  // Text style -> font mapping tables and one-element cache
  sealed constant slot port-font-mapping-table :: <object-table> = make(<table>);
  sealed constant slot port-font-mapping-cache :: <pair> = pair(#f, #f);
  sealed slot port-undefined-text-style :: <text-style> = $undefined-text-style;
  // This specifies how size mapping is done.
  //  - #"exact" -- the size given in the text style is exactly the size
  //    that should be used during mapping.
  //  - #"loose" -- the font whose size is closest to the size in the text
  //    style is used.  (In this case, the mapping table ignores the size
  //    and each bucket in the table is a list sorted by size.)
  //  - #"scalable" -- the size is ignored in the cache, and the back-end
  //    takes care of scaling the font
  sealed constant slot %text-style-size-mapping = #"loose",
    init-keyword: text-style-size-mapping:;
  sealed slot port-alive? :: <boolean> = #f;
end class <basic-port>;

define thread-slot port-alive? :: <boolean> of <abstract-port>;

define method port (_port :: <port>) => (port :: <port>)
  _port
end method port;

define method port (object) => (port :: singleton(#f))
  #f
end method port;

define method port-default-frame-manager
    (_port :: <port>) => (framem :: false-or(<frame-manager>))
  let framems = port-frame-managers(_port);
  if (empty?(framems))
    find-frame-manager(port: _port)
  else
    framems[0]
  end
end method port-default-frame-manager;

// Default port method uses just gets the color from the port's default palette
define method find-color
    (name, port :: <basic-port>, #key error? = #t) => (color :: <color>)
  find-color(name, port-default-palette(port), error?: error?)
end method find-color;

define variable *default-port-class-name* :: false-or(<symbol>) = #f;
define variable *port-classes* :: <object-table> = make(<object-table>);
define variable *ports* :: <stretchy-object-vector> = make(<stretchy-vector>);

define method initialize (_port :: <port>, #key)
  next-method();
  add!(*ports*, _port);
  restart-port(_port)
end method initialize;

define method initialize (_port :: <basic-port>, #key server-path)
  port-server-path(_port) := (server-path & copy-sequence(server-path));
  next-method()
end method initialize;

define method register-port-class
    (name :: <symbol>, class :: subclass(<port>), #key default? :: <boolean> = #f) => ()
  *port-classes*[name] := class;
  if (default?)
    *default-port-class-name* := name
  end
end method register-port-class;


/// Making ports

define macro with-port-locked
  { with-port-locked (?object:expression) ?:body end }
    => { begin
	   let _port = port(?object);
	   with-lock (port-lock(_port))
	     ?body;
	   failure
	     error("Couldn't get port lock for %=", _port);
	   end
	 end }
end macro with-port-locked;

define inline function do-ports (function :: <function>) => ()
  dynamic-extent(function);
  do(function, *ports*)
end function do-ports;

define variable *default-port* :: false-or(<port>) = #f;

define method default-port (#key server-path) => (_port :: false-or(<port>))
  *default-port*
  | (*default-port* := find-port(server-path: server-path | *default-server-path*))
end method default-port;

define method default-port-setter (_port :: <port>) => (_port :: <port>)
  *default-port* := _port
end method default-port-setter;

// "Primary" back-ends will supply a 'class-for-make-port' method
// that maps "local" to something more interesting...
define variable *default-server-path* = #(#"local");

define variable *global-lock* :: <simple-lock> = make(<simple-lock>);

define method find-port
    (#rest initargs,
     #key server-path = *default-server-path*, #all-keys)
 => (port :: <port>)
  dynamic-extent(initargs);
  with-lock (*global-lock*)
    block (return)
      local method match-port (_port) => ()
	      when (port-matches-server-path?(_port, server-path))
		return(_port)
	      end
	    end method;
      dynamic-extent(match-port);
      do-ports(match-port);
      with-keywords-removed (initargs = initargs, #[server-path:])
	apply(make, <port>, server-path: server-path, initargs)
      end
    end
  failure
    error("Couldn't get the global lock");
  end
end method find-port;

//--- How should server path matching really work?.  For instance, should an
//--- unspecified option match any value, or should there be some protocol for
//--- getting at the port-specific default value (noting that we may not have
//--- made any port with the default value yet).  For now, we just do a
//--- strict value-by-value-match.
define method port-matches-server-path?
    (_port :: <port>, server-path) => (true? :: <boolean>)
  destructuring-let ((type, #rest options) = server-path)
    destructuring-let ((port-type, #rest port-options) = port-server-path(_port))
      type == port-type
      // Now verify that the options are equivalent, ignoring ordering.
      // Copy the options so we can keep the lists in sync easily.
      & begin
	  let port-options = copy-sequence(port-options);
	  let options = concatenate-as(<list>, options);
	  block (return)
	    while (#t)
	      when (empty?(options))
		return(empty?(port-options))
	      end;
	      let indicator = pop!(options);
	      let value = pop!(options);
	      unless (get-property(port-options, indicator) = value)
		return(#f)
	      end;
	      remove-property!(port-options, indicator)
	    end
	  end
	end
    end
  end
end method port-matches-server-path?;

define sealed inline method make
    (class == <port>, #rest initargs, #key server-path, #all-keys)
 => (port :: <port>)
  dynamic-extent(initargs);
  let (port-class, new-initargs)
    = apply(class-for-make-port, first(server-path), initargs);
  apply(make, port-class, new-initargs | initargs)
end method make;

// If a class came in, send it back out
define method class-for-make-port
    (type :: <class>, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(type, #f)
end method class-for-make-port;

define method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  unless (*default-port-class-name*)
    error("Cannot create a port, as no port classes were registered")
  end;
  apply(class-for-make-port, *default-port-class-name*, initargs)
end method class-for-make-port;

define method class-for-make-port
    (type, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  error("Cannot create a port of type %=", type)
end method class-for-make-port;

define method destroy-port (_port :: <port>) => ()
  when (port-event-thread(_port))
    destroy-thread(port-event-thread(_port))
  end;
  while (~empty?(port-frame-managers(_port)))
    destroy-frame-manager(port-frame-managers(_port)[0])
  end;
  do(destroy-sheet, port-displays(_port));
  port-displays(_port).size := 0;
  remove!(*ports*, _port)
end method destroy-port;

define method destroy-port (_port :: <basic-port>) => ()
  do(destroy-medium, _port.%medium-cache);
  next-method()
end method destroy-port;


/// Ports vs. pointers

define sealed method port-pointer
    (_port :: <basic-port>) => (pointer :: <pointer>)
  _port.%pointer
  | (_port.%pointer
       := make(<pointer>, port: _port))
end method port-pointer;

define sealed method port-pointer-setter
    (pointer :: <pointer>, _port :: <basic-port>) => (pointer :: <pointer>)
  _port.%pointer := pointer
end method port-pointer-setter;


/// Input focus handling

define method port-input-focus-setter
    (new :: false-or(<sheet>), _port :: <basic-port>)
 => (new :: false-or(<sheet>))
  let old = port-input-focus(_port);
  unless (new == old)
    let new-frame = new & sheet-frame(new);
    _port.%input-focus := new;
    if (new-frame)
      frame-input-focus(new-frame) := new
    end;
    when (old)
      note-focus-out(_port, old)
    end;
    when (new)
      note-focus-in(_port, new)
    end;
  end;
  new
end method port-input-focus-setter;

define method note-focus-out
    (_port :: <basic-port>, sheet :: <sheet>) => ()
  distribute-event(_port, make(<input-focus-out-event>, sheet: sheet))
end method note-focus-out;

define method note-focus-in
    (_port :: <basic-port>, sheet :: <sheet>) => ()
  distribute-event(_port, make(<input-focus-in-event>,  sheet: sheet))
end method note-focus-in;


/// Port event loop

define method restart-port (_port :: <port>) => ()
  // If the event processing loop is supposed to run in it's own
  // thread, start it up now
  select (port-event-processor-type(_port))
    #"n" => #f;
    #"n+1" =>
      when (port-event-thread(_port))
	destroy-thread(port-event-thread(_port))
      end;
      port-event-thread(_port)
	:= make(<thread>,
		function: method () port-event-loop(_port) end,
		name: format-to-string("DUIM Event Dispatcher for %=",
				       port-server-path(_port)));
    #"2n" => #f;	//---*** what do we do about this case?
  end
end method restart-port;

define method port-event-loop
    (_port :: <port>) => ()
  block (return)
    dynamic-bind (port-alive?(_port) = #t)
      simple-restart-loop ("Exit event loop for port %a", port-name(_port))
        simple-restart-loop ("Restart event loop for port %a", port-name(_port))
          process-next-event(_port)
        end
      end
    end
  end
end method port-event-loop;

define method note-port-terminated (_port :: <port>, condition) => ()
  port-alive?(_port) := #f;
  for (framem in port-frame-managers(_port))
    for (frame in frame-manager-frames(framem))
      distribute-event(_port, make(<port-terminated-event>,
				   condition: condition, frame: frame));
      let sheet = top-level-sheet(frame);
      when (sheet)
	generate-trigger-event(_port, sheet)
      end
    end
  end;
  remove!(*ports*, _port)
end method note-port-terminated;


/// "Resources"

define sealed method get-default-foreground
    (_port :: <port>, sheet :: <sheet>, #key foreground, default = $default-foreground)
 => (foreground :: <ink>)
  foreground
  | default-foreground(sheet)
  | (sheet-frame(sheet) & default-foreground(sheet-frame(sheet)))
  | port-default-foreground(_port, sheet)	// consult resources here...
  | default
end method get-default-foreground;

define method port-default-foreground
    (_port :: <port>, sheet :: <sheet>) => (foreground :: false-or(<ink>))
  #f
end method port-default-foreground;


define sealed method get-default-background
    (_port :: <port>, sheet :: <sheet>, #key background, default = $default-background)
 => (background :: <ink>)
  background
  | default-background(sheet)
  | (sheet-frame(sheet) & default-background(sheet-frame(sheet)))
  | port-default-background(_port, sheet)	// consult resources here...
  | default
end method get-default-background;

define method port-default-background
    (_port :: <port>, sheet :: <sheet>) => (background :: false-or(<ink>))
  #f
end method port-default-background;


define sealed method get-default-text-style
    (_port :: <port>, sheet :: <sheet>, #key text-style, default = $null-text-style)
 => (text-style :: false-or(<text-style>))
  let style
    = text-style
      | default-text-style(sheet)
      | (sheet-frame(sheet) & default-text-style(sheet-frame(sheet)))
      | default;
  if (style & fully-merged-text-style?(style))
    style
  else
    let default-style
      = port-default-text-style(_port, sheet) | $default-text-style;
    style & merge-text-styles(style, default-style)
  end
end method get-default-text-style;

define method port-default-text-style
    (_port :: <port>, sheet :: <sheet>) => (text-style :: false-or(<text-style>))
  #f
end method port-default-text-style;
