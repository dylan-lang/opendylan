Module:    environment-tools
Synopsis:  Progress window/build dialog
Author:    Scott McKay, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Compiler locking

define constant $project-compilation-timeout :: <integer> = 10;
define variable $compiler-lock :: <simple-lock> = make(<simple-lock>);

define macro with-compiler-locked
  { with-compiler-locked (?owner:expression, #rest ?options:*) ?:body end }
    => { begin
	   let _continue? = #t;
	   while (_continue?)
	     with-lock ($compiler-lock, ?options)
	       block ()
		 ?body
	       cleanup
		 _continue? := #f
	       end
	     failure
	       unless (environment-question
			 ("The compiler is already busy; keep waiting for it?", owner: ?owner))
		 _continue? := #f
	       end
	     end
	   end
	 end }
end macro with-compiler-locked;


/// Combined build dialog and compiler progress notification

define constant <build-operation>
  = one-of(#"parse", #"compile", #"compile-and-link", #"release", #"link");

define constant $build-operations
  = #[#[#"parse",            "Parse only"],
      #[#"compile",          "Compile only"],
      #[#"compile-and-link", "Compile and link"],
      #[#"release",          "Compile, link and release"],
      #[#"link",             "Link only"]];


define constant <link-mode>
  = one-of(#"warnings",		//--- obsolete, but it might be in the registry
	   #"always",		//--- obsolete, but it might be in the registry
	   #"force", #"ask", #"no-warnings");

define constant $link-modes
  = #[#[#"force",            "Always link, even if there are serious warnings"],
      #[#"ask",              "Ask whether to link if there are serious warnings"],
      #[#"no-warnings",      "Don't link if there are serious warnings"]];

define frame <compiler-progress-window> 
    (<frame-window-settings-mixin>,
     <dialog-frame>)
  sealed slot %lightweight? = #f,
    init-keyword: lightweight?:;
  // Internals slots for maintaining the dialog state
  sealed slot %build-operation :: <build-operation> = #"compile-and-link",
    init-keyword: build-operation:;
  sealed slot %clean-build? :: <boolean> = #f,
    init-keyword: clean-build?:;
  sealed slot %copy-sources? :: <boolean> = #f,
    init-keyword: copy-sources?:;
  sealed slot %save-databases? :: <boolean> = #t,
    init-keyword: save-databases?:;
  sealed slot %process-subprojects? :: <boolean> = #t,
    init-keyword: process-subprojects?:;
  sealed slot %link-mode :: <link-mode> = #"ask",
    init-keyword: link-mode:;
  sealed slot %upgrade-warnings? :: <boolean> = #f,
    init-keyword: upgrade-warnings?:;
  // Slots for maintaining the compiler progress
  sealed slot %progress-note-function :: <function> = always(#f),
    init-keyword: progress-function:;
  sealed slot %progress-body-function :: <function> = always(#f),
    init-keyword: body-function:;
  sealed slot %progress-cleanup-function :: <function> = always(#f),
    init-keyword: cleanup-function:;
  sealed slot %progress-function-frame  :: false-or(<frame>)  = #f,
    init-keyword: progress-frame:;
  sealed slot %stop-progress?    :: <boolean> = #f;	// request a stop
  sealed slot %progress-stopped? :: <boolean> = #t;	// we are now stopped
  virtual slot compiler-progress-heading-label :: <string>;
  virtual slot compiler-progress-item-label    :: <string>;
  virtual slot compiler-progress-numerator     :: <integer>;
  virtual slot compiler-progress-denominator   :: <integer>;
  // The dialog panes...
  pane %build-operation-pane (frame)
    make(<option-box>,
	 items: $build-operations,
	 value-key: first, label-key: second,
	 value: frame.%build-operation,
	 value-changed-callback:
	   method (b)
	     frame.%build-operation := gadget-value(b)
	   end method,
	 documentation: "Choose the build operation.");
  pane %clean-build-pane (frame)
    make(<check-button>,
	 label: "&Clean build",
	 value: frame.%clean-build?,
	 value-changed-callback:
	   method (b)
	     frame.%clean-build? := gadget-value(b)
	   end method,
	 documentation: "Compile all source files rather than only changed ones.");
  /* ---*** Removed for 2.0 Beta 1 -- put it back in later
  pane %copy-sources-pane (frame)
    make(<check-button>,
	 label: "Sa&ve 'canonical' sources in the build area for the build",
	 value: frame.%copy-sources?,
	 value-changed-callback:
	   method (b)
	     frame.%copy-sources? := gadget-value(b)
	   end method,
	 documentation: "Choose whether to copy canonical sources to the build area before building."); */
  pane %save-databases-pane (frame)
    make(<check-button>,
	 label: "&Save compiler databases after build",
	 value: frame.%save-databases?,
	 value-changed-callback:
	   method (b)
	     frame.%save-databases? := gadget-value(b)
	   end method,
	 documentation: "Choose whether to save compiler databases.");
  pane %process-subprojects-pane (frame)
    make(<check-button>,
	 label: "P&rocess subprojects",
	 value: frame.%process-subprojects?,
	 value-changed-callback:
	   method (b)
	     frame.%process-subprojects? := gadget-value(b)
	   end method,
	 documentation: "Choose whether to build subprojects.");
  pane %link-mode-pane (frame)
    make(<option-box>,
	 items: $link-modes,
	 value-key: first, label-key: second,
	 value: frame.%link-mode,
	 value-changed-callback:
	   method (b)
	     frame.%link-mode := gadget-value(b)
	   end method,
	 documentation: "Choose the link mode.");
  pane %upgrade-warnings-pane (frame)
    make(<check-button>,
	 label: "Treat all &warnings as serious warnings",
	 value: frame.%upgrade-warnings?,
	 value-changed-callback:
	   method (b)
	     frame.%upgrade-warnings? := gadget-value(b)
	   end method,
	 documentation: "Choose whether to treat all warnings as serious warnings.");
  pane %progress-heading-label (frame)
    make(<label>, min-width: 1, max-width: $fill);
  pane %progress-item-label (frame)
    make(<label>, min-width: 1, max-width: $fill);
  pane %progress-bar (frame)
    make(<progress-bar>);
  pane %build-button (frame)
    make(<push-button>,
	 label: "Build",
	 activate-callback:
	   method (b)
	     start-build-in-progress-window(frame)
	   end method,
	 documentation: "Build the project.",
	 enabled?: #t);
  pane %close-button (frame)
    make(<push-button>,
	 label: "Close",
	 activate-callback:
	   method (b)
	     exit-dialog(frame)
	   end method,
	 documentation: "Close this dialog.",
	 enabled?: #t);
  pane %stop-button (frame)
    make(<push-button>,
	 label: "Stop",
	 activate-callback:
	   method (b)
	     stop-build-in-progress-window(frame)
	   end method,
	 documentation: "Stops the current build.",
	 enabled?: frame.%lightweight?);
  pane %lightweight-layout (frame)
    horizontally (spacing: 10)
      vertically (spacing: 8, equalize-widths?: #t)
	make(<group-box>,
	     label: "Build progress",
	     child: vertically (spacing: 8)
		      frame.%progress-heading-label;
		      frame.%progress-item-label;
		      frame.%progress-bar;
		    end);
      end;
      vertically (spacing: 8, equalize-widths?: #t)
	frame.%stop-button;
      end;
    end;
  pane %heavyweight-layout (frame)
    horizontally (spacing: 10)
      vertically (spacing: 8, equalize-widths?: #t)
	make(<group-box>,
	     label: "Build progress",
	     child: vertically (spacing: 8)
		      frame.%progress-heading-label;
		      frame.%progress-item-label;
		      frame.%progress-bar;
		    end);
	make(<group-box>,
	     label: "Build options",
	     child: vertically (spacing: 8)
		      frame.%build-operation-pane;
		      frame.%clean-build-pane;
		      /* frame.%copy-sources-pane; */
		      frame.%save-databases-pane;
		      frame.%process-subprojects-pane;
		    end);
	make(<group-box>,
	     label: "Warnings options",
	     child: vertically (spacing: 8)
		      frame.%link-mode-pane;
		      frame.%upgrade-warnings-pane;
		    end);
      end;
      vertically (spacing: 8, equalize-widths?: #t)
	frame.%build-button;
	frame.%close-button;
	frame.%stop-button;
      end;
    end;
  layout (frame)
    if (frame.%lightweight?) frame.%lightweight-layout
    else frame.%heavyweight-layout end;
  keyword icon:  = $build-bitmap;
  keyword width: = 400;
  keyword mode:  = #"modeless";
  keyword exit-callback:   = #f;
  keyword cancel-callback: = #f;
end frame <compiler-progress-window>;

define window-settings 
  build-window :: <compiler-progress-window> = "Build Window";


/// Creation

//---*** The project window should be the dialog's owner
define method initialize
    (frame :: <compiler-progress-window>,
     #rest initargs,
     #key save-databases?   = $unsupplied,
	  copy-sources?     = $unsupplied,
	  link-mode         = $unsupplied,
	  upgrade-warnings? = $unsupplied,
	  heading-label :: <string>  = "",
          item-label    :: <string>  = "",
          numerator     :: <integer> = 0,
          denominator   :: <integer> = 1)
  when (unsupplied?(save-databases?))
    save-databases? := environment-default-save-databases()
  end;
  when (unsupplied?(copy-sources?))
    copy-sources? := environment-default-copy-sources()
  end;
  when (unsupplied?(link-mode))
    link-mode := environment-default-link-mode()
  end;
  when (unsupplied?(upgrade-warnings?))
    upgrade-warnings? := environment-default-upgrade-warnings()
  end;
  apply(next-method, frame,
	save-databases?:   save-databases?,
	copy-sources?:     copy-sources?,
	link-mode:         link-mode,
	upgrade-warnings?: upgrade-warnings?,
	initargs);

  compiler-progress-heading-label(frame) := heading-label;
  compiler-progress-item-label(frame)    := item-label;
  compiler-progress-numerator(frame)     := numerator;
  compiler-progress-denominator(frame)   := denominator;
  frame
end method initialize;

define function make-compiler-progress-window
    (frame :: <frame>, #rest initargs,
     #key lightweight? = #f, #all-keys)
 => (window :: <compiler-progress-window>)
  find-compiler-progress-window(frame)
  | apply(make-environment-frame, port(frame), <compiler-progress-window>,
	  //---*** andrewa: this causes some wierd effects
	  // owner: frame,
	  progress-frame: frame,
	  minimize-box?: #t, maximize-box?: #t,
	  initargs)
end function make-compiler-progress-window;

define method find-compiler-progress-window
    (frame :: <frame>)
 => (window :: false-or(<compiler-progress-window>))
  block (return)
    do-frames(method (window)
		when (instance?(window, <compiler-progress-window>)
		      & frame == window.%progress-function-frame
		      & frame-mapped?(window))
		  return(window)
		end
              end method)
  end
end method find-compiler-progress-window;

/*---*** This isn't currently used
define method set-compiler-progress-state
    (window :: <compiler-progress-window>,
     #key parse? = $unsupplied, compile? = $unsupplied, link? = $unsupplied,
	  clean? = $unsupplied,
	  save-databases? = $unsupplied, copy-sources? = $unsupplied,
	  process-subprojects? = $unsupplied) => ()
  when (supplied?(parse?) | supplied?(compile?) | supplied?(link?))
    let operation
      = case
	  compile? & link? => #"compile-and-link";
	  compile?         => #"compile";
	  link?            => #"link";
	  parse?           => #"parse";
	end;
    window.%build-operation := operation;
    unless (window.%lightweight?)
      gadget-value(window.%build-operation-pane, do-callback?: #t) := operation
    end
  end;
  when (supplied?(clean?))
    window.%clean-build? := clean?;
    unless (window.%lightweight?)
      gadget-value(window.%clean-build-pane, do-callback?: #t) := clean?
    end
  end;
  when (supplied?(process-subprojects?))
    window.%process-subprojects? := process-subprojects?;
    unless (window.%lightweight?)
      gadget-value(window.%process-subprojects-pane, do-callback?: #t) := process-subprojects?
    end
  end;
  when (supplied?(save-databases?))
    window.%save-databases? := save-databases?;
    unless (window.%lightweight?)
      gadget-value(window.%save-databases-pane, do-callback?: #t) := save-databases?
    end
  end;
  when (supplied?(copy-sources?))
    window.%copy-sources? := copy-sources?;
    unless (window.%lightweight?)
      /* gadget-value(window.%copy-sources-pane, do-callback?: #t) := copy-sources? */
    end
  end;
end method set-compiler-progress-state;
*/


/// Event handling

define method handle-event
    (window :: <compiler-progress-window>, event :: <frame-mapped-event>) => ()
  // Start the progress function on its own thread
  let frame       = window.%progress-function-frame;
  let thread-name = frame-thread-name(frame, object-class(frame));
  make(<thread>,
       name: concatenate("Building for ", thread-name),
       function: method () => ()
		   with-abort-restart ()
		     block ()
		       window.%progress-body-function();
		     cleanup
		       gadget-enabled?(window.%stop-button) := #f;
		       when (window.%progress-function-frame
			       & window.%progress-cleanup-function)
			 call-in-frame(window.%progress-function-frame,
				       window.%progress-cleanup-function)
		       end
		     end
		   end
		 end method);
  next-method()
end method handle-event;

define method handle-event
    (window :: <compiler-progress-window>, event :: <dialog-exit-event>) => ()
  window.%stop-progress?    := #t;
  window.%progress-stopped? := #f;
  next-method()
end method handle-event;


/// Progress values

define method compiler-progress-heading-label
    (window :: <compiler-progress-window>) => (label :: <string>)
  gadget-label(window.%progress-heading-label)
end method compiler-progress-heading-label;

define method compiler-progress-heading-label-setter
    (label :: <string>, window :: <compiler-progress-window>) => (label :: <string>)
  gadget-label(window.%progress-heading-label) := label
end method compiler-progress-heading-label-setter;

define method compiler-progress-item-label
    (window :: <compiler-progress-window>) => (label :: <string>)
  gadget-label(window.%progress-item-label)
end method compiler-progress-item-label;

define method compiler-progress-item-label-setter
    (label :: <string>, window :: <compiler-progress-window>) => (label :: <string>)
  gadget-label(window.%progress-item-label) := label
end method compiler-progress-item-label-setter;

define method compiler-progress-denominator
    (window :: <compiler-progress-window>) => (denominator :: <integer>)
  size(gadget-value-range(window.%progress-bar)) - 1
end method compiler-progress-denominator;

define method compiler-progress-denominator-setter
    (denominator :: <integer>, window :: <compiler-progress-window>) => (denominator :: <integer>)
  gadget-value-range(window.%progress-bar) := range(from: 0, to: denominator);
  denominator
end method compiler-progress-denominator-setter;

define method compiler-progress-numerator
    (window :: <compiler-progress-window>) => (numerator :: <integer>)
  gadget-value(window.%progress-bar)
end method compiler-progress-numerator;

define method compiler-progress-numerator-setter
    (numerator :: <integer>, window :: <compiler-progress-window>) => (numerator :: <integer>)
  gadget-value(window.%progress-bar) := numerator
end method compiler-progress-numerator-setter;

define method compiler-progress-stopped?
    (window :: <compiler-progress-window>) => (stopped? :: <boolean>)
  window.%progress-stopped?
end method compiler-progress-stopped?;

ignorable(compiler-progress-heading-label,
	  compiler-progress-heading-label-setter,
	  compiler-progress-item-label,
	  compiler-progress-item-label-setter,
	  compiler-progress-denominator,
	  compiler-progress-denominator-setter,
	  compiler-progress-numerator,
	  compiler-progress-numerator-setter);


/// Starting and stopping builds

// This must be called in the progress window's own thread
define method start-build-in-progress-window
    (window :: <compiler-progress-window>) => ()
  let timeout = $project-compilation-timeout;
  gadget-enabled?(window.%stop-button) := #t;
  unless (window.%lightweight?)
    gadget-enabled?(window.%build-button) := #f;
    gadget-enabled?(window.%close-button) := #f;
  end;
  window.%stop-progress?    := #f;
  window.%progress-stopped? := #f;
  // Build the project according to the current parameters
  let (parse?, compile?, link?, release?)
    = select (window.%build-operation)
	#"parse"            => values(#t, #f, #f, #f);
	#"compile"          => values(#f, #t, #f, #f);
	#"compile-and-link" => values(#f, #t, #t, #f);
	#"release"          => values(#f, #t, #t, #t);
	#"link"             => values(#f, #f, #t, #f);
      end;
  let frame   = window.%progress-function-frame;
  let project = frame-current-project(frame);
  let note-compiler-progress = window.%progress-note-function;
  local method build-project () => ()
          with-abort-restart ()
	    with-compiler-locked (window, timeout: timeout)
	      let built? = #f;
	      block ()
		built?
		  := frame-do-build-project-1
		       (frame, project,
			parse?:   parse?,
			compile?: compile?,
			link?:    link?,
			release?: release?,
			clean?:   window.%clean-build?,
			process-subprojects?: window.%process-subprojects?,
			save-databases?:      window.%save-databases?,
			copy-sources?:        window.%copy-sources?,
			link-mode:            window.%link-mode,
			upgrade-warnings?:    window.%upgrade-warnings?,
			note-compiler-progress: note-compiler-progress,
			progress-window:        window);
	      cleanup
		let (message, warnings?)
		  = compilation-status
		      (frame, parse?: parse?, compile?: compile?, link?: link?,
		       aborted?: ~built?);
		frame-status-message(frame) := message;
		note-compiler-progress(1, 1, heading-label: message, item-label: "");
		// Can't hurt to do this here...
		call-in-frame
		  (window,
		   method (window :: <compiler-progress-window>)
		     gadget-enabled?(window.%build-button) := #t;
		     gadget-enabled?(window.%close-button) := #t;
		     gadget-enabled?(window.%stop-button)  := #f
		   end method,
		   window)
	      end
	    end
	  end
	end method build-project;
  call-in-frame(frame, build-project)
end method start-build-in-progress-window;

// This must be called in the progress window's own thread
define method stop-build-in-progress-window 
    (window :: <compiler-progress-window>) => ()
  // Don't disable the stop button until the thing actually stops
  unless (window.%lightweight?)
    gadget-enabled?(window.%build-button) := #t;
    gadget-enabled?(window.%close-button) := #t;
  end;
  window.%stop-progress?    := #t;
  window.%progress-stopped? := #f;
end method stop-build-in-progress-window;


/// Progress window utilities

define macro with-compiler-progress
  { with-compiler-progress
       ((?window:name, ?callback:name) = ?frame:expression, #rest ?keys:*)
      ?main-body:body
    end }
    => { begin
	   let compiler-progress-body
	     = method (?window, ?callback) ?main-body end;
	   let compiler-progress-cleanup
	     = method (?window) ?window; #f end;
	   do-with-compiler-progress
	     (?frame, compiler-progress-body, compiler-progress-cleanup, ?keys)
	 end }
  { with-compiler-progress
       ((?window:name, ?callback:name) = ?frame:expression, #rest ?keys:*)
      ?main-body:body
    cleanup
      ?cleanup-body:body
    end }
    => { begin
	   let compiler-progress-body
	     = method (?window, ?callback) ?main-body end;
	   let compiler-progress-cleanup
	     = method (?window) ?window; ?cleanup-body end;
	   do-with-compiler-progress
	     (?frame, compiler-progress-body, compiler-progress-cleanup, ?keys)
	 end }
end macro with-compiler-progress;

define method do-with-compiler-progress
    (frame :: <environment-frame>,
     body-function :: <function>, cleanup-function :: <function>,
     #rest initargs,
     #key title, lightweight? = #f, own-thread? = #t, #all-keys)
 => (#rest values)
  let window 
    = apply(make-compiler-progress-window, frame,
	    owner: unless (own-thread?) frame end,
	    initargs);
  local method do-note-progress (numerator, denominator, #rest keys)
	  apply(update-compiler-progress-window, window, numerator, denominator, keys)
	end method,
        method do-body-function ()
	  body-function(window, do-note-progress)
	end method,
	method do-cleanup-function ()
	  block ()
	    cleanup-function(window)
	  cleanup
	    if (window.%lightweight?)
	      call-in-frame(window, exit-dialog, window)
	    else
	      call-in-frame
		(window,
		 method (window)
		   gadget-enabled?(window.%build-button) := #t;
		   gadget-enabled?(window.%close-button) := #t;
		   gadget-enabled?(window.%stop-button)  := #f
		 end method,
		 window)
	    end
	  end
	end method do-cleanup-function;
  window.%progress-note-function    := do-note-progress;
  window.%progress-body-function    := do-body-function;
  window.%progress-cleanup-function := do-cleanup-function;
  if (own-thread?)
    fork-environment-function(window, object-class(window), always(window))
  else
    start-frame(window)
  end;
  window
end method do-with-compiler-progress;

define method update-compiler-progress-window
    (window :: <compiler-progress-window>, numerator, denominator,
     #rest keys, #key heading-label, item-label, cursor) => ()
  ignore(heading-label, item-label, cursor);
  // Dispatch to progress window frame's thread
  apply(call-in-frame,
	window,
	do-update-compiler-progress-window, window, numerator, denominator, keys);
  // Stop progress function if requested
  when (window.%stop-progress?)
    call-in-frame
      (window,
       method (window)
	 gadget-enabled?(window.%stop-button) := #f;
       end,
       window);
    window.%stop-progress?    := #f;	// in case a cleanup updates the progress!
    window.%progress-stopped? := #t;
    abort()
  end
end method update-compiler-progress-window;

define method do-update-compiler-progress-window
    (window :: <compiler-progress-window>, numerator, denominator,
     #key heading-label, item-label, cursor) => ()
  let pointer = port-pointer(port(window));
  when (pointer & cursor)
    pointer-cursor(pointer) := cursor
  end;
  when (heading-label)
    compiler-progress-heading-label(window) := heading-label
  end;
  when (item-label)
    compiler-progress-item-label(window) := item-label
  end;
  compiler-progress-denominator(window) := denominator;
  compiler-progress-numerator(window)   := numerator
end method do-update-compiler-progress-window;
