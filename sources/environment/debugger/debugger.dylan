Module:    environment-debugger
Author:    Bill Chiles, Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <DEBUGGER> (internal)

//---*** andrewa: I think these are irritating, so I've switched them off
define constant $debugger-pane-tooltips? = #f;

define constant $default-debugger-frame-width :: <integer>  = 900;
define constant $default-debugger-frame-height :: <integer> = 600;

define thread variable *debugger* :: false-or(<debugger>) = #f;

define sealed frame <debugger>
    (<frame-refresh-mixin>, 
     <frame-undo-mixin>,
     <frame-module-gadget-mixin>,
     <frame-cascading-window-mixin>,
     <environment-project-tool>)
  sealed slot debugger-thread :: false-or(<thread-object>) = #f,
    init-keyword: native-thread:,
    setter: %thread-setter;
  sealed slot debugger-thread-index :: <integer> = 1,
    init-keyword: thread-index:;
  // See the method on 'debugger-zoom-setter' for more info on this slot.
  // NB Methods on make and reinitialize-frame ensure that this is initialized
  // appropriately, even if "zoom:" defaults to or is given as #f.
  sealed slot debugger-zoom :: false-or(<symbol>) = #f,
    setter: %debugger-zoom-setter,
    init-keyword: zoom:;
  sealed slot debugger-updated? :: <boolean> = #f;
  sealed slot debugger-enabled? :: <boolean> = #t;
  sealed slot debugger-interactor-transaction :: <object> = #f;
  constant sealed slot debugger-filtered-functions = make(<object-table>);
  pane debugger-context-pane (debugger)     make(<debugger-context-pane>);
  pane debugger-stack-pane (debugger)       make(<debugger-stack-pane>);
  pane debugger-source-pane (debugger)      make(<debugger-source-pane>);
  pane debugger-interactor1-pane (debugger)
    make(<interactor-pane>, 
	 project: debugger.ensure-frame-project,
	 thread:  debugger.debugger-thread);
  pane debugger-interactor2-pane (debugger)
    make(<interactor-pane>, 
	 project: debugger.ensure-frame-project,
	 thread:  debugger.debugger-thread);
  pane debugger-status-bar (debugger)
    make(<debugger-status-bar>);
  pane debugger-stack-source-splitter (debugger)
    make(<row-splitter>,
	 children: vector(debugger.debugger-stack-pane,
			  debugger.debugger-source-pane));
  pane debugger-basic-debugging-layout (debugger)
    make(<column-splitter>,
	 children:
	   vector(debugger.debugger-stack-source-splitter,
		  debugger.debugger-interactor1-pane));
  pane debugger-switchable-layout (debugger)
    make(<stack-layout>,
	 children: vector(debugger.debugger-basic-debugging-layout,
			  debugger.debugger-interactor2-pane));
  pane debugger-default-layout (debugger)
    vertically (spacing: $vertical-spacing)
      debugger.debugger-context-pane;
      debugger.debugger-switchable-layout;
    end;
  layout (debugger)
    dynamic-bind (*debugger* = debugger)
      debugger.debugger-default-layout
    end;
  tool-bar (debugger) make-environment-tool-bar(debugger);
  command-table (debugger) *debugger-command-table*;
  status-bar (debugger) debugger.debugger-status-bar.pane-layout;
  keyword width:  = $default-debugger-frame-width;
  keyword height: = $default-debugger-frame-height;
  keyword icon: = $debugger-window-small-icon;
  keyword frame-class-name: = #"debugger";
end frame <debugger>;

define sealed domain make (singleton(<debugger>));

define method make-clone
    (debugger :: <debugger>, #rest initargs)
 => (debugger :: <debugger>)
  apply(next-method, debugger, 
	native-thread: debugger.debugger-thread,
	thread-index:  debugger.debugger-thread-index,
	zoom:          debugger.debugger-auto-zoom,
	initargs)
end method make-clone;

define sealed method initialize
    (debugger :: <debugger>, #key zoom) => ()
  dynamic-bind (*debugger* = debugger)
    next-method();
    let project = debugger.frame-project;
    let thread = debugger.debugger-thread;
    debugger.debugger-zoom := zoom | debugger-auto-zoom(debugger)
  end
end method initialize;

define sealed method reinitialize-frame
    (debugger :: <debugger>, #key native-thread, stop-reason, zoom)
 => ()
  next-method();
  debugger.debugger-thread := native-thread;
  debugger.debugger-zoom := zoom | debugger-auto-zoom(debugger);
  activate-debugger(debugger);
end method reinitialize-frame;

define sealed method handle-event 
    (debugger :: <debugger>, event :: <frame-mapped-event>)
 => ()
  next-method();
  update-debugger(debugger);
  update-debugger-focus(debugger);
end method handle-event;

define sealed method generate-frame-title 
    (debugger :: <debugger>)
 => (title :: <byte-string>)
  with-output-to-string (stream)
    let project = debugger.frame-project;
    let application = project.project-application;
    let thread :: false-or(<thread-object>) = debugger.debugger-thread;
    let machine = application & application.application-machine;
    if (thread)
      write(stream, frame-default-object-name(debugger, thread));
      write(stream, " - ")
    end;
    write(stream, "Debugging ");
    write(stream, environment-object-primitive-name(project, project));
    if (machine)
      write(stream, " on ");
      write(stream, machine.machine-hostname)
    end;
    write(stream, " - ");
    write(stream, release-product-name())
  end
end method generate-frame-title;


/// Window settings

define cascading-window-settings
  debugger-window :: <debugger> = "Debugger Window";

define method save-window-settings
    (debugger :: <debugger>) => ()
  next-method();

  let ratios = debugger.debugger-stack-source-splitter.gadget-ratios;
  $debugger-settings.stack-pane-ratio := ratios[0];
  $debugger-settings.source-pane-ratio := ratios[1];

  let ratios = debugger.debugger-basic-debugging-layout.gadget-ratios;
  $debugger-settings.stack-source-ratio := ratios[0];
  $debugger-settings.interactor-pane-ratio := ratios[1];
end method save-window-settings;

define method restore-window-settings
    (debugger :: <debugger>) => ()
  next-method();
  debugger.debugger-stack-source-splitter.gadget-ratios
    := vector($debugger-settings.stack-pane-ratio,
	      $debugger-settings.source-pane-ratio);
  debugger.debugger-basic-debugging-layout.gadget-ratios
    := vector($debugger-settings.stack-source-ratio,
	      $debugger-settings.interactor-pane-ratio);
end method restore-window-settings;


/// Debugger virtual slots

// Returns whichever of the interactor panes is currently mapped.
define function debugger-interactor-pane 
    (debugger :: <debugger>)
 => (interactor-pane :: <interactor-pane>)
  let interactor1-pane = debugger.debugger-interactor1-pane;
  if (sheet-mapped?(interactor1-pane))
    interactor1-pane
  else
    debugger.debugger-interactor2-pane
  end
end function;


/// Debugger thread handling

define method application-default-thread
    (project :: <project-object>) => (thread :: false-or(<thread-object>))
  let application =  project.project-application;
  if (application)
    let threads = application-threads(application);
    ~empty?(threads) & threads[0]
  end
end method application-default-thread;

define method debugger-thread-setter
    (thread :: false-or(<thread-object>), debugger :: <debugger>)
 => (thread :: false-or(<thread-object>))
  unless (thread == debugger.debugger-thread)
    debugger.%thread := thread;
    call-in-frame(debugger, note-debugger-thread-changed, debugger)
  end;
  thread
end method debugger-thread-setter;

define variable $select-thread-dialog-width  :: <integer> = 500;
define variable $select-thread-dialog-height :: <integer> = 300;

define method debugger-select-thread
    (debugger :: <debugger>) => ()
  let project = debugger.ensure-frame-project;
  let application = project.project-application;
  if (application)
    let threads = application.application-threads;
    let (thread :: false-or(<thread-object>), success?, width, height)
      = choose-from-dialog(threads,
			   owner: debugger,
			   title: "Select Thread",
			   label-key: curry(frame-default-object-name, debugger),
			   value:  debugger.debugger-thread,
			   width:  $select-thread-dialog-width,
			   height: $select-thread-dialog-height);
    when (success? & thread)
      $select-thread-dialog-width  := width;
      $select-thread-dialog-height := height;
      debugger.debugger-thread := thread
    end
  else
    environment-action-unavailable
      (debugger, "Cannot switch threads as there is no running application.")
  end
end method debugger-select-thread;

define method frame-note-application-threads-changed
    (debugger :: <debugger>) => ()
  next-method();
  let application = debugger.ensure-frame-project.project-application;
  let threads = if (application) application.application-threads else #[] end;
  let thread = debugger.debugger-thread;
  case
    ~thread =>
      let index = debugger.debugger-thread-index;
      let new-thread
	= block (return)
	    for (thread :: <thread-object> in threads)
	      let thread-index = thread-index(application, thread);
	      if (index == thread-index) return(thread) end
	    end;
	    #f
	  end;
      if (new-thread)
	debugger.debugger-thread := new-thread
      end;
    ~member?(thread, threads) =>
      deactivate-debugger(debugger);
    otherwise =>
      #f;
  end
end method frame-note-application-threads-changed;


/// Debugger layout handling

/// This is basically a "write-only" slot.  Writing it resets the
/// layout to one of a number of presets.  The last preset written is
/// stored, but only so that a handle-event method on <frame-mapped-event>
/// can call this again at the appropriate point.

define sealed method debugger-zoom-setter 
    (zoom :: <symbol>, debugger :: <debugger>)
 => (zoom :: <symbol>)
  unless (debugger.debugger-zoom == zoom)
    with-busy-cursor (debugger)
      unless (zoom == #"zoom-debugging" | zoom == #"zoom-interacting")
	debug-message("Invalid debugger zoom setting: %=", zoom);
	zoom := #"zoom-debugging";
      end;
      // Keep the point in a sensible place after switching layouts
      let (current, new)
	= select (zoom)
	    #"zoom-debugging"   =>
	      values(debugger.debugger-interactor2-pane.%interactor-control,
		     debugger.debugger-interactor1-pane.%interactor-control);
	    #"zoom-interacting" =>
	      values(debugger.debugger-interactor1-pane.%interactor-control,
		     debugger.debugger-interactor2-pane.%interactor-control);
	  end;
      let point = window-point(current);
      move-bp!(window-point(new), bp-line(point), bp-index(point));
      // Now switch layouts
      let stack = debugger.debugger-switchable-layout;
      let children = sheet-children(stack);
      let child-index
	= select (zoom)
	    #"zoom-debugging"   => 0;
	    #"zoom-interacting" => 1;
	  end;
      stack-layout-mapped-page(stack) := children[child-index];
      //--- Some panes that weren't mapped may need to be updated
      //--- when they become mapped.
      debugger.debugger-updated? := #f;
      debugger.%debugger-zoom := zoom;
      update-debugger-focus(debugger)
    end
  end;
  zoom
end method debugger-zoom-setter;

define function zoom-description-from-startup-option
    (startup :: false-or(<application-startup-option>))
 => (z :: false-or(<symbol>))
  select(startup)
    #"interact"    => #"zoom-interacting";
    #"debug"       => #"zoom-debugging";
    otherwise      => #f;
  end select
end function zoom-description-from-startup-option;

define function debugger-auto-zoom 
    (debugger :: <debugger>)
 => (zoom :: <symbol>)
  let project = debugger.frame-project;
  let application = project.project-application;
  let state = application & application.application-state;
  let thread :: false-or(<thread-object>) = debugger.debugger-thread;
  let zoom-according-to-startup
    = zoom-description-from-startup-option(project.application-startup-option)
        | #"zoom-debugging";
  select (state)
    #f, #"uninitialized", #"running", #"closed" =>
      #"zoom-debugging";
    #"stopped" =>
      case
	application-just-initialized?(application)
	  | application-reached-interaction-point?(application) =>
	  zoom-according-to-startup;
	thread
	  & (application-just-interacted?(project, thread)
	       | application-just-stepped?(project, thread)) =>
	  debugger.debugger-zoom | zoom-according-to-startup;
	otherwise =>
	  #"zoom-debugging";
      end;
    otherwise =>
      #"zoom-debugging";
  end select;
end function debugger-auto-zoom;


/// Debugger reuse

define function do-project-debuggers 
    (function :: <function>,
     project :: <project-object>,
     #key thread :: false-or(<thread-object>),
          test :: false-or(<function>),
          in-frame? :: <boolean> = #t)
 => ()
  local method matching-frame?
	    (frame :: <frame>) => (match? :: <boolean>)
	  when (instance?(frame, <debugger>)
		  & frame.frame-state ~== #"destroyed"
		  & frame.frame-current-project == project)
	    let frame-thread = frame.debugger-thread;
	    (~thread | thread == frame-thread)
	      & (~test | test(frame))
	  end
	end method matching-frame?;
  do-frames
    (method (frame :: <frame>) => ()
       when (matching-frame?(frame))
	 case
	   in-frame? => call-in-frame(frame, function, frame);
	   otherwise => function(frame)
	 end
       end
     end method,
     z-order: #"top-down")
end function do-project-debuggers;

define sealed sideways method find-debugger-from-environment 
    (portd :: type-union(<port>, <frame>),
     #key project :: <project-object>,
          thread :: false-or(<thread-object>) = #f,
          zoom :: false-or(<symbol>) = #f)
 => ()
  let thread :: false-or(<thread-object>)
    = thread | project.application-default-thread;
  if (thread)
    ensure-environment-frame(portd, <debugger>,
			     project:       project,
			     native-thread: thread,
			     zoom:          zoom)
  else
    let frame = instance?(portd, <frame>) & portd;
    environment-error-message("No application thread to interact with.",
			      owner: frame)
  end if;
end method find-debugger-from-environment;


/// Debugger startup

define method choose-debugger-for-thread
    (project :: <project-object>, thread :: <thread-object>,
     #key reuse? :: <boolean> = ~$debugger-settings.one-debugger-per-thread)
 => (debugger :: false-or(<debugger>))
  let first-debugger :: false-or(<debugger>) = #f;
  block (return)
    do-project-debuggers
      (method (debugger :: <debugger>)
	 if (~first-debugger)
	   first-debugger := debugger
	 end;
	 if (debugger.debugger-thread == thread)
	   return(debugger)
	 end
       end,
       project, in-frame?: #f);
    reuse? & first-debugger
  end
end method choose-debugger-for-thread;

define method reuse-debugger
    (debugger :: <debugger>, thread :: <thread-object>,
     #key zoom, interacted?) => ()
  if (zoom)
    debugger.debugger-zoom := zoom
  end;
  if (debugger.debugger-thread == thread)
    activate-debugger(debugger, interacted?: interacted?)
  else
    debugger.debugger-thread := thread
  end;
  raise-frame(debugger, activate?: #t)
end method reuse-debugger;

define method start-debugging
    (project :: <project-object>, thread :: false-or(<thread-object>),
     startup-option :: false-or(<application-startup-option>))
 => ()
  let debug?
    = if (thread)
	choose-debug?(project, thread)
      else
	(startup-option | $debugger-settings.open-debugger-on-pause)
      end;
  let thread = thread | project.application-default-thread;
  let ignore-interactive-breakpoint?
    = ignore-interactive-breakpoint-on-thread?(project, thread);
  if (debug? & thread & ~ignore-interactive-breakpoint?)
    let debugger = choose-debugger-for-thread(project, thread);
    let refresh-all? = $debugger-settings.refresh-all-on-debug;
    let zoom
      = case
	  application-just-hit-error?(project, thread) =>
	    #"zoom-debugging";
	  otherwise =>
	    zoom-description-from-startup-option(startup-option);
	end;
    do-project-debuggers
      (if (refresh-all?) activate-debugger else enable-debugger end,
       project, test: curry(\~==, debugger));
    if (debugger)
      let interacted? 
	= application-just-interacted?(project, thread);
      debugger.debugger-updated? := #f;
      call-in-frame
	(debugger, reuse-debugger, debugger, thread,
	 zoom: zoom, interacted?: interacted?)
    else
      fork-environment-frame
	(default-port(), <debugger>,
	 project:       project, 
	 native-thread: thread,
	 zoom:          zoom)
    end
  end
end method start-debugging;

//--- Don't open a debugger on a new thread just to show
//--- interactive results...
define function ignore-interactive-breakpoint-on-thread?
    (project :: <project-object>, thread :: <thread-object>)
 => (ignore? :: <boolean>)
  application-just-interacted?(project, thread)
  & ~choose-debugger-for-thread(project, thread, reuse?: #f);
end function;

define method frame-continue-application
    (frame :: <debugger>) => ()
  let project = frame.ensure-frame-project;

  continue-application(project, thread: frame.debugger-thread)
end method frame-continue-application;

define function note-project-process-started
    (project :: <project-object>) => ()
  local method close (debugger :: <debugger>)
	  within-frame (debugger)
	    exit-frame(debugger, destroy?: #t)
	  end;
        end method;
  if ($debugger-settings.one-debugger-per-thread)
    do-project-debuggers(close, project)
  end
end function note-project-process-started;

define function activate-debugger 
    (debugger :: <debugger>, #key interacted?) => ()
  enable-debugger(debugger, update?: #f);
  update-debugger(debugger, interacted?: interacted?)
end function activate-debugger;

define function enable-debugger
    (debugger :: <debugger>, #key update? = #t) => ()
  unless (debugger.debugger-enabled?)
    debugger.debugger-enabled? := #t;
    debugger.debugger-stack-displayer.displayer-enabled? := #t;
    debugger.debugger-interactor1-pane.interactor-pane-enabled? := #t;
    debugger.debugger-interactor2-pane.interactor-pane-enabled? := #t;

    // NB debuggers can be enabled without being updated so update 
    // enough of the debugger now to make it consistent.
    if (update?)
      update-debugger-command-table(debugger); 
      update-debugger-status-bar(debugger);
      update-debugger-context-pane(debugger)
    end
  end
end function enable-debugger;


/// Debugger refreshing

define method refresh-frame (debugger :: <debugger>) => ()
  next-method();
  remove-all-keys!(debugger.debugger-filtered-functions);
  update-debugger(debugger, refresh?: #t)
end method refresh-frame;

define method refresh-all-debuggers
    (project :: <project-object>) => ()
  do-project-debuggers(refresh-frame, project)
end method refresh-all-debuggers;

define function update-debugger
    (debugger :: <debugger>,
     #key refresh? :: <boolean> = ~debugger.debugger-updated?,
          interacted? :: <boolean>)
 => ()
  execute-debugger-function
    (method (debugger :: <debugger>)
       update-debugger-command-table(debugger);
       update-debugger-interactor-pane(debugger, refresh?: refresh?);

       unless (interacted? &
	       debugger-unchanged-during-interaction?(debugger))
	 if (refresh?)
	   update-debugger-context-pane(debugger);
	   update-debugger-source-pane(debugger, refresh?: #t);
	 end;
	 update-debugger-stack-pane(debugger, refresh?: refresh?);
	 update-debugger-register-window(debugger);
       end;

       debugger.debugger-updated? := #t; // NB set flag before updating status display
       debugger.debugger-interactor-transaction := #f;
       update-debugger-status-bar(debugger)
     end,
     debugger, message: "Refreshing display...")
end function update-debugger;

define inline function update-debugger-context-pane
    (debugger :: <debugger>) => ()
  update-context-pane(debugger.debugger-context-pane)
end function update-debugger-context-pane;

define function update-debugger-focus (debugger :: <debugger>) => ()
  when (frame-mapped?(debugger))
    let pane = debugger.debugger-interactor-pane;
    frame-input-focus(debugger) := pane.interactor-pane-default-focus
  end
end function update-debugger-focus;

define method note-debugger-thread-changed
    (debugger :: <debugger>) => ()
  let new-thread = debugger.debugger-thread;
  if (new-thread)
    let project = debugger.ensure-frame-project;
    debugger.debugger-thread-index := thread-index(project, new-thread)
  end;
  debugger.debugger-interactor1-pane.interactor-thread := new-thread;
  debugger.debugger-interactor2-pane.interactor-thread := new-thread;
  debugger.frame-title := generate-frame-title(debugger);
  if (debugger.debugger-enabled?)
    update-debugger(debugger)
  end
end method note-debugger-thread-changed;

// Determine on completion of an interaction if the debugger frame
// has been updated since the interaction began; if so, it needs to
// be updated again, otherwise no updates will be necessary as the
// Debugger Manager should ensure that thread contexts before and 
// after interactions are identical.

define function debugger-unchanged-during-interaction?
    (debugger :: <debugger>) => (unchanged? :: <boolean>)
  let project = debugger.ensure-frame-project;
  let thread = debugger.debugger-thread;
  let interactor-pane = debugger.debugger-interactor-pane;
  let transaction = debugger.debugger-interactor-transaction;

  if (transaction)
    interactor-pane.interactor-transaction == transaction
  end;
end function;


/// Debugger shutdown

define function disable-debugger 
    (debugger :: <debugger>) => ()
  if (debugger.debugger-enabled?)
    debugger.debugger-enabled? := #f;
    debugger.debugger-stack-gadget.gadget-enabled? := #f;
    debugger.debugger-interactor1-pane.interactor-pane-enabled? := #f;
    debugger.debugger-interactor2-pane.interactor-pane-enabled? := #f
  end
end function disable-debugger;

define function deactivate-debugger 
    (debugger :: <debugger>) => ()
  disable-debugger(debugger);
  debugger.debugger-updated? := #f;
  update-debugger-command-table(debugger);
  update-debugger-context-pane(debugger);
  update-debugger-status-bar(debugger);
  debugger-thread(debugger) := #f
end function deactivate-debugger;
