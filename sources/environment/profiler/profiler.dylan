Module:    environment-profiler
Synopsis:  The profiling tool provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profiler

define frame <profiler> 
    (<frame-refresh-mixin>,
     <frame-module-gadget-mixin>,
     <frame-cascading-window-mixin>,
     <profiler-summary>,
     <profiler-call-history>,
     <profiler-functions>,
     <profiler-classes>,
     <profiler-time-line>,
     <environment-project-tool>)
  sealed slot frame-current-thread :: false-or(<thread-object>) = #f,
    setter: %thread-setter;
  sealed slot profiler-application-profile :: false-or(<application-profile>) = #f;
  sealed slot profiler-show-foreign-functions? :: <boolean> = #f,
    setter: %show-foreign-functions?-setter;
  pane thread-selector (frame)
    make(<option-box>,
	 label-key:     curry(frame-default-object-name, frame),
	 documentation: "Select Profiler Thread",
	 min-width:     100,
	 max-width:     $fill,
	 value-changed-callback: method (gadget :: <option-box>) => ()
				   let frame = sheet-frame(gadget);
				   let thread = gadget.gadget-value;
				   frame.frame-current-thread := thread
				 end);
  pane tab-layout (frame)
    make(<tab-control>,
	 pages: vector(frame.summary-page,
		       frame.call-history-page,
		       frame.functions-page,
		       frame.classes-page
		       // ,frame.time-line-page
		       ),
	 value-changed-callback: method (tab-control)
				   ignore(tab-control);
				   refresh-profiler(frame)
				 end);
  pane message-pane (frame)
    make(<label>,
	 label: "",
	 min-width: 150, max-width: 150,
	 documentation: "Message Area");
  pane selection-summary (frame)
    make(<label>,
	 label: "",
	 min-width: 100, max-width: $fill,
	 documentation: "Selection Summary");
  pane main-layout (frame)
    vertically (spacing: 2)
      make(<separator>);
      horizontally (spacing: 2, y-alignment: #"center")
        make(<label>, label: "Thread:");
        frame.thread-selector
      end;
      make(<separator>);
      frame.tab-layout
    end;
  layout     (frame) frame.main-layout;
  tool-bar   (frame) make-environment-tool-bar(frame);
  status-bar (frame) 
    make(<status-bar>,
	 label-pane: frame.message-pane,
	 children: vector(frame.message-pane,
			  frame.selection-summary));
  command-table (frame) *profiler-command-table*;
  keyword width:  = $default-environment-frame-width;
  keyword height: = $default-environment-frame-height;
  keyword icon: = $profile-bitmap;
  keyword frame-class-name: = #"profiler";
end frame <profiler>;

ignore(time-line-page);

define cascading-window-settings
  profiler-window :: <profiler> = "Profiler Window";

define method handle-event
    (frame :: <profiler>, event :: <frame-mapped-event>)
 => ()
  next-method();
  refresh-frame(frame)
end method handle-event;

define sideways method find-profiler-from-environment
    (portd :: type-union(<port>, <frame>), #key project :: <project-object>)
 => ()
  ensure-environment-frame(portd, <profiler>, project: project)
end method find-profiler-from-environment;

define method generate-frame-title
    (frame :: <profiler>) => (title :: <string>)
  let project = frame.frame-project;
  concatenate("Profiling ",
              frame-default-object-name(frame, project),
              " - ", release-name())
end method generate-frame-title;

define method frame-selection-message-setter
    (message :: <string>, frame :: <profiler>)
 => (message :: <string>)
  let label = frame.selection-summary;
  gadget-label(label) := message
end method frame-selection-message-setter;

define method frame-current-thread-setter
    (thread :: false-or(<thread-object>), frame :: <profiler>,
     #key refresh? :: <boolean> = #t,
          clean? :: <boolean> = #f)
 => (thread :: false-or(<thread-object>))
  unless (thread == frame.frame-current-thread)
    frame.%thread := thread;
    gadget-value(frame.thread-selector) := thread;
    refresh? & refresh-profiler-current-page(frame, clean?: clean?)
  end;
  thread
end method frame-current-thread-setter;

define method profiler-show-foreign-functions?-setter
    (foreign? :: <boolean>, frame :: <profiler>)
 => (foreign? :: <boolean>)
  unless (foreign? == frame.profiler-show-foreign-functions?)
    frame.%show-foreign-functions? := foreign?;
    refresh-frame(frame)
  end;
  foreign?
end method profiler-show-foreign-functions?-setter;


/// Utilities

define method profiler-has-results?
    (frame :: <profiler>) => (results? :: <boolean>)
  frame.profiler-application-profile ~== #f
end method profiler-has-results?;

//---*** Make this a protocol?
define method allocation-profiling?
    (profile :: <application-profile>)
 => (allocation-profiling? :: <boolean>)
  let options = profile.application-profile-options;
  let sampling-options = options.profile-sampling-options;
  sampling-options.profile-sampling-style == #"allocation"
end method allocation-profiling?;

define method profiler-has-class-results?
    (frame :: <profiler>) => (results? :: <boolean>)
  let profile = frame.profiler-application-profile;
  profile & profile.allocation-profiling?
end method profiler-has-class-results?;

define method perform-profiler-transaction
    (frame :: <profiler>, function :: <function>)
 => (#rest values)
  perform-application-transaction(frame.ensure-frame-project, function)
end method perform-profiler-transaction;


/// Property page handling

define method refresh-frame 
    (frame :: <profiler>) => ()
  next-method();
  refresh-profiler(frame, clean?: #t)
end method refresh-frame;

define method refresh-profiler
     (frame :: <profiler>, #key clean? :: <boolean> = #f) => ()
  with-busy-cursor (frame)
    let project = frame.ensure-frame-project;
    let application = project.project-application;
    frame-status-message(frame)    := "";
    frame-selection-message(frame) := "";
    if (clean? | ~frame.profiler-application-profile)
      frame.profiler-application-profile := project.project-last-profile;
      invalidate-profiler-pages(frame)
    end;
    let threads
      = keyed-sort(frame.profiler-available-threads,
		   key: method (thread :: <thread-object>)
			  //---*** It is a pity we can't find the index
			  //---*** after the application has shut down.
			  if (application)
			    thread-index(application, thread)
			  else
			    $maximum-integer
			  end
			end);
    gadget-items(frame.thread-selector) := threads;
    unless (member?(frame.frame-current-thread, threads))
      let new-thread = ~empty?(threads) & threads[0];
      frame-current-thread(frame, refresh?: #f) := new-thread
    end;
    refresh-profiler-current-page(frame, clean?: clean?)
  end
end method refresh-profiler;

define method invalidate-profiler-pages
    (frame :: <profiler>) => ()
  for (page in frame.tab-layout.tab-control-pages)
    invalidate-profiler-page(frame, page.gadget-id)
  end
end method invalidate-profiler-pages;

define method profiler-available-threads
    (frame :: <profiler>) => (threads :: <sequence>)
  let project = frame.ensure-frame-project;
  let profile = frame.profiler-application-profile;
  let thread = frame.frame-current-thread;
  let application = project.project-application;
  remove-duplicates
    (concatenate
       (if (application)
	  application.application-threads
	else
	  #[]
	end,
	if (profile)
	  profile.application-profile-threads
	else
	  #[]
	end))
end method profiler-available-threads;

define method refresh-profiler-current-page
    (frame :: <profiler>,
     #key clean? :: <boolean> = #f)
 => ()
  let page = frame.tab-layout.tab-control-current-page;
  refresh-profiler-page(frame, page.gadget-id, clean?: clean?)
end method refresh-profiler-current-page;

define method profiler-activate-callback
    (frame :: <frame>, values :: <object>) => ()
  let value
    = case
	~instance?(values, <sequence>) => values;
	~empty?(values)                => values[0];
	otherwise                      => #f;
      end;
  environment-activate-callback(frame, value)
end method profiler-activate-callback;

define method frame-note-application-state-changed
    (frame :: <profiler>, state :: false-or(<application-state>))
 => ()
  next-method();
  if (~state | state == #"closed")
    frame.profiler-application-profile := #f;
  end
end method frame-note-application-state-changed;


/// Profiler command table

define command-table *profiler-file-io-command-table* (*global-command-table*)
  include *export-command-table*;
  //---*** andrewa: remove printing options for 1.0
  // include *print-command-table*;
  separator;
  menu-item "Close"      = frame-close-file,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Closes the window.";
end command-table *profiler-file-io-command-table*;

define command-table *profiler-basic-view-command-table* (*global-command-table*)
  include *view-refresh-command-table*;
  separator;
  menu-item "Profiling Options..." = frame-edit-options,
    documentation: "Enables you to change profiling options.";
end command-table *profiler-basic-view-command-table*;

define command-table *profiler-view-command-table* (*global-command-table*)
  include *profiler-basic-view-command-table*;
end command-table *profiler-view-command-table*;

define command-table *profiler-go-command-table* (*global-command-table*)
  include *browse-locations-command-table*;
end command-table *profiler-go-command-table*;

define command-table *profiler-command-table* (*global-command-table*)
  menu-item "File"    = *profiler-file-io-command-table*;
  menu-item "Edit"    = *edit-command-table*;
  menu-item "View"    = *profiler-view-command-table*;
  menu-item "Go"      = *profiler-go-command-table*;
  menu-item "Window"  = *windows-command-table*;
  menu-item "Help"    = *environment-help-command-table*;
end command-table *profiler-command-table*;
