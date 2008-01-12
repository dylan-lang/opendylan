Module:    environment-tools
Synopsis:  Environment tools
Author:    Chris Page, Andy Armstrong, Scott McKay, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// PROTOCOLS

define open generic frame-create-breakpoint
    (frame :: <environment-frame>) => ();

define open generic frame-clear-breakpoint
    (frame :: <environment-frame>) => ();

define open generic frame-edit-breakpoint-options
    (frame :: <environment-frame>) => ();

define open generic frame-toggle-breakpoint-enabled?
    (frame :: <environment-frame>) => ();

define open generic frame-create-or-toggle-breakpoint
    (frame :: <environment-frame>) => ();

define open generic frame-run-to-cursor
    (frame :: <environment-frame>, target :: <object>) => ();

define open generic frame-new-breakpoint
    (frame :: <environment-frame>) => ();

define open generic frame-browse-all-breakpoints
    (frame :: <environment-frame>) => ();

define open generic frame-enable-all-breakpoints
    (frame :: <environment-frame>) => ();

define open generic frame-disable-all-breakpoints
    (frame :: <environment-frame>) => ();

define open generic frame-clear-all-breakpoints
    (frame :: <environment-frame>) => ();

define open generic frame-trace-target
    (frame :: <environment-frame>) => ();

define open generic frame-untrace-target
    (frame :: <environment-frame>) => ();

define open generic frame-untrace-all
    (frame :: <environment-frame>) => ();

define open generic record-breakpoint-source-locations
    (project :: <project-object>);


/// TYPES (internal)

// ---*** kludge: use <line-source-location> instead of <source-location> to
// avoid invoking code on the compiler's <range-source-location>s which don't
// know squat about <source-location-table>s.
define constant <breakpoint-location> 
  = type-union(<breakpoint-object>, <line-source-location>);

define constant <breakpointable>
  = type-union(<breakpoint-location>, <function-object>, <class-object>);


/// <BREAKPOINT-SLOT> (internal)

define sealed class <breakpoint-slot> (<object>)
  sealed slot breakpoint-slot-label :: <string>, required-init-keyword: label:;
  sealed slot breakpoint-slot-abbreviation :: <character>, required-init-keyword: abbrev:;
  sealed slot breakpoint-slot-key :: <symbol>, required-init-keyword: key:;
  sealed slot breakpoint-slot-getter :: <function>, required-init-keyword: getter:;
  sealed slot breakpoint-slot-setter :: <function>, required-init-keyword: setter:;
  sealed slot breakpoint-slot-init-value, required-init-keyword: value:;
end class;

define constant $breakpoint-slots =
  vector(
	 make(<breakpoint-slot>,
	      label: "Enabled",
	      abbrev: 'E',
	      key: #"enabled?",
	      value: $default-breakpoint-enabled?,
	      getter: breakpoint-enabled?,
	      setter: breakpoint-enabled?-setter),
	 make(<breakpoint-slot>,
	      label: "Pause application",
	      abbrev: 'B',
	      key: #"stop?",
	      value: $default-breakpoint-stop?,
	      getter: breakpoint-stop?,
	      setter: breakpoint-stop?-setter),
	 make(<breakpoint-slot>,
	      label: "Print message",
	      abbrev: 'M',
	      key: #"message?",
	      value: $default-breakpoint-message?,
	      getter: breakpoint-message?,
	      setter: breakpoint-message?-setter),
	 make(<breakpoint-slot>,
	      label: "Toggle profiling",
	      abbrev: 'P',
	      key: #"profile?",
	      value: $default-breakpoint-profile?,
	      getter: breakpoint-profile?,
	      setter: breakpoint-profile?-setter),
	 make(<breakpoint-slot>,
	      label: "One shot",
	      abbrev: 'T',
	      key: #"transient?",
	      value: $default-breakpoint-transient?,
	      getter: breakpoint-transient?,
	      setter: breakpoint-transient?-setter)
	   );

define function breakpoint-default-keys
    () => (keys :: <sequence>)
  let keys = make(<stretchy-vector>);
  for (item in $breakpoint-slots)
    if (breakpoint-slot-init-value(item))
      keys := add!(keys, breakpoint-slot-key(item));
    end if;
  end for;
  keys
end function;

/// Breakpoint handling

define method breakpoint-current? 
    (breakpoint :: <breakpoint-object>)
 => (stopped-at? :: <boolean>)
  let project = breakpoint.breakpoint-project;
  let application = project.project-application;
  if (application & (application-state(application) = #"stopped"))
    any?(method (thread :: <thread-object>)
	   member?(breakpoint, current-stop-breakpoints(project, thread))
	 end,
	 application-threads(application))
  end
end method breakpoint-current?;

/// Breakpoint coercion

define method coerce-to-breakpoint
    (project :: <project-object>, object :: <breakpoint-object>)
 => (breakpoint :: <breakpoint-object>)
  object
end method coerce-to-breakpoint;

define method coerce-to-breakpoint
    (project :: <project-object>, object :: <object>)
 => (breakpoint :: false-or(<breakpoint-object>))
  find-breakpoint(<breakpoint-object>,
		  project: project,
		  object: object)
end method coerce-to-breakpoint;

define function frame-target-to-breakpoint
    (frame :: <environment-frame>) => (object)
  frame-target-to-browse(frame)
end function frame-target-to-breakpoint;

define function frame-target-breakpoint
    (frame :: <environment-frame>)
 => (breakpoint :: false-or(<breakpoint-object>))
  let object = frame-target-to-breakpoint(frame);
  coerce-to-breakpoint(frame.ensure-frame-project, object)
end function frame-target-breakpoint;

/// Breakpoint commands

define function update-breakpoint-enabled-toggle
    (menu-box) => ()
  let frame = menu-box.sheet-frame;
  let breakpoint = frame-target-breakpoint(frame);
  if (breakpoint)
    let enabled? = breakpoint.breakpoint-enabled?;
    menu-box.gadget-enabled? := #t;
    menu-box.gadget-value := if (enabled?)
			       #[#"enabled?"]
			     else
			       #[]
			     end if;
  else
    menu-box.gadget-enabled? := #f;
    menu-box.gadget-value := #[]
  end if;
end function update-breakpoint-enabled-toggle;


define method frame-create-breakpoint
    (frame :: <environment-frame>) => ()
  let target  = frame-target-to-breakpoint(frame);
  if (target & ~instance?(target, <breakpoint-object>))
    make(<breakpoint-object>,
	 project: frame.ensure-frame-project,
	 object:  target);
  end if;
end method frame-create-breakpoint;

define method frame-clear-breakpoint
    (frame :: <environment-frame>) => ()
  let breakpoint = frame-target-breakpoint(frame);
  if (breakpoint)
    destroy-breakpoint(breakpoint);
  end if;
end method frame-clear-breakpoint;

define method frame-edit-breakpoint-options
    (frame :: <environment-frame>) => ()
  let project    = frame.ensure-frame-project;
  let target     = frame-target-to-breakpoint(frame);
  let breakpoint = coerce-to-breakpoint(project, target);
  let (dialog, ok?) = choose-breakpoint-options(frame, breakpoint | target);
  if (ok?)
    update-from-breakpoint-dialog(dialog, breakpoint | target);
  end if;
end method frame-edit-breakpoint-options;
       
define method update-from-breakpoint-dialog
    (dialog :: <breakpoint-dialog>, target :: <breakpoint-object>) => ()
  apply(reinitialize-breakpoint, target, breakpoint-arguments(dialog));
end method update-from-breakpoint-dialog;

define method update-from-breakpoint-dialog
    (dialog :: <breakpoint-dialog>, target :: <object>) => ()
  apply(make,
	<breakpoint-object>,
	object: target,
	project: dialog.frame-owner.ensure-frame-project,
	breakpoint-arguments(dialog))
end method update-from-breakpoint-dialog;

define method frame-toggle-breakpoint-enabled?
    (frame :: <environment-frame>) => ()
  let breakpoint = frame-target-breakpoint(frame);
  if (breakpoint)
    reinitialize-breakpoint(breakpoint,
			    enabled?: ~breakpoint.breakpoint-enabled?);
  end if;
end method frame-toggle-breakpoint-enabled?;

define method frame-create-or-toggle-breakpoint
    (frame :: <environment-frame>) => ()
  let project    = frame.ensure-frame-project;
  let target     = frame-target-to-breakpoint(frame);
  let breakpoint = coerce-to-breakpoint(project, target);
  if (breakpoint)
    reinitialize-breakpoint(breakpoint,
			    enabled?: ~breakpoint.breakpoint-enabled?);
  else
    make(<breakpoint-object>,
	 project: project,
	 object:  target);
  end if;
end method frame-create-or-toggle-breakpoint;

define method frame-run-to-cursor
    (frame :: <environment-frame>, target :: <breakpoint-location>) => ()
  let project = frame.ensure-frame-project;
  select (target by instance?)
    <source-location> =>
      make(<source-location-breakpoint-object>,
	   project: project,
	   object: target,
	   transient?: #t);
    <breakpoint-object> =>
      // ---*** Ensure we stop if there is already a breakpoint here.
      // However, this means that this now always stops even if it didn't
      // before. Either we need to reintroduce multiple breakpoints per
      // location or some other mechanism (e.g. callback of undo work).
      reinitialize-breakpoint(target, stop?: #t);
  end select;
  let application = project.project-application;
  select (application & application-state(application))
    #"running" => #f;
    #"stopped" =>
      frame-continue-application(frame);
    #"uninitialized", #"closed", #f => 
      if (environment-question("The application is not currently running.\n"
			       "Start it?",
			       owner: frame))
	frame-start-application(frame);
      end if;
  end select;
end method frame-run-to-cursor;

define method frame-run-to-target
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let target  = frame-target-to-breakpoint(frame);
  frame-run-to-cursor(frame, target)
end method frame-run-to-target;

define method frame-new-breakpoint
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let (dialog, ok?) = choose-new-breakpoint(frame);
  if (ok?)
    let name   = dialog.breakpoint-dialog-name-pane.gadget-value;
    let object = find-named-object(frame, name);
    select (object by instance?)
      <function-object> =>
	make(<breakpoint-object>, project: project, object: object);
      <class-object> =>
	make(<class-breakpoint-object>, project: project, object: object);
      otherwise =>
	let module
	  = environment-object-primitive-name(project, frame.frame-current-module);
	let message
	  = format-to-string("'%s' is not a function or a class in module %s.",
			     name, module);
	environment-warning-message(message, owner: frame);
    end
  end
end method frame-new-breakpoint;

define method frame-browse-all-breakpoints
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  find-environment-frame
    (frame, <project-browser>, 
     project: project,
     page: #"breakpoints")
end method frame-browse-all-breakpoints;

define method frame-enable-all-breakpoints
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  with-compressed-breakpoint-state-changes ()
    for (bkp in source-location-breakpoints(project))
      breakpoint-enabled?(bkp) := #t;
    end for;
    for (bkp in environment-object-breakpoints(project))
      breakpoint-enabled?(bkp) := #t;
    end for;
  end;
end method frame-enable-all-breakpoints;

define method frame-disable-all-breakpoints
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  with-compressed-breakpoint-state-changes ()
    for (bkp in source-location-breakpoints(project))
      breakpoint-enabled?(bkp) := #f;
    end for;
    for (bkp in environment-object-breakpoints(project))
      breakpoint-enabled?(bkp) := #f;
    end for;
  end;
end method frame-disable-all-breakpoints;

define method frame-clear-all-breakpoints
    (frame :: <environment-frame>) => ()
  if (environment-question
	("Clear all current breakpoints?", 
	 owner: frame, style: #"warning"))
    let project = frame.ensure-frame-project;
    with-compressed-breakpoint-state-changes ()
      for (bkp in source-location-breakpoints(project))
	destroy-breakpoint(bkp);
      end for;
      for (bkp in environment-object-breakpoints(project))
	destroy-breakpoint(bkp);
      end for;
    end;
  end if;
end method frame-clear-all-breakpoints;

define method frame-clear-obsolete-breakpoints
    (frame :: <environment-frame>, project :: <project-object>)
 => ()
  let invalid-breakpoints = make(<stretchy-vector>);
  for (breakpoint in project.environment-object-breakpoints)
    let object = breakpoint.breakpoint-object;
    unless (environment-object-exists?(project, object))
      add!(invalid-breakpoints, breakpoint)
    end
  end;
  let invalid-count = size(invalid-breakpoints);
  unless (invalid-count == 0)
    let name
      = invalid-count == 1
          & block ()
	      let breakpoint = invalid-breakpoints[0];
	      let object = breakpoint.breakpoint-object;
	      frame-object-unique-name(frame, object)
	    exception (object :: <invalid-object-error>)
	      #f
	    end;
    let message
      = if (name)
	  format-to-string
	    ("Removing breakpoint for '%s' which no longer exists",
	     name);
	else
	  format-to-string
	    ("Removing %d breakpoints for objects that no longer exist",
	     invalid-count)
	end;
    environment-warning-message(message, owner: frame);
    for (breakpoint in invalid-breakpoints)
      destroy-breakpoint(breakpoint)
    end
  end
end method frame-clear-obsolete-breakpoints;


/// Breakpoint Popup Menu

define method update-frame-commands-for-browse-target
    (frame :: <environment-frame>, object :: <breakpointable>) => ()
  next-method();
  let project = frame.ensure-frame-project;
  let breakpoint? = coerce-to-breakpoint(project, object) & #t;
  let source-location? = instance?(object, <source-location>);
  command-enabled?(frame-create-breakpoint, frame) := ~breakpoint?;
  command-enabled?(frame-clear-breakpoint,  frame) := breakpoint?;
  command-enabled?(frame-run-to-target,     frame) := source-location?;
end method update-frame-commands-for-browse-target;


/// Breakpoint Dialog

define function choose-breakpoint-options
    (frame :: <frame>, target, #rest args, #key)
 => (dialog :: <breakpoint-dialog>, ok? :: <boolean>)
  with-frame-manager(frame.frame-manager)
    let dialog = apply(make,
		       <breakpoint-dialog>, 
		       owner: frame,
		       target: target,
		       args);
    values(dialog, start-dialog(dialog) & #t);
  end;
end function choose-breakpoint-options;

define frame <breakpoint-dialog> (<dialog-frame>)
  keyword title: = "Edit Breakpoint Options";
  pane options-box (dialog)
    make(<check-box>,
	 items: $breakpoint-slots,
	 orientation: #"vertical",
	 value-changed-callback:
	   method (gadget)
	     let message? = member?(#"message?", dialog.options-box.gadget-value);
	     dialog.message-field.gadget-enabled? := message?;
	     dialog.message-label.gadget-enabled? := message?;
	   end method,
	 label-key: breakpoint-slot-label,
	 value-key: breakpoint-slot-key);
  pane message-label (dialog)
    make(<label>,
	 label: "Message Text: ");
  pane message-field (dialog)
    make(<text-field>, min-width: 200);
  layout (dialog)
    vertically (spacing: 8, equalize-widths?: #t)
      grouping ("Breakpoint Options")
        dialog.options-box
      end;
      horizontally (spacing: 4)
        dialog.message-label;
        dialog.message-field;
      end;
    end;
end frame <breakpoint-dialog>;

define method initialize
    (dialog :: <breakpoint-dialog>, #key target) => ()
  next-method();
  initialize-breakpoint-dialog(dialog, target);
end method initialize;

define method initialize-breakpoint-dialog
    (dialog :: <breakpoint-dialog>, target :: <breakpoint-object>) => ()
  dialog.options-box.gadget-value := breakpoint-keys(target);
  dialog.message-field.gadget-value := target.breakpoint-message? | "";
end method initialize-breakpoint-dialog;

define method initialize-breakpoint-dialog
    (dialog :: <breakpoint-dialog>, target :: <object>) => ()
  dialog.options-box.gadget-value := breakpoint-default-keys();
  dialog.message-field.gadget-value := "";
end method initialize-breakpoint-dialog;

define function breakpoint-keys
    (breakpoint :: <breakpoint-object>) => (keys :: <sequence>)
  let keys = make(<stretchy-vector>);
  for (item in $breakpoint-slots)
    if (breakpoint-slot-getter(item)(breakpoint))
      add!(keys, breakpoint-slot-key(item))
    end if;
  end for;
  keys
end function breakpoint-keys;

define function breakpoint-arguments
    (dialog :: <breakpoint-dialog>) => (arguments :: <sequence>)
  let selected-keys = dialog.options-box.gadget-value;
  let message? = dialog.message-field.gadget-value;
  let args = make(<vector>, size: $breakpoint-slots.size * 2);
  for (item in $breakpoint-slots,
       i from 0 by 2)
    let key = breakpoint-slot-key(item);
    args[i] := key;
    args[i + 1] :=
      member?(key, selected-keys)
      & if (key = #"message?")
	  message?
	else
	  #t
	end if;
  end for;
  args;
end function breakpoint-arguments;


/// New Breakpoint Dialog

define variable $new-breakpoint-dialog-width :: <integer> = 500;

define frame <new-breakpoint-dialog> (<dialog-frame>)
  constant slot %frame :: <environment-frame>,
    required-init-keyword: environment-frame:;
  pane breakpoint-dialog-name-pane (dialog)
    make(<text-field>,
	 documentation: "Function name optionally qualified by namespaces (name:module:library).",
	 value-changing-callback:
	   method (gadget)
	     let dialog = sheet-frame(gadget);
	     let object = find-named-object(dialog.%frame, gadget-value(gadget) | "");
	     dialog-exit-enabled?(dialog) := object & #t;
	   end method,
	 activate-callback: exit-dialog);
  layout (dialog)
    horizontally (spacing: 4)
      make(<label>, label: "Function:");
      dialog.breakpoint-dialog-name-pane;
    end;
  input-focus (dialog)
    dialog.breakpoint-dialog-name-pane;
  keyword title: = "New Breakpoint";
end frame <new-breakpoint-dialog>;

define method choose-new-breakpoint
    (frame :: <environment-frame>)
 => (dialog :: <new-breakpoint-dialog>, ok? :: <boolean>)
  with-frame-manager(frame.frame-manager)
    let dialog = make(<new-breakpoint-dialog>,
		      owner: frame,
		      width: max($new-breakpoint-dialog-width, 250),
		      environment-frame: frame);
    dialog-exit-enabled?(dialog) := #f;
    let result = start-dialog(dialog) & #t;
    when (result)
      let (width, height) = frame-size(dialog);
      $new-breakpoint-dialog-width := width;
    end;
    values(dialog, result);
  end;
end method choose-new-breakpoint;


/// Breakpoint State Changes Failed Dialog

define open generic make-breakpoint-table-control-displayer
    (frame :: <environment-frame>, #key, #all-keys)
 => (displayer :: <displayer-mixin>);

define frame <breakpoint-state-changes-failed-dialog>
    (<environment-fixed-project-frame>, 
     <frame-module-mixin>,
     <environment-dialog-frame>)
  sealed constant slot failed-breakpoints-state :: <breakpoint-state>,
    required-init-keyword: breakpoint-state:;
  sealed constant slot failed-breakpoints :: <sequence>,
    required-init-keyword: breakpoints:;
  slot breakpoint-displayer :: <displayer-mixin>;
  layout (frame)
    begin
      let state = frame.failed-breakpoints-state;
      let breakpoints = frame.failed-breakpoints;
      let (pronoun-string, breakpoint-string)
	= if (size(breakpoints) > 1)
	    values("these", "breakpoints")
	  else
	    values("this", "breakpoint")
	  end;
      let label1 = concatenate("The following ", breakpoint-string,
			       " could not be changed in the application.");
      let label2 = concatenate("Click 'OK' to continue without ",
			       pronoun-string, " ", breakpoint-string);
      let displayer
	= make-breakpoint-table-control-displayer
	    (frame, 
	     //--- hughg, 1998/03/31: Can't display icons because that
	     //--- requires recursively querying the runtime-manager to
	     //--- get the breakpoint state, which causes a <lock> conflict!
	     icon-function: always(#f),
	     // icon-function: curry(environment-object-icon, project),
	     widths: #[250, 70, 250],
	     children-generator: method (project)
				   ignore(project);
				   frame.failed-breakpoints
				 end);
      // Disable double-click and right-click callbacks.
      let collection-gadget = displayer.displayer-collection-gadget;
      gadget-popup-menu-callback(collection-gadget) := #f;
      gadget-activate-callback(collection-gadget) := #f;
      // Store the displayer for reference in <frame-mapped-event> handler.
      frame.breakpoint-displayer := displayer;
      // At last, the layout!
      vertically (spacing: 8)
	vertically ()
	  make(<label>, label: label1);
	  make(<label>, label: label2);
	end;
	frame.breakpoint-displayer;
      end;
    end;
  keyword mode: = #"modal";
  keyword cancel-callback: = #f;
  keyword width: = 500;
end frame <breakpoint-state-changes-failed-dialog>;

define method handle-event
    (frame :: <breakpoint-state-changes-failed-dialog>,
     event :: <frame-mapped-event>)
 => ()
  raise-frame(frame);
  with-busy-cursor (frame)
    let displayer = frame.breakpoint-displayer;
    // The state has to be an <environment-object>, though we
    // don't need one here.  Just use the project, and ignore it.
    let state = make-displayer-state(displayer, frame.ensure-frame-project);
    displayer.displayer-state := state
  end;
end method handle-event;

define method notify-user-breakpoint-state-changes-failed
    (project :: <project-object>, state :: <breakpoint-state>,
     breakpoints :: <sequence> /* of: <breakpoint-object>s */)
 => ()
  // We want this dialog to block (the calling thread is probably the
  // debugger-manager thread) until the user hits "OK".  Therefore it
  // has to be modal and it has to have no owner (otherwise it might
  // share an event loop, in which case start-dialog might return
  // immediately).  There's a <frame-mapped-event> handler to raise
  // this frame explicitly, in case it appears at the back.
  //---*** hughg, 1998/03/27: Disable all windows associated with the
  //---*** project until the dialog exits?
  let dialog = make(<breakpoint-state-changes-failed-dialog>,
		    project: project,
		    module: #f,
		    breakpoint-state: state, breakpoints: breakpoints);
  start-dialog(dialog);
end method notify-user-breakpoint-state-changes-failed;


/// Tracing

define method frame-trace-target
    (frame :: <environment-frame>) => ()
  let target  = frame-target-to-breakpoint(frame);
  if (target & instance?(target, <function-object>))
    trace-function(frame.ensure-frame-project, target)
  end
end method frame-trace-target;

define method frame-untrace-target
    (frame :: <environment-frame>) => ()
  frame-clear-breakpoint(frame)
end method frame-untrace-target;

define method frame-untrace-all
    (frame :: <environment-frame>) => ()
  //---*** We need to do something smarter here!
  frame-clear-all-breakpoints(frame)
end method frame-untrace-all;
