Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Chris Page, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// General protocols for environment frames

define open generic environment-frame-class-name
  (frame :: <frame>) => (name :: false-or(<symbol>));

define method environment-frame-class-name
    (frame :: <frame>) => (name :: false-or(<symbol>))
  #f
end method environment-frame-class-name;


/// Environment frame

define open abstract class <environment-frame> (<frame>)
  sealed constant each-subclass slot environment-frame-class-name :: false-or(<symbol>) = #f,
    init-keyword: frame-class-name:;
  sealed slot frame-qualify-names? :: <boolean> = environment-qualify-names?(),
    setter: %qualify-names?-setter,
    init-keyword: qualify-names?:;
  sealed slot frame-popup-menu-cache :: <object-table> = make(<object-table>);
  slot frame-command-target :: false-or(<command-target>) = #f,
    setter: %command-target-setter;
end class <environment-frame>;

define open abstract class <environment-simple-frame> 
    (<environment-frame>,
     <simple-frame>)
end class <environment-simple-frame>;

define open abstract class <environment-dialog-frame> 
    (<environment-frame>,
     <dialog-frame>)
end class <environment-dialog-frame>;

define method initialize 
    (frame :: <environment-frame>, #key) => ()
  next-method();
  frame.frame-title := generate-frame-title(frame);
  // Update command enabling
  command-enabled?(undo-minimize-all-frames, frame)
    := ~empty?(*minimized-frames*);
end method initialize;

define method frame-qualify-names?-setter
    (qualify? :: <boolean>, frame :: <environment-frame>)
 => (qualify? :: <boolean>)
  frame.%qualify-names? := qualify?;
  refresh-frame(frame);
  qualify?
end method frame-qualify-names?-setter;

define method frame-primary-object-name
    (frame :: <environment-frame>, object :: <environment-object>)
 => (name :: <string>)
  frame-default-object-name(frame, object)
end method frame-primary-object-name;


/// Environment frame protocols

define open generic generate-frame-title
    (frame :: <environment-frame>) => (title :: <string>);

define open generic frame-current-project 
    (frame :: <frame>)
 => (project :: false-or(<project-object>));

define method generate-frame-title 
    (frame :: <environment-frame>) => (title :: <string>)
  release-name()
end method generate-frame-title;

define method frame-current-project 
    (frame :: <frame>)
 => (project :: singleton(#f))
  #f;
end method frame-current-project;

define function ensure-frame-project
    (frame :: <environment-frame>)
 => (project :: <project-object>)
  let project = frame.frame-current-project;
  assert(project, "Frame '%s' has no project", frame-title(frame));
  project
end function ensure-frame-project;

define method frame-command-target-setter
    (target :: false-or(<command-target>), frame :: <environment-frame>)
 => (target :: false-or(<command-target>))
  frame.%command-target := target;
  note-frame-command-target-updated(frame);
  target
end method frame-command-target-setter;

define method note-frame-command-target-updated
    (frame :: <environment-frame>) => ()
  #f
end method note-frame-command-target-updated;


/// Notification protocols

define open generic frame-note-application-state-changed
    (frame :: <environment-frame>, state :: false-or(<application-state>))
 => ();

define open generic frame-note-application-starting
    (frame :: <environment-frame>)
 => ();

// Note: This function is called when we've finished trying to start the
// application; that is, either the application is about to "really" start
// (all initialization, breakpoint setup etc. is done) or we failed to
// start it at all.  It started if
//   frame.frame-project.project-application.application-closed?
// is false.
define open generic frame-note-application-starting-done
    (frame :: <environment-frame>)
 => ();

define open generic frame-note-application-threads-changed
    (frame :: <environment-frame>) => ();

define method frame-note-application-state-changed
    (frame :: <environment-frame>, state :: false-or(<application-state>))
 => ()
  enable-application-command-table(frame, state);
end method frame-note-application-state-changed;

define method frame-note-application-starting
    (frame :: <environment-frame>) => ()
  //---*** This doesn't deal well with multiple background operations
  frame-cursor-override(frame) := #"starting"
end method frame-note-application-starting;

define method frame-note-application-starting-done
    (frame :: <environment-frame>) => ()
  //---*** This doesn't deal well with multiple background operations
  frame-cursor-override(frame) := #f
end method frame-note-application-starting-done;

define method frame-note-application-threads-changed
    (frame :: <environment-frame>) => ()
  // I don't think we need to do anything in the general case.
end method frame-note-application-threads-changed;

define open generic frame-note-breakpoint-state-changed
    (frame :: <environment-frame>, breakpoint :: <breakpoint-object>, 
     state :: <breakpoint-state>)
 => ();

define method frame-note-breakpoint-state-changed
    (frame :: <environment-frame>, breakpoint :: <breakpoint-object>, 
     state :: <breakpoint-state>)
 => ()
  #f
end method frame-note-breakpoint-state-changed;

define open generic frame-note-interaction-returned
    (frame :: <frame>, thread :: <thread-object>, id :: <object>)
 => ();

define open generic frame-note-interactive-compilation-warnings
    (frame :: <frame>, thread :: <thread-object>, id :: <object>,
     warnings :: <sequence>)
 => ();

define method frame-note-interaction-returned
    (frame :: <environment-frame>, thread :: <thread-object>,
     id :: <object>)
 => ()
  #f
end method frame-note-interaction-returned;

define method frame-note-interactive-compilation-warnings
    (frame :: <environment-frame>, thread :: <thread-object>,
     id :: <object>, warnings :: <sequence>)
 => ()
  #f
end method frame-note-interactive-compilation-warnings;

define open generic frame-note-project-changed
    (frame :: <frame>, project :: false-or(<project-object>))
 => ();

define method frame-note-project-changed
    (frame :: <environment-frame>, project :: false-or(<project-object>))
 => ()
  // Do nothing.
end method frame-note-project-changed;


/// Application message handlers

define function environment-tools-application-message-receiver
    (message :: <application-message>) => ()
  let project = message.message-project;
  let _default-port = default-port();
  select (message by instance?)
    <run-application-requested-message> =>
      do-frames
	(method (frame :: <frame>)
	   when (instance?(frame, <environment-frame>))
	     call-in-frame(frame, frame-note-application-starting, frame);
	   end;
	 end method,
	 port: _default-port);
    <application-initialized-message> =>
      do-frames
	(method (frame :: <frame>)
	   when (instance?(frame, <environment-frame>))
	     call-in-frame(frame, frame-note-application-starting-done, frame);
	   end;
	 end method,
	 port: _default-port);
    <run-application-failed-message> =>
      do-frames
	(method (frame :: <frame>)
	   when (instance?(frame, <environment-frame>))
	     call-in-frame(frame, frame-note-application-starting-done, frame);
	   end;
	 end method,
	 port: _default-port);
      let application = project.project-application;
      with-environment-frame
	  (frame = _default-port, <project-browser>, project: project)
	let project-name = environment-object-display-name(project, project, #f);
	let message
	  = format-to-string("Could not execute the file '%s' to start project '%s'.",
			     application.application-filename, project-name);
	environment-error-message(message, owner: frame);
      end;
    <application-state-changed-message> =>
      let application = project.project-application;
      if (application)
	let state = application.application-state;
	do-project-frames
	  (method (frame :: <environment-frame>)
	     call-in-frame(frame, 
			   frame-note-application-state-changed,
			   frame, state)
	   end method,
	   project,
	   include: <environment-frame>)
      end;
    <application-threads-changed-message> =>
      do-project-frames
	(method (frame :: <environment-frame>)
	   call-in-frame(frame, frame-note-application-threads-changed, frame)
	 end method,
	 project,
	 include: <environment-frame>);
    <thread-interactive-warnings-message> =>
      let thread = message.message-thread;
      let id = message.message-transaction-id;
      let warnings = message.message-warnings;
      do-project-frames
	(method (frame)
	   call-in-frame(frame, frame-note-interactive-compilation-warnings,
			 frame, thread, id, warnings)
	 end method,
	 project);
    otherwise =>
      #f;
  end
end function environment-tools-application-message-receiver;

//---*** Should we do this at top-level, or in some initialisation function?
tune-in($project-channel,
	environment-tools-application-message-receiver,
	message-type: <application-message>);

define function environment-tools-breakpoint-message-receiver
    (message :: <breakpoint-state-change-message>) => ()
  let project = message.message-project;
  let state = message.message-breakpoint-state;
  select (message by instance?)
    <single-breakpoint-state-change-message> =>
      let breakpoint = message.message-breakpoint;
      do-project-frames
	(method (frame :: <environment-frame>)
	   call-in-frame(frame, frame-note-breakpoint-state-changed, 
			 frame, breakpoint, state)
	 end,
	 project,
	 include: <environment-frame>);

    <all-breakpoints-state-change-message> =>
      do-project-frames
	(method (frame :: <environment-frame>)
	   call-in-frame(frame, frame-note-all-breakpoints-changed,
			 frame, state)
	 end,
	 project);
    <breakpoint-state-changes-failed-message> =>
      let breakpoints = message.message-breakpoints;
      notify-user-breakpoint-state-changes-failed
	(project, state, breakpoints);
  end;
end function environment-tools-breakpoint-message-receiver;

//---*** Should we do this at top-level, or in some initialisation function?
tune-in($project-channel,
	environment-tools-breakpoint-message-receiver,
	message-type: <breakpoint-state-change-message>);

define open generic frame-note-all-breakpoints-changed
    (frame :: <environment-frame>, state :: <breakpoint-state>)
 => ();

define method frame-note-all-breakpoints-changed
    (frame :: <environment-frame>, state :: <breakpoint-state>)
 => ()
  #f
end method frame-note-all-breakpoints-changed;


/// Frame operation handling
///--- Should this be in DUIM?

define macro with-frame-background-operation
  { with-frame-background-operation (?frame:name, ?name:expression)
      ?body:body
    end }
 => { call-in-frame(?frame,
		    method() frame-status-message(?frame) := ?name end);
      make(<thread>,
	   name: ?name,
	   function: method ()
		       with-background-cursor (?frame)
			 ?body
		       end
		     end)
      }
end macro with-frame-background-operation;


/// Open

define open generic frame-open-object 
    (frame :: <frame>, object :: <object>) => ();

define method frame-open-object
    (frame :: <frame>, object :: <object>) => ()
  environment-action-unavailable(frame, "No open action available")
end method frame-open-object;

define method frame-open-object
    (frame :: <environment-frame>, project :: <project-object>)
 => ()
  frame-open-project(frame, project)
end method frame-open-object;


/// Locator type handling
///--- Probably should use MIME types, somehow

define method frame-open-by-default?
    (frame :: <environment-frame>, locator :: <file-locator>)
 => (default? :: <boolean>)
  select (locator.environment-locator-type)
    #"dylan", #"html" => #t;
    otherwise         => #f;
  end
end method frame-open-by-default?;

define method locator-has-source?
    (locator :: <file-locator>) => (source? :: <boolean>)
  select (locator.environment-locator-type)
    #f, #"lid", #"dylan", #"dyl", #"spec"           => #t;
    #"c", #"cpp", #"cxx", #"h", #"hpp", #"hxx"      => #t;
    #"inl", #"rc", #"txt", #"text", #"html", #"idl" => #t;
    otherwise                                       => #f;
  end
end method locator-has-source?;


/// Browsing

define open generic frame-browse-object  
    (frame :: <frame>, object :: <object>) => ();
define open generic frame-browse-object? 
    (frame :: <frame>, object :: <object>) => (browse? :: <boolean>);

define open generic frame-browse-object-type
    (frame :: <frame>, object :: <object>) => ();
define open generic frame-browse-object-generic-function
    (frame :: <frame>, object :: <object>) => ();

define open generic frame-describe-object
    (frame :: <frame>, object :: <object>) => ();
define open generic frame-describe-object?
    (frame :: <frame>, object :: <object>) => (describe? :: <boolean>);

define open generic frame-document-object
    (frame :: <frame>, object :: <object>) => ();
define open generic frame-document-object?
    (frame :: <frame>, object :: <object>) => (document? :: <boolean>);

// Default method
define method frame-browse-object
    (frame  :: <frame>, object :: <object>) => ()
  environment-action-unavailable
    (frame,
     format-to-string("No browser information for '%s'.",
		      frame-default-object-name(frame, object)))
end method frame-browse-object;

define method frame-browse-object
    (frame  :: <environment-frame>,
     object :: false-or(<environment-object>))
 => ()
  // This command should only be enabled when there is a project.
  let project :: <project-object> = frame.frame-current-project;
  if (frame-browse-object?(frame, object))
    with-current-environment-frame(frame)
      browse-object(project, object)
    end
  else
    next-method()
  end
end method frame-browse-object;

// Default method
define method frame-browse-object-type
    (frame  :: <frame>, object :: <object>) => ()
  environment-action-unavailable
    (frame,
     format-to-string("No type to browse for '%s'.",
		      frame-default-object-name(frame, object)))
end method frame-browse-object-type;

define method frame-browse-object-type
    (frame :: <environment-frame>, object :: <environment-object>)
 => ()
  // This command should only be enabled when there is a project.
  let project :: <project-object> = frame.frame-current-project;
  with-current-environment-frame (frame)
    browse-object-type(project, object)
  end
    | next-method() 
end method frame-browse-object-type;

// Default method
define method frame-browse-object-generic-function
    (frame  :: <frame>, object :: <object>) => ()
  environment-action-unavailable
    (frame,
     format-to-string("No generic function to browse for '%s'.",
		      frame-default-object-name(frame, object)))
end method frame-browse-object-generic-function;

define method frame-browse-object-generic-function
    (frame :: <environment-frame>,
     object :: type-union(<method-object>, <generic-function-object>))
 => ()
  // This command should only be enabled when there is a project.
  let project :: <project-object> = frame.frame-current-project;
  with-current-environment-frame (frame)
    browse-object-generic-function(project, object)
  end
    | next-method() 
end method frame-browse-object-generic-function;


/// Describe

define method frame-describe-object?
    (frame :: <frame>, object :: <object>)
 => (describe? :: <boolean>)
  #f
end method frame-describe-object?;

define method frame-describe-object?
    (frame :: <frame>, object :: <definition-object>)
 => (describe? :: <boolean>)
  #t
end method frame-describe-object?;

define method frame-describe-object?
    (frame :: <frame>, object :: <user-object>)
 => (describe? :: <boolean>)
  let project = frame.frame-current-project;
  project & application-object-class(project, object) ~== #f
end method frame-describe-object?;

define method frame-describe-object?
    (frame :: <frame>, object :: <collection-object>)
 => (describe? :: <boolean>)
  #t
end method frame-describe-object?;


define method frame-describe-object
    (frame  :: <frame>, object :: <object>) => ()
  environment-action-unavailable
    (frame,
     format-to-string("Cannot describe '%s'.",
		      frame-default-object-name(frame, object)))
end method frame-describe-object;

define method frame-describe-object
    (frame  :: <environment-frame>, object :: <environment-object>) => ()
  let project = frame.frame-current-project;
  let module = frame.frame-current-module;
  let name = frame-default-object-name(frame, object);
  describe-object(name, project, module, object)
end method frame-describe-object;


/// Documentation

define method frame-document-object?
    (frame :: <frame>, object :: <object>)
 => (document? :: <boolean>)
  #f
end method frame-document-object?;

define method frame-document-object?
    (frame :: <frame>, object :: <definition-object>)
 => (document? :: <boolean>)
  #t
end method frame-document-object?;


define method frame-document-object
    (frame  :: <frame>, object :: <object>) => ()
  environment-action-unavailable
    (frame,
     format-to-string("Cannot show documentation for '%s'.",
		      frame-default-object-name(frame, object)))
end method frame-document-object;

define method frame-document-object
    (frame  :: <environment-frame>, object :: <environment-object>) => ()
  let project = frame.frame-current-project;
  let module = frame.frame-current-module;
  let name = frame-default-object-name(frame, object);
  show-documentation(name, project, module, object)
end method frame-document-object;


/// frame-target-object

define method frame-target-object
    (frame :: <environment-frame>, name :: <name-object>)
 => (object :: false-or(<environment-object>))
  let project = frame-current-project(frame);
  project & name-value(project, name)
end method frame-target-object;

define method frame-target-object
    (frame :: <environment-frame>, slot :: <slot-object>)
 => (object :: false-or(<environment-object>))
  let project = frame-current-project(frame);
  project & slot-getter(project, slot)
end method frame-target-object;

define method frame-target-object
    (frame :: <environment-frame>, variable :: <local-variable-object>)
 => (result :: false-or(<environment-object>))
  let project = frame-current-project(frame);
  project & variable-value(project, variable)
end method frame-target-object;

define method frame-target-object
    (frame :: <environment-frame>, 
     breakpoint :: <environment-object-breakpoint-object>)
 => (result :: false-or(<environment-object>))
  breakpoint-object(breakpoint)
end method frame-target-object;


/// Wrapper objects

define open abstract class <object-wrapper> (<object>)
  constant slot wrapper-object :: <object>,
    required-init-keyword: object:;
end class <object-wrapper>;

define open abstract class <project-object-wrapper> (<object-wrapper>)
  constant slot wrapper-project :: <project-object>,
    required-init-keyword: project:;
end class <project-object-wrapper>;

define method command-table-for-target
    (frame :: <environment-frame>, wrapper :: <object-wrapper>)
 => (comtab :: <command-table>)
  command-table-for-target(frame, wrapper.wrapper-object)
end method command-table-for-target;

define method default-command-for-target
    (frame :: <environment-frame>, wrapper :: <object-wrapper>)
 => (command :: false-or(<command-or-function>))
  default-command-for-target(frame, wrapper.wrapper-object)
end method default-command-for-target;

define method frame-target-object
    (frame :: <environment-frame>, wrapper :: <object-wrapper>)
 => (object :: <object>)
  frame-target-object(frame, wrapper.wrapper-object)
end method frame-target-object;

define method environment-object-small-icon
    (project :: <project-object>, wrapper :: <object-wrapper>)
  environment-object-small-icon(project, wrapper.wrapper-object)
end method environment-object-small-icon;

define method environment-object-large-icon
    (project :: <project-object>, wrapper :: <object-wrapper>)
  environment-object-large-icon(project, wrapper.wrapper-object)
end method environment-object-large-icon;

// If this is a performance bottleneck, define \= methods on each subclass
define method \=
    (wrapper1 :: <object-wrapper>, wrapper2 :: <object-wrapper>)
 => (equal? :: <boolean>)
  wrapper1.wrapper-object = wrapper2.wrapper-object
end method \=;


/// Object and location wrappers

define open abstract class <object-and-location-wrapper> (<object-wrapper>)
  constant slot wrapper-source-location :: false-or(<source-location>),
    required-init-keyword: source-location:;
end class <object-and-location-wrapper>;

define method frame-target-edit-object
    (frame :: <environment-frame>, wrapper :: <object-and-location-wrapper>)
 => (object :: <object>)
  wrapper.wrapper-source-location
end method frame-target-edit-object;


/// Source wrappers

define abstract class <source-wrapper> (<project-object-wrapper>)
end class <source-wrapper>;

// A <source-record> wrapper
define class <source-record-wrapper> (<source-wrapper>)
end class <source-record-wrapper>;

// A <file-locator> wrapper
define class <source-locator-wrapper> (<source-wrapper>)
end class <source-locator-wrapper>;

// A <project-object> wrapper
define class <source-project-wrapper> (<source-wrapper>)
end class <source-project-wrapper>;

define generic wrapper-filename
    (wrapper :: <source-wrapper>)
 => (location :: false-or(<file-locator>));

define method wrapper-filename
    (wrapper :: <source-record-wrapper>)
 => (location :: false-or(<file-locator>))
  let project  = wrapper.wrapper-project;
  let record   = wrapper.wrapper-object;
  project-file-location(project, record.source-record-location)
end method wrapper-filename;

define method wrapper-filename
    (wrapper :: <source-locator-wrapper>)
 => (location :: <file-locator>)
  project-file-location(wrapper.wrapper-project, wrapper.wrapper-object)
end method wrapper-filename;

define method wrapper-filename
    (wrapper :: <source-project-wrapper>)
 => (location :: false-or(<file-locator>))
  let project = wrapper.wrapper-object;
  project.project-filename
end method wrapper-filename;

define function project-file-location
    (project :: <project-object>, file :: type-union(<string>, <file-locator>))
 => (location :: <file-locator>)
  merge-locators(as(<file-locator>, file), project.project-directory)
end function project-file-location;

// Instantiation

define sealed method make
    (class == <source-wrapper>, #rest keys, #key object)
 => (wrapper :: <source-wrapper>)
  let concrete-class
    = select (object by instance?)
	<source-record>  => <source-record-wrapper>;
	<file-locator>   => <source-locator-wrapper>;
	<project-object> => <source-project-wrapper>;
      end select;
  apply(make, concrete-class, keys)
end method make;

define sealed domain make (singleton(<source-record-wrapper>));
define sealed domain make (singleton(<source-locator-wrapper>));
define sealed domain make (singleton(<source-project-wrapper>));

define sealed domain initialize (<source-record-wrapper>);
define sealed domain initialize (<source-locator-wrapper>);
define sealed domain initialize (<source-project-wrapper>);


/// frame-target-browse-object

define method frame-target-browse-object
    (frame :: <environment-frame>, stack-frame :: <stack-frame-object>)
 => (object :: false-or(<environment-object>))
  let project = frame-current-project(frame);
  project & stack-frame-environment-object(project, stack-frame)
end method frame-target-browse-object;

define method frame-target-browse-object
    (frame :: <environment-frame>, warning :: <warning-object>)
 => (object :: false-or(<environment-object>))
  let project = frame-current-project(frame);
  project & warning-owner(project, warning)
end method frame-target-browse-object;


/// String coercion

define method target-as-string
    (sheet :: <sheet>, object :: <object>)
 => (string :: <string>)
  select (object by instance?)
    <string> => object;
    otherwise =>
      target-sheet-item-label(sheet, object);
  end
end method target-as-string;

define method target-as-string
    (sheet :: <sheet>, locator :: <file-system-locator>)
 => (string :: <string>)
  as(<string>, locator)
end method target-as-string;

define method target-as-string
    (sheet :: <sheet>, target :: <command-target>)
 => (string :: <string>)
  let object = target.target-object;
  select (object by instance?)
    <command-target-sequence> =>
      let items = object.target-sequence;
      target-sheet-items-to-string(sheet, items);
    <source-wrapper> =>
      as(<string>, object.wrapper-filename);
    otherwise =>
      let frame = sheet-frame(sheet);
      target-as-string(sheet, frame-target-object(frame, target));
  end
end method target-as-string;

define method target-sheet-item-label
    (sheet :: <sheet>, item :: <object>)
 => (label :: <string>)
  frame-default-object-name(sheet-frame(sheet), item)
end method target-sheet-item-label;

define method target-sheet-item-label
    (gadget :: <collection-gadget>, object)
 => (label :: <string>)
  block ()
    gadget-item-label(gadget, object)
  exception (<error>)
    //--- This is disgusting, but sometimes there is
    //--- no applicable gadget-item-label function.
    next-method()
  end
end method target-sheet-item-label;

define method target-sheet-items-to-string
    (sheet :: <sheet>, items :: <sequence>)
 => (string :: <string>)
  with-output-to-string (stream)
    for (item in items)
      let label = target-as-string(sheet, item);
      format(stream, "%s\n", label);
    end
  end
end method target-sheet-items-to-string;

define method target-sheet-items-to-string
    (gadget :: <table-control>, items :: <sequence>)
 => (string :: <string>)
  with-output-to-string (stream)
    let columns = table-control-columns(gadget);
    for (item in items)
      for (column :: <table-column> in columns,
	   prefix = "" then "\t")
	let generator = table-column-generator(column);
	let subitem = generator(item);
	let label = gadget-item-label(gadget, subitem);
	format(stream, "%s%s", prefix, label)
      end;
      format(stream, "\n")
    end
  end
end method target-sheet-items-to-string;


/// Standard popup menu

// Displays and "executes" a target-specific menu
define sealed method display-environment-popup-menu
    (frame :: <environment-frame>, target, #key x, y, #all-keys) => ()
  ignore(x, y);		// let DUIM place the menu appropriately
  do-display-environment-popup-menu(frame, top-level-sheet(frame), target)
end method display-environment-popup-menu;

define sealed method display-environment-popup-menu
    (sheet :: <sheet>, target, #key x, y, #all-keys) => ()
  ignore(x, y);		// let DUIM place the menu appropriately
  do-display-environment-popup-menu(sheet-frame(sheet), sheet, target)
end method display-environment-popup-menu;

define sealed method display-environment-popup-menu
    (sheet :: <collection-gadget>, target, #key x, y, #all-keys) => ()
  ignore(x, y);		// let DUIM place the menu appropriately
  do-display-environment-popup-menu(sheet-frame(sheet), sheet, gadget-value-key(sheet)(target))
end method display-environment-popup-menu;

define open generic make-environment-popup-menu
    (frame :: <frame>, sheet :: <abstract-sheet>, target :: <command-target>)
 => (menu :: false-or(<menu>));

define open generic update-environment-popup-menu
    (frame :: <frame>, menu :: <menu>, target :: <command-target>) => ();

define open generic update-frame-commands-for-target
    (frame :: <frame>, object :: <object>) => ();
define open generic update-frame-commands-for-browse-target
    (frame :: <frame>, object :: <object>) => ();

// Implementations of this should pop up the appropriate menu, allow the user
// to choose something, and then process the choice.  The simplest way to do
// this is to display the results from a 'make-menu-from-command-table-menu'.
define open generic do-display-environment-popup-menu
    (frame :: <frame>, sheet :: <abstract-sheet>, target, #key x, y) => ();

define method do-display-environment-popup-menu
    (frame :: <environment-frame>, sheet :: <sheet>, object, #key x, y) => ()
  let target = make-command-target(sheet, object);
  update-frame-commands-for-target(frame, target);
  let menu = make-environment-popup-menu(frame, sheet, target);
  if (menu)
    frame-command-target(frame) := target;
    update-environment-popup-menu(frame, menu, target);
    // Display the menu, which also happens to call any commands by means
    // of activate callbacks for the menu items.  Cool, huh?
    display-menu(menu, x: x, y: y)
  end
end method do-display-environment-popup-menu;

define method frame-edit-object
    (frame :: <environment-frame>, object :: <object>) => ()
  let project = frame-current-project(frame);
  if (project & object-has-source?(project, object))
    do-frame-edit-object(frame, object)
  else
    environment-action-unavailable
      (frame,
       format-to-string("No known source for '%s'.",
			frame-default-object-name(frame, object)))
  end
end method frame-edit-object;

define open generic frame-edit-objects
    (frame :: <environment-frame>, objects :: <sequence>,
     #key title :: false-or(<string>))
 => ();

define sideways method frame-edit-objects
    (frame :: <environment-frame>, objects :: <sequence>,
     #key title :: false-or(<string>))
 => ()
  let project = frame-current-project(frame);
  when (project)
    editor-edit-definitions(project, objects, title: title)
  end
end method frame-edit-objects;

define method object-exists?
    (server :: <server>, object) => (exists? == #t)
  #t
end method object-exists?;

define method object-exists?
    (server :: <server>, object :: <environment-object>) 
 => (exists? :: <boolean>)
  environment-object-exists?(server, object)
end method object-exists?;

define method object-has-source?
    (server :: <server>, object)
 => (source? :: <boolean>)
  #f
end method object-has-source?;

define method object-has-source?
    (server :: <server>, locator :: <file-locator>)
 => (source? :: <boolean>)
  ignore(server);
  locator-has-source?(locator)
end method object-has-source?;

define method object-has-source?
    (server :: <server>, object :: <file-source-record>)
 => (source? :: <boolean>)
  #t
end method object-has-source?;

define method object-has-source?
    (server :: <server>, location :: <source-location>)
 => (source? :: <boolean>)
  let record = location.source-location-source-record;
  object-has-source?(server, record)
end method object-has-source?;

define method object-has-source?
    (server :: <server>, object :: <environment-object>)
 => (source? :: <boolean>)
  let location = environment-object-source-location(server, object);
  location & object-has-source?(server, location)
end method object-has-source?;

define method object-has-source?
    (server :: <server>, object :: <project-object>)
 => (source? :: <boolean>)
  #f
end method object-has-source?;

define method object-has-source?
    (server :: <server>, object :: <source-location-breakpoint-object>)
 => (found-definition? :: <boolean>)
  let location = object.breakpoint-object;
  object-has-source?(server, location)
end method object-has-source?;

define method object-has-source?
    (server :: <server>, name :: <name-object>)
 => (found-definition? :: <boolean>)
  let value = name-value(server, name);
  value & object-has-source?(server, value)
end method object-has-source?;

define method do-frame-edit-object
    (frame :: <environment-frame>, object)
 => (found-definition? :: <boolean>)
  // The 'object-has-source?' protocol should prevent us from ever getting
  // here, unless it's inconsistent with the implementation of do-frame-
  // edit-object, which would be a bug.  But just in case ...
  environment-action-unavailable
    (frame,
     format-to-string("Don't know how to edit source for '%s'.",
		      frame-default-object-name(frame, object)));
  #f
end method do-frame-edit-object;

define method do-frame-edit-object 
    (frame :: <environment-frame>, locator :: <file-locator>)
 => (found-definition? :: <boolean>)
  if (file-exists?(locator))
    editor-open-file(locator);
    #t
  end
end method do-frame-edit-object;

define method do-frame-edit-object 
    (frame :: <environment-frame>, object :: <file-source-record>)
 => (found-definition? :: <boolean>)
  let project = frame-current-project(frame);
  when (project)
    edit-source-record(project, object);
    #t
  end;
end method do-frame-edit-object;

define method do-frame-edit-object 
    (frame :: <environment-frame>, object :: <source-location>)
 => (found-definition? :: <boolean>)
  let project = frame-current-project(frame);
  block ()
    when (project)
      edit-source-location(project, object);
      #t
    end;
  exception (type-union(<file-does-not-exist-error>,
			<source-record-missing>))
    #f;
  end
end method do-frame-edit-object;

define method do-frame-edit-object 
    (frame :: <environment-frame>, object :: <environment-object>)
 => (found-definition? :: <boolean>)
  let project  = frame-current-project(frame);
  let location = project & environment-object-source-location(project, object);
  location & do-frame-edit-object(frame, location)
end method do-frame-edit-object;

define variable $edit-object-dialog-width  :: false-or(<integer>) = #f;
define variable $edit-object-dialog-height :: false-or(<integer>) = #f;

define method do-frame-edit-object
    (frame :: <environment-frame>, function :: <generic-function-object>)
 => (found-definition? :: <boolean>)
  let project = frame-current-project(frame);
  when (project)
    let methods :: <stretchy-object-vector> = make(<stretchy-vector>);
    // Collect the generic function and all its methods
    add!(methods, function);
    do-generic-function-methods
      (method (object) add!(methods, object) end method,
       project, function);
    // Remove the generic function if it seems to be implicit
    when (any?(method (m)
	         let l1 = environment-object-source-location(project, function);
	         let l2 = environment-object-source-location(project, m);
                 m ~== function
                 & l1
                 & l2
	         & source-location-source-record(l1) = source-location-source-record(l2)
	         & source-location-start-offset(l1) = source-location-start-offset(l2)
               end method,
               methods))
      remove!(methods, function)
    end;
    // We need this helper method in order to avoid recursion when the
    // object is just the generic function again...
    local method edit-object
              (object :: <environment-object>) => (found-definition? :: <boolean>)
            let location
              = project & environment-object-source-location(project, object);
            location & do-frame-edit-object(frame, location)
          end method;
    select (size(methods))
      0 => #f;
      1 => edit-object(methods[0]);
      otherwise =>
        let (methods, success?, width, height)
          = choose-from-dialog
              (methods,
               value: vector(methods[0]),
               label-key: curry(frame-default-object-name, frame),
               selection-mode: #"multiple",
               frame: frame,
               title: "Choose methods to edit",
               width:  $edit-object-dialog-width,
               height: $edit-object-dialog-height);
        when (success? & methods & size(methods) > 0)
          $edit-object-dialog-width  := width;
          $edit-object-dialog-height := height;
          if (size(methods) = 1)
            edit-object(methods[0])
          else
            let title = print-environment-object-name-to-string(project, function);
            frame-edit-objects(frame, methods,
                               title: format-to-string("Methods of %s", title))
          end;
          #t;
        end;
    end
  end
end method do-frame-edit-object;

define method do-frame-edit-object 
    (frame :: <environment-frame>, name :: <name-object>)
 => (found-definition? :: <boolean>)
  let project = frame-current-project(frame);
  let value   = project & name-value(project, name);
  value & do-frame-edit-object(frame, value)
end method do-frame-edit-object;


/// Clipboard handling

define method dylan-clipboard-object
    (frame :: <environment-frame>)
 => (object)
  with-clipboard (clipboard = top-level-sheet(frame))
    (clipboard-data-available?(<object>, clipboard)
       & get-clipboard-data-as(<object>, clipboard))
    | (clipboard-data-available?(<string>, clipboard)
	 & get-clipboard-data-as(<string>, clipboard))
  end
end method dylan-clipboard-object;

define method dylan-clipboard-object-available?
    (frame :: <environment-frame>, class :: <class>)
 => (available? :: <boolean>)
  with-clipboard (clipboard = top-level-sheet(frame))
    (clipboard-data-available?(<object>, clipboard)
       & instance?(get-clipboard-data-as(<object>, clipboard), class))
  end
end method dylan-clipboard-object-available?;

define method dylan-clipboard-object-available?
    (frame :: <environment-frame>, class :: subclass(<string>))
 => (available? :: <boolean>)
  with-clipboard (clipboard = top-level-sheet(frame))
    (clipboard-data-available?(<object>, clipboard)
       & instance?(get-clipboard-data-as(<object>, clipboard), class))
    | clipboard-data-available?(<string>, clipboard)
  end
end method dylan-clipboard-object-available?;


/// Popup menu handling

define method make-environment-popup-menu
    (frame :: <environment-frame>, sheet :: <sheet>, 
     target :: <command-target>)
 => (menu :: false-or(<menu>))
  let framem = frame-manager(frame);
  let object = frame-target-object(frame, target);
  let class = object-class(object);
  let cache = frame-popup-menu-cache(frame);
  // Build a menu from the command table for this object
  element(cache, class, default: #f)
    | begin
	let comtab = frame-command-table-for-target(frame, target);
	let menu
	  = make-menu-from-command-table-menu
	      (command-table-menu(comtab), frame, framem,
	       command-table: comtab,
	       owner: frame);
	element(cache, class) := menu
      end
end method make-environment-popup-menu;

define method update-frame-commands-for-target
    (frame :: <frame>, target :: <command-target>) => ()
  let browse-target = frame-target-browse-object(frame, target);
  update-frame-commands-for-browse-target(frame, browse-target)
end method update-frame-commands-for-target;

define method update-frame-commands-for-target
    (frame :: <frame-clipboard-mixin>, target :: <command-target>) => ()
  next-method();
  let pane = target.target-pane;
  command-enabled?(frame-cut-target, frame)    := cut-object?(pane, target);
  command-enabled?(frame-copy-target, frame)   := copy-object?(pane, target);
  command-enabled?(frame-paste-target, frame)  := paste-object?(pane, target);
  command-enabled?(frame-delete-target, frame) := delete-object?(pane, target);
end method update-frame-commands-for-target;

define method update-frame-commands-for-target
    (frame :: <environment-frame>, target :: <command-target>) => ()
  next-method();
  let project = frame-current-project(frame);
  let edit-target = frame-target-edit-object(frame, target);
  let editable?
    = project
        & object-exists?(project, edit-target)
        & object-has-source?(project, edit-target);
  command-enabled?(frame-edit-target, frame)   := editable?
end method update-frame-commands-for-target;

define method update-frame-commands-for-browse-target
    (frame :: <frame>, object :: <object>) => ()
  #f
end method update-frame-commands-for-browse-target;

define method update-frame-commands-for-browse-target
    (frame :: <environment-frame>, object :: <object>) => ()
  let project = frame.frame-current-project;
  let environment-object? = instance?(object, <environment-object>);
  let (describable?, documentable?, browsable?, type-browsable?, generic?)
    = if (project & object-exists?(project, object))
	let type = environment-object? & environment-object-type(project, object);
	let describable?    = frame-describe-object?(frame, object);
	let documentable?   = frame-document-object?(frame, object);
	let browsable?      = frame-browse-object?(frame, object);
	let type-browsable? = type & (~ instance?(type, <complex-type-expression-object>));
	let generic?
	  = instance?(object, <method-object>)
	      & method-generic-function(project, object) ~= #f;
	values(describable?, documentable?, browsable?, type-browsable?, generic?)
      else
	values(#f, #f, #f, #f, #f)
      end;
  command-enabled?(frame-describe-target,     frame) := describable?;
  command-enabled?(frame-document-target,     frame) := documentable?;
  command-enabled?(frame-browse-target,       frame) := browsable?;
  command-enabled?(frame-browse-target-type, frame)  := type-browsable?;
  command-enabled?(frame-browse-target-generic-function, frame) := generic?;
end method update-frame-commands-for-browse-target;

//---*** Maybe this should be in DUIM?
define method menu-default-gadget-setter
    (default-gadget :: false-or(<gadget>), menu :: <menu>) => ()
  local method update-gadget-default-state
	    (button :: <push-menu-button>) => ()
	  gadget-default?(button) := (button == default-gadget)
	end method update-gadget-default-state;
  local method update-children-default-state
	    (gadget :: <gadget>) => ()
	  for (child in sheet-children(gadget))
	    select (child by instance?)
	      <menu> =>
		//---*** Can we make a menu be the default?
		#f;
	      <menu-box> =>
		update-children-default-state(child);
	      <push-menu-button> =>
		update-gadget-default-state(child);
	      otherwise =>
		#f;
	    end
	  end
	end method update-children-default-state;
  update-children-default-state(menu)
end method menu-default-gadget-setter;

//---*** Maybe this should be in DUIM?
define method find-menu-button 
    (menu :: <menu>, function :: <function>)
 => (button :: false-or(<menu-button>))
  block (return)
    do-sheet-tree
      (method (sheet :: <sheet>)
	 if (instance?(sheet, <menu-button>)
	       & gadget-command(sheet) == function)
	   return(sheet)
	 end
       end,
       menu)
  end
end method find-menu-button;  

//---*** andrewa: this is a lot of work to update the default command.
//---*** Ideally we'd be able to change a command table's default command
//---*** without having to know about all of this UI!
define method update-environment-popup-menu
    (frame :: <environment-frame>, menu :: <menu>, target :: <command-target>)
 => ()
  let browse-target = frame-target-browse-object(frame, target);
  let default-command = default-command-for-target(frame, target);
  let default-button
    = default-command & find-menu-button(menu, default-command);
  menu-default-gadget(menu) := default-button
end method update-environment-popup-menu;


/// Activation

define method environment-activate-callback
    (sheet :: <sheet>, object :: <object>) => ()
  let frame = sheet-frame(sheet);
  let target = make-command-target(sheet, object);
  let command = default-command-for-target(frame, target);
  if (command)
    frame-command-target(frame) := target;
    command(frame)
  else
    let object = frame-target-browse-object(frame, target);
    let name = frame-default-object-name(frame, object);
    environment-action-unavailable
      (frame,
       format-to-string("No action available",
			if (name)
			  format-to-string(" for %s", name)
			else
			  ""
			end))
  end
end method environment-activate-callback;

define method environment-activate-callback
    (frame :: <frame>, object :: <object>) => ()
  environment-activate-callback(top-level-sheet(frame), object)
end method environment-activate-callback;


/// Environment status bar

define method make-environment-status-bar 
    (frame :: <environment-frame>) => (bar :: <status-bar>)
  with-frame-manager (frame-manager(frame))
    make(<status-bar>)
  end
end method make-environment-status-bar;


/// Searching

define constant $search-string-count = 10;

define settings <search-settings> (<open-dylan-user-settings>)
  key-name "Search";
  slot search-batch?      :: <boolean> = #f;
  slot search-wrap?       :: <boolean> = #f;
  slot search-boundaries? :: <boolean> = #f;
  slot match-word?        :: <boolean> = #f;
  slot match-case?        :: <boolean> = #f;
  slot match-regexp?      :: <boolean> = #f;
  slot search1   :: <string> = "";
  slot search2   :: <string> = "";
  slot search3   :: <string> = "";
  slot search4   :: <string> = "";
  slot search5   :: <string> = "";
  slot search6   :: <string> = "";
  slot search7   :: <string> = "";
  slot search8   :: <string> = "";
  slot search9   :: <string> = "";
  slot search10  :: <string> = "";
  slot replace1  :: <string> = "";
  slot replace2  :: <string> = "";
  slot replace3  :: <string> = "";
  slot replace4  :: <string> = "";
  slot replace5  :: <string> = "";
  slot replace6  :: <string> = "";
  slot replace7  :: <string> = "";
  slot replace8  :: <string> = "";
  slot replace9  :: <string> = "";
  slot replace10 :: <string> = "";
end settings <search-settings>;

define constant $search-settings = make(<search-settings>);

// Store search settings
define function store-search-settings () => ()
  let description = current-search-description();
  $search-settings.search-batch?      := description.search-description-batch?;
  $search-settings.search-wrap?       := description.search-description-wrap?;
  $search-settings.search-boundaries? := description.search-description-boundaries?;
  $search-settings.match-word?        := description.search-description-match-word?;
  $search-settings.match-case?        := description.search-description-match-case?;
  $search-settings.match-regexp?      := description.search-description-match-regexp?;

  local method pad (c :: <collection>, min :: <integer>) => (c :: <collection>)
	  let blanks = min - size(c);
	  if (blanks > 0)
	    concatenate(c, make(<vector>, fill: "", size: blanks))
	  else
	    c
	  end;
	end method pad;

  let search-strings = pad(previous-search-strings(), $search-string-count);
  $search-settings.search1  := search-strings[0];
  $search-settings.search2  := search-strings[1];
  $search-settings.search3  := search-strings[2];
  $search-settings.search4  := search-strings[3];
  $search-settings.search5  := search-strings[4];
  $search-settings.search6  := search-strings[5];
  $search-settings.search7  := search-strings[6];
  $search-settings.search8  := search-strings[7];
  $search-settings.search9  := search-strings[8];
  $search-settings.search10 := search-strings[9];

  let replace-strings = pad(previous-replace-strings(), $search-string-count);
  $search-settings.replace1  := replace-strings[0];
  $search-settings.replace2  := replace-strings[1];
  $search-settings.replace3  := replace-strings[2];
  $search-settings.replace4  := replace-strings[3];
  $search-settings.replace5  := replace-strings[4];
  $search-settings.replace6  := replace-strings[5];
  $search-settings.replace7  := replace-strings[6];
  $search-settings.replace8  := replace-strings[7];
  $search-settings.replace9  := replace-strings[8];
  $search-settings.replace10 := replace-strings[9];
end function store-search-settings;

// Restore search settings
define function restore-search-settings () => ()
  let description = current-search-description();
  description.search-description-batch?        := $search-settings.search-batch?;
  description.search-description-wrap?         := $search-settings.search-wrap?;
  description.search-description-boundaries?   := $search-settings.search-boundaries?;
  description.search-description-match-word?   := $search-settings.match-word?;
  description.search-description-match-case?   := $search-settings.match-case?;
  description.search-description-match-regexp? := $search-settings.match-regexp?;
  current-search-description() := description;
  
  previous-search-strings() := vector($search-settings.search1,
				      $search-settings.search2,
				      $search-settings.search3,
				      $search-settings.search4,
				      $search-settings.search5,
				      $search-settings.search6,
				      $search-settings.search7,
				      $search-settings.search8,
				      $search-settings.search9,
				      $search-settings.search10);
  
  previous-replace-strings() := vector($search-settings.replace1,
				       $search-settings.replace2,
				       $search-settings.replace3,
				       $search-settings.replace4,
				       $search-settings.replace5,
				       $search-settings.replace6,
				       $search-settings.replace7,
				       $search-settings.replace8,
				       $search-settings.replace9,
				       $search-settings.replace10);
end function restore-search-settings;

restore-search-settings();

//---*** cpage: 1998.08.27 Is it too late to do this here? Does settings-definer
//              use this message for storing settings?

define function store-search-settings-receiver
    (message :: <environment-stopping-message>) => ()
  ignore(message);
  store-search-settings();
end function store-search-settings-receiver;

tune-in($environment-channel,
	store-search-settings-receiver,
	message-type: <environment-stopping-message>);

// Subclass <search-frame> in order to add persistent settings

//--- cpage: 1998.06.29 Frankly, I consider this a bit of a kludge.
//           We probably need to move most of the searching functionality
//           out of the framework and into this library. Alternatively,
//           we probably need a more general approach to customizing
//           framework-defined frames.

define frame <environment-search-frame>
    (<frame-window-settings-mixin>, <search-frame>)
  keyword icon: = $find-window-small-icon;
end frame <environment-search-frame>;

define window-settings
  find-window :: <environment-search-frame> = "Find Window";

define method frame-search-frame-class
    (frame :: <environment-frame>)
 => (class :: <class>)
  <environment-search-frame>
end method frame-search-frame-class;


/// Window Layering

// Perform some actions on all frames when certain frames are manipulated
// in order to treat all the environment frames as a "layer" as on Mac OS,
// for example.

// When to perform a given action on all frames:
//   #"none"          => never
//   #"primary-frame" => when action is performed on primary frame
//   #"all-frames"    => when action is performed on any frame
define constant <trigger-frame> = one-of(#"none", #"primary-frame", #"all-frames");


// This is used to prevent recursion when raising all frames
// This is a global variable rather than a thread variable because raising
// could conceivably be requested from multiple frames simultaneously
define variable *raising-all-frames?* :: <boolean> = #f;

define method raise-all-frames
    (frame :: <environment-frame>) => ()
  // TESTING: Check whether this case ever arises
  when (*raising-all-frames?*)
    duim-debug-message("raise-all-frames: Prevented Recursion");
  end;
  unless (*raising-all-frames?*)
    block ()
      *raising-all-frames?* := #t;
      next-method();
    cleanup
      *raising-all-frames?* := #f;
    end;
  end unless;
end method raise-all-frames;

// Raise all frames when trigger frame is raised
define method handle-event
    (frame :: <environment-frame>, event :: <frame-focus-in-event>) => ()
  next-method();
  when (environment-auto-raise-all-frames() == #"all-frames"
	| (environment-auto-raise-all-frames() == #"primary-frame"
	   & instance?(frame, <environment-primary-frame>)))
    raise-all-frames(frame);
  end when;
end method handle-event;


// This is used to prevent recursion when minimizing or restoring all frames
// This is a global variable rather than a thread variable because raising
// could conceivably be requested from multiple frames simultaneously
define variable *minimizing-or-restoring-all-frames?* :: <boolean> = #f;

define method minimize-all-frames
    (frame :: <environment-frame>) => ()
  when (*minimizing-or-restoring-all-frames?*)
    // TESTING: Check whether this case ever arises
    duim-debug-message("minimize-all-frames: Prevented Recursion");
  end;
  unless (*minimizing-or-restoring-all-frames?*)
    block ()
      *minimizing-or-restoring-all-frames?* := #t;
      next-method();
    cleanup
      *minimizing-or-restoring-all-frames?* := #f;
    end;
  end unless;
end method minimize-all-frames;

define method undo-minimize-all-frames
    (frame :: <environment-frame>) => ()
  when (*minimizing-or-restoring-all-frames?*)
    // TESTING: Check whether this case ever arises
    duim-debug-message("undo-minimize-all-frames: Prevented Recursion");
  end;
  unless (*minimizing-or-restoring-all-frames?*)
    block ()
      *minimizing-or-restoring-all-frames?* := #t;
      next-method();
    cleanup
      *minimizing-or-restoring-all-frames?* := #f;
    end;
  end unless;
end method undo-minimize-all-frames;

// Minimize/restore all frames when trigger frame is minimized/restored
define method note-frame-state-changed
    (frame :: <environment-frame>, old-state, new-state) => ()
  next-method();
  // If the state hasn't actually changed, don't trigger anything
  unless (new-state = old-state)
    when (environment-auto-lower-all-frames() == #"all-frames"
	  | (environment-auto-lower-all-frames() == #"primary-frame"
	     & instance?(frame, <environment-primary-frame>)))
      select (new-state)
	#"iconified"  =>
	  minimize-all-frames(frame);
	  // Make sure the trigger frame gets restored by
	  // undo-minimize-all-frames, too.
	  *minimized-frames* := add-new!(*minimized-frames*, frame);
	#"deiconified" =>
	  undo-minimize-all-frames(frame);
	otherwise      => 
	  #f;
      end select;
    end when;
  end unless;
end method note-frame-state-changed;


/// Environment project frame
/// Frames "permanently" associated with one project

define open abstract class <environment-fixed-project-frame> (<environment-frame>)
  // This slot is "almost constant" -- the only places that set
  // the project are 'reinitialize-frame' methods
  //---*** This accessor is an attractive nuisance
  //---*** You don't want to use it, use 'frame-current-project' instead
  slot frame-project :: <project-object>,
    setter: %frame-project-setter,
    required-init-keyword: project:;
  slot frame-exiting? :: <boolean> = #f;
end class <environment-fixed-project-frame>;

define sealed inline method frame-current-project
    (frame :: <environment-fixed-project-frame>)
 => (project :: <project-object>)
  frame.frame-project
end method frame-current-project;

define sealed method frame-note-project-changed
    (frame :: <environment-fixed-project-frame>, 
     project :: false-or(<project-object>))
 => ()
  error("Changed project in a fixed-project frame.")
end method frame-note-project-changed;

define method make-clone
    (frame :: <environment-fixed-project-frame>, #rest initargs)
 => (new-frame :: <environment-fixed-project-frame>)
  apply(next-method, frame, project: frame.frame-project, initargs)
end method make-clone;

define method reuse-matching-frame?
    (portd :: <port-designator>,
     frame :: <environment-fixed-project-frame>,
     class :: subclass(<environment-fixed-project-frame>),
     #rest initargs, #key project :: false-or(<project-object>) = #f)
 => (reuse? :: <boolean>)
  frame.frame-project = project
    & next-method()
end method reuse-matching-frame?;
    
define method handle-event
    (frame :: <environment-fixed-project-frame>, event :: <frame-mapped-event>)
 => ()
  next-method();
  let project     = frame.frame-project;
  let application = project & project.project-application;
  frame-note-application-state-changed
    (frame, application & application.application-state);
end method handle-event;


/// Cloning

define open generic clone-tool
    (frame :: <environment-frame>) => ();

define open generic clone-and-link-tool
    (frame :: <environment-frame>) => ();

define method clone-tool (frame :: <environment-frame>) => ()
  let tool = make-clone(frame);
  fork-environment-function(frame, object-class(frame), always(tool))
end method clone-tool;

define method clone-and-link-tool
    (frame :: <environment-frame>) => ()
  let tool = make-clone(frame);
  link-frames(frame, tool);
  fork-environment-function(frame, object-class(frame), always(tool))
end method clone-and-link-tool;

define command-table *clone-command-table* (*global-command-table*)
  menu-item "New Window"        = clone-tool,
    accelerator:   make-keyboard-gesture(#"n", #"control"),
    documentation: "Opens another window for the document.";
//---*** andrewa: removed for 1.0
//  menu-item "New Linked Window" = clone-and-link-tool,
//    documentation: "Opens another window for the document, linked to its selection.";
end command-table *clone-command-table*;

define command-table *windows-command-table* (*global-command-table*)
  include *clone-command-table*;
  include *window-actions-command-table*;
  include *window-settings-command-table*;
  include *open-windows-command-table*;
end command-table *windows-command-table*;

define variable $clone-bitmap :: <label-type> = "New Window";

define method make-clone-tool-bar-buttons
    (frame :: <environment-frame>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $clone-bitmap,
	      documentation: "New Window",
              command: clone-tool,
              activate-callback: method (sheet)
                                   let frame = sheet-frame(sheet);
                                   clone-tool(frame)
                                 end))
end method make-clone-tool-bar-buttons;


/// Import files

//--- cpage: 1997.08.27 I'm not sure this Import LID stuff belongs here, but
//           it is used by more than one tool and the emulator requires this
//           to be compiled before the referencing sources.

define open generic frame-import-file
    (frame :: <environment-frame>, #key filename :: false-or(<file-locator>)) => ();

// Import a LID file as a project file

define function import-lid-file
    (#key frame :: false-or(<frame>) = #f, 
          filename :: false-or(<file-locator>) = #f)
 => (status-code :: false-or(<integer>))
  let filename
    = filename
        | environment-choose-file
            (title:   "Import LID",
	     owner:   frame,
	     filters: #[#"lid"]);
  when (filename)
    let project = import-project-from-file(filename);
    //--- cpage: 1997.09.10 We need to add a real error message.
    debug-assert(project, "Cannot convert %= to a project", project);
    if (frame)
      frame-open-project(frame, project);
    else
      find-project-browser(project);
    end if;
    0
  end when
end function import-lid-file;

// Default method
define method frame-import-file
    (frame :: <environment-frame>, 
     #key filename :: false-or(<file-locator>) = #f)
 => ()
  import-lid-file(frame: frame, filename: filename);
end method frame-import-file;

define command-table *import-LID-file-command-table* (*global-command-table*)
  menu-item "Import LID..." = frame-import-file,
    documentation: "Converts a Dylan Library Interchange Description to a project file.";
end command-table *import-LID-file-command-table*;
