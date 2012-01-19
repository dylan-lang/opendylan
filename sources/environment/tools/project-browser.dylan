Module:    environment-tools
Synopsis:  Project Browser
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project Browser

define frame <project-browser>
    (<frame-refresh-mixin>, 
     <frame-cascading-window-mixin>,
     <environment-project-tool>)
  slot frame-initialized-callback :: false-or(<function>) = #f,
    init-keyword: initialized-callback:;
  constant slot frame-application-process :: false-or(<process>) = #f,
    init-keyword: application-process:;
  constant slot frame-application-id :: false-or(<string>) = #f,
    init-keyword: application-id:;
  pane tab-layout (frame)
    make(<environment-property-pane>,
         class: <project-object>,
         frame: frame,
         activate-callback: environment-activate-callback);
  pane main-layout (frame)
    vertically (spacing: 2)
      make(<separator>);
      frame.tab-layout;
    end;
  layout        (frame) frame.main-layout;
  tool-bar      (frame) make-environment-tool-bar(frame);
  status-bar    (frame) make-environment-status-bar(frame);
  command-table (frame) *project-browser-command-table*;
  keyword width:  = $default-environment-frame-width;
  keyword height: = $default-environment-frame-height;
  keyword icon: = $project-window-small-icon;
  keyword frame-class-name:, init-value: #"project-browser";
end frame <project-browser>;

define cascading-window-settings 
  project-window :: <project-browser> = "Project Window";

define method initialize (frame :: <project-browser>, #key) => ()
  next-method();

  let (width, height) = frame-default-size(frame);
  if (width & height)
    set-frame-size(frame, width, height)
  end;

  frame.frame-reusable? := #f;

  // These commands are always disabled.
  // File
  command-enabled?(frame-revert-file, frame)    := #f;
  command-enabled?(frame-save-all, frame)       := #f;
  // Window
  command-enabled?(clone-tool, frame)           := #f;
  command-enabled?(clone-and-link-tool, frame)  := #f;
end method initialize;

tune-in($project-channel,
	method (message :: <project-closed-message>)
	  let project = message.message-project;
	  let project-window 
	    = find-project-browser-showing-project(project);
	  if (project-window & frame-mapped?(project-window))
	    exit-frame(project-window)
	  end
	end,
	message-type: <project-closed-message>);

define method reinitialize-frame
    (frame :: <project-browser>,
     #key page :: false-or(<symbol>),
          application-process: process, 
          application-id: id) => ()
  next-method();
  when (page)
    environment-property-pane-page(frame.tab-layout) := page;
  end;
  /* hughg, 1998/10/29: AndrewA and I think this is bogus, since we can
  // currently only ever need to do JIT debugging attachment if we're a
  // new environment, in which case we won't be calling reinitialize-frame.
  // If we ever get JIT debugging by reusing an environment (e.g., via DDE),
  // there are more things we'd need to do here, like check the project
  // isn't already attached to an application.
  when (process)
    let project = frame.ensure-frame-project;
    frame.frame-application-process := process;
    frame.frame-application-id := id;
    frame-attach-application(frame, process: process, id: id)
  end
  */
end method reinitialize-frame;

// Commands that can modify a project
define constant $project-modification-commands
  = vector(frame-insert-source-file,
           frame-remove-selected-sources,
           frame-move-source-file-prev,
           frame-move-source-file-next,
	   frame-build-project,
	   frame-clean-build-project,
	   frame-advanced-build-dialog,
	   frame-clean-project,
	   frame-build-release);

define method handle-event
    (frame :: <project-browser>, event :: <frame-mapped-event>) => ()
  with-busy-cursor (frame)
    frame-status-message(frame) := "Opening database...";

    let project
      = block (opened)
	  let project = frame.ensure-frame-project;
	  block ()
	    open-project-compiler-database
	      (project, error-handler: curry(compiler-condition-handler, frame));
	  exception (error :: <project-error>)
	    display-project-error(frame, project, error);
	    exit-frame(frame);
	    opened(#f)
	  end;
	  project.project-opened-by-user? := #t;
	  project
	exception (<abort>)
	  //--- If the user aborts during this vital phase, then
	  //--- all we can do is exit the frame.
	  exit-frame(frame)
	end;

    if (project)
      // Build-related commands are always disabled for .exe projects
      unless (project.project-can-be-built?)
	for (command in $project-modification-commands)
	  command-enabled?(command, frame) := #f
	end
      end;
      
      // For projects without a .hdp file, disable "Save As"
      let has-project-file? = project.project-filename ~= #f;
      command-enabled?(frame-save-file-as, frame) := has-project-file?;
      
      // Update the property page
      environment-property-pane-object(frame.tab-layout) := project;
      
      // Now that the database is ready we can do the other initializations
      next-method();
      
      // Hook up remote/just-in-time debugging now that the database is open.
      let process = frame.frame-application-process;
      let id = frame.frame-application-id;
      if (process)
	frame-attach-application(frame, process: process, id: id)
      else
	let machine-address = project.project-debug-machine-address;
	if (machine-address)
	  open-remote-connection(owner: frame, default-address: machine-address)
	end if
      end;
      
      // Call the initialized-callback, if there is one.
      let initialized-callback = frame.frame-initialized-callback;
      if (initialized-callback)
	initialized-callback(frame);
	frame.frame-initialized-callback := #f; // so it gets GC'd
      end
    end
  end
end method handle-event;


/// Make a project browser starting with something that can be coerced to a project

define method make-environment-frame
    (portd :: <port-designator>, class :: subclass(<project-browser>),
     #rest initargs, #key project)
 => (frame :: <project-browser>)
  let coerced-project = coerce-project(project);
  debug-assert(coerced-project, "Cannot convert %= to a project", project);
  apply(next-method, portd, class, project: coerced-project, initargs)
end method make-environment-frame;


/// Iterate over project-related frames

define function do-project-frames
    (action :: <function>, project :: <project-object>,
     #key exclude :: false-or(subclass(<frame>)) = #f,
     include :: subclass(<frame>) = <frame>) => ()
  local method project-frame? (frame :: <frame>) => (project-frame? :: <boolean>)
	  instance?(frame, include)
	  & ~(exclude & instance?(frame, exclude))
	  & frame.frame-current-project = project
        end method project-frame?;
  do-frames(method (frame :: <frame>) => ()
              when (project-frame?(frame))
                action(frame);
              end when;
            end);
end function do-project-frames;

/// Check whether the project browser can exit

// Check whether a project window can exit
define method frame-can-exit?
    (frame :: <environment-fixed-project-frame>)
 => (exit? :: <boolean>)
  // Check for build in progress
  frame.frame-exiting?
    | begin
	let progress-window = find-compiler-progress-window(frame);
	~progress-window
	  | begin
	      when (compiler-progress-stopped?(progress-window))
		exit-frame(progress-window);
		#t
	      end
	    end
	  | begin
	      deiconify-frame(progress-window);
	      raise-frame(progress-window);
	      let project = frame.ensure-frame-project;
	      let message
		= format-to-string("The project '%s' cannot be closed until it has stopped"
				   " building. Stop building and close the project?",
				   environment-object-primitive-name(project, project));
	      let exit? = environment-question
		(message, owner: frame, exit-style: #"ok-cancel");
	      when (exit?)
		exit-frame(progress-window);
		#t
	      end
	    end;
      end
end method frame-can-exit?;

define method frame-can-exit?
    (frame :: <project-browser>) => (exit? :: <boolean>)
  frame.frame-exiting?
    | block (return)
	unless (next-method()) return(#f) end;
	let project      = frame.ensure-frame-project;
	let project-name = environment-object-primitive-name(project, project);
	// Check for running target application
	let message
	  = format-to-string("The project '%s' cannot be closed while the target"
			     " application is running.\n\n"
			     "Stop the target application and close the project?",
			     project-name);
	if (frame-warn-if-application-tethered(frame, project, message: message))
	  return(#f)
	end;
	// Check whether all other project-related frames can exit
	let frames = make(<stretchy-vector>);
	do-project-frames(method (frame :: <frame>)
			    frames = add!(frames, frame);
			  end method,
			  project,
			  include: <environment-fixed-project-frame>,
			  exclude: <project-browser>);
	do(method (frame :: <environment-fixed-project-frame>) => ()
	     unless (frame-can-exit?(frame))
	       do(method (frame :: <environment-fixed-project-frame>)
		    frame.frame-exiting? := #f
		  end,
		  frames);
	       return(#f)
	     end;
	     frame.frame-exiting? := #t;
	     exit-frame(frame)
	   end method,
	   frames);
	// Check whether the project database needs saving
	when (project-database-changed?(project))
	  let message
	    = format-to-string("The compiler database for project '%s' contains unsaved"
			       " changes. Do you want to save changes to the database?",
			       project-name);
	  let (save-db?, exit-type) = environment-question
					(message,
					 title: release-name(),
					 owner: frame,
					 style: #"warning",
					 exit-style: #"yes-no-cancel");
	  case
	    save-db? =>
	      with-busy-cursor (frame)
		save-project(project, save-database?: #t);
	      end;
	    exit-type = #"cancel" =>
	      return(#f);
	    otherwise =>
	      // else exit-type = #"no", so fall through and return #t
              #f
          end
	end when;
	// Okay, shut'er down!
	#t
      end block
end method frame-can-exit?;

define method frame-open-selection
    (frame :: <project-browser>) => ()
  // Actually, this just calls the activate callback, so it's equivalent to
  // double clicking on an object, or right-clicking and selecting the
  // default (bold) menu item. Perhaps we should dynamically change the
  // menu item to reflect the default operation (and maybe embolden it)
  // like Explorer does in it's File menu. And perhaps we should rename
  // this function to something more generic like frame-activate-selection.
  //--- cpage: 1998.04.15 Perhaps we should delegate this to the displayer
  //           for the current property page, though I've tried to implement
  //           this using only frame protocols.
  let target = frame-selection-target(frame);
  let object = frame-target-browse-object(frame, target);
  if (instance?(object, <sequence>) & ~instance?(object, <string>))
    do(curry(environment-activate-callback, frame), object)
  else
    environment-activate-callback(frame, object)
  end
end method frame-open-selection;


// Close the project belonging to a project window

define method handle-event
    (frame :: <project-browser>, event :: <frame-exited-event>) => ()
  let project = frame.ensure-frame-project;
  close-project(project);
  project.project-opened-by-user? := #f;
  next-method();
end method handle-event;


/// Command enabling

define method frame-showing-sources?
    (frame :: <project-browser>)
 => (showing-sources? :: <boolean>)
  let property-page       = gadget-id(property-sheet-current-page(frame.tab-layout));
  property-page == #"sources"
end method frame-showing-sources?;

define method note-frame-selection-updated
    (frame :: <project-browser>) => ()
  next-method();
  let project             = frame.ensure-frame-project;
  let database-changed?   = project-database-changed?(project);
  let target              = frame-selection-target(frame);
  let selection           = target & target-object(target);
  let selected-source     = instance?(selection, <source-wrapper>) & selection;
  let items-selected?     = selection ~== #f;
  let source-selected?    = frame-showing-sources?(frame) & selected-source;
  let project-source?     = source-selected? & selected-source.wrapper-project == project;
  let source-modifiable?  = project-source? & ~project-read-only?(project);
  let source-can-move?    = source-modifiable? & instance?(selection, <source-record-wrapper>);
  // File
  command-enabled?(frame-open-selection, frame)          := items-selected?;
  command-enabled?(frame-save-project-database, frame)   := database-changed?;
  // Project
  command-enabled?(frame-remove-selected-sources, frame) := source-modifiable?;
  command-enabled?(frame-move-source-file-prev, frame)   := source-can-move?;
  command-enabled?(frame-move-source-file-next, frame)   := source-can-move?;
end method note-frame-selection-updated;


/// Finding project browsers

/// NB Replace complex old method by new simple method that uses protocol
/// and acknowledges move to single document interface for project browser.
/// Jason - 25 Jun 1997.
define method ensure-project-browser-showing-project
    (project :: <project-object>,
     #rest args,
     #key port: _port :: <port> = default-port(),
     #all-keys) => ()
  apply(ensure-environment-frame, _port, <project-browser>, 
	project: project,
	args)
end method ensure-project-browser-showing-project;

define method find-project-browser-showing-project
    (project :: <project-object>,
     #rest args,
     #key port: _port :: <port> = default-port(),
     #all-keys)
 => (project-browser :: false-or(<project-browser>))
  apply(choose-environment-frame, _port, <project-browser>,
	project: project,
	args)
end method find-project-browser-showing-project;
    
define method reuse-matching-frame?
    (portd :: <port-designator>, frame :: <frame>, class :: subclass(<project-browser>),
     #rest initargs, #key project)
 => (reuse? :: <boolean>)
  instance?(frame, <project-browser>)
    & frame.ensure-frame-project == project
end method reuse-matching-frame?;
  

/// Project browser frame title

define method generate-frame-title
    (frame :: <project-browser>) => (title :: <string>)
  let project = frame.ensure-frame-project;
//--- cpage: 1997.10.20 We need to add a protocol to get a project's file location.
//--- andrewa: 1998.04.16 We now have project-filename, if you want to use it.
//  let location = project-location(project.compiler-proxy);
//  locator-name(location)
  concatenate("Project ",
	      environment-object-primitive-name(project, project),
              " - ", release-name())
end method generate-frame-title;


/// Sheet with selection

define method frame-sheet-with-selection
    (frame :: <project-browser>) => (sheet :: false-or(<sheet>))
  pane-sheet-with-selection(frame.tab-layout)
end method frame-sheet-with-selection;


/// View refresh

define method refresh-frame (frame :: <project-browser>) => ()
  next-method();
  unless (frame-mapped?(frame))
    duim-debug-message("refresh-frame called on unmapped frame %=",
                       frame.frame-title);
  end;
  refresh-frame-view(frame)
end method refresh-frame;

define method refresh-frame-view
    (frame :: <project-browser>, 
     #key pages, new-thread? = #t, refresh-all? = #f) => ()
  //--- Is there a better place to clear the status bar?
  frame-status-message(frame) := "";
  refresh-environment-property-pane
    (frame.tab-layout, pages: pages, 
     clean?: #t, new-thread?: new-thread?, refresh-all?: refresh-all?)
end method refresh-frame-view;


/// Frame refreshing

define constant $database-related-pages
  = #[#"general", #"sources", #"definitions", #"libraries"];

define method frame-note-project-rebuilt
    (frame :: <project-browser>) => ()
  next-method();
  refresh-frame-view(frame, pages: $database-related-pages);
end method frame-note-project-rebuilt;

define method frame-note-project-warnings-updated
    (frame :: <project-browser>) => ()
  next-method();
  let project = frame.ensure-frame-project;
  let warnings?
    = block (return)
	do-compiler-warnings
	  (method (warning) return(#t) end, project, project);
	#f
      end;
  // We have to compute the warning information in this thread,
  // so that we are within the scope of the browser lock if this
  // is in the middle of a build.
  refresh-frame-view
    (frame, pages: #[#"warnings"], 
     new-thread?: #f, refresh-all?: #t);
  if (warnings?)
    call-in-frame
      (frame,
       method ()
	 environment-property-pane-page(frame.tab-layout) := #"warnings"
       end)
  end
end method frame-note-project-warnings-updated;

define method frame-note-project-contents-changed
    (frame :: <project-browser>, #key selection) => ()
  // We don't do this in a new thread for two reasons:
  //  - The updates are small and fast, so it's not worth it.
  //  - We're also setting the selection to something reasonable,
  //    and we'd need to wait for the update to complete before
  //    setting the selection anyway.
  refresh-frame-view(frame, pages: #[#"sources"], new-thread?: #f);
  when (selection)
    let gadget  = frame.frame-sheet-with-selection;
    let wrapper = find-project-browser-wrapper(frame, selection);
    gadget.gadget-value := if (wrapper) vector(wrapper) else #[] end;
    note-frame-selection-updated(frame)
  end
end method frame-note-project-contents-changed;

define method frame-note-all-breakpoints-changed
    (frame :: <project-browser>, state :: <breakpoint-state>) => ()
  next-method();
  refresh-frame-view(frame, pages: #[#"breakpoints"])
end method frame-note-all-breakpoints-changed;

define method frame-note-application-state-changed
    (frame :: <project-browser>, state :: false-or(<application-state>)) => ()
  //--- It seems we don't need to do anything here
  next-method();
end method frame-note-application-state-changed;

define method frame-note-breakpoint-state-changed
    (frame :: <project-browser>, breakpoint :: <breakpoint-object>, 
     state :: <breakpoint-state>) => ()
  next-method();
  refresh-frame-view(frame, pages: #[#"breakpoints"])
end method frame-note-breakpoint-state-changed;


/// Sources handling

//---*** This should really live in environment-property-pages
define method find-project-browser-wrapper
    (frame :: <project-browser>, object :: <object>)
 => (wrapper :: false-or(<object-wrapper>))
  let project = frame.ensure-frame-project;
  let gadget  = frame.frame-sheet-with-selection;
  block (return)
    for (wrapper in gadget-items(gadget))
      when (wrapper.wrapper-object == object)
	return(wrapper)
      end
    end
  end
end method find-project-browser-wrapper;

define method find-project-browser-wrapper
    (frame :: <project-browser>, object :: type-union(<string>, <file-locator>))
 => (wrapper :: false-or(<source-wrapper>))
  let project = frame.ensure-frame-project;
  let gadget  = frame.frame-sheet-with-selection;
  let object  = as(<file-locator>, object);
  block (return)
    for (wrapper in gadget-items(gadget))
      when (instance?(wrapper, <source-wrapper>)
	    & wrapper.wrapper-filename = object)
	return(wrapper)
      end
    end
  end
end method find-project-browser-wrapper;

define method find-project-browser-wrapper
    (frame :: <project-browser>, object :: <source-record>)
 => (wrapper :: false-or(<source-wrapper>))
  let project = frame.ensure-frame-project;
  let gadget  = frame.frame-sheet-with-selection;
  block (return)
    for (wrapper in gadget-items(gadget))
      when (instance?(wrapper, <source-record-wrapper>)
	    & wrapper.wrapper-object == object)
	return(wrapper)
      end
    end
  end
end method find-project-browser-wrapper;


/// Clipboard support

define method cut-object?
    (frame :: <project-browser>, target :: <command-target>)
 => (paste? :: <boolean>)
  delete-object?(frame, target)
end method cut-object?;

define method cut-object
    (frame :: <project-browser>, target :: <command-target>) => ()
  let wrapper :: <source-wrapper> = target.target-object;
  let locator = wrapper.wrapper-filename;
  if (locator & frame-remove-sources(frame, vector(locator)))
    copy-object(target.target-pane, target)
  end
end method cut-object;

define method paste-object?
    (frame :: <project-browser>, object) => (paste? :: <boolean>)
  dylan-clipboard-object-available?(frame, <file-locator>)
end method paste-object?;

define method paste-object
    (frame :: <project-browser>, object) => ()
  let object = dylan-clipboard-object(frame);
  select (object by instance?)
    <file-locator> =>
      do-frame-insert-source-file(frame, filename: object);
    otherwise =>
      #f;
  end
end method paste-object;

define method delete-object?
    (frame :: <project-browser>, target :: <command-target>)
 => (paste? :: <boolean>)
  let object = target.target-object;
  instance?(object, <source-wrapper>)
    & object.wrapper-project == frame.ensure-frame-project
    & object.wrapper-filename ~= #f
end method delete-object?;

define method delete-object
    (frame :: <project-browser>, target :: <command-target>) => ()
  let wrapper :: <source-wrapper> = target.target-object;
  let locator = wrapper.wrapper-filename;
  locator & frame-remove-sources(frame, vector(locator))
end method delete-object;


/// Save commands

define method frame-save-project-database (frame :: <project-browser>) => ()
  let project = frame.frame-project;
  with-busy-cursor (frame)
    save-project-database(project)
  end;
end method frame-save-project-database;

define method frame-save-file-as 
    (frame :: <project-browser>, 
     #key filename :: false-or(<file-locator>) = #f) => ()
  let project = frame.frame-project;
  let (filename, filter)
    = if (filename)
        values(filename, #"project")
      else
        environment-choose-file
	  (title:     "Save As",
	   owner:     frame,
	   direction: #"output",
	   default:   #f /* use project name here? */,
	   filters:   #[#"project", #"lid"])
      end;
  when (filename)
    // If the user hasn't explicitly specified a valid file type extension,
    // add one appropriate for the selected file type filter.
    //--- cpage: 1998.06.16 Note that I don't think this quite meets Windows
    //           UI guidelines, but it probably isn't a big deal. I believe
    //           that the proper behavior is to always tack on an extension
    //           regardless of whether the user explicitly entered one
    //           (i.e. just treat the ".extension" as though it were part
    //           of the base name) unless the user types the name in quotes,
    //           thereby forcing a specific name. Doing that would require
    //           a change to save-project so that it doesn't rely on the
    //           extension to determine the type to save as.
    with-busy-cursor (frame)
      let type = filename.environment-locator-type;
      let filename
	= if (type == #"hdp" | type == #"lid")
	    filename
	  else
	    make(object-class(filename),
		 directory: filename.locator-directory,
		 base:      filename.locator-base,
		 extension: select (filter)
			      #"project" => project-file-extension();
			      #"lid"     => lid-file-extension();
			    end)
	  end;
      save-project(project, filename: filename);
    end
  end
end method frame-save-file-as;


/// Project window options

define method frame-edit-options (frame :: <project-browser>) => ()
//---*** cpage: 1997.11.10 Not yet implemented.
  not-yet-implemented(owner: frame);
end method frame-edit-options;


/// Build commands

define method frame-clean-project
    (frame :: <project-browser>, #key process-subprojects? = #t) => ()
  let project = frame.frame-project;
  if (frame-confirm-clean-project(frame, project))
    if (project-can-be-built?(project))
      unless (frame-warn-if-application-tethered(frame, project))
	do-project-frames
	  (method (frame :: <object-browser>)
	     exit-frame(frame)
	   end,
	   project,
	   include: <object-browser>);
	block ()
	  clean-project
	    (project, process-subprojects?: process-subprojects?);
	cleanup
	  refresh-frame(frame)
	end
      end
    else
      environment-error-message
	(format-to-string
	   (if (project-read-only?(project))
	      "Project '%s' is read-only, so you cannot remove its build products"
	    else
	      "Project '%s' has no build products to remove"
	    end,
	    environment-object-primitive-name(project, project)),
	 owner: frame)
    end
  end
end method frame-clean-project;


/// Project sources commands

define constant $insert-file-filters
  = #[#"common-insert", #"dylan", #"tool-spec", #"project",
      #"resource", #"text", #"c", #"c-include", #"library",
      #"all"];

define method do-frame-insert-source-file
    (frame :: <project-browser>, 
     #key filename :: false-or(<file-locator>), after) => ()
  let project   = frame.frame-project;
  let filename :: false-or(<file-locator>)
    = filename
        | begin
	    let title
	      = format-to-string("Insert File into Project %s",
				 environment-object-display-name
				   (project, project, #f));
	    environment-choose-file
	      (title:     title,
	       owner:     frame,
	       directory: project.project-directory,
	       filters:   $insert-file-filters,
	       filter:    #"common-insert")
	  end;
  when (filename)
    let after :: false-or(<file-locator>)
      = after
          | begin
	      let selection = frame-selection(frame);
	      let item = ~empty?(selection) & selection[0];
	      if (item & instance?(item, <source-record-wrapper>))
		item.wrapper-object.source-record-location;
	      end
	    end;
    local method file-source-record? (sr :: <source-record>)
	    filename = sr.source-record-location
	  end;
    local method after-source-record? (sr :: <source-record>)
	    after = sr.source-record-location
	  end;
    // Warn the user if a file with the same name is already in the project
    let sources   = project-sources(project);
    let duplicate-name? = ~empty?(choose(file-source-record?, sources));
    let insert?
      = ~duplicate-name?
          | begin
              let message
                = format-to-string("The project already contains a source file named '%s'."
                                   " Do you want to replace it?",
				   as(<string>, filename));
              environment-question(message, owner: frame, style: #"warning")
            end;
    // Add the file to the project
    when (insert?)
      // First, remove the existing duplicate, if any
      when (duplicate-name?)
        project-remove-source-record(project, filename);
      end;
      project-add-source-record(project, filename);
      when (after)
	let sources = project-sources(project);
	let new-key = find-key(sources, file-source-record?);
	debug-assert(new-key, "Failed to add source record???");
	let after-key = find-key(sources, after-source-record?);
	debug-assert(after-key, "Failed to select item %= in sources!", after);
	when (new-key & after-key)
	  let new-sources
	    = concatenate(copy-sequence(sources, end: after-key + 1),
			  vector(sources[new-key]),
			  if (after-key < new-key)
			    copy-sequence(sources, 
					  start: after-key + 1,
					  end: new-key)
			  else
			    #[]
			  end);
	  reorder-project-sources(project, new-sources);
	end when;
      end when;
      save-project(project);
      frame-note-project-contents-changed(frame, selection: filename);
    end when;
  end when;
end method do-frame-insert-source-file;

define method frame-remove-selected-sources (frame :: <project-browser>) => ()
  //--- cpage: 1998.04.15 At least temporarily, ignore this command if Sources
  //           is not the current property page. This way, if we get command
  //           enabling/disabling wrong, it will silently do nothing.
  when (gadget-id(property-sheet-current-page(frame.tab-layout)) = #"sources")
    let project = frame.frame-project;
    //---*** cpage: 1997.08.04 Filter out things that cannot be removed. Really,
    //              though, we should make it such that this cannot occur by
    //              disabling the command.
    let entries = choose(method (source)
			   instance?(source, <source-wrapper>)
			      & (source.wrapper-project = project)
			 end,
			 frame-selection(frame));
    frame-remove-sources(frame, entries)
  end when;
end method frame-remove-selected-sources;

define method frame-remove-sources
    (frame :: <project-browser>, sources :: <sequence>)
 => (removed? :: <boolean>)
  let project = frame.frame-project;
  let count = size(sources);
  unless (count < 1)
    local method source-wrapper-name
	      (wrapper) => (name :: <string>)
	    // The name used to remove the source from the project
	    select (wrapper by instance?)
	      <file-locator> =>
		as(<string>, wrapper);
	      <source-record-wrapper> =>
		wrapper.wrapper-object.source-record-name;
	      <source-locator-wrapper> =>
		as(<string>, wrapper.wrapper-object);
	      <source-project-wrapper> =>
		as(<string>, project-filename(wrapper.wrapper-object));
	    end
	  end method source-wrapper-name;
    let message
      = if (count > 1)
	  format-to-string("Remove the selected %d items from the project?", count)
	else
	  let wrapper = sources[0];
	  // Special handling for <file-source-records> to display the
	  // whole file name, rather than just the source record name,
	  // which doesn't include the file extension.
	  let filename
	    = if (instance?(wrapper, <source-record-wrapper>)
		    & instance?(wrapper.wrapper-object, <file-source-record>))
		wrapper.wrapper-object.source-record-location.locator-name
	      else
		source-wrapper-name(wrapper)
	      end if;
	  format-to-string("Remove '%s' from the project?",
			   filename)
	end if;
    when (environment-question(message, owner: frame, style: #"warning"))
      do(method (wrapper)
	   project-remove-source-record
	     (project, source-wrapper-name(wrapper))
	 end,
	 sources);
      save-project(project);
      let gadget = frame.frame-sheet-with-selection;
      let old-value = gadget-value(gadget);
      let old-selection = gadget-selection(gadget);
      frame-note-project-contents-changed(frame);
      gadget-value(gadget) := old-value;
      // Update the selection
      unless (gadget-value(gadget))
	gadget-selection(gadget)
	  := case
	       empty?(gadget-items(gadget)) =>
		 #[];
	       empty?(old-selection) =>
		 vector(0);
	       otherwise =>
		 vector(max(old-selection[0] - 1, 0));
	     end case;
      end unless;
      note-frame-selection-updated(frame);
      #t
    end when;
  end unless;
end method frame-remove-sources;

define function reorder-project-sources
    (project :: <project-object>, ordered-sources :: <sequence>) => ()
  let copy-of-sources = copy-sequence(ordered-sources);
  local method find-source-record-key
	    (name :: <string>) => (key :: <integer>)
          local method key-test
                    (name :: <string>, record :: <source-record>)
                 => (equal? :: <boolean>)
                  name = record.source-record-name;
                end method key-test;
          let key = find-key(copy-of-sources, curry(key-test, name));
	  debug-assert(key, "Failed to find name %s in %s [names: %s]",
		       name, copy-of-sources,
		       map(source-record-name, copy-of-sources));
	  key
	end method find-source-record-key;
  local method compare-source-names
            (name1 :: <string>, name2 :: <string>)
         => (greater-than? :: <boolean>)
          let key1 = find-source-record-key(name1);
          let key2 = find-source-record-key(name2);
          key1 < key2
        end method compare-source-names;
  project-reorder-source-records(project, compare-source-names);
end function reorder-project-sources;

define method frame-move-source-file-prev (frame :: <project-browser>) => ()
  //--- cpage: 1998.04.15 At least temporarily, ignore this command if Sources
  //           is not the current property page. This way, if we get command
  //           enabling/disabling wrong, it will silently do nothing.
  when (gadget-id(property-sheet-current-page(frame.tab-layout)) = #"sources")
    let project = frame.frame-project;
    //---*** cpage: 1997.08.04 Filter out things that cannot be moved. Really,
    //              though, we should make it such that this cannot occur by
    //              disabling the command.
    let entries = choose(method (source)
			   instance?(source, <source-record-wrapper>)
			     & (source.wrapper-project = project)
			 end,
			 frame-selection(frame));
    //---*** cpage: 1997.08.04 Temporarily, warn the user that we only move one item.
    //---*** cpage: 1998.04.15 As it turns out, as long as the Sources page is a
    //              tree-control, it will not allow multiple selection.
    when (size(entries) > 1)
      environment-message
	("Moving more than one item is not yet implemented."
	   " Only the first selected item will be moved.",
	 owner: frame)
    end when;
    unless (size(entries) < 1)
      let wrapper :: <source-record-wrapper> = entries[0];
      let source = wrapper.wrapper-object;
      let source-name = source.source-record-name;
      // Edit the sources in a temporary collection, then reorder the sources to match.
      let sources = copy-sequence(project-sources(project));
      let key = find-key(sources,
			 method (source)
			   source.source-record-name = source-name
			 end);
      debug-assert(key, "Failed to find name %s in %s", source-name, sources);
      let prev-key = key - 1;
      when (prev-key >= 0)
	sources[key] := sources[prev-key];
	sources[prev-key] := source;
	reorder-project-sources(project, sources);
	save-project(project);
	frame-note-project-contents-changed(frame, selection: source);
      end when;
    end unless;
  end when;
end method frame-move-source-file-prev;

define method frame-move-source-file-next (frame :: <project-browser>) => ()
  //--- cpage: 1998.04.15 At least temporarily, ignore this command if Sources
  //           is not the current property page. This way, if we get command
  //           enabling/disabling wrong, it will silently do nothing.
  when (gadget-id(property-sheet-current-page(frame.tab-layout)) = #"sources")
    let project = frame.frame-project;
    //---*** cpage: 1997.08.04 Filter out things that cannot be moved. Really,
    //              though, we should make it such that this cannot occur by
    //              disabling the command.
    let entries = choose(method (source)
			   instance?(source, <source-record-wrapper>)
			     & (source.wrapper-project = project)
			 end,
			 frame-selection(frame));
    //---*** cpage: 1997.08.04 Temporarily, warn the user that we only move one item.
    //---*** cpage: 1998.04.15 As it turns out, as long as the Sources page is a
    //              tree-control, it will not allow multiple selection.
    when (size(entries) > 1)
      environment-message
	("Moving more than one item is not yet implemented."
	   " Only the first selected item will be moved.",
	 owner: frame)
    end when;
    unless (size(entries) < 1)
      let wrapper :: <source-record-wrapper> = entries[0];
      let source = wrapper.wrapper-object;
      let source-name = source.source-record-name;
      // Edit the sources in a temporary collection, then reorder the sources to match.
      let sources = copy-sequence(project-sources(project));
      let key = find-key(sources,
			 method (source)
			   source.source-record-name = source-name
			 end);
      let next-key = key + 1;
      when (next-key < size(sources))
	sources[key] := sources[next-key];
	sources[next-key] := source;
	reorder-project-sources(project, sources);
	save-project(project);
	frame-note-project-contents-changed(frame, selection: source);
      end when;
    end unless;
  end when;
end method frame-move-source-file-next;


/// Project browser command tables

define command-table *project-browser-file-open-command-table* (*global-command-table*)
  menu-item "New..."         = frame-new-file,
    accelerator:   make-keyboard-gesture(#"n", #"control"),
    documentation: "Creates a new document.";
  menu-item "Open..."        = frame-open-file,
    accelerator:   make-keyboard-gesture(#"o", #"control"),
    documentation: "Opens an existing document.";
  menu-item "Op&en"          = frame-open-selection,
    accelerator:   #f,
    documentation: "Opens the selected object(s).";
end command-table *project-browser-file-open-command-table*;

define command-table *project-browser-file-io-command-table* (*global-command-table*)
  include *project-browser-file-open-command-table*;
  separator;
  menu-item "Save As..."  = frame-save-file-as,
    documentation: "Saves a copy of the project with a new name or type.";
  menu-item "Save Compiler Database" = frame-save-project-database,
    documentation: "Saves the compiler database for the project.";
  include *export-command-table*;
  //---*** andrewa: remove printing options for 1.0
  // include *print-command-table*;
  include *recent-projects-command-table*;
  include *recent-files-command-table*;
  menu-item "Close"       = frame-close-file,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Closes the document.";
end command-table *project-browser-file-io-command-table*;

define command-table *project-browser-basic-view-command-table* (*global-command-table*)
  include *view-refresh-command-table*;
/* ---*** hughg, 1998/02/10: Hide for 1.0, since there are no options here!
  menu-item "Project Window Options..." = frame-edit-options,
    documentation: "Enables you to change application options.";
*/
end command-table *project-browser-basic-view-command-table*;

define command-table *project-browser-view-command-table* (*global-command-table*)
  include *bar-options-command-table*;
  include *project-browser-basic-view-command-table*;
end command-table *project-browser-view-command-table*;

define command-table *project-browser-go-command-table* (*global-command-table*)
  include *browse-locations-command-table*;
end command-table *project-browser-go-command-table*;

define command-table *project-browser-project-command-table*
    (*global-command-table*)
  include *project-edit-command-table*;
  include *project-reorder-files-command-table*;
  include *project-command-table*;
end command-table *project-browser-project-command-table*;

define command-table *project-browser-command-table* (*global-command-table*)
  menu-item "File"        = *project-browser-file-io-command-table*;
  menu-item "Edit"        = *edit-command-table*;
  menu-item "View"        = *project-browser-view-command-table*;
  menu-item "Go"          = *project-browser-go-command-table*;
  menu-item "Project"     = *project-browser-project-command-table*;
  menu-item "Application" = *run-command-table*;
  menu-item "Window"      = *window-show-command-table*;
  menu-item "Help"        = *environment-help-command-table*;
end command-table *project-browser-command-table*;
