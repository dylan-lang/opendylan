Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DylanWorks mode

//---*** We should really have <dylanworks-syntax-mixin> that gets used by 
//---*** both <dylanworks-mode> and <dylanworks-shell-mode>
define open class <dylanworks-mode> (<dylan-mode>)
end class <dylanworks-mode>;

// Override the usual mapping to <dylan-mode>
begin
  gethash(*keyword->major-mode*,   #"dylan") := <dylanworks-mode>;
  gethash(*file-type->major-mode*, #"dylan") := <dylanworks-mode>
end;

define method initialize-major-mode
    (mode :: <dylanworks-mode>, #key command-set) => ()
  ignore(command-set);
  // First initialize the stuff that looks like <dylan-mode>
  next-method();
  // Then add the <dylanworks-mode> bindings
  let command-set = mode-command-set(mode);
  let nothing = 0;
  let shift   = deuce/$shift-key;
  let control = deuce/$control-key;
  let control+shift = logior(deuce/$control-key, deuce/$shift-key);
  select (command-set-name(command-set))
    #"emacs" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector('.',   control,       edit-next-compiler-warning),
		    vector(#"f9", nothing,       frame-create-or-toggle-breakpoint),
		    vector(#"f9", shift,         frame-new-breakpoint),
		    vector(#"f9", control,       frame-run-to-target),
		    vector(#"f9", control+shift, frame-edit-breakpoint-options),
		    vector(#"f7", shift,         frame-advanced-build-dialog));
    #"windows" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector(#"f9", nothing,       frame-create-or-toggle-breakpoint),
		    vector(#"f9", shift,         frame-new-breakpoint),
		    vector(#"f9", control,       frame-run-to-target),
		    vector(#"f9", control+shift, frame-edit-breakpoint-options),
		    vector(#"f7", shift,         frame-advanced-build-dialog));
    otherwise =>
      #[];
  end
end method initialize-major-mode;

define method mode-name
    (mode :: <dylanworks-mode>) => (name :: <byte-string>)
  "Dylan"
end method mode-name;


/// Auxilliary functions

define generic interval->location-info
    (interval :: type-union(<interval>, <string>))
 => (pathname :: false-or(<string>), coords :: <vector>);

define method interval->location-info
    (interval :: <interval>)
 => (pathname :: false-or(<string>), coords :: <vector>)
  let start-bp    = interval-start-bp(interval);
  let end-bp      = interval-end-bp(interval);
  // Get the interval as a pair of line+index pairs
  let start-line  = bp->line-index(start-bp);
  let start-index = bp-index(start-bp);
  let end-line    = bp->line-index(end-bp);
  let end-index   = bp-index(end-bp);
  let section     = line-section(bp-line(start-bp));
  // Now get the pathname for the section's home buffer
  let buffer      = section-home-buffer(section, editor: $environment-editor);
  let pathname    = buffer & buffer-pathname-as-string(buffer);
  values(pathname, vector(start-line, start-index, end-line, end-index))
end method interval->location-info;

define method interval->location-info
    (string :: <string>)
 => (pathname :: false-or(<string>), coords :: <vector>)
  values(string, #[0, 0, 0, 0])
end method interval->location-info;


/// Deuce primary object

define method frame-primary-object
    (frame :: <environment-editor>)
 => (object :: false-or(<environment-object>))
  let window = frame-window(frame);
  with-editor-state-bound (window)
    when (mark())
      let interval = make-interval(point(), mark());
      let section  = line-section(bp-line(interval-start-bp(interval)));
      let (module, project) = section-module(section);
      when (module)
	let name = as(<byte-string>, interval);
	find-environment-object(project, name, module: module)
      end
    end
  end
end method frame-primary-object;


/// Deuce command targets

define class <deuce-command-target> (<command-target>)
  sealed constant slot target-pane :: <sheet>,
    required-init-keyword: pane:;
  sealed constant slot target-buffer :: <basic-buffer>,
    required-init-keyword: buffer:;
  sealed constant slot target-project :: false-or(<project-object>),
    required-init-keyword: project:;
end class <deuce-command-target>;


define sealed class <bp-command-target> (<deuce-command-target>)
  sealed constant slot target-bp :: <basic-bp>,
    required-init-keyword: bp:;
end class <bp-command-target>;

define method target-object
    (target :: <bp-command-target>)
 => (object :: false-or(<environment-object>))
  #f
end method target-object;

define method target-read-only?
    (target :: <bp-command-target>)
 => (editable? :: <boolean>)
  let buffer = target-buffer(target);
  let bp     = target-bp(target);
  buffer-read-only?(buffer) | line-read-only?(bp-line(bp))
end method target-read-only?;

define method make-command-target
    (pane :: <environment-deuce-pane>, bp :: <basic-bp>)
 => (target :: <bp-command-target>)
  with-editor-state-bound (buffer = pane)
    let frame   = sheet-frame(pane);
    let project = frame-current-project(frame);
    make(<bp-command-target>,
	 pane:    pane,
         buffer:  buffer,
	 bp:      bp,
	 project: project)
  end
end method make-command-target;


define sealed class <interval-command-target> (<deuce-command-target>)
  sealed constant slot target-interval :: <basic-interval>,
    required-init-keyword: interval:;
end class <interval-command-target>;

define method target-object
    (target :: <interval-command-target>)
 => (object :: false-or(<environment-object>))
  let pane :: <environment-deuce-pane> = target-pane(target);
  with-editor-state-bound (pane)
    let interval = target-interval(target);
    let section  = line-section(bp-line(interval-start-bp(interval)));
    let (module, project) = section-module(section);
    when (module)
      let name = as(<byte-string>, interval);
      find-environment-object(project, name, module: module)
    end
  end
end method target-object;

define method target-read-only?
    (target :: <interval-command-target>)
 => (editable? :: <boolean>)
  let buffer   = target-buffer(target);
  let interval = target-interval(target);
  buffer-read-only?(buffer) | interval-read-only?(interval)
end method target-read-only?;

define method make-command-target
    (pane :: <environment-deuce-pane>, interval :: <basic-interval>)
 => (target :: <interval-command-target>)
  with-editor-state-bound (buffer = pane)
    let frame   = sheet-frame(pane);
    let project = frame-current-project(frame);
    make(<interval-command-target>,
	 pane:     pane,
         buffer:   buffer,
	 interval: interval,
	 project:  project)
  end
end method make-command-target;

define method frame-sheet-target
    (frame :: <environment-frame>, pane :: <environment-deuce-pane>)
 => (target :: false-or(<command-target>))
  let point = window-point(pane);
  let mark  = window-mark(pane);
  if (mark)
    make-command-target(pane, make-interval(point, mark))
  else
    make-command-target(pane, point)
  end
end method frame-sheet-target;


define sealed class <section-command-target> (<deuce-command-target>)
  sealed constant slot target-section :: <basic-section>,
    required-init-keyword: section:;
end class <section-command-target>;

define method target-object
    (target :: <section-command-target>)
 => (object :: false-or(<environment-object>))
  #f
end method target-object;

define method target-read-only?
    (target :: <section-command-target>)
 => (editable? :: <boolean>)
  let buffer  = target-buffer(target);
  let section = target-section(target);
  buffer-read-only?(buffer)
  | buffer-read-only?(section-home-buffer(section, editor: $environment-editor))
end method target-read-only?;

define method make-command-target
    (pane :: <environment-deuce-pane>, section :: <basic-section>)
 => (target :: <section-command-target>)
  with-editor-state-bound (buffer = pane)
    let frame   = sheet-frame(pane);
    let project = frame-current-project(frame);
    make(<section-command-target>,
	 pane:    pane,
         buffer:  buffer,
	 section: section,
	 project: project)
  end
end method make-command-target;


define sealed class <buffer-command-target> (<deuce-command-target>)
end class <buffer-command-target>;

define method target-object
    (target :: <buffer-command-target>)
 => (object :: false-or(<environment-object>))
  #f
end method target-object;

define method target-read-only?
    (target :: <buffer-command-target>)
 => (editable? :: <boolean>)
  let buffer = target-buffer(target);
  buffer-read-only?(buffer)
end method target-read-only?;

define sealed method make-command-target
    (pane :: <environment-deuce-pane>, buffer :: <basic-buffer>)
 => (target :: <buffer-command-target>)
  with-editor-state-bound (buffer = pane)
    let frame   = sheet-frame(pane);
    let project = frame-current-project(frame);
    make(<buffer-command-target>,
	 pane:    pane,
         buffer:  buffer,
	 project: project)
  end
end method make-command-target;


/// Presentations

define method do-handle-presentation-event
    (mode :: <dylanworks-mode>, window :: <environment-deuce-pane>,
     atom :: <basic-interval>, type == <dylan-atom>,
     #rest keys,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
	  menu-function = dylanworks-atom-menu) => ()
  ignore(x, y);
  case
    gesture-matches?($edit-gesture, button, modifiers, event-type: event-type) =>
      // Move the point so that the right definition is edited
      clear-mark!(window: window);
      move-point!(bp, window: window);
      queue-redisplay(window, $display-point);
      redisplay-window(window);
      frame-edit-primary-object(sheet-frame(window));
    otherwise =>
      apply(next-method, mode, window, atom, type,
	    menu-function: menu-function, keys);
  end
end method do-handle-presentation-event;

define method dylanworks-atom-menu
    (window :: <environment-deuce-pane>, mode :: <dylanworks-mode>,
     atom :: <basic-interval>, #key bp, x, y) => ()
  ignore(bp, x, y);
  // Set the primary object first, then pop up the menu
  block ()
    primary-object-interval(window) := atom;
    display-environment-popup-menu(window, atom);
  cleanup
    primary-object-interval(window) := #f;
  end
end method dylanworks-atom-menu;


define method do-handle-presentation-event
    (mode :: <dylanworks-mode>, window :: <environment-deuce-pane>,
     line :: <dylan-definition-line>, type == <dylan-definition-line>,
     #rest keys,
     #key bp, x, y, button, modifiers, event-type,
          menu-function = dylanworks-definition-line-menu) => ()
  ignore(bp, x, y, button, modifiers, event-type);
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    menu? =>
      apply(next-method, mode, window, line, type,
	    menu-function: menu-function, keys);
    event-type == #"double-click"
    & gesture-matches?($move-gesture, button, modifiers) =>
      edit-home-definition(window-frame(window), section: line-section(line));
      redisplay-window(window);
    otherwise =>
      #f;
  end
end method do-handle-presentation-event;

define method dylanworks-definition-line-menu
    (window :: <environment-deuce-pane>, mode :: <dylanworks-mode>,
     line :: <dylan-definition-line>, #key bp, x, y) => ()
  ignore(bp, x, y);
  // Set the primary object first, then pop up the menu
  block ()
    let section = line-section(line);
    primary-object-interval(window) := as(<string>, section-definition-name(section));
    display-environment-popup-menu(window, section);
  cleanup
    primary-object-interval(window) := #f;
  end
end method dylanworks-definition-line-menu;


/*---*** Should be replaced by the default method
define method do-handle-presentation-event
    (mode :: <dylanworks-mode>, window :: <environment-deuce-pane>,
     nothing, type == <blank-area>,
     #rest keys,
     #key bp, x, y, button, modifiers, event-type,
	  menu-function = dylanworks-blank-area-menu) => ()
  ignore(bp, x, y, button, modifiers, event-type);
  apply(next-method, mode, window, nothing, type,
	menu-function: menu-function, keys)
end method do-handle-presentation-event;

define method dylanworks-blank-area-menu
    (window :: <environment-deuce-pane>, mode :: <dylanworks-mode>,
     nothing, #key bp, x, y) => ()
  dylanworks-default-editor-menu(window, mode, nothing, bp: bp, x: x, y: y)
end method dylanworks-blank-area-menu;
*/

// Default methods for within the environment
define method do-handle-presentation-event
    (mode :: <major-mode>, window :: <environment-deuce-pane>,
     nothing, type == <blank-area>,
     #rest keys,
     #key bp, x, y, button, modifiers, event-type,
	  menu-function = dylanworks-default-editor-menu) => ()
  ignore(bp, x, y, button, modifiers, event-type);
  apply(next-method, mode, window, nothing, type,
	menu-function: menu-function, keys)
end method do-handle-presentation-event;

define method do-handle-presentation-event
    (mode :: <major-mode>, window :: <environment-deuce-pane>,
     atom :: <basic-interval>, type == <dylan-atom>,
     #rest keys,
     #key bp, x, y, button, modifiers, event-type,
	  menu-function = dylanworks-default-editor-menu) => ()
  ignore(bp, x, y, button, modifiers, event-type);
  apply(next-method, mode, window, atom, type,
	menu-function: menu-function, keys)
end method do-handle-presentation-event;

define method dylanworks-default-editor-menu
    (window :: <environment-deuce-pane>, mode :: <major-mode>,
     object, #key bp, x, y) => ()
  ignore(object, bp, x, y);
  with-editor-state-bound (buffer = window)
    primary-object-interval(window) := #f;
    display-environment-popup-menu(window, buffer)
  end
end method dylanworks-default-editor-menu;

define method blank-area-menu
    (window :: <environment-deuce-pane>, mode :: <major-mode>,
     nothing, #key bp, x, y) => ()
  dylanworks-default-editor-menu(window, mode, nothing, bp: bp, x: x, y: y)
end method blank-area-menu;


/// Mode-dependent Notifications

// From Deuce

define method window-note-mode-entered
    (window :: <environment-editor-pane>, mode :: <major-mode>) => ()
  next-method();		// this will display the mode
  let frame = sheet-frame(window);
  when (frame)
    frame-note-project-updated(frame)
  end
end method window-note-mode-entered;

define method window-note-mode-entered
    (window :: <environment-editor-pane>, mode :: <fundamental-mode>) => ()
  next-method();
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(reset-optimization-colors, frame)    := #f;
    command-enabled?(color-dispatch-optimizations, frame) := #f;
    local method disable-button (gadget)
	    gadget-enabled?(gadget) := #f
	  end method;
    do-command-menu-gadgets(disable-button, frame, color-dispatch-optimizations-callback)
  end
end method window-note-mode-entered;

define method window-note-mode-entered
    (window :: <environment-editor-pane>, mode :: <dylanworks-mode>) => ()
  next-method();		// this will display the mode
  let frame = sheet-frame(window);
  when (frame)
    command-enabled?(reset-optimization-colors, frame)    := #t;
    command-enabled?(color-dispatch-optimizations, frame) := #t;
    local method enable-button (gadget)
	    gadget-enabled?(gadget) := #t
	  end method;
    do-command-menu-gadgets(enable-button, frame, color-dispatch-optimizations-callback);
    frame-note-project-updated(frame)
  end
end method window-note-mode-entered;

define function dylanworks-mode-commands-enabled?-setter
    (enabled? :: <boolean>, frame :: <frame>)
 => (enabled? :: <boolean>)
  for (table in $dylanworks-mode-command-tables)
    command-enabled?(table, frame, do-inherited?: #t) := enabled?;
  end;
  // This one isn't in any command-table, only in the toolbar :-/
  command-enabled?(frame-start-or-resume-application, frame) := enabled?;
  enabled?
end function dylanworks-mode-commands-enabled?-setter;

// From the Environment

define method frame-note-application-state-changed
    (frame :: <environment-editor>, state :: false-or(<application-state>))
 => ()
  // Unless the current buffer is a Dylan buffer with a buffer-project,
  // ignore any application-state-changed messages.
  //--- hughg, 1998/01/19: This method should maybe be on 'enable-
  //--- application-command-table', but at the moment (a) that isn't 
  //--- exported from environment-tools; and (b) the only thing our
  //--- 'next-method' does is call that.
  let buffer = frame-buffer(frame);
  let major-mode = buffer & buffer-major-mode(buffer);
  when (instance?(major-mode, <dylanworks-mode>) & frame-current-project(frame))
    next-method();
  end;
end method;


/// FRAME-NOTE-PROJECT-UPDATED (internal)

// Called by the DEUCE-EDITOR-PROJECT-MESSAGE-RECEIVER and by
// the methods on WINDOW-NOTE-MODE-ENTERED above.
define generic frame-note-project-updated (frame :: <frame>) => ();

// Default method, just in case.
define method frame-note-project-updated
    (frame :: <frame>) => ()
  #f
end method frame-note-project-updated;


/// Hooks to the Dylan environment

define variable *environment-editor-token* :: false-or(<symbol>) = #f;

define function environment-editor-token () => (token :: <symbol>)
  unless (*environment-editor-token*)
    *environment-editor-token*
      := as(<symbol>, concatenate(release-product-name(), " Editor"));
  end;
  *environment-editor-token*
end function;


define method do-edit-definition
    (mode :: <dylanworks-mode>,
     interval :: <basic-interval>, window :: <basic-window>, #key name)
 => (success? :: <boolean>)
  let object = primary-object-interval(window) | interval;
  let name   = name | as(<byte-string>, object);
  let (pathname, coords) = interval->location-info(object);
  when (pathname)
    edit-definitions(environment-editor-token(), "", pathname, coords, name);
    #t
  end
end method do-edit-definition;


define method do-complete-name
    (mode :: <dylanworks-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key menu? = #f)
 => (completion :: type-union(<string>, <boolean>), ambiguous? :: <boolean>)
  complete-dylan-name(mode, interval, window, menu?: menu?)
end method do-complete-name;

define method complete-dylan-name
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key menu? = #f)
 => (completion :: type-union(<string>, <boolean>), ambiguous? :: <boolean>)
  let string   = as(<string>, interval);
  let section  = line-section(bp-line(interval-start-bp(interval)));
  let (module, project) = section-module(section);
  let (module, project)
    = if (module & project) values(module, project)
      else window-module(window) end;	// returns both values...
  if (module & project)
    local method generate-completions (string :: <string>, completer :: <function>)
	    do-namespace-names
	      (method (name)
		 // Use the primitive name to avoid qualifiers
		 completer(environment-object-primitive-name(project, name), name)
	       end method,
	       project, module)
	  end method;
    let frame = sheet-frame(window);
    if (menu?)
      let (string, success?, object, n-matches, completions)
	= with-busy-cursor (frame)
	    complete-from-generator(string, generate-completions, #['-'],
				    action: #"completions")
	  end;
      ignore(string, success?, object, n-matches);
      if (empty?(completions))
	values(#f, #f)
      else
	let completion
	  = deuce/choose-from-dialog(window, completions,
				     title: "Choose a completion",
				     label-key: first, value-key: first);
	values(completion, #f)
      end
    else
      let (string, success?, object, n-matches, completions)
	= with-busy-cursor (frame)
	    complete-from-generator(string, generate-completions, #['-'])
	  end;
      ignore(success?, object, completions);
      values(n-matches > 0 & string, n-matches > 1)
    end
  else
    command-error("No module information available to complete over")
  end
end method complete-dylan-name;


/// Browsing and describing

define method do-describe-object
    (mode :: <dylanworks-mode>,
     interval :: <basic-interval>, window :: <basic-window>, #key name)
 => (success? :: <boolean>)
  let object = primary-object-interval(window) | interval;
  let name   = name | as(<byte-string>, object);
  let (pathname, coords) = interval->location-info(object);
  when (pathname)
    describe(environment-editor-token(), "", pathname, coords, name)
  end
end method do-describe-object;

define method do-browse-object
    (mode :: <dylanworks-mode>,
     interval :: <basic-interval>, window :: <basic-window>, #key name)
 => (success? :: <boolean>)
  let object = primary-object-interval(window) | interval;
  let name   = name | as(<byte-string>, object);
  let (pathname, coords) = interval->location-info(object);
  when (pathname)
    browse(environment-editor-token(), "", pathname, coords, name)
  end
end method do-browse-object;

define method do-browse-class
    (mode :: <dylanworks-mode>,
     interval :: <basic-interval>, window :: <basic-window>, #key name)
 => (success? :: <boolean>)
  let object = primary-object-interval(window) | interval;
  let name   = name | as(<byte-string>, object);
  let (pathname, coords) = interval->location-info(object);
  when (pathname)
    browse-type(environment-editor-token(), "", pathname, coords, name)
  end
end method do-browse-class;

define method do-browse-function
    (mode :: <dylanworks-mode>,
     interval :: <basic-interval>, window :: <basic-window>, #key name)
 => (success? :: <boolean>)
  let object = primary-object-interval(window) | interval;
  let name   = name | as(<byte-string>, object);
  let (pathname, coords) = interval->location-info(object);
  when (pathname)
    browse-function(environment-editor-token(), "", pathname, coords, name)
  end
end method do-browse-function;


/// Show arglist and documentation

define method do-show-arglist
    (mode :: <dylanworks-mode>,
     interval :: <basic-interval>, window :: <basic-window>, #key name)
 => (success? :: <boolean>)
  let object = primary-object-interval(window) | interval;
  let name   = name | as(<byte-string>, object);
  let (pathname, coords) = interval->location-info(object);
  when (pathname)
    //---*** For lack of anything better, just use 'describe'
    describe(environment-editor-token(), "", pathname, coords, name)
  end
end method do-show-arglist;

define method do-show-documentation
    (mode :: <dylanworks-mode>,
     interval :: <basic-interval>, window :: <basic-window>, #key name)
 => (success? :: <boolean>)
  let object = primary-object-interval(window) | interval;
  let name   = name | as(<byte-string>, object);
  let frame  = sheet-frame(window);
  // Off to HTML Help or whatever...
  frame-help-on-keyword(frame, name);
  #t
end method do-show-documentation;


/// Browser buffers

define method definition-browser-parameters
    (mode :: <dylanworks-mode>, interval :: <basic-interval>, what)
 => (definition, name-key :: <function>, generator :: <function>,
     major-mode :: <major-mode>, node-class :: subclass(<definition-node>))
  // Get the definition object from its name
  let name     = as(<byte-string>, interval);
  let section  = line-section(bp-line(interval-start-bp(interval)));
  let (module, project) = section-module(section);
  let definition = module & find-named-definition(project, module, name);
  // Got it, now figure out what generator to use
  let name-key :: false-or(<function>)
    = definition
      & method (object) environment-object-display-name(project, object, module) end;
  let definition-generator :: false-or(<function>)
    = definition 
      & select (what)
	  #"class-subclasses" =>
	    instance?(definition, <class-object>)
	      & do-direct-subclasses;
	  #"class-superclasses" =>
	    instance?(definition, <class-object>)
	      & do-direct-superclasses;
	  #"class-methods" =>
	    instance?(definition, <class-object>)
	      & do-direct-methods;
	  #"generic-function-methods" =>
	    instance?(definition, <generic-function-object>)
	      & do-generic-function-methods;
	  #"function-callers" =>	// actually we can handle any kind of definition...
	    instance?(definition, <definition-object>)
	      & do-client-source-forms;
	  #"function-callees" =>	// actually we can handle any kind of definition...
	    instance?(definition, <definition-object>)
	      & do-used-definitions;
	end;
  if (definition-generator)
    local method section-generator
	      (definition :: <definition-object>) => (sections :: <stretchy-object-vector>)
	    let sections :: <stretchy-object-vector> = make(<stretchy-vector>);
	    local method definition->section (definition) => ()
		    let section
		      = find-section-for-definition(project, definition, source-type: #"newest");
		    section & add!(sections, section)
		  end method;
	    definition-generator(definition->section, project, definition);
	    // More than one definition might live in the same Deuce section,
	    // thus the call to 'remove-duplicates!'
	    remove-duplicates!(sections)
	  end method;
    values(definition, name-key, section-generator,
	   mode, <dylan-definition-node>)
  else
    values(#f, always(""), always(#[]),
	   mode, <dylan-definition-node>)
  end
end method definition-browser-parameters;


/// Mapping from a definition to a source location

define constant <code-viewer-source-type> = one-of(#"newest", #"canonical");
define constant $code-viewer-default-source-type :: <code-viewer-source-type> = #"newest";

define method find-section-for-definition
    (project :: <project-object>, object :: <environment-object>,
     #key source-type :: <code-viewer-source-type> = $code-viewer-default-source-type)
 => (section :: false-or(<basic-section>))
  let location = environment-object-source-location(project, object);
  let record   = location & source-location-source-record(location);
  when (record)
    let (section, line)
      = find-section-for-source-location(project, record, location, source-type: source-type);
    ignore(line);
    section
  end
end method find-section-for-definition;

// This is split out from 'find-section-for-definition' so that it can be
// specialized on different types of source record classes...
define generic find-section-for-source-location
    (project :: <project-object>, record :: <source-record>, location :: <source-location>,
     #key source-type :: <code-viewer-source-type>)
 => (section :: false-or(<basic-section>), line :: false-or(<basic-line>));

define method find-section-for-source-location
    (project :: <project-object>,
     record :: <source-record>, location :: <source-location>,
     #key source-type :: <code-viewer-source-type> = $code-viewer-default-source-type)
 => (section :: false-or(<basic-section>), line :: false-or(<basic-line>))
  ignore(source-type);
  values(#f, #f)
end method find-section-for-source-location;

define method find-section-for-source-location
    (project :: <project-object>,
     record :: <flat-file-source-record>, location :: <source-location>,
     #key source-type :: <code-viewer-source-type> = $code-viewer-default-source-type)
 => (section :: false-or(<basic-section>), line :: false-or(<basic-line>))
  block (return)
    let frame   = default-editor-frame();
    let editor  = frame-editor(frame);
    // We might have a source record from a sub-project, so go find the
    // "parent" project to which this source record really belongs
    let library = find-source-record-library(project, record);
    let project = if (library) library-project(project, library) else project end;
    // Now get the names of the newest and canonical source files
    let locator = source-record-location(record);
    let newest = merge-locators(locator, project-directory(project));
    let canonical = project-canonical-filename(project, newest);
    // OK, what we do is this:
    //  - If there is no canonical source, use the newest source (this is
    //    what happens when canonical source copying is not enabled)
    //  - If the canonical source is desired, use it
    //  - If the newest source is desired and it is not more recent than
    //    the canonical source (i.e., the canonical source is stale), use
    //    the newest source
    //  - Otherwise, locate the section in the canonical source, then
    //    find and use its corresponding section in the newest source
    let index
      = source-record-start-line(record)
	  // Deuce line indices are 0-based, source record line indices are 1-based...
	  + source-offset-line(source-location-start-offset(location)) - 1;
    let buffer
      = case
	  ~canonical | ~file-exists?(canonical) =>
	    find-buffer-from-pathname(editor, newest)
	    | do-find-file(editor, newest, direction: #"input");
	  source-type == #"canonical" =>
	    find-buffer-from-pathname(editor, canonical)
	    | do-find-file(editor, canonical, direction: #"input");
	  get-file-property(newest, #"modification-date")
	    <= get-file-property(canonical, #"modification-date") =>
	    find-buffer-from-pathname(editor, newest)
	    | do-find-file(editor, newest, direction: #"input");
	  otherwise =>
	    // Locate the section in the canonical source
	    let cbuffer   = find-buffer-from-pathname(editor, canonical)
			    | do-find-file(editor, canonical, direction: #"input");
	    let cline     = cbuffer  & line-index->line(cbuffer, index);
	    let csection  = cline    & line-section(cline);
	    let signature = csection & section-definition-signature(csection);
	    when (signature)
	      // Now find the corresponding section in the newest source
	      let nbuffer = find-buffer-from-pathname(editor, newest)
			    | do-find-file(editor, newest, direction: #"input");
	      for (node = buffer-start-node(nbuffer) then node-next(node),
		   until: ~node)
		let section = node-section(node);
		when (section-definition-signature(section) = signature)
		  // We found a matching section, now figure out what line
		  // to point to in the newest file's section
		  let index = line->line-index(csection, cline);
		  let line  = line-index->line(section, index);
		  return(section, line)
		end
	      end
	    end;
	end;
    // OK, we should now have a sectionized Dylan buffer
    // Figure out what section the definition's start line is in
    let line    = buffer & line-index->line(buffer, index);
    let section = line   & line-section(line);
    return(section, line);
  exception (type-union(<file-does-not-exist-error>,
			<source-record-missing>))
    values(#f, #f)
  end
end method find-section-for-source-location;

define method find-section-for-source-location
    (project :: <project-object>, 
     record :: <interactive-source-record>, location :: <source-location>,
     #key source-type :: <code-viewer-source-type> = $code-viewer-default-source-type)
 => (section :: false-or(<basic-section>), line :: false-or(<basic-line>))
  ignore(source-type);
  let section = gethash($interactive-source-sections, record);
  // Deuce line indices are 0-based, source record line indices are 1-based...
  let index = source-record-start-line(record)
		+ source-offset-line(source-location-start-offset(location)) - 1;
  let line  = section & line-index->line(section, index);
  values(section, line)
end method find-section-for-source-location;


/// Source container for interactive source records

define sealed class <interactive-source-container> (<basic-source-container>)
  sealed slot %home-buffer :: false-or(<basic-buffer>) = #f;
  sealed slot %project :: <project-object>,
    required-init-keyword: project:;
end class <interactive-source-container>;

define sealed domain make (singleton(<interactive-source-container>));
define sealed domain initialize (<interactive-source-container>);

define method container-home-buffer
    (container :: <interactive-source-container>, #key editor)
 => (buffer :: false-or(<basic-buffer>))
  ignore(editor);
  container.%home-buffer
end method container-home-buffer;

define sealed inline method container-has-hard-sections?
    (container :: <interactive-source-container>) => (hard-sections? :: singleton(#t))
  #t
end method container-has-hard-sections?;

define sealed method container-read-only?
    (container :: <interactive-source-container>) => (read-only? :: <boolean>)
  #t
end method container-read-only?;

define sealed method read-container-contents
    (container :: <interactive-source-container>, buffer :: <buffer>) => ()
  #f
end method read-container-contents;


/// Buffer for interactive source records

define sealed class <interactive-source-buffer>
    (<file-buffer-mixin>, <basic-buffer>)
end class <interactive-source-buffer>;

define sealed domain make (singleton(<interactive-source-buffer>));
define sealed domain initialize (<interactive-source-buffer>);

define sealed inline method buffer-anonymous?
    (buffer :: <interactive-source-buffer>) => (anonymous? :: <boolean>)
  #t
end method buffer-anonymous?;

define sealed inline method buffer-has-hard-sections?
    (buffer :: <interactive-source-buffer>) => (hard-sections? :: <boolean>)
  #t
end method buffer-has-hard-sections?;


define constant $interactive-source-buffers  :: <object-table> = make(<table>, weak: #"key");
define variable $interactive-source-sections :: <object-table> = make(<table>, weak: #"key");
define variable $interactive-source-records  :: <object-table> = make(<table>, weak: #"value");

define function add-interactive-source-section
    (project :: <project-object>, record :: <interactive-source-record>)
 => (section :: <dylan-section>)
  // Find the buffer and container for this project
  let (buffer, container) = find-interactive-source-buffer(project);
  // Create a new section for this form
  let section = make(<dylan-section>,
		     container: #f,
		     start-line: #f, end-line: #f);
  let code   = as(<byte-string>, source-record-contents(record));
  let stream = make(<string-stream>, contents: code);
  read-section-contents-from-stream(section, stream);
  close(stream);
  // Add the section to the container
  add-section!(container, section);
  // Add a section node for the new section to the buffer
  let start-bp
    = make(<bp>,
	   line: section-start-line(section), index: 0,
	   buffer: buffer);
  let end-bp
    = make(<bp>,
	   line: section-end-line(section), index: deuce/line-length(section-end-line(section)),
	   buffer: buffer,
	   moving?: #t);
  let node
    = make(<dylan-definition-node>,
	   start-bp: start-bp, end-bp: end-bp,
	   section:  section);
  push!(section-nodes(section), node);
  add-node!(buffer, node);
  gethash($interactive-source-sections, record)  := section;
  gethash($interactive-source-records,  section) := record;
  section
end function add-interactive-source-section;

define function find-interactive-source-buffer
    (project :: <project-object>)
 => (buffer :: <interactive-source-buffer>, container :: <interactive-source-container>)
  let buffer = gethash($interactive-source-buffers, project);
  if (buffer)
    values(buffer, buffer-source-container(buffer))
  else
    let name
      = environment-object-primitive-name(project, project);
    let container
      = make(<interactive-source-container>,
	     project: project,
	     pathname: name);		// not strictly correct!
    let buffer
      = make-empty-buffer(<interactive-source-buffer>,
			  name:      name,
			  editor:    $environment-editor,
			  container: container,
			  major-mode:    find-mode(<dylanworks-mode>),
			  section-class: <dylan-section>,
			  node-class:    <dylan-definition-node>);
    container.%home-buffer := buffer;
    gethash($interactive-source-buffers, project) := buffer;
    values(buffer, container)
  end
end function find-interactive-source-buffer;


/// Building

define method compilation-supported?
    (mode :: <dylanworks-mode>) => (compilation-supported? :: <boolean>)
  #t
end method compilation-supported?;

//---*** What do we do about printing compilation warnings?
define method do-compile-to-core
    (mode :: <dylanworks-mode>, interval :: <basic-interval>) => ()
  // Instead of compile partial definitions, just compile each
  // entire section overlapped by the given interval
  let start-node = bp-node(interval-start-bp(interval));
  let end-node   = bp-node(interval-end-bp(interval));
  let timeout    = $interactor-compilation-timeout;
  let window     = frame-window(*editor-frame*);
  block (break)
    let node = start-node;
    while (node)
      let section  = node-section(node);
      when (section)
	let (module, project) = section-module(section);
	if (~module)
	  command-error("Couldn't find a module to compile in")
	else
	  let frame = sheet-frame(window);
	  with-busy-cursor (frame)
	    with-compiler-locked (frame, timeout: timeout)
	      let thread = application-default-interactor-thread(project);
	      if (~thread)
		command-error("Couldn't find an interactive context for downloading")
	      else
		let text = as(<string>, section);
		let transaction-id
		  = project-execute-code(project, text, thread, module: module);
		when (instance?(frame, <environment-editor>))
		  // Record this transaction id so that 'frame-note-interaction-returned'
		  // can add the #"interactive-record" property
		  add!(frame.%transaction-ids, pair(transaction-id, section))
		end
	      end
	    end
	  end
        end
      end;
      if (node == end-node)
	break()
      else
	node := node-next(node)
      end
    end
  end
end method do-compile-to-core;

define method do-build-project
    (mode :: <dylanworks-mode>, buffer :: <basic-buffer>, scope :: <symbol>) => ()
  let pathname = buffer-pathname-as-string(buffer);
  when (pathname)
    block ()
      compile(environment-editor-token(), "", pathname, #[0, 0, 0, 0], "", scope)
    exception (not-found :: <file-project-not-found-warning>)
      command-error(condition-to-string(not-found))
    end;
  end
end method do-build-project;

define method do-macroexpand
    (mode :: <dylanworks-mode>, interval :: <basic-interval>, stream :: <stream>) => ()
  let timeout = $interactor-compilation-timeout;
  let window  = frame-window(*editor-frame*);
  let (module, project)
    = section-module(node-section(bp-node(interval-start-bp(interval))));
  if (~module)
    command-error("Couldn't find a module to use for macroexpansion")
  else
    let frame = sheet-frame(window);
    with-busy-cursor (frame)
      with-compiler-locked (frame, timeout: timeout)
	let text = as(<string>, interval);
	//--- We don't have a trace stream yet...
	let trace-stream = #f;
	project-macroexpand-code(project, module, text,
				 expansion-stream: stream,
				 trace-stream:     trace-stream)
      end
    end
  end
end method do-macroexpand;


/// Source location handling, e.g., for breakpoints

// NB: Don't cache this in the line since the lines can logically
// change line numbers (= source locations).
// 'shadow?: #t' means that 'line-source-location' and friends should
// look for a shadowing interactive source record and use the source
// location from there instead.  Breakpoints use this.
define method line-source-location
    (mode :: <dylanworks-mode>, line :: <basic-line>,
     #key shadow? = #f)
 => (source-location :: false-or(<source-location>))
  let buffer   = section-home-buffer(line-section(line), editor: $environment-editor);
  let pathname = buffer & buffer-pathname-as-string(buffer);
  when (pathname)
    let (source-record, project) = line-source-record(mode, line, shadow?: shadow?);
    when (project & source-record)
      ignore(project);
      let start-line = line-source-location-index(buffer, line, shadow?: shadow?);
      let offset     = source-record-start-line(source-record);
      // Deuce line indices are 0-based, source record line indices are 1-based...
      make-line-location(source-record, start-line - offset + 1)
    end
  end
end method line-source-location;

// Default method, just in case...
define method line-source-location
    (mode :: <major-mode>, line :: <basic-line>,
     #key shadow? :: <boolean> = #f)
 => (source-location :: false-or(<source-location>))
  ignore(shadow?);
  values(#f, #f)
end method line-source-location;

// For ordinary (file) buffers, the source location index for a line
// is the line number within the buffer (i.e., the file)
// Note that this gets called on the home buffer of the line's section
define method line-source-location-index
    (buffer :: <basic-buffer>, line :: <basic-line>,
     #key shadow? = #f)
 => (index :: <integer>)
  dynamic-bind (*buffer* = buffer)	//--- line->line-index should do this
    let section = line-section(line);
    let interactive
      = get-property(line-properties(section-start-line(section)), #"interactive-record");
    if (interactive & shadow?)
      // If there's an interactive source record shadowing this one,
      // use a section-based index.  This works because the text in the
      // interactive record is the same as the text in this section.
      line->line-index(section, line)
    else
      // Try to reconcile the current source section with the contents
      // of the canonical source.  If we can find a corresponding section
      // in the canonical source, use that section to provide the base
      // line number.
      let canonical-section
	= find-canonical-source-section(buffer, section);
      let canonical-offset
	= canonical-section
	  & get-property(line-properties(section-start-line(canonical-section)), #"start-index");
      if (canonical-offset)
	line->line-index(section, line) + canonical-offset
      else
	line->line-index(buffer, line)
      end
    end
  end
end method line-source-location-index;

// For interactive source buffers, the source location index for a line
// is the line number within the section
define method line-source-location-index
    (buffer :: <interactive-source-buffer>, line :: <basic-line>,
     #key shadow? = #f)
 => (index :: <integer>)
  ignore(shadow?);
  dynamic-bind (*buffer* = buffer)	//--- line->line-index should do this
    let section = line-section(line);
    line->line-index(section, line)
  end
end method line-source-location-index;

define method find-canonical-source-section
    (buffer :: <basic-buffer>, section :: <basic-section>)
 => (section :: false-or(<basic-section>))
  #f
end method find-canonical-source-section;

// Given a section in either the newest or the canonical source,
// return the corresponding section in the canonical source
define method find-canonical-source-section
    (buffer :: <basic-buffer>, section :: <dylan-section>)
 => (section :: false-or(<basic-section>))
  let container = buffer-source-container(buffer);
  let project   = buffer-project(buffer);
  block (return)
    when (container & project)
      // This is partly cribbed from 'find-section-for-source-location'...
      //---*** Uh oh, we need to do this, too...
      //---*** let library = find-source-record-library(project, record);
      //---*** let project = if (library) library-project(project, library) else project end;
      let newest = as(<file-locator>, container-pathname(container));
      let canonical = project-canonical-filename(project, newest);
      //--- Issue a warning that the breakpoint may be in the
      //--- wrong place if the file is newer than the last build?
      when (canonical & canonical ~= newest & file-exists?(canonical))
	let nt = get-file-property(newest,    #"modification-date");
	let ct = get-file-property(canonical, #"modification-date");
	when (nt <= ct)
	  // OK, we've got a saved canonical source file that is no older than
	  // the newest source.  Find the section in the canonical source that
	  // corresponds to the (newest) section that was passed in.
	  // This closely resembles 'find-section-for-source-location'...
	  let signature = section-definition-signature(section);
	  when (signature)
	    let editor  = $environment-editor;
	    let cbuffer = find-buffer-from-pathname(editor, canonical)
			  | do-find-file(editor, canonical, direction: #"input");
	    for (node = buffer-start-node(cbuffer) then node-next(node),
		 until: ~node)
	      let section = node-section(node);
	      when (section-definition-signature(section) = signature)
		return(section)
	      end
	    end
	  end
	end
      end
    end
  end
end method find-canonical-source-section;

define method deuce-note-project-products-changed
    (project :: false-or(<project-object>)) => ()
  // This might need to decache canonical source file stuff...
  #f
end method deuce-note-project-products-changed;


/// Buffer protocols

define open generic buffer-project
    (buffer :: <basic-buffer>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (project :: false-or(<project-object>));

// This returns the active project if the buffer contains a file
// in some library of the active project, otherwise #f.
// This may signal <file-project-not-found-warning> or <file-library-not-found-warning>.
define method buffer-project
    (buffer :: <basic-buffer>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (project :: false-or(<project-object>))
  let pathname = buffer-locator(buffer);
  buffer-project-for-pathname(buffer, pathname, on-error: on-error)
end method buffer-project;

// For definition browsing buffers, just return the active project
//--- Better would be to return the "home project" for 'browsing-buffer-definition'
//--- Nobody uses this right now, we instead use Editor Deuce Backend's
//--- composite buffers. But we might need this sometime in the future.
define method buffer-project
    (buffer :: <definition-browsing-buffer>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (project :: false-or(<project-object>))
  ignore(on-error);
  active-project()
end method buffer-project;

define method buffer-project-for-pathname
    (buffer :: <basic-buffer>, pathname :: false-or(<file-locator>),
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (project :: false-or(<project-object>))
  // This is cached in a slightly unusual way, because we need to get the
  // property often, it might repeatedly be #f, and doing the full lookup
  // is expensive (whether we fail or not).  So I cache #"none" to mean that
  // we did the lookup and didn't find anything; use 'remove-property!' to
  // clear the cache.
  let project  = get-property(buffer-properties(buffer), #"project");
  unless (project)
    project
      := block ()
	   let project = active-project();
	   if (pathname & project)
	     if (find-project-source-record(project, pathname))
	       project
	     else
	       let libraries = find-libraries-from-pathname(pathname);
	       unless (empty?(libraries))
		 project
	       end
	     end
	   else
	     signal(make(<file-project-not-found-warning>,
			 format-arguments: vector(buffer-name(buffer))));
	   end
	 exception (type-union(<file-project-not-found-warning>,
			       <file-library-not-found-warning>),
		    test: method (c) on-error ~== #"signal" end)
	   #f
	 end;
    put-property!(buffer-properties(buffer), #"project", project | #"none")
  end;
  if (project == #"none") #f else project end
end method buffer-project-for-pathname;

define generic buffer-module
    (buffer :: <basic-buffer>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (module :: false-or(<module-object>), 
     project :: false-or(<project-object>));

define method buffer-module
    (buffer :: <basic-buffer>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (module :: false-or(<module-object>),
     project :: false-or(<project-object>))
  let project  = buffer-project(buffer, on-error: on-error);
  let locator  = buffer-locator(buffer);
  let module   = project & locator & file-module(project, locator);
  if (module)
    values(module, project)
  else
    values(#f, #f)
  end
end method buffer-module;

define method window-module
    (window :: <environment-deuce-pane>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (module :: false-or(<module-object>),
     project :: false-or(<project-object>))
  let buffer = window-buffer(window);
  let (module, project) = buffer-module(buffer, on-error: on-error);
  if (module)
    values(module, project)
  else
    let frame = sheet-frame(window);
    values(frame-current-module(frame), 
	   frame-current-project(frame))
  end
end method window-module;

define method section-module
    (section :: <basic-section>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (module :: false-or(<module-object>),
     project :: false-or(<project-object>))
  let buffer = section-home-buffer(section, editor: $environment-editor);
  if (buffer)
    buffer-module(buffer, on-error: on-error)
  else
    values(#f, #f)
  end
end method section-module;


// This may signal <file-project-not-found-warning> or <file-library-
// not-found-warning>.  It's here for code-sharing with
// do-delegate-to-project-browser in commands.dylan.
define method editor-frame-current-project
    (frame :: <environment-editor>)
 => (project :: false-or(<project-object>))
  let buffer = frame-buffer(frame);
  buffer & buffer-project(buffer)
end method editor-frame-current-project;

define method frame-current-project
    (frame :: <environment-editor>)
 => (project :: false-or(<project-object>))
  block ()
    editor-frame-current-project(frame)
  exception (type-union(<file-project-not-found-warning>,
			<file-library-not-found-warning>))
    #f
  end
end method frame-current-project;

define method frame-current-module
    (frame :: <environment-editor>)
 => (module :: false-or(<module-object>))
  block ()
    let buffer = frame-buffer(frame);
    buffer-module(buffer)
  exception (type-union(<file-project-not-found-warning>,
			<file-library-not-found-warning>))
    #f
  end
end method frame-current-module;

define method line-source-record
    (mode :: <dylanworks-mode>, line :: <basic-line>,
     #key shadow? = #f)
 => (source-record :: false-or(<source-record>), project :: false-or(<project-object>))
  let section = line-section(line);
  let interactive
    = get-property(line-properties(section-start-line(section)), #"interactive-record");
  if (interactive & shadow?)
    values(interactive, active-project())
  else
    let buffer = section-home-buffer(section, editor: $environment-editor);
    buffer & buffer-source-record(buffer, line: line)
  end
end method line-source-record;

// Default method, just in case...
define method line-source-record
    (mode :: <major-mode>, line :: <basic-line>,
     #key shadow? = #f)
 => (source-record :: false-or(<source-record>), project :: false-or(<project-object>))
  ignore(shadow?);
  values(#f, #f)
end method line-source-record;

define method buffer-source-record
    (buffer :: <basic-buffer>, #key line)
 => (source-record :: false-or(<source-record>), project :: false-or(<project-object>))
  ignore(line);
  let project = active-project();
  let locator = buffer-locator(buffer);
  let record  = project & locator & find-project-source-record(project, locator);
  if (record)
    values(record, project)
  else
    let result
      = when (locator)
	  let libraries = find-libraries-from-pathname(locator);
	  let result    = ~empty?(libraries) & libraries[0];	//--- arbitrary
	  result
	end;
    if (result)
      values(result[1], buffer-project(buffer))
    else
      values(#f, #f)
    end
  end
end method buffer-source-record;

define method buffer-source-record
    (buffer :: <interactive-source-buffer>, #key line)
 => (source-record :: false-or(<source-record>), project :: false-or(<project-object>))
  let section = line & line-section(line);
  let record = gethash($interactive-source-records, section);
  when (record)
    let container :: <interactive-source-container> = section-container(section);
    values(record, container.%project)
  end
end method buffer-source-record;


/// DylanWorks shells

//--- It would be good for this to be abstract, but we can't make 
//--- environment-deuce-gadget abstract.
define class <environment-shell-window>
    (<primary-object-interval-mixin>, <environment-deuce-gadget>)
end class <environment-shell-window>;

define open generic shell-parse-input
    (pane :: <environment-shell-window>, text :: <string>)
 => (complete? :: <boolean>, message :: false-or(<string>));

define open generic shell-execute-code
    (pane :: <environment-shell-window>, text :: <string>, bp :: <basic-bp>)
 => ();

define sealed class <dylanworks-shell-mode> (<dylan-shell-mode>)
end class <dylanworks-shell-mode>;

define sealed domain make (singleton(<dylanworks-shell-mode>));
define sealed domain initialize (<dylanworks-shell-mode>);

// Override the usual mapping to <dylan-shell-mode>
begin
  gethash(*keyword->major-mode*, #"dylan-shell") := <dylanworks-shell-mode>
end;

define method mode-name
    (mode :: <dylanworks-shell-mode>) => (name :: <byte-string>)
  "Dylan shell"
end method mode-name;

define sealed method do-complete-name
    (mode :: <dylanworks-shell-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key menu? = #f)
 => (completion :: type-union(<string>, <boolean>), ambiguous? :: <boolean>)
  complete-dylan-name(mode, interval, window, menu?: menu?)
end method do-complete-name;

define sealed method shell-input-complete?
    (mode :: <dylanworks-shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>)
 => (complete? :: <boolean>, message :: false-or(<string>))
  let window = frame-window(*editor-frame*);
  let text = as(<string>, section);
  shell-parse-input(window, text)
end method shell-input-complete?;

define sealed method do-process-shell-input
    (mode :: <dylanworks-shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>,
     #key window = frame-window(*editor-frame*)) => ()
  let text = as(<string>, section);
  let bp = line-end(section-end-line(section));
  queue-redisplay(window, $display-text);
  shell-execute-code(window, text, bp);
  move-point!(bp, window: window)
end method do-process-shell-input;

define method note-multi-line-deletion
    (mode :: <dylanworks-shell-mode>, interval :: <basic-interval>) => ()
  // Lose any Dylan objects we might be hanging on to...
  do-lines(method (line, si, ei, last?)
	     ignore(si, ei, last?);
	     remove-property!(line-properties(line), #"object");
	   end method, interval)
end method note-multi-line-deletion;


define sealed class <dylanworks-shell-section> (<basic-shell-section>)
  slot %transaction-id = #f;
end class <dylanworks-shell-section>;

define sealed domain make (singleton(<dylanworks-shell-section>));
define sealed domain initialize (<dylanworks-shell-section>);

// Not correct, but it gives reasonable results for indentation
define sealed method section-definition-type
    (section :: <dylanworks-shell-section>) => (type :: false-or(<symbol>))
  #"function"
end method section-definition-type;


/// Presentations within shells

define sealed method do-presentation-at-position
    (mode :: <dylanworks-shell-mode>, window :: <environment-deuce-pane>,
     x :: <integer>, y :: <integer>,
     #key button = $left-button, modifiers = 0, event-type = #"press")
 => (presentation :: false-or(<presentation>),
     bp :: false-or(<basic-bp>), move-point? :: <boolean>)
  let (bp, dline) = position->bp(window, x, y);
  let line = bp & bp-line(bp);
  let over-icon?  = line & x < $prompt-image-width;
  let (sbp, ebp)  = bp & ~over-icon? & atom-under-bp(bp);
  let properties  = if (line) line-properties(line) else #() end;
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    over-icon? =>
      // If we're over the prompt, just return a BP
      values(#f, bp, #t);
    ~(sbp & ebp)
      | gesture-matches?($move-gesture, button, modifiers, event-type: event-type)
      | gesture-matches?($copy-gesture, button, modifiers, event-type: event-type) =>
      // Copy and move are handled at a lower level...
      values(#f, bp, #t);
    menu? & dline & x > display-line-width(dline) + 10 =>
      // Mouse-right (press) on blank area gets a presentation for a menu
      values(make(<presentation>,
		  object: #f, type: <blank-area>),
	     bp, #f);
    menu? & get-property(properties, #"object") =>
      // This line has an object associated with it
      values(make(<presentation>,
		  object: get-property(properties, #"object"), type: <object>),
	     bp, #f);
    menu? & get-property(properties, #"input") =>
      // This line has shell input associated with it
      values(make(<presentation>,
		  object: get-property(properties, #"input"), type: <shell-input>),
	     bp, #f);
    event-type == #"press" | event-type == #"release" & sbp & ebp =>
      // If it's a button press or release that is not a copy or move,
      // give it a presentation as well
      values(make(<presentation>,
		  object: make-interval(sbp, ebp), type: <dylan-atom>),
	     bp, ~menu?);	// don't move point if a menu is requested
    otherwise =>
      // Everything else just gets a raw BP
      values(#f, bp, #t);
  end
end method do-presentation-at-position;

define sealed method do-handle-presentation-event
    (mode :: <dylanworks-shell-mode>, window :: <environment-deuce-pane>,
     object :: <environment-object>, type == <object>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
     	  menu-function) => ()
  ignore(x, y, menu-function);
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    menu? =>
      // Pop up a menu of operations for the object behind this line
      with-temporary-selection (window, bp)
        display-environment-popup-menu(window, object)
      selecter
	move-mark!(line-start(bp-line(bp)), window: window);
        move-point!(line-end(bp-line(bp)),  window: window);
      end;
    otherwise => #f;
  end
end method do-handle-presentation-event;

define sealed method do-handle-presentation-event
    (mode :: <dylanworks-shell-mode>, window :: <environment-deuce-pane>,
     object :: <result-subset>, type == <object>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
     	  menu-function) => ()
  ignore(x, y, menu-function);
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    menu? =>
      // Pop up a menu of operations for the object behind this line
      with-temporary-selection (window, bp)
        display-environment-popup-menu(window, object)
      selecter
	move-mark!(line-start(bp-line(bp)), window: window);
        move-point!(line-end(bp-line(bp)),  window: window);
      end;
    otherwise => #f;
  end
end method do-handle-presentation-event;

define sealed method do-handle-presentation-event
    (mode :: <dylanworks-shell-mode>, window :: <environment-deuce-pane>,
     input :: <basic-interval>, type == <shell-input>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
	  menu-function) => ()
  ignore(x, y, menu-function);
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    menu? =>
      // Pop up a menu of operations for the object behind this line
      with-temporary-selection (window, bp)
	let object = make(<shell-input>,
			  start-bp: interval-start-bp(input),
			  end-bp:   interval-end-bp(input));
        display-environment-popup-menu(window, object)
      selecter
	move-mark!(line-start(bp-line(bp)), window: window);
        move-point!(line-end(bp-line(bp)),  window: window);
      end;
    otherwise => #f;
  end
end method do-handle-presentation-event;


/// Optimization coloring
/// Available so far: normal; by dispatch-optimization.

define command reset-optimization-colors (frame :: <environment-editor>)
    "Remove all coloring from the buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  local method do-reset (buffer :: <basic-buffer>)
	  do-lines(method (line, si, ei, last?)
		     ignore(si, ei, last?);
		     remove-property!(line-contents-properties(line), #"colors");
		   end method, buffer);
	  put-property!(buffer-contents-properties(buffer), #"optimization-colors", #f);
	end method;
  if (composite-buffer?(buffer))
    do(do-reset, buffer-associated-buffers(buffer));
    put-property!(buffer-contents-properties(buffer), #"optimization-colors", #f)
  else
    do-reset(buffer)
  end;
  update-optimization-commands(window, buffer);
  queue-redisplay(window, $display-all);
  frame-last-command-type(frame) := #"file"
end command reset-optimization-colors;

define command color-dispatch-optimizations (frame :: <environment-editor>)
    "Color the buffer according to the dispatch optimizations in it."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let frame    = sheet-frame(window);
  let success? = #f;
  local method do-color (buffer :: <basic-buffer>)
	  let mode = buffer-major-mode(buffer);
	  let (data, record) = find-dispatch-color-data(buffer);
	  when (data)
	    process-dispatch-colorization-data(mode, buffer, data, record)
	  end;
	  put-property!(buffer-contents-properties(buffer), #"optimization-colors", data);
	  data
	end method;
  with-busy-cursor (frame)
    if (composite-buffer?(buffer))
      for (b in buffer-associated-buffers(buffer))
	let data = do-color(b);
	success? := success? | (data ~= #f);
      end;
      when (success?)
	put-property!(buffer-contents-properties(buffer), #"optimization-colors", #t)
      end
    else
      let data = do-color(buffer);
      success? := (data ~= #f)
    end
  end;
  if (success?)
    update-optimization-commands(window, buffer);
    queue-redisplay(window, $display-all)
  else
    //--- This warning is a little misleading in that the problem is not
    //--- so much that it can't find color info, but that can't find the
    //--- project/source-record pair for this file.  But I'm not sure what
    //--- workaround to suggest in that case...
    deuce/warning-dialog
      (window, "Cannot find color information for this file in the database.\n"
	       "Try 'Project > Build' to create it.")
  end;
  frame-last-command-type(frame) := #"file"
end command color-dispatch-optimizations;

define method force-redisplay (frame :: <environment-editor>) => ()
  let buffer :: <basic-buffer> = frame-buffer(frame);
  // If the colorization data has changed, re-read it
  unless (composite-buffer?(buffer))
    //--- Should just get notified when the database changed and recompute then
    let old-data = get-property(buffer-contents-properties(buffer), #"optimization-colors");
    when (old-data)
      let new-data = find-dispatch-color-data(buffer);
      when (new-data)
	unless (new-data == old-data)
	  reset-optimization-colors(frame);
	  color-dispatch-optimizations(frame)
	end
      end
    end
  end;
  // Assume the buffer project is out of date, just in case...
  remove-property!(buffer-properties(buffer), #"project");
  next-method()
end method force-redisplay;

//---*** We should maybe have a UI to let people customise these
define constant $dispatch-colors :: <table>
  = begin
      let table = make(<table>);
      table[#"not-all-methods-known"]            := deuce/$magenta;
      table[#"failed-to-select-where-all-known"] := deuce/$red;
      table[#"lambda-call"]                      := deuce/$blue;
      table[#"slot-accessor-fixed-offset"]       := make-color(  0, 128,   0);	// forest green, sort of
      table[#"inlining"]                         := make-color(128, 128, 128);	// standard "shadow" grey
      table[#"eliminated"]                       := make-color(192, 192, 192);	// standard "control" grey
      //--- Ignoring dynamic-extent for now
      // table[#"dynamic-extent"]                   := deuce/$cyan;
      //--- Ignoring program-notes for now
      // table[#"program-note"]                     := deuce/$yellow;
      table
    end;

define method find-dispatch-color-data
    (buffer :: <basic-buffer>)
 => (data :: false-or(<vector>), record :: false-or(<source-record>))
  let (record, project)
    = buffer-source-record(buffer);
  let data
    = record & project & source-record-colorization-info(project, record);
  if (data)
    values(data, record)
  else
    values(#f, #f)
  end
end method find-dispatch-color-data;

define method process-dispatch-colorization-data
    (mode :: <dylanworks-mode>, buffer :: <basic-buffer>,
     dispatch-info :: <vector>, record :: <source-record>) => ()
  // Collect all the lines into a vector for quick access
  let n-lines :: <integer> = count-lines(buffer);
  let lines  :: <simple-object-vector> = make(<vector>, size: n-lines);
  let colors :: <simple-object-vector> = make(<vector>, size: n-lines);
  let i :: <integer> = 0;
  do-lines(method (line, si, ei, last?)
	     ignore(si, ei, last?);
	     lines[i] := line;
	     inc!(i)
	   end method, buffer);
  // Deuce line indices are 0-based, source record line indices are 1-based
  // hence the "- 1" here to convert to deuce line indices
  let record-sl :: <integer> = source-record-start-line(record) - 1;
  for (i :: <integer> from 0 below dispatch-info.size by 3)
    let start-offset  = dispatch-info[i];
    let end-offset    = dispatch-info[i + 1];
    let dispatch-type = dispatch-info[i + 2];
    let color = element($dispatch-colors, dispatch-type, default: #f);
    let sl :: <integer> = record-sl + source-offset-line(start-offset);
    let si :: <integer> = source-offset-column(start-offset);
    let el :: <integer> = record-sl + source-offset-line(end-offset);
    let ei :: <integer> = source-offset-column(end-offset);
    for (i :: <integer> from sl to el)
      let this-si = if (i = sl) si else 0 end;
      let this-ei = if (i = el) ei else deuce/line-length(lines[i]) end;
      let old-colors :: <list> = colors[i] | #();
      colors[i] := concatenate!(old-colors, list(this-si, this-ei, color));
    end;
  end;
  for (i :: <integer> from 0 below n-lines)
    let line :: <basic-line> = lines[i];
    let line-colors = colors[i];
    when (line-colors)
      put-property!(line-contents-properties(line),
		    #"colors", as(<vector>, line-colors))
    end
  end;
end method process-dispatch-colorization-data;
