Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Chris Page, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Command targets

define open class <command-target> (<object>)
end class <command-target>;

define open generic target-object
    (target :: <command-target>) => (object :: <object>);

define open generic target-pane
    (target :: <command-target>)
 => (pane :: <sheet>);

define open generic make-command-target
    (pane :: <sheet>, object)
 => (target :: <command-target>);


define open generic frame-command-target
    (frame :: <environment-frame>)
 => (target :: false-or(<command-target>));

define open generic frame-command-target-setter
    (target :: false-or(<command-target>), frame :: <environment-frame>)
 => (target :: false-or(<command-target>));

define open generic note-frame-command-target-updated
    (frame :: <environment-frame>) => ();

define open generic frame-target-object
    (frame :: <environment-frame>, object :: <object>)
 => (object :: <object>);

define open generic frame-target-browse-object
    (frame :: <environment-frame>, object :: <object>)
 => (object :: <object>);

define open generic frame-target-edit-object
    (frame :: <environment-frame>, object :: <object>)
 => (object :: <object>);

define open generic frame-target-as-string
    (frame :: <environment-frame>, object :: <object>)
 => (string :: <string>);


define open class <basic-command-target> (<command-target>)
  sealed constant slot target-object,
    required-init-keyword: object:;
  sealed constant slot target-pane :: <sheet>,
    required-init-keyword: pane:;
end class <basic-command-target>;

define method make-command-target
    (pane :: <sheet>, object)
 => (target :: <basic-command-target>)
  make(<basic-command-target>, object: object, pane: pane)
end method make-command-target;

define method frame-target-object
    (frame :: <environment-frame>, object :: <object>)
 => (object :: <object>)
  object
end method frame-target-object;

define method frame-target-browse-object
    (frame :: <environment-frame>, object :: <object>)
 => (object :: <object>)
  let browse-object = frame-target-object(frame, object);
  if (browse-object == object)
    browse-object
  else
    frame-target-browse-object(frame, browse-object)
  end
end method frame-target-browse-object;

define method frame-target-edit-object
    (frame :: <environment-frame>, object :: <object>)
 => (object :: <object>)
  let edit-object = frame-target-object(frame, object);
  if (edit-object == object)
    edit-object
  else
    frame-target-edit-object(frame, edit-object)
  end
end method frame-target-edit-object;

define method frame-target-object
    (frame :: <environment-frame>, target :: <command-target>)
 => (object :: <object>)
  frame-target-object(frame, target.target-object)
end method frame-target-object;

define method frame-target-browse-object
    (frame :: <environment-frame>, target :: <command-target>)
 => (object :: <object>)
  frame-target-browse-object(frame, target.target-object)
end method frame-target-browse-object;

define method frame-target-edit-object
    (frame :: <environment-frame>, target :: <command-target>)
 => (object :: <object>)
  frame-target-edit-object(frame, target.target-object)
end method frame-target-edit-object;

define method frame-target-as-string
    (frame :: <environment-frame>, target :: <command-target>)
 => (string :: <string>)
  target-as-string(target.target-pane, target)
end method frame-target-as-string;


define method frame-target-to-browse
    (frame :: <environment-frame>, #key target)
 => (object :: <object>)
  let target = target | frame-command-target(frame);
  frame-target-browse-object(frame, target)
end method frame-target-to-browse;

define method frame-target-to-edit
    (frame :: <environment-frame>, #key target) => (object :: <object>)
  let target = target | frame-command-target(frame);
  frame-target-edit-object(frame, target)
end method frame-target-to-edit;


/// Command target sequence

define sealed class <command-target-sequence> (<object>)
  constant slot target-sequence :: <sequence>,
    required-init-keyword: sequence:;
end class <command-target-sequence>;

define method make-command-target
    (pane :: <sheet>, object :: <sequence>)
 => (target :: <basic-command-target>)
  if (instance?(object, <string>) | instance?(object, <range>))
    next-method()
  else
    let object
      = select (size(object))
	  0 => #f;
	  1 => object[0];
	  otherwise =>
	    make(<command-target-sequence>, sequence: object);
	end;
    make(<basic-command-target>, object: object, pane: pane)
  end
end method make-command-target;

define method frame-target-object
    (frame :: <environment-frame>, target :: <command-target-sequence>)
 => (object :: <object>)
  map(curry(frame-target-object, frame), target.target-sequence)
end method frame-target-object;

define method frame-target-browse-object
    (frame :: <environment-frame>, target :: <command-target-sequence>)
 => (object :: <object>)
  map(curry(frame-target-browse-object, frame), target.target-sequence)
end method frame-target-browse-object;

define method frame-target-edit-object
    (frame :: <environment-frame>, target :: <command-target-sequence>)
 => (object :: <object>)
  map(curry(frame-target-edit-object, frame), target.target-sequence)
end method frame-target-edit-object;


/// Default frame target

define open generic frame-target-pane
    (frame :: <frame>) => (sheet :: <sheet>);

define open generic frame-selection-target
    (frame :: <frame>) => (target :: false-or(<command-target>));

define open generic frame-sheet-target
    (frame :: <frame>, sheet :: <sheet>)
 => (target :: false-or(<command-target>));

define method frame-target-pane
    (frame :: <environment-frame>)
 => (sheet :: <sheet>)
  frame-input-focus(frame)
    | frame-sheet-with-selection(frame)
    | top-level-sheet(frame)
end method frame-target-pane;

define method frame-selection-target
    (frame :: <environment-frame>)
 => (target :: false-or(<command-target>))
  let sheet = frame-target-pane(frame);
  sheet & frame-sheet-target(frame, sheet)
end method frame-selection-target;

define method frame-sheet-target
    (frame :: <environment-frame>, sheet :: <sheet>)
 => (target :: false-or(<command-target>))
  let object = frame-sheet-selection(frame, sheet);
  object & make-command-target(sheet, object)
end method frame-sheet-target;


/// Target support

define function frame-describe-target
    (frame :: <environment-frame>, #key target) => ()
  frame-describe-object(frame, frame-target-to-browse(frame, target: target))
end function frame-describe-target;

define function frame-document-target
    (frame :: <environment-frame>, #key target) => ()
  frame-document-object(frame, frame-target-to-browse(frame, target: target))
end function frame-document-target;

define function frame-browse-target
    (frame :: <environment-frame>, #key target) => ()
  frame-browse-object(frame, frame-target-to-browse(frame, target: target))
end function frame-browse-target;

define function frame-browse-target-type
    (frame :: <environment-frame>, #key target) => ()
  let class = frame-target-to-browse(frame, target: target);
  frame-browse-object-type(frame, class)
end function frame-browse-target-type;

define function frame-browse-target-generic-function
    (frame :: <environment-frame>, #key target) => ()
  let gf = frame-target-to-browse(frame, target: target);
  frame-browse-object-generic-function(frame, gf)
end function frame-browse-target-generic-function;


define function frame-open-target
    (frame :: <environment-frame>) => ()
  let target = frame-command-target(frame);
  frame-open-object(frame, frame-target-edit-object(frame, target))
end function frame-open-target;

define function frame-open-target-project
    (frame :: <environment-frame>, #key target) => ()
  let library = frame-target-to-browse(frame, target: target);
  if (library)
    let project = library-project(frame.ensure-frame-project, library);
    find-project-browser(project)
  else
    environment-action-unavailable(frame, "'Open Project' is not available")
  end
end function frame-open-target-project;


define function frame-edit-target
    (frame :: <environment-frame>, #key target) => ()
  frame-edit-object(frame, frame-target-to-edit(frame, target: target))
end function frame-edit-target;

define function frame-edit-target-clients
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  when (project)
    let object = frame-target-to-browse(frame, target: target);
    if (instance?(object, <source-form-object>))
      let callers
	= concatenate-as(<vector>,
			 vector(object), source-form-clients(project, object));
      let title
	= print-environment-object-name-to-string(project, object);
      frame-edit-objects(frame, callers,
			 title: format-to-string("Callers of %s", title))
    else
      environment-action-unavailable(frame, "'Edit Clients' is not available")
    end
  end
end function frame-edit-target-clients;

define function frame-edit-target-used-definitions
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  when (project)
    let object = frame-target-to-browse(frame, target: target);
    if (instance?(object, <source-form-object>))
      let callees
	= concatenate-as(<vector>,
			 vector(object), source-form-used-definitions(project, object));
      let title
	= print-environment-object-name-to-string(project, object);
      frame-edit-objects(frame, callees,
			 title: format-to-string("Used definitions of %s", title))
    else
      environment-action-unavailable
	(frame, "'Edit Used Definitions' is not available")
    end
  end
end function frame-edit-target-used-definitions;

define function frame-edit-target-subclasses
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  when (project)
    let class = frame-target-to-browse(frame, target: target);
    if (instance?(class, <class-object>))
      let subclasses
	= concatenate-as(<vector>,
			 vector(class), class-direct-subclasses(project, class));
      let title
	= print-environment-object-name-to-string(project, class);
      frame-edit-objects(frame, subclasses,
			 title: format-to-string("Subclasses of %s", title))
    else
      environment-action-unavailable
	(frame, "'Edit Subclasses' is not available")
    end
  end
end function frame-edit-target-subclasses;

define function frame-edit-target-superclasses
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  when (project)
    let class = frame-target-to-browse(frame, target: target);
    if (instance?(class, <class-object>))
      let superclasses
	= concatenate-as(<vector>,
			 vector(class), class-direct-superclasses(project, class));
      let title
	= print-environment-object-name-to-string(project, class);
      frame-edit-objects(frame, superclasses,
			 title: format-to-string("Superclasses of %s", title))
    else
      environment-action-unavailable
	(frame, "'Edit Superclasses' is not available")
    end
  end
end function frame-edit-target-superclasses;

define function frame-edit-target-class-methods
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  when (project)
    let class = frame-target-to-browse(frame, target: target);
    if (instance?(class, <class-object>))
      let methods = class-direct-superclasses(project, class);
      let title   = print-environment-object-name-to-string(project, class);
      frame-edit-objects(frame, methods,
			 title: format-to-string("Methods of %s", title))
    else
      environment-action-unavailable
	(frame, "'Edit Class Methods' is not available")
    end
  end
end function frame-edit-target-class-methods;

define function frame-edit-target-generic-methods
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  when (project)
    let function = frame-target-to-browse(frame, target: target);
    let generic
      = select (function by instance?)
	  <generic-function-object> => function;
	  <method-object>           => method-generic-function(project, function);
	  otherwise                 => #f;
	end;
    if (generic)
      let methods
	= concatenate-as(<vector>,
			 vector(generic), generic-function-object-methods(project, generic));
      let title
	= print-environment-object-name-to-string(project, generic);
      frame-edit-objects(frame, methods,
			 title: format-to-string("Methods of %s", title))
    else
      environment-action-unavailable
	(frame, "'Edit Generic Methods' is not available")
    end
  end
end function frame-edit-target-generic-methods;


define function frame-edit-project-target-settings
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-target-to-browse(frame, target: target);
  if (instance?(project, <project-object>))
    frame-edit-project-settings(frame, project: project)
  else
    environment-action-unavailable
      (frame, "'Edit Project Settings' is not available")
  end
end function frame-edit-project-target-settings;


define function frame-cut-target
    (frame :: <environment-frame>) => ()
  let target = frame.frame-command-target;
  cut-object(target.target-pane, target)
end function frame-cut-target;

define function frame-copy-target
    (frame :: <environment-frame>) => ()
  let target = frame.frame-command-target;
  copy-object(target.target-pane, target)
end function frame-copy-target;

define function frame-paste-target
    (frame :: <environment-frame>) => ()
  let target = frame.frame-command-target;
  paste-object(target.target-pane, target)
end function frame-paste-target;

define function frame-delete-target
    (frame :: <environment-frame>) => ()
  let target = frame.frame-command-target;
  delete-object(target.target-pane, target)
end function frame-delete-target;


define function display-target-properties
    (frame :: <environment-frame>, #key target) => ()
  let object = frame-target-to-browse(frame, target: target);
  display-object-properties(frame, object)
end function display-target-properties;


define function frame-debug-target
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  let thread = frame-target-to-browse(frame, target: target);
  when (project & instance?(thread, <thread-object>))
    find-debugger-from-environment(default-port(),
				   project: project,
				   thread: thread)
  end
end function frame-debug-target;

define function frame-suspend-target
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  let thread = frame-target-to-browse(frame, target: target);
  when (project & instance?(thread, <thread-object>))
    suspend-application-thread(project, thread)
  end
end function frame-suspend-target;

define function frame-resume-target
    (frame :: <environment-frame>, #key target) => ()
  let project = frame-current-project(frame);
  let thread = frame-target-to-browse(frame, target: target);
  when (project & instance?(thread, <thread-object>))
    resume-application-thread(project, thread)
  end
end function frame-resume-target;


/// Commands

define constant $describe-target-doc
  = "Displays summary information about the selected item.";
define constant $document-target-doc
  = "Shows on-line documentation for the selected item.";
define constant $browse-target-doc
  = "Opens a browser on the selected item.";
define constant $browse-target-type-doc
  = "Opens a browser on the type of the selected item.";
define constant $browse-target-generic-function-doc
  = "Opens a browser on the generic function of the selected method.";

define constant $open-target-doc
  = "Opens the selected item.";

define constant $open-target-project-doc
  = "Opens the project for the selected item.";

define constant $edit-target-source-doc
  = "Opens an editor on the source for the selected item.";
define constant $edit-target-clients-doc
  = "Opens an editor on a document containing the users of the selected definition.";
define constant $edit-target-used-definitions-doc
  = "Opens an editor on a document containing the definitions used by the selected definition.";
define constant $edit-target-subclasses-doc
  = "Opens an editor on a document containing the subclasses of the selected class.";
define constant $edit-target-superclasses-doc
  = "Opens an editor on a document containing the superclasses of the selected class.";
define constant $edit-target-class-methods-doc
  = "Opens an editor on a document containing the methods defined on the selected class.";
define constant $edit-target-generic-methods-doc
  = "Opens an editor on a document containing the methods of the selected generic function.";

define constant $cut-target-doc
  = "Removes the selected items and copies them onto the Clipboard.";
define constant $copy-target-doc
  = "Copies the selected items onto the Clipboard.";
define constant $paste-into-target-doc
  = "Inserts the items you have copied or cut into the selected location.";
define constant $delete-target-doc
  = "Removes the selected items without copying them onto the Clipboard.";

define command-table *popup-menu-clipboard-command-table*
    (*global-command-table*)
  menu-item "Cut"           = frame-cut-target,
    documentation: $cut-target-doc;
  menu-item "Copy"          = frame-copy-target,
    documentation: $copy-target-doc;
  menu-item "Paste"         = frame-paste-target,
    documentation: $paste-into-target-doc;
  menu-item "Delete"        = frame-delete-target,
    documentation: $delete-target-doc;
end command-table *popup-menu-clipboard-command-table*;

define function make-command-decorator
    (label :: <string>, function :: <function>, #rest initargs)
 => (decorator :: <command-decorator>)
  apply(make, <command-decorator>,
	label: label,
	object: function,
	type: <function>,
	initargs)
end function make-command-decorator;

define constant $describe-target-command
  = make-command-decorator("Describe", frame-describe-target,
			   documentation: $describe-target-doc);

define constant $browse-target-command
  = make-command-decorator("Browse", frame-browse-target,
			   documentation: $browse-target-doc);

define constant $browse-target-type-command
  = make-command-decorator("Browse Type", frame-browse-target-type,
			   documentation: $browse-target-type-doc);

define constant $browse-target-generic-function-command
  = make-command-decorator("Browse Generic Function", frame-browse-target-generic-function,
			   documentation: $browse-target-generic-function-doc);

define command-table *popup-menu-browse-command-table*
    (*global-command-table*)
  command $describe-target-command;  
  command $browse-target-command;  
  command $browse-target-type-command;  
end command-table *popup-menu-browse-command-table*;

define command-table *popup-menu-method-browse-command-table*
    (*global-command-table*)
  command $describe-target-command;
  command $browse-target-command;  
  command $browse-target-type-command;
  command $browse-target-generic-function-command;
end command-table *popup-menu-method-browse-command-table*;


define constant $document-target-command
  = make-command-decorator("Show Documentation", frame-document-target,
			   documentation: $document-target-doc);

define command-table *popup-menu-documentation-command-table*
    (*global-command-table*)
  command $document-target-command;  
end command-table *popup-menu-documentation-command-table*;


/// Editing commands

define constant $open-target-command
  = make-command-decorator("Open", frame-open-target,
			   documentation: $open-target-doc);

define constant $open-target-project-command
  = make-command-decorator("Open Project", frame-open-target-project,
			   documentation: $open-target-project-doc);

define constant $edit-target-source-command
  = make-command-decorator("Edit Source", frame-edit-target,
			   documentation: $edit-target-source-doc);

define constant $edit-target-clients-command
  = make-command-decorator("Edit Clients", frame-edit-target-clients,
			   documentation: $edit-target-clients-doc);

define constant $edit-target-used-definitions-command
  = make-command-decorator("Edit Used Definitions", frame-edit-target-used-definitions,
			   documentation: $edit-target-used-definitions-doc);

define constant $edit-target-subclasses-command
  = make-command-decorator("Edit Subclasses", frame-edit-target-subclasses,
			   documentation: $edit-target-subclasses-doc);

define constant $edit-target-superclasses-command
  = make-command-decorator("Edit Superclasses", frame-edit-target-superclasses,
			   documentation: $edit-target-superclasses-doc);

define constant $edit-target-class-methods-command
  = make-command-decorator("Edit Methods", frame-edit-target-class-methods,
			   documentation: $edit-target-class-methods-doc);

define constant $edit-target-generic-methods-command
  = make-command-decorator("Edit Methods", frame-edit-target-generic-methods,
			   documentation: $edit-target-generic-methods-doc);

define command-table *popup-menu-edit-command-table*
    (*global-command-table*)
  command $edit-target-source-command;
end command-table *popup-menu-edit-command-table*;

define command-table *popup-menu-form-edit-command-table*
    (*global-command-table*)
  command $edit-target-source-command;
  command $edit-target-clients-command;
  command $edit-target-used-definitions-command;
end command-table *popup-menu-form-edit-command-table*;

define command-table *popup-menu-class-edit-command-table*
    (*global-command-table*)
  command $edit-target-source-command;
  command $edit-target-clients-command;
  command $edit-target-used-definitions-command;
  command $edit-target-subclasses-command;
  command $edit-target-superclasses-command;
  command $edit-target-class-methods-command;
end command-table *popup-menu-class-edit-command-table*;

define command-table *popup-menu-generic-edit-command-table*
    (*global-command-table*)
  command $edit-target-source-command;
  command $edit-target-clients-command;
  command $edit-target-used-definitions-command;
  command $edit-target-generic-methods-command;
end command-table *popup-menu-generic-edit-command-table*;

define command-table *popup-menu-properties-command-table*
    (*global-command-table*)
//---*** cpage: 97.07.22 This is not currently used. Restore this command if we
//              decide it is useful. Also, we need to add code to disable it when
//              appropriate.
/*
  menu-item "Properties"    = display-target-properties,
    documentation: "Displays the properties of the selected items.";
*/
end command-table *popup-menu-properties-command-table*;
  

/// Breakpoint commands

define command-table *all-breakpoints-command-table* (*global-command-table*)
  menu-item "New Breakpoint..." = frame-new-breakpoint,
    accelerator:   make-keyboard-gesture(#"f9", #"shift"),
    documentation: "Creates a new breakpoint from a named function or class.";
  menu-item "Enable All Breakpoints"     = frame-enable-all-breakpoints,
    documentation: "Enables all current breakpoints.";
  menu-item "Disable All Breakpoints"    = frame-disable-all-breakpoints,
    documentation: "Disables all current breakpoints.";
  menu-item "Clear All Breakpoints"      = frame-clear-all-breakpoints,
    documentation: "Clears all current breakpoints.";
end command-table *all-breakpoints-command-table*;

define command-table *tracing-command-table* (*global-command-table*)
  menu-item "Trace"             = frame-trace-target,
    documentation: "Sets a trace point for the selected function.";
  menu-item "Untrace"           = frame-untrace-target,
    documentation: "Removes the trace point for the selected function.";
  menu-item "Untrace All"       = frame-untrace-all,
    documentation: "Removes all of the trace points for the current project.";
end command-table *tracing-command-table*;

define command-table *single-breakpoint-command-table* (*global-command-table*)
  menu-item "Set Breakpoint"             = frame-create-breakpoint,
    documentation: "Sets a breakpoint.";
  menu-item "Clear Breakpoint"           = frame-clear-breakpoint,
    documentation: "Clears a breakpoint.";
  menu-item "Edit Breakpoint Options..." = frame-edit-breakpoint-options,
    accelerator: make-keyboard-gesture(#"f9", #"control", #"shift"),
    documentation: "Modifies a breakpoint's options.";
end command-table *single-breakpoint-command-table*;

add-command-table-menu-item
  (*single-breakpoint-command-table*,
   "", <check-box>, #[],
   items: #[#["Breakpoint Enabled?", #"enabled?"]],
   label-key: first,
   value-key: second,
   update-callback: update-breakpoint-enabled-toggle,
   callback: method (menu-box)
               frame-toggle-breakpoint-enabled?(menu-box.sheet-frame);
             end);

define command-table *breakpoint-location-command-table* (*global-command-table*)
  include *single-breakpoint-command-table*;
end command-table *breakpoint-location-command-table*;


/// Popup menu command tables

define command-table *popup-menu-command-table* (*global-command-table*)
  include *popup-menu-edit-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *popup-menu-command-table*;

define command-table *object-popup-menu-command-table* (*global-command-table*)
  include *popup-menu-browse-command-table*;
  include *popup-menu-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *object-popup-menu-command-table*;

define command-table *project-popup-menu-command-table*
    (*global-command-table*)
  command $open-target-command;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
  menu-item "Edit Project Settings..." = frame-edit-project-target-settings,
    documentation: "Enables you to change the project settings.";
end command-table *project-popup-menu-command-table*;

define command-table *library-popup-menu-command-table* (*global-command-table*)
  command $open-target-project-command;
  include *popup-menu-browse-command-table*;
  include *popup-menu-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *library-popup-menu-command-table*;

define command-table *form-popup-menu-command-table* (*global-command-table*)
  include *popup-menu-browse-command-table*;
  include *popup-menu-form-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *form-popup-menu-command-table*;

define command-table *class-popup-menu-command-table* (*global-command-table*)
  include *popup-menu-browse-command-table*;
  include *popup-menu-class-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *single-breakpoint-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *class-popup-menu-command-table*;

define command-table *function-popup-menu-command-table* (*global-command-table*)
  include *popup-menu-browse-command-table*;
  include *popup-menu-form-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *tracing-command-table*;
  include *single-breakpoint-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *function-popup-menu-command-table*;

define command-table *generic-popup-menu-command-table* (*global-command-table*)
  include *popup-menu-browse-command-table*;
  include *popup-menu-generic-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *tracing-command-table*;
  include *single-breakpoint-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *generic-popup-menu-command-table*;

define command-table *method-popup-menu-command-table* (*global-command-table*)
  include *popup-menu-method-browse-command-table*;
  include *popup-menu-generic-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *tracing-command-table*;
  include *single-breakpoint-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *method-popup-menu-command-table*;

define command-table *thread-command-table* (*global-command-table*)
  menu-item "Debug" = frame-debug-target,
    documentation: "Opens a debugger on the selected thread.";
  separator;
  menu-item "Suspend" = frame-suspend-target,
    documentation: "Suspend this application thread.";
  menu-item "Resume"  = frame-resume-target,
    documentation: "Resume this application thread.";
end command-table *thread-command-table*;

define command-table *dylan-file-popup-menu-command-table*
    (*global-command-table*)
  command $open-target-command;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *dylan-file-popup-menu-command-table*;

define command-table *file-popup-menu-command-table*
    (*global-command-table*)
  command $open-target-command;
  command $edit-target-source-command;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *file-popup-menu-command-table*;


/// Command table selection

define open generic command-table-for-target
    (frame :: <frame>, target) => (comtab :: <command-table>);

define open generic frame-extra-command-table-for-target
    (frame :: <frame>, target) => (comtab :: false-or(<command-table>));

define method frame-command-table-for-target
    (frame :: <frame>, target :: <command-target>) => (comtab :: <command-table>)
  let command-table = command-table-for-target(frame, target);
  let extra-table = frame-extra-command-table-for-target(frame, target);
  if (extra-table)
    let new-table
      = make(<command-table>,
	     name: command-table-name(command-table),
	     inherit-from: #[]);
    add-command-table-menu-item(new-table, #f, <separator>, extra-table);
    add-command-table-menu-item(new-table, #f, <separator>, command-table);
    new-table
  else
    command-table
  end
end method frame-command-table-for-target;

define method frame-extra-command-table-for-target
    (frame :: <frame>, target) => (comtab :: false-or(<command-table>))
  #f
end method frame-extra-command-table-for-target;

define method frame-extra-command-table-for-target
    (frame :: <frame>, target :: <command-target>)
 => (comtab :: false-or(<command-table>))
  frame-extra-command-table-for-target(frame, target.target-object)
end method frame-extra-command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, object :: <object>)
 => (comtab :: <command-table>)
  *popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, target :: <command-target>)
 => (comtab :: <command-table>)
  command-table-for-target(frame, target.target-object)
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, object :: <environment-object>)
 => (comtab :: <command-table>)
  *object-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, object :: <breakpoint-location>)
 => (comtab :: <command-table>)
  *breakpoint-location-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, form :: <source-form-object>)
 => (comtab :: <command-table>)
  *form-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, class :: <class-object>)
 => (comtab :: <command-table>)
  *class-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, function :: <function-object>)
 => (comtab :: <command-table>)
  *function-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, function :: <generic-function-object>)
 => (comtab :: <command-table>)
  *generic-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, function :: <method-object>)
 => (comtab :: <command-table>)
  *method-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, project :: <project-object>)
 => (comtab :: <command-table>)
  *project-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, project :: <library-object>)
 => (comtab :: <command-table>)
  *library-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, thread :: <thread-object>)
 => (command-table :: <command-table>)
  *thread-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, stack-frame :: <stack-frame-object>)
 => (command-table :: <command-table>)
  let project = frame-current-project(frame);
  let object = stack-frame-environment-object(project, stack-frame);
  if (object)
    command-table-for-target(frame, object)
  else
    next-method()
  end
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, locator :: <file-locator>)
 => (comtab :: <command-table>)
  *file-popup-menu-command-table*
end method command-table-for-target;

define method command-table-for-target
    (frame :: <environment-frame>, record :: <source-record>)
 => (comtab :: <command-table>)
  *dylan-file-popup-menu-command-table*
end method command-table-for-target;


/// Default command handling

define constant <command-or-function> = type-union(<function>, <command>);

define open generic default-command-for-target
    (frame :: <frame>, object :: <object>)
 => (command :: false-or(<command-or-function>));

define method default-command-for-target
    (frame :: <environment-frame>, target :: <command-target>)
 => (command :: false-or(<command-or-function>))
  default-command-for-target(frame, target.target-object)
end method default-command-for-target;

//---*** andrewa: is this really a sensible default? It may not be
//---*** sensible to try and 'do the right thing' if that makes a
//---*** complicated user model.
define method default-command-for-target
    (frame :: <environment-frame>, target :: <object>)
 => (command :: false-or(<function>))
  let browse-target = frame-target-browse-object(frame, target);
  let edit-target = frame-target-edit-object(frame, target);
  let project = frame-current-project(frame);
  case
    browse-target & frame-browse-object?(frame, browse-target) =>
      frame-browse-target;
    project & object-has-source?(project, edit-target) =>
      frame-edit-target;
    otherwise =>
      #f;
  end
end method default-command-for-target;

define method default-command-for-target
    (frame :: <environment-frame>, object :: <file-locator>)
 => (command :: false-or(<function>))
  let project = frame-current-project(frame);
  if (project
	& ~frame-open-by-default?(frame, object)
	& object-has-source?(project, object))
    frame-edit-target
  else
    frame-open-target
  end
end method default-command-for-target;

define method default-command-for-target
    (frame :: <environment-frame>, object :: <compiler-warning-object>)
 => (command :: false-or(<function>))
  let project = frame-current-project(frame);
  if (project & object-has-source?(project, object))
    frame-edit-target
  end
end method default-command-for-target;

define method default-command-for-target
    (frame :: <environment-frame>, project :: <project-object>)
 => (command :: false-or(<function>))
  frame-open-target
end method default-command-for-target;

define method default-command-for-target
    (frame :: <environment-frame>, object :: <thread-object>)
 => (command :: false-or(<function>))
  frame-debug-target
end method default-command-for-target;


/// Primary object command table

define open generic frame-describe-primary-object
    (frame :: <environment-frame>) => ();

define open generic frame-browse-primary-object
    (frame :: <environment-frame>) => ();

define open generic frame-browse-primary-object-type
    (frame :: <environment-frame>) => ();

define open generic frame-browse-primary-object-generic-function
    (frame :: <environment-frame>) => ();

define open generic frame-edit-primary-object
    (frame :: <environment-frame>) => ();

define open generic frame-display-primary-object-properties
    (frame :: <environment-frame>) => ();

define open generic frame-document-primary-object
    (frame :: <environment-frame>) => ();

define variable $edit-source-bitmap    :: <label-type> = "Edit Source";

define method frame-describe-primary-object
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-describe-object(frame, frame-target-browse-object(frame, target))
end method frame-describe-primary-object;

define method frame-browse-primary-object
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-browse-object(frame, frame-target-browse-object(frame, target))
end method frame-browse-primary-object;

define method frame-browse-primary-object-type
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-browse-object-type(frame, frame-target-browse-object(frame, target))
end method frame-browse-primary-object-type;

define method frame-browse-primary-object-generic-function
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-browse-object-generic-function(frame, frame-target-browse-object(frame, target))
end method frame-browse-primary-object-generic-function;

define method frame-edit-primary-object
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-edit-object(frame, frame-target-edit-object(frame, target))
end method frame-edit-primary-object;

define method frame-edit-primary-object-clients
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-edit-target-clients(frame, target: frame-target-edit-object(frame, target))
end method frame-edit-primary-object-clients;

define method frame-edit-primary-object-used-definitions
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-edit-target-used-definitions(frame, target: frame-target-edit-object(frame, target))
end method frame-edit-primary-object-used-definitions;

define method frame-edit-primary-object-subclasses
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-edit-target-subclasses(frame, target: frame-target-edit-object(frame, target))
end method frame-edit-primary-object-subclasses;

define method frame-edit-primary-object-superclasses
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-edit-target-superclasses(frame, target: frame-target-edit-object(frame, target))
end method frame-edit-primary-object-superclasses;

define method frame-edit-primary-object-class-methods
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-edit-target-class-methods(frame, target: frame-target-edit-object(frame, target))
end method frame-edit-primary-object-class-methods;

define method frame-edit-primary-object-generic-methods
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-edit-target-generic-methods(frame, target: frame-target-edit-object(frame, target))
end method frame-edit-primary-object-generic-methods;

define method frame-display-primary-object-properties
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  display-object-properties(frame, frame-target-object(frame, target))
end method frame-display-primary-object-properties;

define method frame-document-primary-object
    (frame :: <environment-frame>) => ()
  let target = frame-primary-object(frame);
  frame-document-object(frame, frame-target-browse-object(frame, target))
end method frame-document-primary-object;

define command-table *primary-object-browse-command-table* (*global-command-table*)
  menu-item "Describe" = frame-describe-primary-object,
    accelerator:   make-keyboard-gesture(#"f2", #"control"),
    documentation: "Displays summary information about the object being browsed.";
  menu-item "Browse" = frame-browse-primary-object,
    accelerator:   make-keyboard-gesture(#"f2"),
    documentation: "Opens a browser on the object being browsed.";
  menu-item "Browse Type" = frame-browse-primary-object-type,
    accelerator:   make-keyboard-gesture(#"f2", #"alt"),
    documentation: "Opens a browser on the type of the object being browsed.";
  menu-item "Browse Generic Function" = frame-browse-primary-object-generic-function,
    accelerator:   make-keyboard-gesture(#"f2", #"shift", #"alt"),
    documentation: "Opens a browser on the generic function of the object being browsed.";
end command-table *primary-object-browse-command-table*;

define command-table *primary-object-documentation-command-table* (*global-command-table*)
  menu-item "Show Documentation" = frame-document-primary-object,
    accelerator:   make-keyboard-gesture(#"f1"),
    documentation: "Show documentation for the object being browsed.";
end command-table *primary-object-documentation-command-table*;

define command-table *primary-object-edit-command-table* (*global-command-table*)
  menu-item "Edit Source"   = frame-edit-primary-object,
    accelerator:   make-keyboard-gesture(#"f2", #"shift"),
    documentation: "Opens an editor on the source for the object being browsed.";
end command-table *primary-object-edit-command-table*;

define command-table *primary-object-properties-command-table* (*global-command-table*)
//---*** cpage: 97.07.22 This is not currently used. Restore this command if we
//              decide it is useful. Also, we need to add code to disable it when
//              appropriate.
/*
  menu-item "Properties"    = frame-display-primary-object-properties,
    documentation: "Displays the properties of the object being browsed.";
*/
end command-table *primary-object-properties-command-table*;

//---*** andrewa: this is so hideous, we should really go the whole hog
//---*** and change the command table dynamically to be identical to the
//---*** popup menu that would appear for the object. Either that or get
//---*** rid of this stupid menu altogether...
define command-table *primary-object-command-table* (*global-command-table*)
  include *primary-object-browse-command-table*;
  include *primary-object-documentation-command-table*;
  include *primary-object-edit-command-table*;
  include *primary-object-properties-command-table*;
end command-table *primary-object-command-table*;
