Module:    environment-tools
Synopsis:  Environment tools
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ISSUES
// ---*** How do I ...
//   - make it not steal focus when started?
//   - do "Always on top?"
//   - add things to the window pop-up/pull-down menu?
//   - make the window non-maximisable (via DUIM)?
// ---*** Should I be using a Deuce gadget, so I can get context menus?
// ---*** What about when we want to embed this as a pane?
// ---*** Do I need the <frame-help-mixin>?



/// "Describer" tool

define constant $describer-name :: <string> = "Describe";
define constant $please-wait-doc = "Please wait ...";

// This is like an <environment-frame> but has fewer mixins.
define frame <environment-describer>
    (<frame-module-mixin>,
     <frame-clipboard-mixin>,
     <frame-selection-mixin>,
     <frame-reuse-mixin>,
     <frame-help-mixin>,
     <frame-window-settings-mixin>,
     <environment-simple-frame>)
  pane main-layout (frame)
    // Could also display the current project and module context, since
    // we keep them; but I expect env-frames to start doing that some
    // time in the future anyway.
    frame.describer-text-editor;
  layout (frame) frame.main-layout;
  // Would be nice to auto-select the contents, for quick copying, when
  // the text editor gadget is selected by the user.
  constant slot describer-text-editor :: <text-editor>
    = make(<text-editor>,
           documentation:
             "Displays summary information about the selected item.",
           columns: 80, lines: 4,
	   text-style:  make(<text-style>, family: #"fix"),
           text:        $please-wait-doc,
	   read-only?:  #t, tab-stop?: #t,
           scroll-bars: #"vertical");
  // We need to explicitly keep the project, as well as the module.
  slot frame-current-project :: false-or(<project-object>) = #f,
    init-keyword: describer-project:;
  slot describer-current-object :: false-or(<environment-object>) = #f,
    init-keyword: describer-object:;
  keyword icon: = $describer-window-small-icon;
end frame <environment-describer>;

define window-settings 
  describer-window :: <environment-describer> = "Describer Window";

// We need this method since we don't inherit from <environment-frame>.
define method initialize
    (frame :: <environment-describer>,
     #key describer-project, module, describer-object,
     #all-keys)
  next-method();
end method initialize;


// Describer-specific methods

define generic get-description
    (frame :: <environment-describer>, project :: <project-object>,
     object :: <environment-object>, module :: <module-object>)
 => (description :: <string>);

define open generic environment-object-contents
    (project :: <project-object>, environment-object :: <environment-object>)
 => (contents :: <string>);

define method get-description
    (frame :: <environment-describer>, project :: <project-object>,
     object :: <environment-object>, module :: <module-object>)
 => (description :: <string>)
  environment-object-unique-name(project, object, module, qualify-names?: #t)
end method get-description;

define method get-description
    (frame :: <environment-describer>, project :: <project-object>,
     object :: <definition-object>, module :: <module-object>)
 => (description :: <string>)
  let home-name
    = environment-object-home-name(project, object);
  if (instance?(home-name, <binding-name-object>))
    let name         = environment-object-primitive-name(project, home-name);
    let home-module  = name-namespace(project, home-name);
    let home-library = environment-object-library(project, home-module);
    let module-name  = environment-object-primitive-name(project, home-module);
    let library-name = environment-object-primitive-name(project, home-library);
    let location     = print-environment-object-location(project, object, module);
    let type-name    = environment-object-type-name(object);
    concatenate
      (// The "Name:" is in the title bar.
       "In module ",   module-name,
       ", library ",   library-name,
       ", from file ", location, ":\n",
       type-name, " ", name)
  else
    environment-object-unique-name
      (project, object, module, qualify-names?: #t)
  end
end method get-description;

define method get-description
    (frame :: <environment-describer>, project :: <project-object>,
     object :: <user-object>, module :: <module-object>)
 => (description :: <string>)
  if (instance?(object, <definition-object>))
    next-method()
  else
    let class = application-object-class(project, object);
    concatenate
      (format-to-string
	 ("Object %s:\n",
	  environment-object-unique-name
	    (project, object, module, qualify-names?: #t)),
       if (class)
	 get-description(frame, project, class, module)
       else
	 #[]
       end)
  end
end method get-description;

define method get-description
    (frame :: <environment-describer>, project :: <project-object>,
     object :: <collection-object>, module :: <module-object>)
 => (description :: <string>)
  let size = collection-size(project, object);
  concatenate
    (format-to-string
       ("Collection %s:\n",
	environment-object-unique-name
	  (project, object, module, qualify-names?: #t)),
     format-to-string("Size: %s\n", size | ""))
end method get-description;

define macro get-description-method-definer
  { define get-description-method ( ?class:name )
      ?:body
    end }
 => { define method get-description
          (?=frame :: <environment-describer>, ?=project :: <project-object>,
           ?=object :: ?class, ?=module :: <module-object>,
           #next next-method)
       => (description :: <string>)
        concatenate(next-method(), begin ?body end | "")
      end method get-description }
end macro get-description-method-definer;

define function names->name-list
    (names :: <sequence> /* of: <string> */)
 => (name-list :: <string>)
  concatenate
    ("(",
     select (names.size)
       0 => "";
       1 => names[0];
       otherwise =>
	 reduce1
	   (method (names-so-far, new-name)
	      concatenate(names-so-far, ", ", new-name)
	    end,
	    names);
     end,
     ")")
end function names->name-list;

define get-description-method (<class-object>)
  // ---*** Can we show init-keywords, including those accepted by make
  // and initialize methods? 
  let superclass-names :: <sequence>
    = map(method (class-object)
	    environment-object-display-name(project, class-object, module)
	  end,
          class-direct-superclasses(project, object));
  names->name-list(superclass-names)
end get-description-method;

define get-description-method (<domain-object>)
  let specializers = domain-specializers(project, object);
  names->name-list(specializers)
end get-description-method;

// Covers "define {generic,method,function}".
define get-description-method (<dylan-function-object>)
  // ---*** Can we, for <generic-function-object>s, show all keywords
  // accepted by all methods (of those defined at compile-time)?
  // ---*** For make and initialize, can we show keyword parameters from
  // slots descriptions for the relevant class?
  concatenate
    ("\n    ",
     print-function-parameters(project, object, module),
     "\n => ",
     print-function-values(project, object, module))
end get-description-method;

define get-description-method (<library-object>)
  concatenate
    ("\nContents:\t", 
     environment-object-contents(project, object));
end get-description-method;

define get-description-method (<macro-object>)
  // ---*** Would like the lhs of the first main rule.
end get-description-method;

define get-description-method (<module-object>)
  concatenate
    ("\nContents:\t", 
     environment-object-contents(project, object));
end get-description-method;

define get-description-method (<module-variable-object>)
  let type = variable-type(project, object);
  concatenate
    (" :: ",
     if (type)
       environment-object-display-name(project, type, module)
     else
       $unknown
     end)
end get-description-method;

define get-description-method (<constant-object>)
  let type = variable-type(project, object);
  concatenate
    (" :: ",
     if (type)
       environment-object-display-name(project, type, module)
     else
       $unknown
     end)
end get-description-method;


// "Service" function, mainly for use by environment-manager library.
define sideways method show-definition-summary
    (name :: <string>,
     project :: false-or(<project-object>),
     module :: false-or(<module-object>),
     object :: false-or(<definition-object>))
 => (success? :: <boolean>)
  describe-object(name, project, module, object)
end method show-definition-summary;


// Main entry point

define sideways method describe-object
    (name :: <string>,
     project :: false-or(<project-object>),
     module :: false-or(<module-object>),
     object :: false-or(<environment-object>))
 => (success? :: <boolean>)
  with-environment-frame (describer = default-port(), <environment-describer>,
			  describer-project: project, module: module,
			  describer-object: object)
    gadget-text(describer-text-editor(describer)) := $please-wait-doc;
    let project = frame-current-project(describer);
    let module = frame-current-module(describer);
    let object = describer-current-object(describer);
    let description :: <string>
      = case
	  ~project | ~module =>
	    format-to-string
	      ("No project and module context in which to find %s.", name);
	  object =>
	    with-busy-cursor (describer)
	      get-description(describer, project, object, module)
	    end;
	  otherwise =>
	    let module-name = environment-object-home-name(project, module);
	    if (module-name)
	      let library = name-namespace(project, module-name);
	      format-to-string
		("No object found for '%s' in module %s, library %s.",
		 name, 
		 environment-object-primitive-name(project, module-name),
		 environment-object-primitive-name(project, library))
	    else
	      format-to-string("Internal error: no name found for module %=",
			       module)
	    end;
	end;
    gadget-text(describer-text-editor(describer)) := description;
    frame-title(describer) := generate-frame-title(describer)
  end;
  project & module & object & #t
end method describe-object;


// General frame-related methods

define method generate-frame-title
    (frame :: <environment-describer>) => (title :: <string>)
  let project = frame-current-project(frame);
  let module = frame-current-module(frame);
  let object = describer-current-object(frame);
  let object-name
    = if (project & object)
	environment-object-unique-name(project, object, module)
      else
        "No definition"
      end;
  concatenate($describer-name, " - ", object-name)
end method generate-frame-title;

// ---*** What other general frame methods are there?



// Module mixin

// ---*** Do we want to allow people to enter object/module/library
// names, like a cut-down object browser, or can this window only be
// sent objects by other windows?  (The latter would make it easier, and
// maybe smaller, but might be less useful.)

// See above: explicit slot for frame-current-project.



// Selection mixin

// ---*** Can I use the default methods for frame-selection and
// frame-selected-objects [what's the difference?] etc., provided I give
// a frame-primary-collection-gadget method, or will I need to assemble
// strings myself, from several fields (and supply methods for all these
// GFs)?



// Reuse mixin

define sealed method frame-reusable?
    (frame :: <environment-describer>)
 => (reusable? :: singleton(#t))
  #t
end method frame-reusable?;

define sealed method frame-reusable?-setter
    (reusable? :: <boolean>, frame :: <environment-describer>)
 => (reusable? :: singleton(#t))
  reusable? // We ignore this, and stay always reusable.
end method frame-reusable?-setter;

define sealed method reuse-frames?
    (class :: subclass(<environment-describer>))
 => (reuse-frames? :: singleton(#t))
  #t
end method reuse-frames?;

define sealed method reuse-frames?-setter
    (value :: <boolean>, class :: subclass(<environment-describer>))
 => (value :: <boolean>)
  value // We ignore this, and stay always reusable.
end method reuse-frames?-setter;

define sealed method reinitialize-frame 
    (frame :: <environment-describer>,
     #key describer-project, module, describer-object,
     #all-keys)
 => ()
  next-method();
  // Initialize the context slots.
  frame-current-project(frame) := describer-project;
  // Should be a method on <frame-module-mixin> to do this, but there isn't.
  frame-current-module(frame) := module;
  describer-current-object(frame) := describer-object;
  // Update the frame's title
  frame-title(frame) := generate-frame-title(frame);
  // Ensure we're visible.
  deiconify-frame(frame);
  raise-frame(frame, activate?: #f);
end method reinitialize-frame;



// Help mixin

// ---*** Anything to do here?  Maybe I just want a "Help" button?  Or
// one of those "Context help buttons in the title bar" which Windows
// can do?

