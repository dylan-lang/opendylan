Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project property pages

define sideways method frame-property-types
    (frame :: <environment-frame>, class :: subclass(<project-object>))
 => (types :: <list>)
  concatenate(next-method(),
              #(#"sources",
                #"warnings",
		#"definitions",
		#"libraries",
		#"breakpoints"))
end method frame-property-types;

define sideways method frame-default-property-type
    (frame :: <environment-frame>, class :: subclass(<project-object>))
 => (type :: false-or(<symbol>))
  #"sources"
end method frame-default-property-type;


/// Project definitions property page

define constant $definition-type-filters
  = #[#["All definitions", #"definitions"],
      #["Classes",         #"classes"],
      #["Constants",       #"constants"],
      #["Domains",         #"domains"],
      #["Functions",       #"functions"],
      #["Macros",          #"macros"],
      #["Methods",         #"methods"],
      #["Variables",       #"variables"]];

define constant $default-definition-type-filter = #"definitions";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<environment-object>),
     type == #"definitions")
 => (label :: <string>, displayer :: <filtering-tree-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<filtering-tree-control-displayer>,
	   element-label: "definition",
	   children-generator: curry(definition-contents, frame),
	   filter-types: $definition-type-filters,
	   filter-type: $default-definition-type-filter,
	   filter-function: curry(filter-definition-contents, frame),
	   label-key: curry(definition-label-key, frame),
	   value-changed-callback: method (displayer, value)
				     ignore(value);
				     note-frame-selection-updated(frame)
				   end,
	   popup-menu-callback: display-environment-popup-menu,
	   items-changed-callback: note-definition-contents-updated);
  values("Definitions", displayer)
end method make-frame-property-page-displayer;

define method note-definition-contents-updated
    (displayer :: <filtering-tree-control-displayer>) => ()
  let frame = sheet-frame(displayer);
  let project = frame.ensure-frame-project;
  //---*** This shouldn't use the internal details like this...
  let state = displayer.displayer-state;
  let root = state.displayer-state-object;
  let modules :: <sequence>
    = select (root by instance?)
	<project-object> =>
	  let library = project-library(root);
	  if (library)
	    tree-control-displayer-children(displayer, library);
	  else
	    #[]
	  end;
	<library-object> =>
	  tree-control-displayer-children(displayer, root);
	<module-object> =>
	  vector(root);
      end;
  let message
    = if (project.project-compiler-database)
	let all-definitions = 0;
	let definitions = 0;
	for (module in modules)
	  let (n, total) = displayer-object-items-count(displayer, module);
	  definitions := definitions + n;
	  all-definitions := all-definitions + total
	end;
	format-to-string("%d %s%s",
			 definitions,
			 string-pluralize("definition", count: definitions),
			 if (all-definitions ~= definitions)
			   format-to-string(" (%d total)",
					    all-definitions)
			 else
			   ""
			 end)
      else
	project-not-built-message(project)
      end;
  frame-status-message(frame) := message
end method note-definition-contents-updated;

define method filter-definition-contents
    (frame :: <environment-frame>, contents :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let no-filter? = empty?(substring-filter);
  local method object-matches-type-filter? 
	    (object :: <environment-object>) => (matches? :: <boolean>)
	  select (type-filter)
	    #"definitions" => #t;
	    #"classes"     => instance?(object, <class-object>);
	    #"constants"   => instance?(object, <constant-object>);
	    #"domains"     => instance?(object, <domain-object>);
	    #"functions"   => instance?(object, <function-object>);
	    #"macros"      => instance?(object, <macro-object>);
	    #"methods"     => instance?(object, <method-object>);
	    #"variables"   => instance?(object, <variable-object>);
	    otherwise      => #f;
	  end
	end method object-matches-type-filter?;
  local method object-matches-substring-filter?
	    (object :: <environment-object>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let label = definition-label-key(frame, object);
		subsequence-position(label, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (object :: <environment-object>) => (show? :: <boolean>)
	  ~instance?(object, <definition-object>)
	    | instance?(object, <module-object>)
	    | instance?(object, <library-object>)
	    | (object-matches-type-filter?(object)
		 & object-matches-substring-filter?(object))
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in contents)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-definition-contents;
  
// Definition labels

define method definition-label-key
    (frame :: <environment-frame>, object :: <environment-object>)
 => (name :: <string>)
  frame-object-unique-name(frame, object)
end method definition-label-key;

// Definition contents

define method definition-contents
    (frame :: <environment-frame>, project :: <project-object>)
 => (contents :: <sequence>)
  let library = project-library(project);
  if (library) vector(library) else #[] end
end method definition-contents;

define method definition-contents
    (frame :: <environment-frame>, library :: <library-object>)
 => (contents :: <sequence>)
  let project = frame.ensure-frame-project;
  frame-order-sequence
    (frame,
     library-modules(project, library, imported?: #f),
     test: \<,
     label-key: curry(frame-default-object-name, frame))
end method definition-contents;

define method definition-contents
    (frame :: <environment-frame>, module :: <module-object>)
 => (contents :: <sequence>)
  let project = frame.ensure-frame-project;
  frame-order-sequence
    (frame,
     module-definitions(project, module, imported?: #f),
     test: \<,
     label-key: curry(frame-default-object-name, frame))
end method definition-contents;

define method definition-contents
    (frame :: <environment-frame>, object :: <environment-object>)
 => (contents :: <sequence>)
  #[]
end method definition-contents;


/// Project libraries page



define constant $library-filters
  = #[#["Used libraries", #"used-libraries"],
      #["All libraries",  #"libraries"]];

define constant $default-library-filter = #"used-libraries";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<project-object>),
     type == #"libraries")
 => (label :: <string>, displayer :: <filtering-table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<filtering-table-control-displayer>,
	   element-label: "library",
	   children-generator: curry(frame-project-libraries, frame),
	   headings: #["Library", "Location"],
	   widths: #[200, 500],
	   generators: vector(identity,
			      curry(project-library-location-name, project)),
	   sort-orders: #[#"library", #"location"],
	   sort-order: #"library",
	   sort-function: curry(frame-sort-project-libraries, frame),
	   filter-types: $library-filters,
	   filter-type: $default-library-filter,
	   filter-function: curry(filter-project-libraries, frame),
	   label-key: method (object)
			select (object by instance?)
			  <library-object> =>
			    frame-default-object-name(frame, object);
			  <string> =>
			    object;
			end
		      end);
  values("Libraries", displayer)
end method make-frame-property-page-displayer;

define method frame-project-libraries
    (frame :: <environment-frame>, project :: <project-object>)
 => (libraries :: <sequence>)
  let library = project-library(project);
  if (~library)
    #[]
  else
    project-used-libraries(project, project)
  end
end method frame-project-libraries;

define method frame-sort-project-libraries
    (frame :: <environment-frame>, libraries :: <sequence>,
     order :: <symbol>)
 => (libraries :: <sequence>)
  let project = frame.ensure-frame-project;
  select (order)
    #"library" =>
      frame-sort-items(frame, libraries);
    #"read-only" =>
      keyed-sort(libraries,
		 key: curry(library-read-only?, project),
		 test: method (ro?-1 :: <boolean>, ro?-2 :: <boolean>)
			 ~ro?-1 & ro?-2
		       end);
    #"location" =>
      keyed-sort(libraries,
		 key: curry(project-library-location-name, project));
  end
end method frame-sort-project-libraries;

define method filter-project-libraries
    (frame :: <environment-frame>, libraries :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let project = frame.ensure-frame-project;
  let no-filter? = empty?(substring-filter);
  let library = project-library(project);
  let used-libraries
    = if (library)
	source-form-used-definitions(project, library)
      else
	#[]
      end;
  local method object-matches-type-filter? 
	    (library :: <library-object>) => (matches? :: <boolean>)
	  select (type-filter)
	    #"libraries"      => #t;
	    #"used-libraries" => 
	      member?(library, used-libraries);
	  end
	end method object-matches-type-filter?;
  local method object-matches-substring-filter?
	    (library :: <library-object>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let name = frame-default-object-name(frame, library);
		subsequence-position(name, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (library :: <library-object>) => (show? :: <boolean>)
	  object-matches-type-filter?(library)
	    & object-matches-substring-filter?(library)
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in libraries)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-project-libraries;
  
define method project-library-location-name
    (project :: <project-object>, library :: <library-object>)
 => (name :: <string>)
  let read-only? = library-read-only?(project, library);
  if (read-only?)
    "[system]"
  else
    let filename = library-project-filename(project, library);
    (filename & as(<string>, filename)) | $n/a
  end
end method project-library-location-name;


/// Project breakpoints property page

/// Breakpoint property page

define method frame-project-breakpoints
    (frame :: <environment-frame>, project :: <project-object>)
 => (breakpoints :: <sequence>)
  let fn-breakpoints = as(<vector>, environment-object-breakpoints(project));
  let sl-breakpoints = as(<vector>, source-location-breakpoints(project));
  concatenate(fn-breakpoints, sl-breakpoints)
end method frame-project-breakpoints;

define method frame-sort-project-breakpoints
    (frame :: <environment-frame>, breakpoints :: <sequence>,
     order :: <symbol>)
 => (breakpoints :: <sequence>)
  local method breakpoint-location<
	    (breakpoint-info1 :: <pair>, breakpoint-info2 :: <pair>)
	 => (less-than? :: <boolean>)
	  let name1 = breakpoint-info1.head;
	  let name2 = breakpoint-info2.head;
	  case
	    name1 = name2 =>
	      breakpoint-info1.tail < breakpoint-info2.tail;
	    ~name1 =>
	      #t;
	    ~name2 =>
	      #f;
	    otherwise =>
	      name1 < name2;
	  end
	end method breakpoint-location<;
  select (order)
    #"location" =>
      keyed-sort(breakpoints, 
		 key: method (breakpoint :: <breakpoint-object>)
			let object = breakpoint-object(breakpoint);
			select (breakpoint by instance?)
			  <environment-object-breakpoint-object> =>
			    pair(#f,
				 frame-default-object-name(frame, object));
			  <source-location-breakpoint-object> =>
			    let record = source-location-source-record(object);
			    let name
			      = select (record by instance?)
				  <source-record> => 
				    record.source-record-name;
				  <file-source-record> =>
				    locator-name(record.source-record-location);
				end;
			    let line
			      = object.source-location-start-offset.source-offset-line;
			    pair(name,
				 line);
			end
		      end,
		 test: breakpoint-location<);
    #"message" =>
      keyed-sort(breakpoints,
		 key: method (breakpoint)
			breakpoint-message?(breakpoint) | ""
		      end method);
    #"library" =>
      let project = frame.ensure-frame-project;
      keyed-sort(breakpoints,
		 key: method (breakpoint)
			let library = environment-object-library(project, breakpoint);
			(library & environment-object-primitive-name(project, library))
			  | ""
		      end method);
  end
end method frame-sort-project-breakpoints;

/// MAKE-BREAKPOINT-TABLE-CONTROL-DISPLAYER (internal)
/// --- hughg: Separated out for use here and elsewhere
/// --- in the "Couldn't set these breakpoints" dialog.

define sideways method make-breakpoint-table-control-displayer
    (frame :: <environment-frame>,
     #rest initargs,
     #key, #all-keys)
 => (displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  apply(make, <table-control-displayer>,
        concatenate
	  (initargs,
	   vector(element-label: "breakpoint",
		  children-generator: curry(frame-project-breakpoints, frame),
		  headings: #["Location", "Library", "Message Text"],
		  widths:   #[300, 70, 400],
		  sort-orders: #[#"location", #"library", #"message"],
		  sort-order: #"location",
		  sort-function: curry(frame-sort-project-breakpoints, frame),
		  generators: vector(identity,
				     curry(environment-object-library, project),
				     method (breakpoint)
				       breakpoint-message?(breakpoint) | ""
				     end method),
		  label-key: curry(breakpoint-object-label, frame)),
	   #[]))
end method make-breakpoint-table-control-displayer;

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<project-object>),
     type == #"breakpoints")
 => (label :: <string>, displayer :: <displayer-mixin>)
  values("Breakpoints", make-breakpoint-table-control-displayer(frame))
end method make-frame-property-page-displayer;

define method breakpoint-object-label
    (frame :: <frame>, object == #f)
 => (label :: <string>)
  ""
end method breakpoint-object-label;

define method breakpoint-object-label 
    (frame :: <frame>, label :: <string>)
 => (label :: <string>)
  label
end method breakpoint-object-label;

define method breakpoint-object-label 
    (frame :: <frame>, breakpoint :: <source-location-breakpoint-object>)
 => (label :: <string>)
  let stream :: <string-stream> = make(<string-stream>, direction: #"output");
  print-source-location(stream, breakpoint.breakpoint-object);
  as(<string>, stream-contents(stream));
end method breakpoint-object-label;

define method breakpoint-object-label
    (frame :: <frame>, breakpoint :: <environment-object-breakpoint-object>)
 => (label :: <string>)
  frame-default-object-name(frame, breakpoint.breakpoint-object)
end method breakpoint-object-label;

define method breakpoint-object-label
    (frame :: <frame>, library :: <library-object>)
 => (label :: <string>)
  let project = frame.frame-current-project;
  environment-object-primitive-name(project, library) | $n/a
end method breakpoint-object-label;

/*---*** andrewa: this seems like the wrong place to be putting this...
define sideways method refresh-frame-property-page
    (frame :: <environment-frame>,
     viewer :: <table-control-displayer>,
     project :: <project-object>,
     type == #"breakpoints",
     #key clean?)
 => ()
  next-method();
  update-gadget(displayer-collection-gadget(viewer)); // NB force update because attributes may have changed
  command-enabled?(frame-create-breakpoint, frame) := #f;
  command-enabled?(frame-run-to-cursor, frame) := #f;
end method refresh-frame-property-page;
*/


/// Project sources tree

//---*** Switch this to #f if we don't want subprojects to appear here.
define constant $show-subprojects = #t;

define method project-sources-contents
    (frame :: <environment-frame>, object)
 => (contents :: <sequence>)
  #[]
end method project-sources-contents;

define method project-sources-contents
    (frame :: <environment-frame>, project :: <project-object>)
 => (contents :: <sequence>)
  if (project == frame.ensure-frame-project | $show-subprojects)
    let sources 
      = map(method (record :: <source-record>)
	      make(<source-record-wrapper>,
		   project: project,
		   object: record)
	    end,
	    project.project-sources);
    let directory = project.project-directory;
    let other-sources :: <sequence>
      = sort(map(method (filename :: <file-locator>)
		   let location = merge-locators(filename, directory);
		   make(<source-locator-wrapper>,
			project: project,
			object: location)
		 end,
		 project-other-sources(project)),
	     test: method 
		       (wrapper1 :: <source-locator-wrapper>,
			wrapper2 :: <source-locator-wrapper>)
		    => (less-than? :: <boolean>)
		     let l1 = wrapper1.wrapper-object;
		     let l2 = wrapper2.wrapper-object;
		     let extension1 = l1.locator-extension | "";
		     let extension2 = l2.locator-extension | "";
		     if (extension1 = extension2)
		       let base1 = l1.locator-base;
		       let base2 = l2.locator-base;
		       base1 < base2
		     else
		       extension1 < extension2
		     end
		   end);
    //--- Might the order ever be useful?
    let used-projects :: <sequence>
      = sort(map(method (source-project :: <project-object>)
		   make(<source-project-wrapper>,
			project: project,
			object: source-project)
		 end,
		 project-used-projects(project)),
	     test: method 
		       (wrapper1 :: <source-project-wrapper>,
			wrapper2 :: <source-project-wrapper>)
		    => (less-than? :: <boolean>)
		     let p1 = wrapper1.wrapper-object;
		     let p2 = wrapper2.wrapper-object;
		     let name1 :: <string>
		       = environment-object-primitive-name(p1, p1);
		     let name2 :: <string>
		       = environment-object-primitive-name(p2, p2);
		     name1 < name2
		   end);
    concatenate(sources, other-sources, used-projects)
  else
    #[]
  end
end method project-sources-contents;

define method project-sources-contents
    (frame :: <environment-frame>, wrapper :: <source-wrapper>)
 => (contents :: <sequence>)
  project-sources-contents(frame, wrapper.wrapper-object)
end method project-sources-contents;

define method project-sources-contents
    (frame :: <environment-frame>, 
     wrapper :: <source-record-wrapper>)
 => (contents :: <sequence>)
  let project = frame.ensure-frame-project;
  let subproject = wrapper.wrapper-project;
  let record = wrapper.wrapper-object;
  let canonical-record = project-canonical-source-record(subproject, record);
  if (canonical-record)
    let top-level-forms
      = source-record-top-level-forms
          (project, canonical-record, project: subproject); 
/*
    frame-order-sequence
      (frame, top-level-forms,
       test: \<,
       label-key: curry(frame-default-object-name, frame))
*/
    top-level-forms
  else
    #[]
  end
end method project-sources-contents;

define method project-sources-contents
    (frame :: <environment-frame>, form :: <macro-call-object>)
 => (contents :: <sequence>)
  let project = frame.ensure-frame-project;
  frame-order-sequence
    (frame,
     macro-call-source-forms(project, form),
     test: \<,
     label-key: curry(frame-default-object-name, frame))
end method project-sources-contents;


// Return the label of a tree node

define method project-sources-label-key 
    (frame :: <environment-frame>, wrapper :: <source-wrapper>)
 => (name :: <string>)
  project-sources-label-key(frame, wrapper.wrapper-object)
end method project-sources-label-key;

define method project-sources-label-key 
    (frame :: <environment-frame>, locator :: <locator>)
 => (name :: <string>)
  locator.locator-name
end method project-sources-label-key;

define method project-sources-label-key 
    (frame :: <environment-frame>, object :: <environment-object>)
 => (name :: <string>)
  frame-object-unique-name(frame, object)
end method project-sources-label-key;

define method project-sources-label-key 
    (frame :: <environment-frame>, record :: <source-record>)
 => (name :: <string>)
  source-record-name(record)
end method project-sources-label-key;

define method project-sources-label-key 
    (frame :: <environment-frame>, object :: <file-source-record>)
 => (name :: <string>)
  //---*** cpage: For now, call locator-name() directly in order to include the
  //              file suffix, since source-record-name() only calls
  //              locator-base(). Perhaps the source record protocol should
  //              support this.
  locator-name(object.source-record-location)

  //---*** cpage: Here's an alternative: Append the source record's module name.
  //              Perhaps we should add a View menu item to toggle this. Or,
  //              perhaps we should have a view that lists module nodes with
  //              source record children.
//  concatenate(locator-name(object.source-record-location), ":",
//              as-lowercase(as(<string>, source-record-module-name(object))))
end method project-sources-label-key;

/*---*** andrewa: I don't believe that this is used...
define method project-sources-label-key 
    (frame :: <environment-frame>, object :: <source-location>)
 => (name :: <string>)
  let line-contents = first-line(as(<string>, copy-source-location-contents(object)));
  line-contents;
end method project-sources-label-key;

define function first-line 
    (str :: <string>) => (line :: <string>)
  let newline? = find-key(str,
			  method (ch)
			    member?(ch, #('\n', '\r'))
			  end method);
  if (newline?)
    copy-sequence(str, end: newline?)
  else
    str
  end if;
end function first-line;
*/


/// Sources page

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<project-object>),
     type == #"sources")
 => (label :: <string>, displayer :: <tree-control-displayer>)
  let displayer
    = make(<tree-control-displayer>,
	   information-available?-function: always(#t),
	   children-generator: curry(project-sources-contents, frame),
	   label-key: curry(project-sources-label-key, frame),
	   always-show-selection?: #t,
	   items-changed-callback: note-project-sources-updated);
  values("Sources", displayer)
end method make-frame-property-page-displayer;


// Update the item count in the status bar

define method note-project-sources-updated
    (displayer :: <tree-control-displayer>) => ()
  let frame = sheet-frame(displayer);
  let project = frame.ensure-frame-project;
  let sources = tree-control-displayer-children(displayer, project);
  let count = size(sources);
  let message
    = format-to-string("%d %s",
		       count,
		       string-pluralize("source", count: count));
  frame-status-message(frame) 
    := project-not-built-message(project, message: message)
end method note-project-sources-updated;
