Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class property pages

define sideways method frame-property-types
    (frame :: <environment-frame>, class :: subclass(<class-object>))
 => (types :: <list>)
  concatenate(next-method(),
              #(#"slots", #"init-keywords", #"class-methods",
                #"subclasses", #"superclasses"))
end method frame-property-types;


/// Class Slots

define sealed class <class-slot-wrapper> (<object-wrapper>)
  sealed constant slot wrapper-attributes :: <string>,
    required-init-keyword: attributes:;
  sealed constant slot wrapper-type :: false-or(<environment-object>),
    required-init-keyword: type:;
  sealed constant slot wrapper-class :: false-or(<class-object>),
    required-init-keyword: class:;
  sealed constant slot wrapper-direct? :: <boolean>,
    required-init-keyword: direct?:;
  sealed constant slot wrapper-local? :: <boolean>,
    required-init-keyword: local?:;
  sealed constant slot wrapper-visible? :: <boolean>,
    required-init-keyword: visible?:;
end class <class-slot-wrapper>;
  
define sealed domain make (singleton(<class-slot-wrapper>));
define sealed domain initialize (<class-slot-wrapper>);

define constant $slot-filters
  = #[#["Direct slots",  #"direct"],
      #["Visible slots", #"visible"],
      #["Local slots",   #"local"],
      #["All slots",     #"all"]];

define constant $default-slot-filter = #"direct";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, class :: subclass(<class-object>),
     type == #"slots")
 => (label :: <string>, displayer :: <filtering-table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<filtering-table-control-displayer>,
	   element-label: "slot",
	   children-generator: curry(frame-class-slots, frame),
	   headings: #["Name", "Attributes", "Type", "Class"],
	   widths:   #[200, 70, 150, 500],
	   generators: vector(wrapper-object,
			      wrapper-attributes,
			      wrapper-type,
			      wrapper-class),
	   sort-orders: #[#"name", #"attributes", #"type", #"class"],
	   sort-order: #"name",
	   sort-function: curry(frame-sort-class-slots, frame),
	   filter-types: $slot-filters,
	   filter-type: $default-slot-filter,
	   filter-function: curry(filter-class-slots, frame),
	   label-key: curry(frame-default-object-name, frame));
  values("Slots", displayer)
end method make-frame-property-page-displayer;

define method frame-class-slots
    (frame :: <environment-frame>, class :: <class-object>)
 => (slots :: <sequence>)
  let project = frame.ensure-frame-project;
  let library = frame.frame-current-library;
  let current-module = frame.frame-current-module;
  let slots = make(<stretchy-vector>);
  let object-class = find-environment-object(project, $<object>-id);
  local method method-visible?
	    (object :: <method-object>) => (visible? :: <boolean>)
	  let gf = method-generic-function(project, object);
	  environment-object-name(project, gf | object, current-module) ~= #f
	end method method-visible?;
  do-all-slots
    (method (slot :: <slot-object>)
       let home-class = slot-class(project, slot);
       let home-library = environment-object-library(project, home-class);
       let direct? = home-class == class;
       let type = slot-type(project, slot);
       let getter = slot-getter(project, slot);
       add!(slots,
	    make(<class-slot-wrapper>,
		 object:     slot,
		 attributes: slot-attributes(project, slot),
		 type:       unless (type == object-class) type end,
		 class:      unless (direct?) home-class end,
		 direct?:    direct?,
		 local?:     home-library == library,
		 visible?:   getter & method-visible?(getter)))
     end,
     project,
     class);
  slots
end method frame-class-slots;

define method frame-sort-class-slots 
    (frame :: <environment-frame>, slots :: <sequence>,
     order :: <symbol>)
 => (slots :: <sequence>)
  let project = frame.ensure-frame-project;
  frame-sort-items
    (frame, slots,
     key: select (order)
	    #"name"       => wrapper-object;
	    #"type"       => wrapper-type;
	    #"attributes" => wrapper-attributes;
	    #"class"      => wrapper-class;
	  end)
end method frame-sort-class-slots;

define method slot-attributes
    (project :: <project-object>, slot :: <slot-object>)
 => (attributes :: <string>)
  let allocation-keys = slot-allocation(project, slot);
  let keys-member? = rcurry(member?, allocation-keys);
  concatenate(case
		keys-member?(#"sealed")        => "S";
		otherwise                      => " ";
	      end,
	      " ",
	      case
		keys-member?(#"class")         => "C";
		keys-member?(#"each-subclass") => "E";
		keys-member?(#"virtual")       => "V";
		otherwise                      => " ";
	      end,
	      " ",
	      case
		keys-member?(#"constant")      => "$";
		otherwise                      => " ";
	      end)
end method slot-attributes;

define method filter-class-slots
    (frame :: <environment-frame>, slots :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (filtered-slots :: <sequence>)
  let project = frame.ensure-frame-project;
  let library = frame.frame-current-library;
  let no-filter? = empty?(substring-filter);
  local method object-matches-type-filter? 
	    (wrapper :: <class-slot-wrapper>) => (matches? :: <boolean>)
	  select (type-filter)
	    #"direct"  => wrapper.wrapper-direct?;
	    #"local"   => wrapper.wrapper-local?;
	    #"visible" => wrapper.wrapper-visible?;
	    #"all"     => #t;
	  end
	end method object-matches-type-filter?;
  local method object-matches-substring-filter?
	    (wrapper :: <class-slot-wrapper>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let slot = wrapper.wrapper-object;
		let name = frame-default-object-name(frame, slot);
		subsequence-position(name, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (wrapper :: <class-slot-wrapper>) => (show? :: <boolean>)
	  object-matches-type-filter?(wrapper)
	    & object-matches-substring-filter?(wrapper)
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in slots)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-class-slots;
  

/// Class methods page

define sealed class <class-method-wrapper> (<object-wrapper>)
  sealed constant slot wrapper-class :: false-or(<class-object>),
    required-init-keyword: class:;
  sealed constant slot wrapper-library :: false-or(<library-object>),
    required-init-keyword: library:;
end class <class-method-wrapper>;
  
define sealed domain make (singleton(<class-method-wrapper>));
define sealed domain initialize (<class-method-wrapper>);

define constant $class-methods-filters
  = #[#["Direct local methods",   #"direct-local"],
      #["Direct visible methods", #"direct-visible"],
      #["Direct methods",         #"direct"],
      #["All local methods",      #"all-local"],
      #["All visible methods",    #"all-visible"],
      #["All methods",            #"all"]];

define constant $default-class-methods-filter = #"direct-local";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<class-object>), 
     type == #"class-methods")
 => (label :: <string>, displayer :: <filtering-table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<filtering-table-control-displayer>,
	   element-label: "method",
	   children-generator: curry(frame-class-methods, frame),
	   filter-types: $class-methods-filters,
	   filter-type: $default-class-methods-filter,
	   filter-function: curry(filter-class-methods, frame),
	   headings: #["Method", "Class", "Library"],
	   widths: #[200, 150, 150],
	   sort-orders: #[#"method", #"class", #"library"],
	   sort-order: #"method",
	   sort-function: curry(frame-sort-class-methods, frame),
	   generators: vector(wrapper-object,
			      wrapper-class,
			      wrapper-library),
	   label-key: curry(frame-default-object-name, frame));
  values("Methods", displayer)
end method make-frame-property-page-displayer;

define method frame-class-methods
    (frame :: <environment-frame>, class :: <class-object>)
 => (methods :: <sequence>)
  let project = frame.ensure-frame-project;
  let library = frame.frame-current-library;
  let methods = make(<stretchy-vector>);
  //--- Unfortunately we always compute all the methods (which can be
  //--- slow) so that we know the grand total. Maybe we can do something
  //--- smarter than this at some point.
  do-all-methods
    (method (direct-class :: <class-object>, object :: <method-object>)
       let method-library = environment-object-library(project, object);
       let maybe-library = if (method-library ~= library) method-library end;
       let maybe-class = if (direct-class ~= class) direct-class end;
       add!(methods,
	    make(<class-method-wrapper>,
		 object:  object,
		 class:   maybe-class,
		 library: maybe-library))
     end,
     project, class);
  methods
end method frame-class-methods;

define method frame-sort-class-methods
    (frame :: <environment-frame>, methods :: <sequence>,
     order :: <symbol>)
 => (names :: <sequence>)
  frame-sort-items(frame, methods,
		   key: select (order)
			  #"method"  => wrapper-object;
			  #"class"   => wrapper-class;
			  #"library" => wrapper-library;
			end)
end method frame-sort-class-methods;

define method filter-class-methods
    (frame :: <environment-frame>, contents :: <sequence>,
     name-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let no-filter? = empty?(substring-filter);
  let project = frame.ensure-frame-project;
  let current-module = frame.frame-current-module;
  local method method-visible?
	    (object :: <method-object>) => (visible? :: <boolean>)
	  let gf = method-generic-function(project, object);
	  environment-object-name(project, gf | object, current-module) ~= #f
	end method method-visible?;
  local method object-matches-name-filter? 
	    (wrapper :: <class-method-wrapper>) => (matches? :: <boolean>)
	  let object = wrapper.wrapper-object;
	  let direct? = ~wrapper.wrapper-class;
	  let local?  = ~wrapper.wrapper-library;
	  select (name-filter)
	    #"direct-local"   => direct? & local?;
	    #"all-local"      => local?;
	    #"direct-visible" => direct? & method-visible?(object);
	    #"all-visible"    => method-visible?(object);
	    #"direct"         => direct?;
	    #"all"            => #t;
          end
	end method object-matches-name-filter?;
  local method object-matches-substring-filter?
	    (wrapper :: <class-method-wrapper>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let object = wrapper.wrapper-object;
		let label = frame-default-object-name(frame, object);
		subsequence-position(label, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (object :: <class-method-wrapper>) => (show? :: <boolean>)
	  object-matches-name-filter?(object)
	    & object-matches-substring-filter?(object)
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in contents)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-class-methods;


/// Init Keywords page

define constant $class-keyword-filters
  = #[#["Direct keywords", #"direct"],
      #["Local keywords",  #"local"],
      #["All keywords",    #"all"]];

define constant $default-class-keyword-filter = #"direct";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<class-object>), 
     type == #"init-keywords")
 => (label :: <string>, displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  let module = frame-current-module(frame);
  let displayer
    = make(<filtering-table-control-displayer>,
	   element-label: "initialization keyword",
	   children-generator: curry(frame-class-init-keywords, frame),
	   filter-types: $class-keyword-filters,
	   filter-type: $default-class-keyword-filter,
	   filter-function: curry(filter-class-init-keywords, frame),
	   headings: #["Keyword", "Type", "Required?", "Definition"],
	   widths: #[150, 150, 70, 300],
	   generators: vector(wrapper-keyword,
			      wrapper-type,
			      wrapper-required?,
			      wrapper-object),
	   sort-orders: #[#"keyword", #"type", #"required", #"definition"],
	   sort-order: #"keyword",
	   sort-function: curry(frame-sort-class-init-keywords, frame),
	   label-key: curry(frame-keyword-label, frame));
  values("Keywords", displayer)
end method make-frame-property-page-displayer;

define sealed class <keyword-wrapper> (<object-wrapper>)
  sealed constant slot wrapper-keyword :: <symbol>,
    required-init-keyword: keyword:;
  sealed constant slot wrapper-type :: false-or(<environment-object>),
    required-init-keyword: type:;
  sealed constant slot wrapper-required? :: <boolean>,
    required-init-keyword: required?:;
  sealed constant slot wrapper-direct? :: <boolean>,
    required-init-keyword: direct?:;
  sealed constant slot wrapper-local? :: <boolean>,
    required-init-keyword: local?:;
end class <keyword-wrapper>;
  
define sealed domain make (singleton(<keyword-wrapper>));
define sealed domain initialize (<keyword-wrapper>);

define method frame-sort-class-init-keywords
    (frame :: <environment-frame>, keywords :: <sequence>,
     order :: <symbol>)
 => (contents :: <sequence>)
  select (order)
    #"keyword" =>
      frame-sort-items(frame, keywords,
		       key: wrapper-keyword,
		       label-key: curry(as, <string>));
    #"definition" =>
      frame-sort-items(frame, keywords, key: wrapper-object);
    #"required" =>
      frame-sort-items(frame, keywords, key: wrapper-required?);
    #"type" =>
      frame-sort-items(frame, keywords, key: wrapper-type);
  end
end method frame-sort-class-init-keywords;

define method frame-class-init-keywords
    (frame :: <environment-frame>, class :: <class-object>)
 => (methods :: <sequence>)
  let project = frame.ensure-frame-project;
  let home-library = environment-object-library(project, class);
  let entries = make(<stretchy-vector>);
  let object-class = find-environment-object(project, $<object>-id);
  do-init-keywords
    (method (definition :: <definition-object>, 
	     keyword :: <symbol>,
	     type :: false-or(<environment-object>),
	     required? :: <boolean>,
	     inherited? :: <boolean>)
       let library = environment-object-library(project, definition);
       let local? = library == home-library;
       add!(entries,
	    make(<keyword-wrapper>, 
		 keyword:    keyword, 
		 object:     definition,
		 type:       unless (type == object-class) type end,
	         required?:  required?,
	         direct?:    ~inherited?,
	         local?:     local?))
     end,
     project,
     class);
  entries
end method frame-class-init-keywords;

define method filter-class-init-keywords
    (frame :: <environment-frame>, contents :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let no-filter? = empty?(substring-filter);
  local method object-matches-type-filter? 
	    (wrapper :: <keyword-wrapper>) => (matches? :: <boolean>)
	  select (type-filter)
	    #"direct" => wrapper.wrapper-direct?;
	    #"local"  => wrapper.wrapper-local?;
	    #"all"    => #t;
	  end
	end method object-matches-type-filter?;
  local method object-matches-substring-filter?
	    (wrapper :: <keyword-wrapper>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let label = frame-keyword-label(frame, wrapper);
		subsequence-position(label, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (wrapper :: <keyword-wrapper>) => (show? :: <boolean>)
	  object-matches-type-filter?(wrapper)
	    & object-matches-substring-filter?(wrapper)
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (wrapper in contents)
    if (show-object?(wrapper))
      add!(results, wrapper)
    end
  end;
  results
end method filter-class-init-keywords;
  
define method frame-keyword-label
    (frame :: <environment-frame>, keyword :: <symbol>)
 => (label :: <string>)
  format-to-string("%s:", keyword)
end method frame-keyword-label;

define method frame-keyword-label
    (frame :: <environment-frame>, object :: <object>)
 => (label :: <string>)
  frame-default-object-name(frame, object)
end method frame-keyword-label;


/// Superclasses

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<class-object>),
     type == #"superclasses")
 => (label :: <string>, displayer :: <tree-control-displayer>)
  let project = frame.ensure-frame-project;
  let module = frame-current-module(frame);
  let displayer
    = make(<tree-control-displayer>,
	   element-label: "class",
	   children-generator: curry(frame-ordered-class-superclasses, frame),
	   label-key: curry(frame-default-object-name, frame));
  values("Superclasses", displayer)
end method make-frame-property-page-displayer;

define method frame-ordered-class-superclasses
    (frame :: <environment-frame>, class :: <class-object>)
 => (superclasses :: <sequence>)
  let project = frame.ensure-frame-project;
  /*---*** It is better to leave this unsorted until it is a user setting,
    ---*** because the CPL is vital information.
  frame-order-sequence
    (frame,
     class-direct-superclasses(project, class),
     test: \<,
     label-key: curry(frame-default-object-name, frame))
  */
  class-direct-superclasses(project, class)
end method frame-ordered-class-superclasses;

//--- It is allowable to have constants or even arbitrary expressions in
//--- a class's hierarchy, so we'll just pretend they have no superclasses.
define method frame-ordered-class-superclasses
    (frame :: <environment-frame>, object :: <environment-object>)
 => (superclasses :: <sequence>)
  #[]
end method frame-ordered-class-superclasses;


/// Subclasses

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<class-object>),
     type == #"subclasses")
 => (label :: <string>, displayer :: <tree-control-displayer>)
  let displayer
    = make(<tree-control-displayer>,
	   element-label: "class",
	   children-generator: curry(frame-ordered-class-subclasses, frame),
	   label-key: curry(frame-default-object-name, frame));
  values("Subclasses", displayer)
end method make-frame-property-page-displayer;

define method frame-ordered-class-subclasses
    (frame :: <environment-frame>, class :: <class-object>)
 => (subclasses :: <sequence>)
  let project = frame.ensure-frame-project;
  frame-order-sequence
    (frame,
     class-direct-subclasses(project, class),
     test: \<,
     label-key: curry(frame-default-object-name, frame))
end method frame-ordered-class-subclasses;

//--- It is allowable to have constants or even arbitrary expressions in
//--- a class's hierarchy, so we'll just pretend they have no subclasses.
define method frame-ordered-class-subclasses
    (frame :: <environment-frame>, object :: <environment-object>)
 => (subclasses :: <sequence>)
  #[]
end method frame-ordered-class-subclasses;
