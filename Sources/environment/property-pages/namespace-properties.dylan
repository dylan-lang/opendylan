Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Namespace property pages

define sideways method frame-property-types
    (frame :: <environment-frame>, 
     class :: subclass(<namespace-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"definitions"))
end method frame-property-types;

define sideways method frame-property-types
    (frame :: <environment-frame>, 
     class :: subclass(<module-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"module-names"))
end method frame-property-types;

define sideways method frame-property-types
    (frame :: <environment-frame>, 
     library :: subclass(<library-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"library-names"))
end method frame-property-types;

/*---*** This page is too slow to show for now
define sideways method frame-default-property-type
    (frame :: <environment-frame>, 
     class :: subclass(<library-object>))
 => (type :: false-or(<symbol>))
  #"library-names"
end method frame-default-property-type;

define sideways method frame-default-property-type
    (frame :: <environment-frame>, 
     class :: subclass(<module-object>))
 => (type :: false-or(<symbol>))
  #"module-names"
end method frame-default-property-type;
*/


/// Name wrappers

define sealed class <name-wrapper> (<object-wrapper>)
  sealed constant slot wrapper-name :: <string>,
    required-init-keyword: name:;
  sealed constant slot wrapper-home-name :: false-or(<name-object>),
    required-init-keyword: home-name:;
  sealed constant slot wrapper-home-namespace :: false-or(<namespace-object>),
    required-init-keyword: home-namespace:;
  sealed constant slot wrapper-exported? :: <boolean>,
    required-init-keyword: exported?:;
end class <name-wrapper>;
  
define sealed domain make (singleton(<name-wrapper>));
define sealed domain initialize (<name-wrapper>);


/// Names page

define constant $name-filters
  = #[#["Exported names",       #"exports"],
      #["Imported names",       #"imports"],
      #["Local exported names", #"local-exports"],
      #["Local names",          #"locals"],
      #["All names",            #"names"]];

define constant $default-name-filter         = #"exports";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<library-object>),
     type == #"library-names")
 => (label :: <string>, displayer :: <filtering-table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<filtering-table-control-displayer>,
	   element-label: "name",
	   children-generator: curry(frame-namespace-names, frame),
	   filter-text-label: "containing",
	   filter-types: $name-filters,
	   filter-type: $default-name-filter,
	   filter-function: method (names, type-filter, substring-filter)
			      filter-namespace-names
				(frame, names, 
				 type-filter, #f, substring-filter)
			    end,
	   headings: #["Name", "Exported", "Original name", "Library"],
	   widths: #[150, 60, 150, 200],
	   sort-orders: #[#"name", #"exported", #"original", #"namespace"],
	   sort-order: #"name",
	   sort-function: curry(frame-sort-namespace-names, frame),
	   generators: vector(wrapper-name,
			      wrapper-exported?,
			      wrapper-home-name,
			      wrapper-home-namespace),
	   label-key: curry(namespace-name-label-key, frame));
  values("Names", displayer)
end method make-frame-property-page-displayer;

define constant $binding-filters
  = #[#["Exported",       #"exports"],
      #["Imported",       #"imports"],
      #["Local exported", #"local-exports"],
      #["Local",          #"locals"],
      #["All",            #"names"]];

define constant $binding-type-filters
  = #[#["names",          #"names"],
      #["class names",    #"classes"],
      #["constant names", #"constants"],
      #["function names", #"functions"],
      #["macro names",    #"macros"],
      #["unbound names",  #"unbound"],
      #["variable names", #"variables"]];

define constant $default-binding-type-filter = #"names";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<module-object>),
     type == #"module-names")
 => (label :: <string>, displayer :: <filtering-table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<filtering-table-control-displayer>,
	   element-label: "name",
	   children-generator: curry(frame-namespace-names, frame),
	   filter-text-label: "containing",
	   filter-types: $binding-filters,
	   filter-type: $default-name-filter,
	   filter-extra-types: $binding-type-filters,
	   filter-extra-type: $default-binding-type-filter,
	   filter-function: curry(filter-namespace-names, frame),
	   headings: #["Name", "Exported", "Original name", "Module"],
	   widths: #[150, 60, 150, 200],
	   sort-orders: #[#"name", #"exported", #"original", #"namespace"],
	   sort-order: #"name",
	   sort-function: curry(frame-sort-namespace-names, frame),
	   generators: vector(wrapper-name,
			      wrapper-exported?,
			      wrapper-home-name,
			      wrapper-home-namespace),
	   label-key: curry(namespace-name-label-key, frame));
  values("Names", displayer)
end method make-frame-property-page-displayer;

define method frame-sort-namespace-names
    (frame :: <environment-frame>, names :: <sequence>,
     order :: <symbol>)
 => (names :: <sequence>)
  let project = frame.ensure-frame-project;
  select (order)
    #"name" =>
      frame-sort-items
	(frame, names,
	 key: wrapper-name,
	 label-key: curry(as, <string>));
    #"exported" =>
      keyed-sort(names,
		 key: wrapper-exported?,
		 test: method (exported?-1 :: <boolean>,
			       exported?-2 :: <boolean>)
			 exported?-1 & ~exported?-2
		       end);
    #"original" =>
      frame-sort-items(frame, names, key: wrapper-home-name);
    #"namespace" =>
      frame-sort-items(frame, names, key: wrapper-home-namespace);
  end
end method frame-sort-namespace-names;

define method frame-namespace-names
    (frame :: <environment-frame>, 
     namespace :: <namespace-object>)
 => (names :: <sequence>)
  let project = frame.ensure-frame-project;
  let names = make(<stretchy-vector>);
  do-namespace-names
    (method (name :: <name-object>)
       //---*** We really should get the actual namespace passed in here...
       let namespace = namespace;
       let value = name-value(project, name);
       let home-name = value & environment-object-home-name(project, value);
       let home-namespace = home-name & name-namespace(project, home-name);
       let same-name?
	 = home-name
	     & (environment-object-primitive-name(project, name)
		  = environment-object-primitive-name(project, home-name));
       let local? = ~home-name | home-namespace == namespace;
       add!(names,
	    make(<name-wrapper>,
		 object: value,
		 name: environment-object-primitive-name(project, name),
		 exported?: name-exported?(project, name),
		 home-name: unless (same-name?) home-name end,
		 home-namespace: unless (local?) home-namespace end))
     end,
     project, namespace, imported?: #t);
  names
end method frame-namespace-names;

define method namespace-name-label-key
    (frame :: <environment-frame>, name :: <name-object>)
 => (label :: <string>)
  let project = frame.ensure-frame-project;
  environment-object-primitive-name(project, name)
end method namespace-name-label-key;

define method namespace-name-label-key
    (frame :: <environment-frame>, object :: <object>)
 => (label :: <string>)
  frame-default-object-name(frame, object)
end method namespace-name-label-key;

define method filter-namespace-names
    (frame :: <environment-frame>, names :: <sequence>,
     name-filter :: <symbol>, name-type-filter :: false-or(<symbol>),
     substring-filter :: <string>)
 => (names :: <sequence>)
  let no-filter? = empty?(substring-filter);
  local method object-matches-name-filter? 
	    (wrapper :: <name-wrapper>) => (matches? :: <boolean>)
	  let local? = ~wrapper.wrapper-home-namespace;
	  let exported? = wrapper.wrapper-exported?;
	  select (name-filter)
	    #"local-exports" => exported? & local?;
	    #"exports"       => exported?;
	    #"imports"       => ~local?;
	    #"locals"        => local?;
	    #"names"         => #t;
          end
	end method object-matches-name-filter?;
  local method object-matches-name-type-filter?
	    (wrapper :: <name-wrapper>) => (matches? :: <boolean>)
	  let object = wrapper.wrapper-object;
	  select (name-type-filter)
	    #"names"       => #t;
	    #"classes"     => instance?(object, <class-object>);
	    #"constants"   => instance?(object, <constant-object>);
	    #"functions"   => instance?(object, <function-object>);
	    #"macros"      => instance?(object, <macro-object>);
	    #"unbound"     => ~object;
	    #"variables"   => instance?(object, <variable-object>);
	    #f             => #t;
          end
	end method object-matches-name-type-filter?;
  local method object-matches-substring-filter?
	    (wrapper :: <name-wrapper>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let name = wrapper.wrapper-name;
		let label = namespace-name-label-key(frame, name);
		subsequence-position(label, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (object :: <name-wrapper>) => (show? :: <boolean>)
	  object-matches-name-filter?(object)
	    & object-matches-name-type-filter?(object)
	    & object-matches-substring-filter?(object)
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in names)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-namespace-names;

