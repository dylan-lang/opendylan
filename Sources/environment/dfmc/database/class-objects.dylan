Module:    dfmc-environment-database
Synopsis:  DFM compiler database class information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class objects

define sealed method do-direct-subclasses
    (function :: <function>, server :: <dfmc-database>, class :: <class-object>,
     #key client)
 => ()
  let project-object = server.server-project;
  let class-definition :: <class-definition> = compiler-object-proxy(class);
  let context = browsing-context(server, class-definition);
  let direct-subclasses
    = collect-from-all-client-contexts
        (method (context)
	   class-direct-subclass-definitions(context, class-definition)
	 end,
	 server, context);
  do(method (subclass) => ()
       let environment-subclass
         = make-environment-object-for-source-form(project-object, subclass);
       function(environment-subclass)
     end,
     direct-subclasses)
end method do-direct-subclasses;

define sealed method do-direct-superclasses
    (function :: <function>, server :: <dfmc-database>, class :: <class-object>,
     #key client)
 => ()
  let project-object = server.server-project;
  let class-definition :: <class-definition> = class.compiler-object-proxy;
  let context = browsing-context(server, class-definition);
  let direct-superclasses = class-definition.class-definition-superclass-types;
  do(method (superclass-type) => ()
       let type-object
         = select (superclass-type)
	     #t => 
	       $complex-type-expression-object;
	     otherwise =>
	       let superclass
		 = variable-active-definition(context, superclass-type);
	       superclass
		 & make-environment-object-for-source-form
		     (project-object, superclass);
	   end;
       type-object & function(type-object)
     end,
     direct-superclasses)
end method do-direct-superclasses;

define sealed method do-direct-methods
    (function :: <function>, server :: <dfmc-database>, class :: <class-object>,
     #key client)
 => ()
  let project = server-project(server);
  let class-definition :: <class-definition> = compiler-object-proxy(class);
  let context = browsing-context(server, class-definition);
  let direct-methods
    = collect-from-all-client-contexts
        (method (context)
	   class-direct-method-definitions(context, class-definition)
	 end,
	 server, context);
  do(method (method-definition) => ()
       let environment-method
         = make-environment-object(<method-object>,
				   project: project,
				   compiler-object-proxy: method-definition);
       function(environment-method);
     end,
     direct-methods)
end method do-direct-methods;

define sealed method do-class-and-superclasses
    (function :: <function>, server :: <dfmc-database>, 
     class :: <class-object>,
     #key non-classes? = #t, ignore)
 => ()
  local method do-class
	    (class :: <environment-object>)
	  if (class ~== ignore
		& (non-classes?
		     | instance?(class, <class-object>)))
	    function(class)
	  end
	end method do-class;
  do-class(class);
  do-all-superclasses(do-class, server, class)
end method do-class-and-superclasses;

define sealed method do-all-methods
    (function :: <function>, server :: <dfmc-database>, class :: <class-object>,
     #key client)
 => ()
  let project-object = server.server-project;
  local method do-methods
	    (class :: <class-object>) => ()
	  let class-definition :: <class-definition> = class.compiler-object-proxy;
	  let context = browsing-context(server, class-definition);
	  let direct-methods
	    = collect-from-all-client-contexts
	        (method (context)
		   class-direct-method-definitions(context, class-definition)
		 end,
		 server, context);
	  do(method (method-definition) => ()
	       let environment-method
	         = make-environment-object
	             (<method-object>,
		      project: project-object,
		      compiler-object-proxy: method-definition);
	       function(class, environment-method)
	     end,
	     direct-methods)
	end method do-methods;
  let object = find-environment-object(server, $<object>-id);
  //---*** andrewa: this brute force approach won't be too efficient,
  //---*** we could do with a more direct browser-support API.
  do-class-and-superclasses
    (do-methods, server, class,
     non-classes?: #f,
     ignore:       unless (class == object)
		     object
		   end)
end method do-all-methods;

define sealed method do-direct-slots
    (function :: <function>, server :: <dfmc-database>, class :: <class-object>,
     #key client)
 => ()
  let project-object = server.server-project;
  let class-definition = compiler-object-proxy(class);
  let direct-slots = class-definition-slot-definitions(class-definition);
  do(method (slot-definition) => ()
       let environment-slot
         = make-environment-object(<slot-object>,
				   project: project-object,
				   compiler-object-proxy: slot-definition);
       function(environment-slot);
     end,
     direct-slots);
end method do-direct-slots;

define function class-all-superclasses
    (server :: <dfmc-database>, class :: <class-object>)
 => (classes :: <sequence>)
  //---*** andrewa: ultimately we should use this from dfmc-browser-support,
  //---*** but currently it doesn't work in loose mode.
  // class-all-superclasses-definitions(context, class-definition);
  let classes = make(<stretchy-vector>);
  local method do-superclasses
	    (superclass :: <environment-object>)
	  add!(classes, superclass);
	  if (instance?(superclass, <class-object>))
	    do-direct-superclasses(do-superclasses, server, superclass)
	  end
	end method do-superclasses;
  do-direct-superclasses(do-superclasses, server, class);
  remove-duplicates(classes)
end function class-all-superclasses;

define sealed method do-all-superclasses
    (function :: <function>, server :: <dfmc-database>, 
     class :: <class-object>,
     #key client)
 => ()
  do(function, class-all-superclasses(server, class))
end method do-all-superclasses;

define sealed method do-all-slots
    (function :: <function>, server :: <dfmc-database>, class :: <class-object>,
     #key client)
 => ()
  let project-object = server.server-project;
  let class-definition :: <class-definition> = compiler-object-proxy(class);
  let context = browsing-context(server, class-definition);
  let all-slots = class-all-slot-definitions(context, class-definition);
  //---*** cpage: It appears that class-all-slot-definitions() returns #f
  //              if the code has only been parsed, but not compiled. So,
  //              make sure that at least direct slots are returned.
  //              Find out whether this should be implemented like this
  //              permanently.
  if (~all-slots)
    do-class-and-superclasses
      (method (class :: <class-object>)
	 do-direct-slots(function, server, class, client: client)
       end,
       server, class, non-classes?: #f)
  else
    do(method (slot-definition) => ()
	 let environment-slot
	   = make-environment-object(<slot-object>,
				     project: project-object,
				     compiler-object-proxy: slot-definition);
	 function(environment-slot);
       end,
       all-slots);
  end if;
end method do-all-slots;

define constant $initialize-id
  = make(<definition-id>, 
	 name: "initialize",
	 module: $dylan-module-id);

define sealed method all-initialize-methods
    (server :: <dfmc-database>) => (methods :: <sequence>)
  let initialize-function
    = find-environment-object(server.server-project, $initialize-id);
  if (instance?(initialize-function, <generic-function-object>))
    let definition :: <definition> = initialize-function.compiler-object-proxy;
    let variable = definition.source-form-variable;
    if (variable)
      collect-from-all-client-contexts
	(method (context)
	   variable-active-method-definitions(context, variable)
	 end,
	 server, browsing-context(server, definition))
    end
  end
    | #[]
end method all-initialize-methods;

define sealed method initialize-keywords-for-class
    (server :: <dfmc-database>, class :: <class-definition>,
     methods :: <sequence>)
 => (environment-method :: false-or(<method-object>), 
     keywords :: <sequence>, types :: <sequence>)
  block (return)
    for (method-definition :: <method-definition> in methods)
      let context = browsing-context(server, method-definition);
      local method type-definition (type) => (definition)
	      instance?(type, <variable>)
	        & variable-active-definition(context, type)
	  end method type-definition;
      let (req-types, rest-type, next-type, key-types)
	= functional-parameter-types(method-definition);
      ignore(rest-type, next-type);
      let type = ~empty?(req-types) & req-types[0];
      let first-argument = type-definition(type);
      if (first-argument == class)
	let keywords = functional-keys(method-definition);
	// let keywords = map(fragment-value, functional-keys(method-definition));
	let types = map(type-definition, key-types);
	let environment-method
	  = make-environment-object(<method-object>,
				    project: server.server-project,
				    compiler-object-proxy: method-definition);
	return(environment-method, keywords, types)
      end
    end;
    values(#f, #(), #())
  end
end method initialize-keywords-for-class;

define sealed method do-init-keywords
    (function :: <function>, server :: <dfmc-database>, main-class :: <class-object>,
     #key client, inherited? :: <boolean> = #t)
 => ()
  //---*** andrewa: this brute force approach won't be too efficient,
  //---*** we could do with a more direct browser-support API.
  let initialize-methods = all-initialize-methods(server);
  local method do-keyword
	    (definition :: false-or(<definition-object>), keyword :: <symbol>,
	     type, required? :: <boolean>, inherited? :: <boolean>)
	  let type
	    = case
		~type =>
		  #f;
		instance?(type, <source-form>) =>
		  make-environment-object-for-source-form(server, type);
		otherwise =>
		  make-environment-object-for-type-expression(server, type);
	      end;
	  function(definition, keyword, type, required?, inherited?)
	end method do-keyword,

	//--- We find keywords in the following places:
	//---   - the class definition
	//---   - each of the slots of the class
	//---   - the initialize method for the class
        method do-class-keywords (class :: <class-object>)
	  let class-definition = class.compiler-object-proxy;
	  let init-keywords = class-definition-init-keywords(class-definition);
	  let direct-slots = class-definition-slot-definitions(class-definition);
	  let (environment-method, method-keywords, method-keyword-types)
	    = initialize-keywords-for-class
	        (server, class-definition, initialize-methods);
	  let inherited? = class ~== main-class;
	  for (keyword in init-keywords)
	    do-keyword(class, keyword, #f, #f, inherited?)
	  end;
	  for (slot in direct-slots)
	    let (keyword, required?) = slot-definition-keyword(slot);
	    if (keyword)
	      let type = slot-definition-type(slot);
	      do-keyword(class, keyword, type, required?, inherited?)
	    end
	  end;
	  for (keyword in method-keywords,
	       type in method-keyword-types)
	    do-keyword(environment-method, keyword, type, #f, inherited?)
	  end;
	end method do-class-keywords;

  if (inherited?)
    do-class-and-superclasses(do-class-keywords, server, main-class, non-classes?: #f)
  else
    do-class-keywords(main-class)
  end
end method do-init-keywords;

define sealed method application-object-class
    (server :: <dfmc-database>, object :: <application-object>)
 => (class :: false-or(<class-object>))
  #f
end method application-object-class;

define sealed method application-object-class
    (server :: <dfmc-database>, object :: <class-object>)
 => (class :: false-or(<class-object>))
  find-environment-object(server, $<class>-id)
end method application-object-class;

define sealed method application-object-class
    (server :: <dfmc-database>, object :: <method-object>)
 => (class :: false-or(<class-object>))
  find-environment-object(server, $<method>-id)
end method application-object-class;

define sealed method application-object-class
    (server :: <dfmc-database>, object :: <generic-function-object>)
 => (class :: false-or(<class-object>))
  find-environment-object(server, $<generic-function>-id)
end method application-object-class;


/// Macro calls

//--- A class expands into its getters and setters
define sealed method do-macro-call-source-forms
    (function :: <function>, server :: <dfmc-database>, 
     class :: <class-object>)
 => ()
  do-direct-slots
    (method (slot :: <slot-object>)
       let getter = slot-getter(server, slot);
       let setter = slot-setter(server, slot);
       getter & function(getter);
       setter & function(setter);
     end,
     server, class)
end method do-macro-call-source-forms;
