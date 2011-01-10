Module:    dfmc-environment-database
Synopsis:  DFM compiler function information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Function objects

// The type definition for <object>, to be used as the specializer of
// parameters without explicit specializers.
define constant $<object> = make-variable(#"<object>", #"dylan");

define sealed method function-parameters
    (server :: <dfmc-database>, function :: <dylan-function-object>)
 => (required :: <parameters>,
     rest :: false-or(<parameter>),
     keys :: <optional-parameters>,
     all-keys? :: <boolean>,
     next :: false-or(<parameter>),
     values :: <parameters>,
     rest-value :: false-or(<parameter>))
  let function-definition :: false-or(<definition>)
    = compiler-object-proxy(function);
  if (function-definition)
    let project = server-project(server);
    let (req-vars, rest-var, next-var, key-vars, value-vars, rest-value-var)
      = functional-parameters(function-definition);
    let (req-types, rest-type, next-type, key-types, value-types, rest-value-type)
      = functional-parameter-types(function-definition);
    let (keys, all-keys?) = functional-keys(function-definition);
    if (#f) //---*** TESTING
      debug-message("req-vars:  %=\n           req-types: %=\n           "
		    "rest-var:  %=\n           rest-type: %=\n           "
		    "next-var:  %=\n           next-type: %=\n           "
		    "key-vars:  %=\n           key-types: %=\n           "
		    "keys:      %=\n           all-keys?: %=\n           "
		    "value-vars:  %=\n           value-types: %=\n           "
		    "rest-value-var:  %=\n           rest-value-type: %=",
		    req-vars, req-types, rest-var, rest-type,
		    next-var, next-type, key-vars, key-types,
		    keys, all-keys?,
		    value-vars, value-types, rest-value-var, rest-value-type);
    end; //---*** TESTING
    local method make-parameter
              (var, type :: <type-expression>)
           => (parameter :: <parameter>)
	    let name = name-to-string(var.variable-name);
	    make(<parameter>,
		 name: name-to-string(var.variable-name),
		 type: make-environment-object-for-type-expression(server, type))
	  end method;
    local method make-optional-parameter
	      //---*** Currently, it doesn't look like there's a way to get default
	      //       values from dfmc-browser-support, so let's force it to #f.
	      (var, type :: <type-expression>, key :: <symbol>) // ... default-value)
	   => (parameter :: <optional-parameter>)
            let name = var.variable-name;
	    make(<optional-parameter>,
		 name: name-to-string(name),
		 type: make-environment-object-for-type-expression(server, type),
		 keyword: unless (key == name)
			    name-to-string(key)
			  end,
		 default-value: $false-object
				/* make-environment-object
				  (<environment-object>,
				   project: project,
				   compiler-object-proxy: default-value) */ )
	  end method;
    let env-required
      = req-vars
         & map(make-parameter,
	       req-vars,
	       req-types
		 | make(<simple-vector>, size: size(req-vars), fill: $<object>));
    let env-rest
      = rest-var
	 & make-parameter(rest-var, rest-type | $<object>);
    let env-keys
      = key-vars
	 & map(make-optional-parameter,
	       key-vars,
	       key-types
		| make(<simple-vector>, size: size(key-vars), fill: $<object>),
	       keys);
    let env-next
      = next-var
	 & make-parameter(next-var, next-type | $<object>);
    let env-values
      = value-vars
	 & map(make-parameter,
	       value-vars,
	       value-types
		| make(<simple-vector>, size: size(value-vars), fill: $<object>));
    let env-rest-value
      = rest-value-var
	 & make-parameter(rest-value-var, rest-value-type | $<object>);
    values(env-required | #(), env-rest, env-keys | #(), all-keys?, 
	   env-next, env-values | #(), env-rest-value)
  else
    values(#(), #f, #(), #f, #f, #(), #f)
  end
end method function-parameters;

define sealed method do-generic-function-methods
    (function :: <function>, server :: <dfmc-database>,
     generic-function :: <generic-function-object>,
     #key client)
 => ()
  let definition :: false-or(<generic-definition>)
    = generic-function.compiler-object-proxy;
  if (definition)
    let project-object = server.server-project;
    do-generic-definition-methods
      (method (method-definition :: <method-definition>) => ()
	 let environment-method
	   = make-environment-object(<method-object>,
				     project: project-object,
				     compiler-object-proxy: method-definition);
	 function(environment-method)
       end,
       server, definition)
  end
end method do-generic-function-methods;

define method do-generic-definition-methods
    (function :: <function>, server :: <dfmc-database>, 
     definition :: <generic-definition>,
     #key context :: false-or(<context>))
 => ()
  let variable = definition.source-form-variable;
  if (variable)
    let context = context | browsing-context(server, definition);
    let method-definitions
      = collect-from-all-client-contexts
          (method (context :: <context>)
	     variable-active-method-definitions(context, variable)
	   end,
	   server, context);
    do(function, method-definitions)
  end
end method do-generic-definition-methods;

define sealed method method-specializers
    (server :: <dfmc-database>, object :: <method-object>)
 => (specializers :: <sequence>)
  let specializers = make(<stretchy-vector>);
  let definition = object.compiler-object-proxy;
  let context = browsing-context(server, definition);

  let req-types //...rest-type, next-type, key-types, value-types, rest-value-type)
    = functional-parameter-types(definition);
  if (req-types)
    for (type in req-types)
      add!(specializers, make-environment-object-for-type-expression(server, type));
    end
  else
    let req-vars //...rest-var, next-var, key-vars, value-vars, rest-value-var)
      = functional-parameters(definition);
    let object-class-definition = find-<object>(server);
    for (i from 0 below size(req-vars))
      add!(specializers, object-class-definition)
    end
  end;
  specializers
end method method-specializers;

// Minimize the number of contexts we search, since the method can only
// be defined in a context where both the gf and the specializers
// are available. Note that we only check the first specializer.
define sealed method find-method-with-specializers
    (server :: <dfmc-database>, definition :: <generic-definition>,
     specializers :: <sequence>)
 => (method-definition :: false-or(<method-definition>))
  let class-definition :: false-or(<class-definition>)
    = ~empty?(specializers) & specializers[0];
  let gf-context = browsing-context(server, definition);
  let context :: <context>
    = if (class-definition)
	let class-context = browsing-context(server, class-definition);
	more-specific-context(class-context, gf-context) | gf-context
      else
	gf-context
      end;
  block (return)
    do-generic-definition-methods
      (method (method-definition :: <method-definition>)
	 let method-specializers 
	   = method-definition-specializers(server, method-definition);
	 if (method-specializers = specializers)
	   return(method-definition)
	 end
       end,
       server, definition,
       context: context);
    #f
  end
end method find-method-with-specializers;

define sealed method method-generic-function
    (server :: <dfmc-database>, object :: <method-object>) 
 => (function :: false-or(<generic-function-object>))
  let method-definition = object.compiler-object-proxy;
  let context = browsing-context(server, method-definition);
  let definition
    = method-definition-generic-definition(context, method-definition);
  definition
    & make-environment-object(<generic-function-object>,
			      project: server.server-project,
			      compiler-object-proxy: definition)
end method method-generic-function;


/// Definition level functions
//
// Note: we use <definition> as the discriminator as there is no other
// common superclass of both <method-definition> and <constant-method-definition>.

define sealed method do-method-definition-specializers
    (function :: <function>, server :: <dfmc-database>, definition :: <definition>)
 => ()
  let context = browsing-context(server, definition);
  let req-types //...rest-type, next-type, key-types, value-types, rest-value-type)
    = functional-parameter-types(definition);
  if (req-types)
    for (type in req-types)
      let definition = type-expression-to-definition(server, type);
      function(definition)
    end
  else
    let req-vars //...rest-var, next-var, key-vars, value-vars, rest-value-var)
      = functional-parameters(definition);
    let object-class-definition = find-<object>(server);
    for (i from 0 below size(req-vars))
      function(object-class-definition)
    end
  end
end method do-method-definition-specializers;

define method method-definition-specializers
    (server :: <dfmc-database>, definition :: <definition>)
 => (specializers :: <stretchy-vector>)
  let specializers = make(<stretchy-vector>);
  do-method-definition-specializers
    (method (type :: false-or(<definition>))
       add!(specializers, type)
     end,
     server, definition);
  specializers
end method method-definition-specializers;

define sealed method method-definition-generic-definition
    (context :: <context>, method-definition :: <definition>)
 => (function :: false-or(<generic-definition>))
  let variable = method-definition.source-form-variable;
  let generic-definition
    = variable & variable-active-definition(context, variable);
  if (instance?(generic-definition, <generic-definition>))
    generic-definition
  end
end method method-definition-generic-definition;


/// ID handling

define sealed method find-compiler-database-proxy
    (server :: <dfmc-database>, id :: <method-id>, #key imported? = #f)
 => (definition :: false-or(<method-definition>))
  ignore(imported?);
  let function-id = id.id-generic-function;
  let specializer-ids = id.id-specializers;
  let definition :: false-or(<definition>)
    = find-compiler-database-proxy(server, function-id, imported?: imported?);
  if (instance?(definition, <generic-definition>))
    block (return)
      let specializers
	= map(method (id :: <definition-id>)
		let definition
		  = find-compiler-database-proxy(server, id, imported?: #t);
		// Note that we currently can only find methods that have
		// exclusively classes as their specializers. This is because
		// the compiler doesn't provide us a model of type-unions etc.
		unless (instance?(definition, <class-definition>))
		  return(#f)
		end;
		definition
	      end,
	      specializer-ids);
      find-method-with-specializers(server, definition, specializers)
    end
  end
end method find-compiler-database-proxy;

define sealed method compiler-database-proxy-id
    (server :: <dfmc-database>, definition :: <method-definition>)
 => (id :: false-or(<id>))
  let context = browsing-context(server, definition);
  let gf = method-definition-generic-definition(context, definition);
  let function-id = gf & compiler-database-proxy-id(server, gf);
  if (function-id)
    block (return)
      let specializer-ids = make(<stretchy-vector>);
      do-method-definition-specializers
	(method (definition :: false-or(<definition>))
	   let id
	     = instance?(definition, <class-definition>)
	         & compiler-database-proxy-id(server, definition);
	   if (id)
	     add!(specializer-ids, id)
	   else
	     return(#f)
	   end
	 end,
	 server, definition);
      make(<method-id>,
	   generic-function: function-id,
	   specializers: as(<simple-object-vector>, specializer-ids))
    end
  end
    | next-method()
end method compiler-database-proxy-id;

define sealed method internal-dylan-method?
    (server :: <dfmc-database>, definition :: <definition>)
 => (internal? :: <boolean>)
  let context = browsing-context(server, definition);
  let variable = definition.source-form-variable;
  if (variable)
    let home-variable = variable-home(context, variable);
    let (var-name, module-name) = variable-name(home-variable);
    module-name == as(<symbol>, $dispatch-engine-module-id.id-name)
  else
    let gf = method-definition-generic-definition(context, definition);
    let (name, module) = gf & definition-home-name-and-module(server, gf);
    if (module)
      compiler-database-proxy-id(server, module) == $dispatch-engine-module-id
    end
  end
end method internal-dylan-method?;
