Module:    dfmc-environment-database
Synopsis:  DFM compiler definition information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Source form objects

define sealed method source-form-proxy
    (source-form :: <source-form-object>) => (definition :: <source-form>)
  source-form.compiler-object-proxy
end method source-form-proxy;

define sealed method source-form-proxy
    (library :: <library-object>) => (definition :: <library-definition>)
  library.library-definition
end method source-form-proxy;

define method source-form-variable
    (source-form :: <source-form>) => (variable)
  let variables = source-form.source-form-defined-variables;
  /*---*** andrewa: it is better to not blow up for now...
  debug-assert(instance?(source-form, <slot-definition>)
		 | size(variables) <= 1,
	       "More than one variable defined for %=: %=",
	       source-form, map(variable-name, variables));
  */
  ~empty?(variables) & variables[0]
end method source-form-variable;

//---*** Do we really need to recurse here?
define function find-source-form-location
    (source-form :: <source-form>) => (location :: false-or(<source-location>))
  source-form-location(source-form)
    | begin
	let parent-form = source-form-parent-form(source-form);
	parent-form & find-source-form-location(parent-form)
      end
end function find-source-form-location;

define sealed method environment-object-source-location
    (server :: <dfmc-database>, object :: <source-form-object>)
 => (location :: false-or(<source-location>))
  let source-form = object.source-form-proxy;
  source-form & find-source-form-location(source-form)
end method environment-object-source-location;

define sealed method source-form-uses-definitions?
    (server :: <dfmc-database>, object :: <source-form-object>,
     #key modules, libraries, client)
 => (uses-definitions? :: <boolean>)
  ignore(modules, libraries, client);
  let source-form :: <source-form> = object.source-form-proxy;
  let context = browsing-context(server, source-form);
  let variables = source-form-referenced-variables(context, source-form);
  variables & ~empty?(variables)
end method source-form-uses-definitions?;

define sealed method do-used-definitions
    (function :: <function>, server :: <dfmc-database>,
     object :: <source-form-object>,
     #key modules, libraries, client)
 => ()
  ignore(modules, libraries, client);
  let source-form :: <source-form> = object.source-form-proxy;
  let context = browsing-context(server, source-form);
  let variables = source-form-referenced-variables(context, source-form);
  when (variables)
    local method do-variable (variable :: <variable>) => ()
            let definition = variable-active-definition(context, variable);
	    unless (definition == source-form)
	      let environment-definition
		= make-environment-object-for-source-form(server, definition);
	      function(environment-definition)
	    end
          end method;
    do(do-variable, variables);
  end when;
end method do-used-definitions;

define sealed method source-form-has-clients?
    (server :: <dfmc-database>, source-form :: <source-form-object>,
     #key modules, libraries, client)
 => (has-clients? :: <boolean>)
  #f
end method source-form-has-clients?;

define sealed method do-client-source-forms
    (function :: <function>, server :: <dfmc-database>,
     source-form :: <source-form-object>,
     #key modules, libraries, client)
 => ()
  #f
end method do-client-source-forms;

define sealed method source-form-has-clients?
    (server :: <dfmc-database>, definition :: <definition-object>,
     #key modules, libraries, client)
 => (has-clients? :: <boolean>)
  ignore(modules, libraries, client);
  let source-form :: <source-form> = definition.source-form-proxy;
  let context = browsing-context(server, source-form);
  let variable = source-form.source-form-variable;
  if (variable)
    any-results-from-all-client-contexts?
      (method (context)
	 variable-referencing-forms(context, variable)
       end,
       server, context)
  end
end method source-form-has-clients?;

define sealed method do-client-source-forms
    (function :: <function>, server :: <dfmc-database>,
     definition :: <definition-object>,
     #key modules, libraries, client)
 => ()
  ignore(modules, libraries, client);
  let source-form :: <source-form> = definition.source-form-proxy;
  let context = browsing-context(server, source-form);
  let variable = source-form.source-form-variable;
  if (variable)
    let clients
      = collect-from-all-client-contexts
          (method (context)
	     variable-referencing-forms(context, variable)
	   end,
	   server, context);
    local method do-source-form (client :: <source-form>) => ()
	    unless (client == source-form)
	      let environment-object
		= make-environment-object-for-source-form(server, client);
	      function(environment-object)
	    end
	  end method;
    do(do-source-form, clients)
  end
end method do-client-source-forms;


/// Primitive name for top level expressions

define sealed method get-environment-object-primitive-name
    (server :: <dfmc-database>, object :: <top-level-expression-object>)
 => (name :: false-or(<string>))
  let source-form = object.compiler-object-proxy;
  unless (source-form-parent-form(source-form))
    let location = environment-object-source-location(server, object);
    location & source-location-first-line(location)
  end
end method get-environment-object-primitive-name;

define function source-location-first-line
    (location :: <source-location>) => (line :: false-or(<string>))
  block ()
    let text = copy-source-location-contents(location);
    first-line(as(<byte-string>, text))
  exception (<source-record-missing>)
    #f;
  end
end function source-location-first-line;


/// Macro calls

define sealed method do-macro-call-source-forms
    (function :: <function>, server :: <dfmc-database>, 
     object :: <macro-call-object>)
 => ()
  #f
end method do-macro-call-source-forms;

define sealed method do-macro-call-source-forms
    (function :: <function>, server :: <dfmc-database>, 
     object :: <simple-macro-call-object>)
 => ()
  let source-form = object.compiler-object-proxy;
  let forms = source-form.macro-form-expanded-forms;
  for (form :: <source-form> in forms)
    let object = make-environment-object-for-source-form(server, form);
    function(object)
  end
end method do-macro-call-source-forms;


/// Environment object library

define sealed method environment-object-library
    (server :: <dfmc-database>, object :: <source-form-object>)
 => (library :: false-or(<library-object>))
  let source-form :: <source-form> = object.source-form-proxy;
  let context = browsing-context(server, source-form);
  let project = context.compilation-context-project;
  project
    & make-environment-object(<library-object>,
			      project: server.server-project,
			      compiler-object-proxy: project)
end method environment-object-library;


/// ID handling

define sealed method find-compiler-database-proxy
    (server :: <dfmc-database>, id :: <object-location-id>,
     #key imported? = #f)
 => (source-form :: <source-form>)
  error("Not yet implemented, use <library-object-location-id> if you can!")
end method find-compiler-database-proxy;

define sealed method find-compiler-database-proxy
    (server :: <dfmc-database>, id :: <library-object-location-id>,
     #key imported? = #f)
 => (source-form :: false-or(<source-form>))
  ignore(imported?);
  let library-id = id.id-library;
  let project = find-compiler-database-proxy(server, library-id);
  if (project)
    let locator = as(<file-locator>, id.id-filename);
    let line-number = id.id-line-number;
    let record :: false-or(<source-record>)
      = block (return)
	  let records = project.project-canonical-source-records;
	  for (record :: <source-record> in records)
	    if (record.source-record-location = locator)
	      return(record)
	    end
	  end
	end;
    record 
      & source-record-source-form(server, project, record, line-number)
  end
end method find-compiler-database-proxy;

define sealed method compiler-database-definition-id
    (server :: <dfmc-database>, definition :: <definition>)
 => (id :: false-or(<definition-id>))
  //---*** Is there an easier way to do this select?
  select (definition by instance?)
    <class-definition>, <macro-definition>,
    <generic-definition>, <constant-method-definition>,
    <constant-definition>, <variable-definition> =>
      let (name, module-definition)
	= definition-home-name-and-module(server, definition);
      if (name & module-definition)
	let module-id
	  = compiler-database-proxy-id(server, module-definition);
	make(<definition-id>, name: name, module: module-id)
      end;
    otherwise =>
      #f
  end
end method compiler-database-definition-id;

define sealed method compiler-database-proxy-id
    (server :: <dfmc-database>, source-form :: <source-form>)
 => (id :: false-or(<id>))
  compiler-database-definition-id(server, source-form)
  /*---*** andrewa: this isn't a good idea for now, because
    ---*** source locations aren't unique.
    | begin
	let location = find-source-form-location(source-form);
	let record = source-location-source-record(location);
	if (instance?(record, <file-source-record>))
	  let locator = record.source-record-location;
	  let context = browsing-context(server, source-form);
	  let project = context.compilation-context-project;
	  let library-id = compiler-database-proxy-id(server, project);
	  let offset = location.source-location-start-offset;
	  let line = offset.source-offset-line + record.source-record-start-line;
	  make(<library-object-location-id>,
	       library: library-id,
	       filename: as(<string>, locator),
	       line-number: line)
	end
      end
  */
end method compiler-database-proxy-id;

define sealed method source-record-source-form
    (server :: <dfmc-database>, project :: <project>,
     record :: <source-record>, line :: <integer>)
 => (object :: false-or(<source-form>))
  let context = browsing-context(server, project);
  block (return)
    let forms = dfmc/source-record-top-level-forms(context, record);
    for (form :: <source-form> in forms)
      let form-location = find-source-form-location(form);
      let form-start :: <integer>
	= form-location.source-location-start-line;
      let form-end :: <integer>
	= form-location.source-location-end-line;
      if (form-start <= line & line <= form-end)
	return(form)
      end;
    end;
    #f
  end
end method source-record-source-form;

define sealed method source-record-environment-object
    (server :: <dfmc-database>, project :: <project>,
     record :: <source-record>, line :: <integer>)
 => (object :: false-or(<source-form-object>))
  let form = source-record-source-form(server, project, record, line);
  if (form)
    make-environment-object-for-source-form(server.server-project, form)
  end
end method source-record-environment-object;
