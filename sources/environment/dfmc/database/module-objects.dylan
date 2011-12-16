Module:    dfmc-environment-database
Synopsis:  DFM compiler module information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Module object

//---*** cpage: Currently, there is no <module-definition> object associated with
//              the module "dylan-user".

// Return the primitive name of a module
define sealed method get-environment-object-primitive-name
    (server :: <dfmc-database>, module :: <module-object>)
 => (name :: false-or(<string>))
  name-to-string(module.compiler-object-proxy.module-definition-name)
end method get-environment-object-primitive-name;

define sealed method module-project-proxy
    (server :: <dfmc-database>, module :: <module-object>)
 => (project :: <project>)
  let definition :: <module-definition> = module.compiler-object-proxy;
  let context = browsing-context(server, definition);
  context.compilation-context-project
end method module-project-proxy;

// Search for a module by name throughout all libraries.
// Note: this is a brute force search that returns just the first match,
// so generally it is better to find the module directly using a context.
define sealed method search-for-module-definition
    (server :: <dfmc-database>, module-name :: <symbol>)
 => (definition :: false-or(<module-definition>))
  let project-object = server.server-project;
  block (return)
    local method maybe-return-module (project :: <project>)
	    let context = browsing-context(server, project);
	    let definition = find-module-definition(context, module-name);
	    definition & return(definition)
	  end method maybe-return-module;
    do-all-projects(maybe-return-module, server)
  end
end method search-for-module-definition;

// Search for a module by name in a library
define sealed method find-module
    (server :: <dfmc-database>, name :: <string>, 
     #key library :: false-or(type-union(<string>, <library-object>)),
          imported? = #t, all-libraries?)
 => (module :: false-or(<module-object>))
  let library-id :: false-or(<library-id>)
    = select (library by instance?)
	<string> =>
	  make(<library-id>, name: library);
	otherwise =>
	  let library = library | project-library(server.server-project);
	  library & environment-object-id(server, library);
      end;
  let module-id :: <module-id> = parse-module-name(name, library: library-id);
  let definition
    = find-compiler-database-proxy(server, module-id, imported?: imported?)
        | if (all-libraries?)
	    search-for-module-definition(server, as(<symbol>, module-id.id-name))
	  end;
  if (definition)
    make-environment-object(<module-object>,
			    project: server.server-project,
			    compiler-object-proxy: definition)
  end
end method find-module;

define sealed method source-form-uses-definitions?
    (server :: <dfmc-database>, module :: <module-object>,
     #key modules, libraries, client)
 => (uses-definitions? :: <boolean>)
  ignore(modules, libraries, client);
  let project = server-project(server);
  let library = project-library(project);
  let module-definition = compiler-object-proxy(module);
  ~empty?(remove(module-definition-used-modules(module-definition),
		 #"dylan-user"))
end method source-form-uses-definitions?;

define sealed method do-used-definitions
    (function :: <function>, server :: <dfmc-database>,
     module :: <module-object>,
     #key modules, libraries, client)
 => ()
  ignore(modules, libraries, client);
  let module-definition :: <module-definition> = compiler-object-proxy(module);
  let context = browsing-context(server, module-definition);
  let used-modules = module-definition-used-modules(module-definition);
  local method do-module (module-name :: <symbol>) => ()
	  //---*** dylan-user doesn't have a definition, for some reason
	  unless (module-name == #"dylan-user")
	    let definition = find-module-definition(context, module-name);
	    let module
	      = definition
	          & make-environment-object(<module-object>,
					    project: server.server-project,
					    compiler-object-proxy: definition);
            module & function(module)
          end
        end method;
  do(do-module, used-modules);
end method do-used-definitions;

define sealed method do-module-client-modules
    (function :: <function>, server :: <dfmc-database>,
     module :: <module-object>)
 => ()
  let project = server-project(server);
  let library = project-library(project);
  let module-definition :: <module-definition> = compiler-object-proxy(module);
  let module-name = module-definition.module-definition-name;
  do-all-client-contexts
    (method (context)
       local method do-module 
		 (used-module-name :: <symbol>, kind :: <export-kind>) => ()
	       //---*** dylan-user doesn't have a definition, for some reason
	       unless (module-name == #"dylan-user")
		 let definition = find-module-definition(context, used-module-name);
		 let used-module-names
		   = definition.module-definition-used-modules;
		 if (member?(module-name, used-module-names))
		   function(definition)
		 end
	       end
	     end method do-module;
       dfmc/do-library-modules
	 (context, do-module, inherited?: #f, internal?: #t)
     end,
     server, browsing-context(server, module-definition))
end method do-module-client-modules;

define sealed method source-form-has-clients?
    (server :: <dfmc-database>, module :: <module-object>,
     #key modules, libraries, client)
 => (has-clients? :: <boolean>)
  ignore(modules, libraries, client);
  block (return)
    do-module-client-modules
      (method (definition :: <module-definition>)
	 return(#t)
       end,
       server, module);
    #f
  end
end method source-form-has-clients?;

define sealed method do-client-source-forms
    (function :: <function>, server :: <dfmc-database>,
     module :: <module-object>,
     #key modules, libraries, client)
 => ()
  ignore(modules, libraries, client);
  do-module-client-modules
    (method (definition :: <module-definition>)
       let module
	 = make-environment-object
	     (<module-object>,
	      project: server.server-project,
	      compiler-object-proxy: definition);
       function(module)
     end,
     server, module)
end method do-client-source-forms;

// Do all definitions in a module
define sealed method do-module-definitions
    (function :: <function>, server :: <dfmc-database>,
     module :: <module-object>,
     #key imported?, client)
 => ()
  ignore(imported?, client);
  local method do-source-form (source-form :: <source-form-object>)
	  if (instance?(source-form, <definition-object>))
	    function(source-form)
	  end;
	  if (instance?(source-form, <macro-call-object>))
	    do-macro-call-source-forms(do-source-form, server, source-form)
	  end
	end method do-source-form;
  let project = module-project-proxy(server, module);
  let context = browsing-context(server, project);
  let definition = compiler-object-proxy(module);
  let module-name = definition.module-definition-name;
  for (record :: <source-record> in project-canonical-source-records(project))
    block ()
      if (module-name == source-record-module-name(record))
	let forms = dfmc/source-record-top-level-forms(context, record);
	for (form :: <source-form> in forms)
	  let object = make-environment-object-for-source-form(server, form);
	  do-source-form(object)
	end
      end
    //--- We'll ignore source records with badly formed file headers
    exception (<badly-formed-file-header>)
      #f
    end
  end
end method do-module-definitions;

// Do all visible names in a module
define sealed method do-namespace-names
    (function :: <function>, server :: <dfmc-database>,
     module :: <module-object>,
     #key client, imported? = #t)
 => ()
  let project-object = server.server-project;
  let project = module-project-proxy(server, module);
  let context = browsing-context(server, project);
  let module-definition = module.compiler-object-proxy;
  local method do-variable
	    (variable :: <variable>, export-kind :: <export-kind>) => ()
          //---*** cpage: Some variables don't have definitions. It appears that
          //              this only happens for <variable>s that are parameters
          //              and some other names that the compiler sees, but that
          //              are not meaningful to our browsers. For now, just omit
          //              <variable>s that do not have definitions. We need to
          //              find out whether there are any we really want to
          //              browse anyway.
          if (~variable-active-definition(context, variable))
	    let (name, module) = variable-name(variable);
            debug-out(#"dfmc-environment-database",
                      "do-namespace-names: Variable '%s' has no definition in '%s'",
                      name-to-string(name),
                      name-to-string(module))
          else
            let environment-name
              = make-environment-object(<binding-name-object>,
                                        project: project-object,
                                        compiler-object-proxy: variable);
            function(environment-name)
          end
        end method do-variable;
  do-module-variables(context, module-definition,  do-variable,
		      inherited?: imported?, internal?: #t);
end method do-namespace-names;

define sealed method environment-object-name
    (server :: <dfmc-database>, object :: <definition-object>,
     library :: <library-object>)
 => (name :: false-or(<binding-name-object>))
  //--- definition's can't be found in library namespaces
  #f
end method environment-object-name;

// This code finds the home name for a variable, and then tries to find
// the same name within the current module. If it can and this name is
// bound to the same object we return it, otherwise we fail.
define sealed method environment-object-name
    (server :: <dfmc-database>, object :: <definition-object>,
     module :: <module-object>)
 => (name :: false-or(<binding-name-object>))
  let definition = object.source-form-proxy;
  let variable = definition.source-form-variable;
  if (variable)
    let module-name = environment-object-primitive-name(server, module);
    let local-variable = make-variable(variable-name(variable), module-name);
    let project = module-project-proxy(server, module);
    let context = browsing-context(server, project);
    let home-definition = variable-active-definition(context, local-variable);
    if (definition == home-definition)
      make-environment-object(<binding-name-object>,
			      project: server.server-project,
			      compiler-object-proxy: local-variable)
    end
  end
end method environment-object-name;

define sealed method environment-object-name
    (server :: <dfmc-database>, object :: <library-object>,
     namespace :: <namespace-object>)
 => (name == #f)
  #f
end method environment-object-name;

// Find the "home" name of an object
define sealed method environment-object-home-name
    (server :: <dfmc-database>, object :: <definition-object>)
 => (name :: false-or(<name-object>))
  let project = server-project(server);
  let source-form :: <source-form> = object.source-form-proxy;
  let context = browsing-context(server, source-form);
  let variable = source-form.source-form-variable;
  if (variable)
    let home-variable = variable-home(context, variable);
    make-environment-object(<binding-name-object>,
                            project: project,
                            compiler-object-proxy: home-variable)
  end
end method environment-object-home-name;

define sealed method environment-object-name
    (server :: <dfmc-database>, module :: <module-object>,
     namespace :: <module-object>)
 => (name :: false-or(<module-name-object>))
  //--- Modules can't be found in a module namespace
  #f
end method environment-object-name;

define sealed method environment-object-name
    (server :: <dfmc-database>, module :: <module-object>,
     library :: <library-object>)
 => (name :: false-or(<module-name-object>))
  //---*** This isn't right, but will do for now
  environment-object-home-name(server, module)
end method environment-object-name;

define sealed method environment-object-home-name
    (server :: <dfmc-database>, module :: <module-object>)
 => (name :: false-or(<name-object>))
  let module-definition = module.compiler-object-proxy;
  let module-name = module-definition.module-definition-name;
  let library = environment-object-library(server, module);
  %make-module-name(server, library, module-name)
end method environment-object-home-name;

define sealed method environment-object-library
    (server :: <dfmc-database>, module :: <module-object>)
 => (library :: <library-object>)
  let project = module-project-proxy(server, module);
  make-environment-object(<library-object>,
			  project: server.server-project,
			  compiler-object-proxy: project)
end method environment-object-library;

// Search for the name of an object in a module, specified by a string
define sealed method find-name
    (server :: <dfmc-database>, name :: <string>, module :: <module-object>,
     #key imported? = #t)
 => (name-object :: false-or(<name-object>))
  let module-definition :: <module-definition> = module.compiler-object-proxy;
  let (definition, definition-variable)
    = find-definition-in-module
        (server, name, module-definition, imported?: imported?);
  if (definition-variable)
    make-environment-object(<binding-name-object>,
                            project: server.server-project,
                            compiler-object-proxy: definition-variable)
  end
end method find-name;

define sealed method find-definition-in-module
    (server :: <dfmc-database>, name :: <string>,
     module-definition :: <module-definition>,
     #key imported? = #t)
 => (definition :: false-or(<definition>), 
     variable :: false-or(<variable>))
  // Create a <variable> with the desired name, then see if it defines
  // anything in the module. If so, get the "real" <variable>.
  let module-name = module-definition.module-definition-name;
  let variable = make-variable(name, module-name);
  let context = browsing-context(server, module-definition);
  let definition = variable-active-definition(context, variable);
  let definition-variable = definition & definition.source-form-variable;
  if (definition-variable
	& (imported? 
	     | begin
		 let home = variable-home(context, definition-variable);
		 let (home-name, home-module-name) = variable-name(home);
		 home-module-name == module-name
	       end))
    values(definition, definition-variable)
  else
    values(#f, #f)
  end
end method find-definition-in-module;


/// ID handling

define sealed method find-compiler-database-proxy
    (server :: <dfmc-database>, id :: <module-id>, #key imported? = #f)
 => (definition :: false-or(<module-definition>))
  ignore(imported?);
  let library-id = id.id-library;
  let project :: false-or(<project>)
    = find-compiler-database-proxy(server, library-id);
  if (project)
    let module-name = as(<symbol>, id.id-name);
    let context = browsing-context(server, project);
    let (definition, kind) = find-module-definition(context, module-name);
    if (imported? | kind == #"defined")
      definition
    end
  end
end method find-compiler-database-proxy;

define sealed method compiler-database-proxy-id
    (server :: <dfmc-database>, definition :: <module-definition>)
 => (id :: false-or(<module-id>))
  let context = browsing-context(server, definition);
  let project = context.compilation-context-project;
  let library-id = compiler-database-proxy-id(server, project);
  if (library-id)
    let module-name = definition.module-definition-name;
    let name = name-to-string(module-name);
    make(<module-id>, name: name, library: library-id)
  end
end method compiler-database-proxy-id;
