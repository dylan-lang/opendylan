Module:    dfmc-environment-database
Synopsis:  DFM compiler library information
Author:    Andy Armstrong, Chris Page, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Import/export types for dfmc-browser-support

define constant <export-kind> = one-of(#"internal", #"exported");

/// Library object

define sealed method library-definition
    (library :: <library-object>)
 => (definition :: <library-definition>)
  let project = library.library-compiler-project;
  let context = project.project-browsing-context;
  let definition = context & context.project-library-definition;
  assert(definition,
	 "Library %s is missing its definition",
	 name-to-string(project.project-library-name));
  definition
end method library-definition;

define sealed method library-compiler-project
    (library :: <library-object>)
 => (project :: <project>)
  library.compiler-object-proxy
end method library-compiler-project;

// Return the primitive name of a library
define sealed method get-environment-object-primitive-name
    (server :: <dfmc-database>, library :: <library-object>)
 => (name :: false-or(<string>))
  let project = library-compiler-project(library);
  name-to-string(project.project-library-name)
end method get-environment-object-primitive-name;

// Search for a library by name
define sealed method find-library
    (server :: <dfmc-database>, name :: <string>)
 => (library :: false-or(<library-object>))
  let library-name = as(<symbol>, name);
  let project 
    = find-project-for-library-name(server, library-name, error?: #f);
  if (project)
    make-environment-object(<library-object>,
			    project: server.server-project,
			    compiler-object-proxy: project)
  end
end method find-library;

define method project-executable-pathname
    (server :: <server>, project :: <project>, 
     #key type, full-path?)
 => (name :: <file-locator>)
  let library-name :: <symbol> = project.project-library-name;
  let merged-name :: <symbol> = merged-project-name(library-name);
  let dll-project :: <project>
    = if (merged-name = library-name)
	project
      else
	let project-object = server.server-project;
	let database = project-object.project-compiler-database;
	//---*** How can we close the project opened through
	//---*** lookup-named-project again without accidentally
	//---*** closing it if the user already had it open?
	if (database)
	  find-project-for-library-name(database, merged-name, error?: #f)
	end
	  | lookup-named-project(merged-name)
      end;
  let base = dll-project.project-executable-name;
  let extension
    = select (type | dll-project.project-target-type)
	#"dll"        => "dll";
	#"executable" => "exe";
      end;
  let filename = make(<file-locator>, base: base, extension: extension);
  if (full-path?)
    let directory
      = if (project.project-read-only?)
	  as(<directory-locator>, release-runtime-directory())
	else
	  project.project-build-location
	end;
    merge-locators(filename, directory)
  else
    filename
  end
end method project-executable-pathname;

// return the DLL/EXE filename for the specified library
define sealed method library-filename
    (server :: <dfmc-database>, library :: <library-object>)
 => (filename :: <file-locator>)
  let project-object = server.server-project;
  let main-library = project-object.project-library;
  let main-library? = main-library == library;
  let project = library.library-compiler-project;
  as(<file-locator>,
     project-executable-pathname
       (server, project, 
	type: unless (main-library?) #"dll" end,
	full-path?: #t))
end method library-filename;

define sealed method library-project-filename
    (server :: <dfmc-database>, library :: <library-object>)
 => (filename :: <file-locator>)
  library.library-compiler-project.project-location
end method library-project-filename;

define sealed method library-interactive?
    (server :: <dfmc-database>, library :: <library-object>)
 => (interactive? :: <boolean>)
  let project = library.library-compiler-project;
  project.project-interaction-allowed?
end method library-interactive?;

define sealed method library-read-only?
    (server :: <dfmc-database>, library :: <library-object>) 
 => (read-only? :: <boolean>)
  let project = library.library-compiler-project;
  project.project-read-only?
end method library-read-only?;

define sealed method library-read-only?-setter
    (read-only? :: <boolean>, server :: <dfmc-database>, 
     library :: <library-object>)
 => (read-only? :: <boolean>);
  let project = library.library-compiler-project;
  project.project-read-only? := read-only?
end method library-read-only?-setter;

// Do all visible module names in a library
define sealed method do-namespace-names
    (function :: <function>, server :: <dfmc-database>,
     library :: <library-object>,
     #key client, imported? = #t)
 => ()
  let context = browsing-context(server, library.library-compiler-project);
  local method do-module (name :: <symbol>, export-kind :: <export-kind>) => ()
	  unless (name == #"dylan-user")
	    let environment-name = %make-module-name(server, library, name);
	    function(environment-name)
	  end
	end method;
  dfmc/do-library-modules(context, do-module, inherited?: imported?, internal?: #t);
end method do-namespace-names;

define sealed method source-form-uses-definitions?
    (server :: <dfmc-database>, library :: <library-object>,
     #key modules, libraries, client)
 => (uses-definitions? :: <boolean>)
  ignore(modules, libraries, client);
  let definition = library.library-definition;
  ~empty?(definition.library-definition-used-libraries)
end method source-form-uses-definitions?;

define sealed method do-used-definitions
    (function :: <function>, server :: <dfmc-database>,
     library :: <library-object>,
     #key modules, libraries, client)
 => ()
  ignore(modules, libraries, client);
  let definition = library.library-definition;
  let used-libraries = definition.library-definition-used-libraries;
  for (library-name :: <symbol> in used-libraries)
    let project = find-project-for-library-name(server, library-name);
    let library
      = make-environment-object(<library-object>,
				project: server.server-project,
				compiler-object-proxy: project);
    function(library)
  end
end method do-used-definitions;

define sealed method source-form-has-clients?
    (server :: <dfmc-database>, library :: <library-object>,
     #key modules, libraries, client)
 => (has-clients? :: <boolean>)
  let project = library.compiler-object-proxy;
  let context = browsing-context(server, project);
  let library-name :: <symbol> = project.project-library-name;
  block (return)
    do-all-client-contexts
      (method (context :: <context>)
	 let definition = context.project-library-definition;
	 let used-libraries = definition.library-definition-used-libraries;
	 if (member?(library-name, used-libraries))
	   return(#t)
	 end
       end,
       server, context);
    #f
  end block;
end method source-form-has-clients?;

define sealed method do-client-source-forms
    (function :: <function>, server :: <dfmc-database>,
     library :: <library-object>,
     #key modules, libraries, client)
 => ()
  let project = library.compiler-object-proxy;
  let context = browsing-context(server, project);
  let library-name :: <symbol> = project.project-library-name;
  do-all-client-contexts
    (method (context :: <context>)
       let definition = context.project-library-definition;
       let used-libraries = definition.library-definition-used-libraries;
       if (member?(library-name, used-libraries))
	 let name = definition.library-definition-name;
	 let project = find-project-for-library-name(server, name);
	 let library
	   = make-environment-object(<library-object>,
				     project: server.server-project,
				     compiler-object-proxy: project);
	 function(library)
       end
     end,
     server, context)
end method do-client-source-forms;


/// ID handling

define sealed method find-compiler-database-proxy
    (server :: <dfmc-database>, id :: <library-id>, #key imported? = #f)
 => (definition :: false-or(<project>))
  ignore(imported?);
  block (return)
    let library-name = as(<symbol>, id-name(id));
    local method maybe-return-project
	      (project :: <project>) => ()
	    if (project.project-library-name == library-name)
	      let context = project.project-browsing-context;
	      if (context & context.project-library-definition)
		return(project)
	      end
	    end
	  end method maybe-return-project;
    //---*** andrewa: is there a more efficient way to do this?
    do-all-projects(maybe-return-project, server)
  end
end method find-compiler-database-proxy;

define sealed method compiler-database-proxy-id
    (server :: <dfmc-database>, project :: <project>)
 => (id :: <library-id>)
  make(<library-id>, name: name-to-string(project.project-library-name))
end method compiler-database-proxy-id;


/// Implement do-project-file-libraries

define sealed method do-project-file-libraries
    (function :: <function>, server :: <dfmc-database>, file :: <file-locator>)
 => ()
  let project = server.server-project;
  let library = project.project-library;
  local method do-library-file-libraries
	    (library :: <library-object>) => ()
	  let project = library.library-compiler-project;
	  let source-files = project.project-dylan-sources;
	  for (source-file in source-files)
	    when (file = source-file)
	      let (record, modified?) 
		= project-source-canonical-source-record(project, source-file);
	      record & function(library, record)
	    end
	  end
	end method do-library-file-libraries;
  library & do-library-file-libraries(library);
  do-project-used-libraries(do-library-file-libraries, project, project)
end method do-project-file-libraries;


/// Library lookup

define sealed method environment-object-library
    (server :: <dfmc-database>, library :: <library-object>)
 => (library :: <library-object>)
  library
end method environment-object-library;
