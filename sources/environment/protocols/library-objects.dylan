Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Library object

define class <library-object> (<namespace-object>)
end class <library-object>;

define open generic project-library
    (server :: <server>) => (library :: false-or(<library-object>));

define open generic library-project 
    (server :: <server>, library :: <library-object>)
 => (project :: false-or(<project-object>));

define open generic library-filename
    (server :: <server>, library :: <library-object>)
 => (filename :: false-or(<file-locator>));

define open generic library-project-filename
    (server :: <server>, library :: <library-object>)
 => (filename :: false-or(<file-locator>));

define open generic find-library
    (server :: <server>, name :: <string>)
 => (library :: false-or(<library-object>));

define open generic library-interactive?
    (server :: <server>, library :: <library-object>)
 => (interactive? :: <boolean>);

define open generic library-read-only?
    (server :: <server>, library :: <library-object>) 
 => (read-only? :: <boolean>);

define open generic library-read-only?-setter
    (read-only? :: <boolean>, server :: <server>, library :: <library-object>) 
 => (read-only? :: <boolean>);


/// Project dispatching methods

// I guess main library
define method project-library
    (project :: <project-object>) => (library :: false-or(<library-object>))
  let database = project-compiler-database(project);
  database & project-library(database)
end method project-library;

// this has to be changed to return a sequence of <project-object>s
define method library-project
    (project :: <project-object>, library :: <library-object>)
 => (project :: <project-object>)
  let server = choose-server(project, library, error?: #t);
  library-project(server, library)
end method library-project;

define method library-filename
    (project :: <project-object>, library :: <library-object>)
 => (filename :: false-or(<file-locator>))
  let server = choose-server(project, library, error?: #t);
  library-filename(server, library)
end method library-filename;

define method library-project-filename
    (project :: <project-object>, library :: <library-object>)
 => (filename :: false-or(<file-locator>))
  let server = choose-server(project, library, error?: #t);
  library-project-filename(server, library)
end method library-project-filename;

define method find-library
    (project :: <project-object>, name :: <string>)
 => (library :: false-or(<library-object>))
  let database = project-compiler-database(project);
  database & find-library(database, name)
end method find-library;

define method library-interactive?
    (project :: <project-object>, library :: <library-object>)
 => (interactive? :: <boolean>)
  let server = choose-server(project, library);
  server & library-interactive?(server, library)
end method library-interactive?;

define method library-read-only?
    (project :: <project-object>, library :: <library-object>)
 => (read-only? :: <boolean>)
  let database = ensure-database-server(project, library);
  database & library-read-only?(database, library)
end method library-read-only?;

define method library-read-only?-setter
    (read-only? :: <boolean>, project :: <project-object>, 
     library :: <library-object>)
 => (read-only? :: <boolean>)
  let database = ensure-database-server(project, library);
  database & library-read-only?-setter(read-only?, database, library)
end method library-read-only?-setter;


/// Implementation

define function library-modules
    (server :: <server>, library :: <library-object>,
     #key client, imported? = #t)
 => (modules :: <sequence>)
  collect-environment-objects(do-library-modules, server, library,
			      client: client, imported?: imported?)
end function library-modules;

define method environment-object-type-name
    (object :: <library-object>) => (label :: <string>)
  "Library"
end method environment-object-type-name;

define function do-library-modules
    (function :: <function>, server :: <server>, library :: <library-object>,
     #key client, imported? = #t)
 => ()
  do-namespace-names
    (method (object)
       let module = name-value(server, object);
       module & function(module)
     end,
     server, library,
     client: client,
     imported?: imported?)
end function do-library-modules;

// Search for a library by name
define method find-library
    (server :: <server>, name :: <string>)
 => (library :: false-or(<library-object>))
  let project = server-project(server);
  block (return)
    local method maybe-return-library
	      (library :: <library-object>) => ()
	    if (environment-object-primitive-name(server, library) = name)
	      return(library)
	    end
	  end method maybe-return-library;
    maybe-return-library(project-library(project));
    do-project-used-libraries(maybe-return-library, server, project)
  end
end method find-library;

//---*** andrewa: it might be nice for users to record this information
//---*** in their project.
define function library-default-module
    (server :: <server>, library :: <library-object>)
 => (module :: false-or(<module-object>))
  block (return)
    do-library-modules(return, server, library, imported?: #f);
    do-library-modules(return, server, library, imported?: #t);
    #f
  end
end function library-default-module;
