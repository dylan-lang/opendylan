Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Module object

define class <module-object> (<namespace-object>)
end class <module-object>;

define open generic module-project-proxy
    (server :: <server>, module :: <module-object>)
 => (proxy);

define open generic find-module
    (server :: <server>, name :: <string>, 
     #key library, imported?, all-libraries?)
 => (module :: false-or(<module-object>));

//--- hughg, 1998/03/25: Why is this open?
define open generic file-module
    (project :: false-or(<project-object>),
     filename :: type-union(<file-locator>, <string>))
 => (module :: false-or(<module-object>),
     library :: false-or(<library-object>));

define open generic do-module-definitions
    (function :: <function>, server :: <server>, module :: <module-object>,
     #key imported?, client)
 => ();


/// Project dispatching methods

define method module-project-proxy
    (project :: <project-object>, module :: <module-object>)
 => (proxy)
  let server = choose-server(project, module, error?: #t);
  module-project-proxy(server, module)
end method module-project-proxy;

define method find-module
    (project :: <project-object>, name :: <string>, 
     #key library, all-libraries?,
          imported? = #t)
 => (module :: false-or(<module-object>))
  let database = project-compiler-database(project);
  database
    & find-module(database, name, 
		  library: library, 
		  all-libraries?: all-libraries?,
		  imported?: imported?)
end method find-module;

define method do-module-definitions
    (function :: <function>, project :: <project-object>,
     module :: <module-object>,
     #key client, imported? = #t)
 => ()
  let server = choose-server(project, module);
  server 
    & do-module-definitions(function, server, module, 
			    client: client, imported?: imported?)
end method do-module-definitions;


/// Implementation

define method environment-object-type-name
    (object :: <module-object>) => (label :: <string>)
  "Module"
end method environment-object-type-name;

define function module-definitions
    (server :: <server>, module :: <module-object>,
     #key client, imported? = #t)
 => (names :: <sequence>)
  collect-environment-objects(do-module-definitions, server, module,
			      client: client, imported?: imported?)
end function module-definitions;

// Search for a module by name in a library
define method find-module
    (server :: <server>, name :: <string>, 
     #key library, imported? = #t, all-libraries?)
 => (module :: false-or(<module-object>))
  let project = server-project(server);
  block (return)
    local method maybe-return-module 
	      (library :: <library-object>, #key imported?) => ()
	    let name = find-name(server, name, library, imported?: imported?);
	    let module = name & name-value(server, name);
	    module & return(module)
	  end method maybe-return-module;
    let library
      = select (library by instance?)
	  <library-object> => library;
	  <string>         => find-library(server, library);
	  otherwise        => project-library(project);
	end;
    maybe-return-module(library, imported?: imported?);
    if (all-libraries?)
      do-project-used-libraries(maybe-return-module, server, project)
    end
  end
end method find-module;


/// Filename->module translation

define method file-module
    (project :: false-or(<project-object>),
     filename :: type-union(<file-locator>, <string>))
 => (module :: false-or(<module-object>),
     library :: false-or(<library-object>))
  block (return)
    do-project-file-libraries
      (method (library :: <library-object>, record :: <source-record>)
	 let module-name = source-record-module-name(record);
	 let module
	   = find-module(project, as(<string>, module-name),
			 library: library);
	 module & return(module, library)
       end,
       project,
       as(<file-locator>, filename));
    values(#f, #f)
  end
end method file-module;
