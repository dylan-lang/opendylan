Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Compiler warnings

define abstract class <warning-object> (<environment-object>)
end class <warning-object>;

define class <compiler-warning-object> 
    (<compiler-object>,
     <environment-object-with-library>,
     <warning-object>)
end class <compiler-warning-object>;

define class <project-warning-object> (<warning-object>)
  sealed slot environment-object-proxy = #f,
    required-init-keyword: proxy:;
end class <project-warning-object>;

//---*** andrewa: short term hack, we should classify this more carefully
define class <serious-compiler-warning-object> (<compiler-warning-object>)
end class <serious-compiler-warning-object>;

define class <compiler-error-object> (<compiler-warning-object>)
end class <compiler-error-object>;


define open generic do-compiler-warnings
    (function :: <function>, server :: <server>, 
     object :: <environment-object>,
     #key client)
 => ();

define open generic compiler-warning-short-message
    (server :: <server>, warning :: <warning-object>)
 => (message :: <string>);

define open generic compiler-warning-full-message
    (server :: <server>, warning :: <warning-object>)
 => (message :: <string>);

define open generic warning-owner
    (server :: <server>, warning :: <warning-object>)
 => (owner :: false-or(<environment-object>));


/// Project dispatching methods

define method do-compiler-warnings
    (function :: <function>, project :: <project-object>, 
     object :: <environment-object>,
     #key client)
 => ()
  let database = ensure-database-server(project, object);
  database & do-compiler-warnings(function, database, object, client: client)
end method do-compiler-warnings;

define method do-compiler-warnings
    (function :: <function>, project :: <project-object>, 
     object :: <project-object>,
     #key client)
 => ()
  assert(project = object,
	 "Querying %= through different project %=!",
	 object, project);
  local method do-library-warnings
	    (library :: <library-object>) => ()
	  do-compiler-warnings(function, project, library, client: client)
	end method do-library-warnings;
  let library = project-library(project);
  library & do-library-warnings(library);
  do-project-used-libraries
    (method (library :: <library-object>)
       if (~library-read-only?(project, library))
	 do-library-warnings(library)
       end
     end,
     project, project)
end method do-compiler-warnings;

define method compiler-warning-short-message
    (project :: <project-object>, warning :: <compiler-warning-object>)
 => (message :: <string>)
  let database = ensure-database-server(project, warning, error?: #t);
  compiler-warning-short-message(database, warning)
end method compiler-warning-short-message;

define method compiler-warning-full-message
    (project :: <project-object>, warning :: <compiler-warning-object>)
 => (message :: <string>)
  let database = ensure-database-server(project, warning, error?: #t);
  compiler-warning-full-message(database, warning)
end method compiler-warning-full-message;

define method warning-owner
    (project :: <project-object>, warning :: <compiler-warning-object>)
 => (owner :: false-or(<source-form-object>))
  // gz, 18-Feb-00: Changed this to not signal error, because it was
  // causing problems when the warnings pane got updated at certain
  // points during compilation, when the database is cleared out.
  // Probably the right thing is to cache the warning owner with
  // the warning object, so don't need to look it up in a possibly
  // since-changed database world.  But I don't know enough to do that.
  let database = ensure-database-server(project, warning, error?: /* #t */ #f);
  database & warning-owner(database, warning)
end method warning-owner;

//--- This is to override the default method on <compiler-object>,
//--- probably we should reorganize the hierarchy so this isn't
//--- necessary.
define method environment-object-home-name
    (project :: <project-object>, warning :: <compiler-warning-object>)
 => (name :: false-or(<name-object>))
  #f
end method environment-object-home-name;

//--- This is to override the default method on <compiler-object>,
//--- probably we should reorganize the hierarchy so this isn't
//--- necessary.
define method environment-object-name
    (project :: <project-object>, warning :: <compiler-warning-object>,
     namespace :: <namespace-object>)
 => (name :: false-or(<name-object>))
  #f
end method environment-object-name;


/// Some convenience functions built on these protocols

define method environment-object-source
    (server :: <server>, object :: <compiler-warning-object>)
 => (source :: false-or(<string>))
  let source-form = warning-owner(server, object);
  source-form & environment-object-source(server, source-form)
end method environment-object-source;

define function source-form-compiler-warnings
    (server :: <server>, object :: <source-form-object>, #key client)
 => (warnings :: <sequence>)
  collect-environment-objects
    (do-compiler-warnings, server, object, client: client)
end function source-form-compiler-warnings;

define function project-warnings
    (project :: <project-object>, #key client)
 => (warnings :: <sequence>)
  collect-environment-objects
    (do-compiler-warnings, project, project, client: client)
end function project-warnings;


/// Printing support

define method environment-object-type-name
    (object :: <warning-object>) => (name :: <string>)
  "Warning"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <serious-compiler-warning-object>) => (name :: <string>)
  "Serious warning"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <compiler-error-object>) => (name :: <string>)
  "Error"
end method environment-object-type-name;
