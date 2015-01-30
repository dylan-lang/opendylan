Module:    environment-protocols
Synopsis:  Environment Protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Source form objects

define open abstract class <source-form-object>
    (<application-code-object>,
     <application-and-compiler-object>,
     <environment-object-with-id>)
end class <source-form-object>;

define open generic source-form-uses-definitions?
    (server :: <server>, object :: <source-form-object>,
     #key modules, libraries, client)
 => (uses-definitions? :: <boolean>);

define open generic source-form-has-clients?
    (server :: <server>, object :: <source-form-object>,
     #key modules, libraries, client)
 => (has-clients? :: <boolean>);

define open generic do-used-definitions
    (function :: <function>, server :: <server>,
     object :: <source-form-object>,
     #key modules, libraries, client)
 => ();

define open generic do-client-source-forms
    (function :: <function>, server :: <server>,
     object :: <source-form-object>,
     #key modules, libraries, client)
 => ();


/// Macro call objects

define abstract class <macro-call-object> (<source-form-object>)
end class <macro-call-object>;

define open generic do-macro-call-source-forms
    (function :: <function>, server :: <server>,
     object :: <macro-call-object>)
 => ();


/// Top level expression objects

define class <top-level-expression-object> (<source-form-object>)
end class <top-level-expression-object>;

define method environment-object-type-name
    (object :: <top-level-expression-object>) => (label :: <string>)
  "Top level expression"
end method environment-object-type-name;


/// Macro call objects

define class <simple-macro-call-object>
    (<top-level-expression-object>, <macro-call-object>)
end class <simple-macro-call-object>;

define method environment-object-type-name
    (object :: <simple-macro-call-object>) => (label :: <string>)
  "Macro call"
end method environment-object-type-name;


/// Project dispatching methods

define method source-form-uses-definitions?
    (project :: <project-object>, object :: <source-form-object>,
     #key modules, libraries, client)
 => (uses-definitions? :: <boolean>)
  let database = ensure-database-server(project, object);
  database
    & source-form-uses-definitions?
        (database, object,
         modules: modules, libraries: libraries, client: client)
end method source-form-uses-definitions?;

define method source-form-has-clients?
    (project :: <project-object>, object :: <source-form-object>,
     #key modules, libraries, client)
 => (has-clients? :: <boolean>)
  let database = ensure-database-server(project, object);
  database
    & source-form-has-clients?
        (database, object,
         modules: modules, libraries: libraries, client: client)
end method source-form-has-clients?;

define method do-used-definitions
    (function :: <function>, project :: <project-object>,
     object :: <source-form-object>,
     #key modules, libraries, client)
 => ()
  let database = ensure-database-server(project, object);
  database
    & do-used-definitions(function, database, object,
                          modules: modules, libraries: libraries,
                          client: client)
end method do-used-definitions;

define method do-client-source-forms
    (function :: <function>, project :: <project-object>,
     object :: <source-form-object>,
     #key modules, libraries, client)
 => ()
  let database = ensure-database-server(project, object);
  database
    & do-client-source-forms(function, database, object,
                             modules: modules, libraries: libraries,
                             client: client)
end method do-client-source-forms;

define method do-macro-call-source-forms
    (function :: <function>, project :: <project-object>,
     object :: <macro-call-object>)
 => ()
  let database = ensure-database-server(project, object);
  database & do-macro-call-source-forms(function, database, object)
end method do-macro-call-source-forms;


/// Some convenience functions built on these protocols

define function source-form-used-definitions
    (server :: <server>, source-form :: <source-form-object>,
     #key libraries, modules, client)
 => (used-definitions :: <sequence>)
  collect-environment-objects
    (do-used-definitions, server, source-form,
     libraries: libraries, modules: modules, client: client)
end function source-form-used-definitions;

define function source-form-clients
    (server :: <server>, source-form :: <source-form-object>,
     #key libraries, modules, client)
 => (clients :: <sequence>)
  collect-environment-objects
    (do-client-source-forms, server, source-form,
     libraries: libraries, modules: modules, client: client)
end function source-form-clients;

define function macro-call-source-forms
    (server :: <server>, source-form :: <macro-call-object>)
 => (source-forms :: <sequence>)
  collect-environment-objects(do-macro-call-source-forms, server, source-form)
end function macro-call-source-forms;
