Module:    dfmc-environment-database
Synopsis:  DFM compiler database
Author:    Andy Armstrong, Chris Page, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DFM compiler database

define class <dfmc-database> (<compiler-database>)
  constant slot dfmc-database-proxy :: <project>,
    required-init-keyword: proxy:;
  constant slot module-name-proxies :: <object-table> = make(<object-table>);
end class <dfmc-database>;

define sealed method get-environment-object-primitive-name
    (server :: <server>, object :: <dfmc-database>)
 => (name :: <string>)
  "dfmc-database"
end method get-environment-object-primitive-name;



/// Context handling

//---*** It would be nice to be able to have a real type for this!
define constant <context> = <object>;

define sealed method browsing-context
    (server :: <dfmc-database>, source-form :: <source-form>)
 => (context :: <context>)
  let server-context = browsing-context(server, server);
  source-form-browsing-context(server-context, source-form)
    | context-missing-error(server, source-form)
end method browsing-context;

define sealed method browsing-context
    (server :: <dfmc-database>, project :: <project>)
 => (context :: <context>)
  ignore(server);
  project.project-browsing-context
    | context-missing-error(server, project)
end method browsing-context;

define sealed method browsing-context
    (server :: <dfmc-database>, database :: <dfmc-database>)
 => (context :: <context>)
  assert(server == database, 
	 "Querying database %= using different database %=!",
	 database, server);
  let project = server.dfmc-database-proxy;
  project.project-browsing-context
    | context-missing-error(server, project)
end method browsing-context;

define sealed method context-missing-error
    (server :: <dfmc-database>, project :: <project>)
  let name = project.project-library-name;
  error("No compilation context found for project '%s'",
	name-to-string(name))
end method context-missing-error;

define sealed method context-missing-error
    (server :: <dfmc-database>, object :: <object>)
  error("No compilation context found for '%='", object)
end method context-missing-error;


/// Macroexpansion

define method project-macroexpand-code
    (database :: <dfmc-database>, module :: <module-object>,
     code :: <byte-string>,
     #key expansion-stream :: false-or(<stream>) = #f,
          trace-stream :: false-or(<stream>) = #f)
 => ()
  let project = module-project-proxy(database, module);
  let module-name = module.compiler-object-proxy.module-definition-name;
  macroexpand-expression
    (project, module-name, code,
     expansion-stream: expansion-stream,
     trace-stream:     trace-stream)
end method project-macroexpand-code;
