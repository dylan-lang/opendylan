Module:    dfmc-environment-database
Synopsis:  DFM compiler namespace name information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Module name objects

// We have our own class of proxy because the browser support library
// only returns us the module name as a symbol, so we have no means
// to get back to its namespace.
define class <module-name-proxy> (<object>)
  sealed constant slot proxy-name :: <symbol>,
    required-init-keyword: name:;
  sealed constant slot proxy-library :: <library-object>,
    required-init-keyword: library:;
end class <module-name-proxy>;

define method %make-module-name
    (server :: <dfmc-database>, library :: <library-object>, name :: <symbol>)
 => (name :: <module-name-object>)
  let proxies = server.module-name-proxies;
  let library-table
    = element(proxies, library, default: #f)
        | begin
            element(proxies, library) := make(<object-table>)
          end;
  let proxy
    = element(library-table, name, default: #f)
        | begin
            let proxy = make(<module-name-proxy>, name: name, library: library);
            element(library-table, name) := proxy
          end;
  make-environment-object(<module-name-object>,
                          project: server.server-project,
                          compiler-object-proxy: proxy)
end method %make-module-name;

define method browsing-context
    (server :: <dfmc-database>, name :: <module-name-object>)
 => (context :: <context>)
  let library = name-namespace(server, name);
  browsing-context(server, library.library-compiler-project)
end method browsing-context;

define sealed method get-environment-object-primitive-name
    (server :: <dfmc-database>, name-object :: <module-name-object>)
 => (name :: false-or(<string>))
  name-to-string(name-object.compiler-object-proxy.proxy-name)
end method get-environment-object-primitive-name;

// Return the library of a module name
define sealed method name-namespace
    (server :: <dfmc-database>, name :: <module-name-object>)
 => (namespace :: <library-object>)
  name.compiler-object-proxy.proxy-library
end method name-namespace;

// Return module definition
define sealed method name-value
    (server :: <dfmc-database>, name :: <module-name-object>)
 => (value :: false-or(<module-object>))
  let proxy       = name.compiler-object-proxy;
  let module-name = proxy.proxy-name;
  let project     = proxy.proxy-library.library-compiler-project;
  let context     = browsing-context(server, project);
  let definition  = find-module-definition(context, module-name);
  if (definition)
    make-environment-object(<module-object>,
                            project: server.server-project,
                            compiler-object-proxy: definition)
  end
end method name-value;

// Return #t if a module is exported
define sealed method name-exported?
    (server :: <dfmc-database>, name :: <module-name-object>)
 => (exported? :: <boolean>)
  let context = browsing-context(server, name);
  let module-name = name.compiler-object-proxy.proxy-name;
  module-exported?(context, module-name)
end method name-exported?;

// Return #t if a module is imported from another library
define sealed method name-imported?
    (server :: <dfmc-database>, name :: <module-name-object>)
 => (imported? :: <boolean>)
  let proxy       = name.compiler-object-proxy;
  let project     = proxy.proxy-library.library-compiler-project;
  let module-name = proxy.proxy-name;
  let context     = browsing-context(server, project);
  let (definition, export-kind) = find-module-definition(context, module-name);
  export-kind = #"inherited"
end method name-imported?;



/// Binding name objects

define method browsing-context
    (server :: <dfmc-database>, name :: <binding-name-object>)
 => (context :: <context>)
  let module = name-namespace(server, name);
  let project = module-project-proxy(server, module);
  browsing-context(server, project)
end method browsing-context;

define sealed method get-environment-object-primitive-name
    (server :: <dfmc-database>, name-object :: <binding-name-object>)
 => (name :: false-or(<string>))
 name-to-string(variable-name(name-object.compiler-object-proxy))
end method get-environment-object-primitive-name;

// Return the namespace of a name
define sealed method name-namespace
    (server :: <dfmc-database>, name :: <binding-name-object>)
 => (namespace :: <module-object>)
  let variable = name.compiler-object-proxy;
  let (var-name, module-name) = variable-name(variable);
  //---*** Unfortunately we've lost the context, so we have to just
  //---*** search for a module of this name everywhere...
  let definition = search-for-module-definition(server, module-name);
  if (definition)
    make-environment-object(<module-object>,
                            project: server.server-project,
                            compiler-object-proxy: definition)
  else
    error("Internal error: failed to find module %= for variable %=",
          module-name, var-name)
  end
end method name-namespace;

// Return definition of a name in a module
//--- andrewa: for some reason, the compiler sometimes returns
//--- non-definitions so I've loosened the type here a little.
define sealed method name-value
    (server :: <dfmc-database>, name :: <binding-name-object>)
 => (value :: false-or(<source-form-object>))
  let context = browsing-context(server, name);
  let variable = name.compiler-object-proxy;
  let variable-definition = variable-active-definition(context, variable);
  variable-definition
    & make-environment-object-for-source-form(server, variable-definition)
end method name-value;

// Return type of a name in a module
define sealed method name-type
    (server :: <dfmc-database>, name :: <binding-name-object>)
 => (type :: <environment-object>)
  let context = browsing-context(server, name);
  let variable = name.compiler-object-proxy;
  let definition      = variable-active-definition(context, variable);
  let type-expression = source-form-variable-type(definition, variable);
  //---*** Does this method make sense for module variables and constants?
  //---*** Need to add support for returning <singleton-object>?
  make-environment-object-for-type-expression(server, type-expression)
end method name-type;

// Return #t if a name in a module is exported
define sealed method name-exported?
    (server :: <dfmc-database>, name :: <binding-name-object>)
 => (exported? :: <boolean>)
  let context = browsing-context(server, name);
  let variable = name.compiler-object-proxy;
  variable-exported?(context, variable)
end method name-exported?;

define method variable-local?
    (context :: <context>, variable :: <variable>) => (local? :: <boolean>)
  let home = variable-home(context, variable);
  let (name, module) = variable-name(variable);
  let (home-name, home-module) = variable-name(home);
  name == home-name & module == home-module
end method variable-local?;

// Return #t if a name in a module is imported from another module
define sealed method name-imported?
    (server :: <dfmc-database>, name :: <binding-name-object>)
 => (imported? :: <boolean>)
  let context = browsing-context(server, name);
  let variable = name.compiler-object-proxy;
  ~variable-local?(context, variable)
end method name-imported?;
