Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Namespace object

define class <namespace-object> (<definition-object>)
end class <namespace-object>;

define open generic name-namespace
    (server :: <server>, name :: <name-object>)
 => (namespace :: <namespace-object>);

define open generic do-namespace-names
    (function :: <function>, server :: <server>,
     namespace :: <namespace-object>,
     #key imported?, client)
 => ();

define open generic environment-object-home-name
    (server :: <server>, object :: <environment-object>)
 => (name :: false-or(<name-object>));

define open generic environment-object-name
    (server :: <server>, object :: <environment-object>,
     namespace :: <namespace-object>)
 => (name :: false-or(<name-object>));

define open generic find-name
    (server :: <server>, name :: <string>, namespace :: <namespace-object>,
     #key imported? = #t)
 => (name :: false-or(<name-object>));


/// Project dispatching methods

define method name-namespace
    (project :: <project-object>, name :: <name-object>)
 => (namespace :: <namespace-object>)
  let server = choose-server(project, name, error?: #t);
  name-namespace(server, name)
end method name-namespace;

define method do-namespace-names
    (function :: <function>, project :: <project-object>,
     namespace :: <namespace-object>,
     #key client, imported? = #t)
 => ()
  let server = choose-server(project, namespace);
  server 
    & do-namespace-names(function, server, namespace, 
                         client: client, imported?: imported?)
end method do-namespace-names;


define method environment-object-home-name
    (project :: <project-object>, object :: <environment-object>)
 => (name :: false-or(<name-object>))
  #f
end method environment-object-home-name;

define method environment-object-home-name
    (project :: <project-object>, object :: <compiler-object>)
 => (name :: false-or(<name-object>))
  let server = choose-server(project, object);
  server & environment-object-home-name(server, object)
end method environment-object-home-name;

define method environment-object-name
    (project :: <project-object>, object :: <environment-object>,
     namespace :: <namespace-object>)
 => (name :: false-or(<name-object>))
  #f
end method environment-object-name;

define method environment-object-name
    (project :: <project-object>, object :: <compiler-object>,
     namespace :: <namespace-object>)
 => (name :: false-or(<name-object>))
  let server = choose-server(project, object);
  if (server & ensure-database-server(project, namespace))
    environment-object-name(server, object, namespace)
  end
end method environment-object-name;


/// Implementation

define function namespace-names
    (server :: <server>, namespace :: <namespace-object>,
     #key client, imported? = #t)
 => (names :: <sequence>)
  collect-environment-objects(do-namespace-names, server, namespace,
			      client: client, imported?: imported?)
end function namespace-names;

define method environment-object-home-name
    (server :: <server>, object :: <environment-object>)
 => (name :: false-or(<name-object>))
  #f
end method environment-object-home-name;

define method environment-object-name
    (server :: <server>, object :: <environment-object>,
     namespace :: <namespace-object>)
 => (name :: false-or(<name-object>))
  #f
end method environment-object-name;


/// find-name

define method find-name
    (project :: <project-object>, name :: <string>,
     namespace :: <namespace-object>,
     #key imported? = #t)
 => (name-object :: false-or(<name-object>))
  let server = choose-server(project, namespace);
  server & find-name(server, name, namespace, imported?: imported?)
end method find-name;

define method find-name
    (server :: <server>, name :: <string>, namespace :: <namespace-object>,
     #key imported? = #t)
 => (name-object :: false-or(<name-object>))
  block (return)
    let name = as-lowercase(name);
    do-namespace-names
      (method (name-object)
         let primitive-name
           = environment-object-primitive-name(server, name-object);
         if (name = as-lowercase(primitive-name))
           return(name-object)
         end
       end,
       server,
       namespace,
       imported?: imported?);
    #f
  end
end method find-name;
