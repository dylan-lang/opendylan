Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class objects

define class <class-object> (<type-object>)
end class <class-object>;

define open generic application-object-class
    (server :: <server>, object :: <application-object>)
 => (class :: false-or(<class-object>));

define open generic do-direct-subclasses
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client)
 => ();

define open generic do-direct-superclasses
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client)
 => ();

define open generic do-direct-methods
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client)
 => ();

define open generic do-all-methods
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client)
 => ();

define open generic do-direct-slots
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client)
 => ();

define open generic do-all-slots
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client)
 => ();

define open generic do-all-superclasses
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client)
 => ();

define open generic do-init-keywords
    (function :: <function>, server :: <server>, class :: <class-object>,
     #key client, inherited? :: <boolean>)
 => ();


/// Project dispatching methods

define method application-object-class
    (project :: <project-object>, object :: <application-object>)
 => (class :: false-or(<class-object>))
  let server = choose-server(project, object, default-server: #"application");
  server & application-object-class(server, object)
end method application-object-class;

define method do-direct-subclasses
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>,
     #key client)
 => ()
  let server = choose-server(project, class);
  server & do-direct-subclasses(function, server, class, client: client)
end method do-direct-subclasses;

define method do-direct-superclasses
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>,
     #key client)
 => ()
  let server = choose-server(project, class);
  server & do-direct-superclasses(function, server, class, client: client)
end method do-direct-superclasses;

define method do-direct-methods
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>, 
     #key client)
 => ()
  let server = choose-server(project, class);
  server & do-direct-methods(function, server, class, client: client)
end method do-direct-methods;

define method do-all-methods
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>, 
     #key client)
 => ()
  let server = choose-server(project, class);
  server & do-all-methods(function, server, class, client: client)
end method do-all-methods;

define method do-direct-slots
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>,
     #key client)
 => ()
  let server = choose-server(project, class);
  server & do-direct-slots(function, server, class, client: client)
end method do-direct-slots;

define method do-all-superclasses
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>,
     #key client)
 => ()
  let server = choose-server(project, class);
  server & do-all-superclasses(function, server, class, client: client)
end method do-all-superclasses;

define method do-all-slots
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>,
     #key client)
 => ()
  let server = choose-server(project, class);
  server & do-all-slots(function, server, class, client: client)
end method do-all-slots;

define method do-init-keywords
    (function :: <function>, project :: <project-object>, 
     class :: <class-object>,
     #key client, inherited? :: <boolean> = #t)
 => ()
  let server = choose-server(project, class);
  server
    & do-init-keywords(function, server, class, client: client, inherited?: inherited?)
end method do-init-keywords;


/// Some convenience functions built on these protocols

define function class-direct-subclasses
    (server :: <server>, class :: <class-object>) => (classes :: <sequence>)
  collect-environment-objects(do-direct-subclasses, server, class)
end function class-direct-subclasses;

define function class-direct-superclasses
    (server :: <server>, class :: <class-object>) => (classes :: <sequence>)
  collect-environment-objects(do-direct-superclasses, server, class)
end function class-direct-superclasses;

define function class-direct-methods
    (server :: <server>, class :: <class-object>) => (methods :: <sequence>)
  collect-environment-objects(do-direct-methods, server, class)
end function class-direct-methods;

define function class-direct-slots
    (server :: <server>, class :: <class-object>) => (slots :: <sequence>)
  collect-environment-objects(do-direct-slots, server, class)
end function class-direct-slots;

define function class-slots
    (server :: <server>, class :: <class-object>) => (slots :: <sequence>)
  collect-environment-objects(do-all-slots, server, class)
end function class-slots;


/// Object printing

define method environment-object-type
    (server :: <server>, object :: <application-object>)
 => (type :: false-or(<environment-object>))
  application-object-class(server, object)
end method environment-object-type;

define method environment-object-type-name
    (object :: <class-object>) => (label :: <string>)
  "Class"
end method environment-object-type-name;
