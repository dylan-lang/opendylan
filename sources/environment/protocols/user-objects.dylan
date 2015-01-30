Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// User objects

define class <user-object> (<composite-object>, <environment-object-with-id>)
end class <user-object>;

define open generic user-object-slot-values
    (server :: <server>, object :: <user-object>)
 => (functions :: <sequence>, values :: <sequence>);


/// Internal objects
//
// A special subclass of <user-object> whose contents is only of
// interest to internal developers (or expert external developers)

define class <internal-object> (<user-object>)
end class <internal-object>;


/// Project dispatching methods

define method user-object-slot-values
    (project :: <project-object>, object :: <user-object>)
 => (functions :: <sequence>, values :: <sequence>)
  let application = project-application(project);
  if (application)
    user-object-slot-values(application, object)
  else
    values(#(), #())
  end
end method user-object-slot-values;

define method environment-object-type-name
    (object :: <user-object>) => (name :: <string>)
  "Instance"
end method environment-object-type-name;


/// Support for defining subclasses of <user-object>

define constant $user-object-classes = make(<deque>);

define sealed class <user-class-info> (<object>)
  sealed constant slot user-class-info-class :: <class>,
    required-init-keyword: class:;
  sealed constant slot user-class-info-id :: <definition-id>,
    required-init-keyword: id:;
end class <user-class-info>;

define function user-object-class-mappings
    () => (mappings :: <sequence>)
  $user-object-classes
end function user-object-class-mappings;

//---*** We should partial order these, but for the moment we should
//---*** just ensure that they get added in the correct order.
define function register-user-object-class
    (class :: subclass(<user-object>), name :: <string>,
     module-name :: <string>, library-name :: <string>)
 => ()
  let library = make(<library-id>, name: library-name);
  let module = make(<module-id>, name: module-name, library: library);
  let id = make(<definition-id>, name: name, module: module);
  add-new!($user-object-classes,
           make(<user-class-info>,
                class: class,
                id: id))
end function register-user-object-class;

define macro user-object-class-definer
  { define user-object-class ?name:name (?superclasses:*)
      binding ?class-name:name,
        module: ?module-name:name,
        library: ?library-name:name;
    end }
    => { define sealed class ?name (?superclasses)
         end class ?name;
         register-user-object-class
           (?name, ?"class-name", ?"module-name", ?"library-name"); }
end macro user-object-class-definer;

