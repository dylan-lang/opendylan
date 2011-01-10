Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Type objects

define class <type-object> 
    (<internal-object>,
     <definition-object>,
     <application-and-compiler-object>)
end class <type-object>;

/*---*** Not used yet, do we need it?
define open generic type-subtype?
    (server :: <server>, subtype :: <type-object>, type :: <type-object>)
 => (subtype? :: <boolean>);
*/

define method environment-object-type-name
    (object :: <type-object>) => (label :: <string>)
  "Type"
end method environment-object-type-name;


/// Singleton objects

define class <singleton-object> (<type-object>)
end class <singleton-object>;

define open generic singleton-value
    (server :: <server>, object :: <singleton-object>)
 => (value :: <environment-object>);


/// Project dispatching methods

define method singleton-value
    (project :: <project-object>, object :: <singleton-object>)
 => (value :: <environment-object>)
  let server = choose-server(project, object, error?: #t);
  singleton-value(server, object)
end method singleton-value;


/// Implementation

define method environment-object-type-name
    (object :: <singleton-object>) => (label :: <string>)
  "Singleton"
end method environment-object-type-name;

