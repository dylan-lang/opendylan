Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Function objects

define abstract class <function-object> (<application-code-object>)
end class <function-object>;

//--- Ideally this would be abstract, but <internal-object>
//--- is concrete...
define class <dylan-function-object>
    (<internal-object>,
     <function-object>,
     <definition-object>,
     <application-and-compiler-object>)
end class <dylan-function-object>;

define class <generic-function-object> (<dylan-function-object>)
end class <generic-function-object>;

define class <method-object> (<dylan-function-object>)
end class <method-object>;

define class <method-constant-object> (<method-object>)
end class <method-constant-object>;

define class <internal-method-object> (<method-object>)
end class <internal-method-object>;

define class <simple-function-object> (<dylan-function-object>)
end class <simple-function-object>;

define class <foreign-function-object> 
    (<function-object>, 
     <foreign-object>)
end class <foreign-function-object>;

define class <parameter> (<object>)
  constant slot parameter-name :: <string>,
    required-init-keyword: name:;
  constant slot parameter-type :: <environment-object>,
    required-init-keyword: type:;
end class <parameter>;
 
define class <optional-parameter> (<parameter>)
  constant slot parameter-keyword :: false-or(<string>) = #f,
    init-keyword: keyword:;
  constant slot parameter-default-value :: <object>,
    required-init-keyword: default-value:;
end class <optional-parameter>;


/// Protocols

// Should probably be limited collections
define constant <parameters> = <sequence>;
define constant <optional-parameters> = <sequence>;

define open generic function-parameters
    (server :: <server>, function :: <dylan-function-object>)
 => (required :: <parameters>,
     rest :: false-or(<parameter>),
     keys :: <optional-parameters>,
     all-keys? :: <boolean>,
     next :: false-or(<parameter>),
     values :: <parameters>,
     rest-value :: false-or(<parameter>));

define open generic do-generic-function-methods
    (function :: <function>, server :: <server>,
     generic-function :: <generic-function-object>,
     #key client)
 => ();

define open generic method-specializers 
    (server :: <server>, object :: <method-object>)
 => (specializers :: <sequence>);

define open generic method-generic-function
    (server :: <server>, object :: <method-object>) 
 => (function :: false-or(<generic-function-object>));



/// Project dispatching methods

define method function-parameters
    (project :: <project-object>, function :: <dylan-function-object>)
 => (required :: <parameters>,
     rest :: false-or(<parameter>),
     keys :: <optional-parameters>,
     all-keys? :: <boolean>,
     next :: false-or(<parameter>),
     values :: <parameters>,
     rest-value :: false-or(<parameter>))
  let server = choose-server(project, function, error?: #t);
  function-parameters(server, function)
end method function-parameters;

define method do-generic-function-methods
    (function :: <function>, project :: <project-object>,
     generic-function :: <generic-function-object>,
     #key client)
 => ()
  let server = choose-server(project, generic-function);
  server
    & do-generic-function-methods(function, server, generic-function,
				  client: client)
end method do-generic-function-methods;

define method method-specializers 
    (project :: <project-object>, function :: <method-object>)
 => (specializers :: <sequence>)
  let server = choose-server(project, function, error?: #t);
  method-specializers(server, function)
end method method-specializers;

define method method-generic-function
    (project :: <project-object>, function :: <method-object>) 
 => (function :: false-or(<generic-function-object>))
  let server = choose-server(project, function);
  server & method-generic-function(server, function)
end method method-generic-function;


/// Function names

define method environment-object-type-name
    (object :: <generic-function-object>) => (label :: <string>)
  "Generic"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <method-object>) => (label :: <string>)
  "Method"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <method-constant-object>) => (label :: <string>)
  "Function"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <foreign-function-object>) => (label :: <string>)
  "Foreign function"
end method environment-object-type-name;


/// Convenience functions

define function generic-function-object-methods
    (server :: <server>, function :: <generic-function-object>)
 => (methods :: <sequence>)
  collect-environment-objects(do-generic-function-methods, server, function)
end function generic-function-object-methods;
