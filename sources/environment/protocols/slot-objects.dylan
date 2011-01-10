Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////// Slot objects

define class <slot-object> (<application-and-compiler-object>)
end class <slot-object>;

define open generic slot-class
    (server :: <server>, slot :: <slot-object>) => (class :: <class-object>);

define open generic slot-getter 
    (server :: <server>, slot :: <slot-object>)
 => (getter :: false-or(<function-object>));

define open generic slot-setter 
    (server :: <server>, slot :: <slot-object>)
 => (setter :: false-or(<function-object>));

define open generic slot-type 
    (server :: <server>, slot :: <slot-object>)
 => (type :: <environment-object>);

/*---*** Not used yet, do we need it?
define open generic slot-init-value 
    (server :: <server>, slot :: <slot-object>)
 => (value :: false-or(<environment-object>));
*/

define open generic user-object-slot-value
    (server :: <server>, obj :: <user-object>, 
     slot :: type-union(<definition-id>, <slot-object>),
     #key repeated-element)
 => (value :: false-or(<environment-object>));


/*---*** cpage: Remove this function?
//---*** What should this return?
define open generic slot-init-function 
    (server :: <server>, slot :: <slot-object>)
 => (function :: false-or(<function-object>));
*/

define open generic slot-init-kind
    (server :: <server>, slot :: <slot-object>)
 => (kind :: false-or(<symbol>));

define open generic slot-init-keyword 
    (server :: <server>, slot :: <slot-object>)
 => (keyword :: false-or(<symbol>), required? :: <boolean>);

define open generic slot-allocation
    (server :: <server>, slot :: <slot-object>)
 => (keywords :: <sequence>);


/// Project dispatching methods

define method slot-class
    (project :: <project-object>, slot :: <slot-object>)
 => (class :: <class-object>)
  let server = choose-server(project, slot, error?: #t);
  slot-class(server, slot)
end method slot-class;

define method slot-getter
    (project :: <project-object>, slot :: <slot-object>)
 => (getter :: false-or(<function-object>))
  let server = choose-server(project, slot, error?: #t);
  slot-getter(server, slot)
end method slot-getter;

define method slot-setter 
    (project :: <project-object>, slot :: <slot-object>)
 => (setter :: false-or(<function-object>))
  let server = choose-server(project, slot, error?: #t);
  slot-setter(server, slot)
end method slot-setter;

define method slot-type 
    (project :: <project-object>, slot :: <slot-object>)
 => (type :: <environment-object>)
  let server = choose-server(project, slot, error?: #t);
  slot-type(server, slot)
end method slot-type;

/*---*** cpage: Cannot currently be implemented; remove this function?
define method slot-init-value 
    (project :: <project-object>, slot :: <slot-object>)
 => (value :: <environment-object>)
  let server = choose-server(project, slot, error?: #t);
  slot-init-value(server, slot)
end method slot-init-value;
*/

/*---*** cpage: Cannot currently be implemented; remove this function?
define method slot-init-function 
    (project :: <project-object>, slot :: <slot-object>)
 => (function)
  let server = choose-server(project, slot, error?: #t);
  slot-init-function(server, slot)
end method slot-init-function;
*/

define method slot-init-kind
    (project :: <project-object>, slot :: <slot-object>)
 => (kind :: false-or(<symbol>))
  let server = choose-server(project, slot, error?: #t);
  slot-init-kind(server, slot)
end method slot-init-kind;

define method slot-init-keyword
    (project :: <project-object>, slot :: <slot-object>)
 => (keyword ::false-or(<symbol>), required? :: <boolean>)
  let server = choose-server(project, slot, error?: #t);
  slot-init-keyword(server, slot)
end method slot-init-keyword;

define method slot-allocation
    (project :: <project-object>, slot :: <slot-object>)
 => (keywords :: <sequence>)
  let server = choose-server(project, slot, error?: #t);
  slot-allocation(server, slot)
end method slot-allocation;


/// Implementation

define method environment-object-type
    (server :: <server>, object :: <slot-object>)
 => (type :: false-or(<environment-object>))
  slot-type(server, object)
end method environment-object-type;

define method environment-object-type-name
    (object :: <slot-object>) => (label :: <string>)
  "Slot"
end method environment-object-type-name;

define method user-object-slot-value
    (server :: <project-object>, object :: <user-object>, 
     slot :: <slot-object>, 
     #key repeated-element = 0)
 => (value :: false-or(<environment-object>))
  let application = project-application(server);
  application
    & user-object-slot-value(application, object, slot,
                             repeated-element: repeated-element)
end method user-object-slot-value;

define method user-object-slot-value
    (server :: <project-object>, object :: <user-object>, 
     slot :: <definition-id>, 
     #key repeated-element = 0)
 => (value :: false-or(<environment-object>))
  let application = project-application(server);
  application
    & user-object-slot-value(application, object, slot,
                             repeated-element: repeated-element)
end method user-object-slot-value;
