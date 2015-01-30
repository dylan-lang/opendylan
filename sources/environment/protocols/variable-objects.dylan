Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Variable objects

define abstract class <variable-object>
    (<application-and-compiler-object>)
end class <variable-object>;

define abstract class <module-variable-object>
    (<variable-object>,
     <definition-object>)
end class <module-variable-object>;

define class <global-variable-object> (<module-variable-object>)
end class <global-variable-object>;

define class <thread-variable-object> (<module-variable-object>)
end class <thread-variable-object>;

define open generic variable-value
    (server :: <server>, variable :: <variable-object>,
     #key thread)
 => (value :: false-or(<application-object>));

define open generic variable-type
    (server :: <server>, variable :: <variable-object>)
 => (type :: false-or(<environment-object>));


/// Project dispatching methods

define method variable-value
    (project :: <project-object>, variable :: <variable-object>,
     #key thread)
 => (value :: false-or(<application-object>))
  let server = ensure-application-server(project, variable);
  server & variable-value(server, variable, thread: thread)
end method variable-value;

define method variable-type
    (project :: <project-object>, variable :: <variable-object>)
 => (type :: false-or(<environment-object>))
  let server = choose-server(project, variable);
  server & variable-type(server, variable)
end method variable-type;


/// Implementation

define method environment-object-type
    (server :: <server>, object :: <variable-object>)
 => (type :: false-or(<environment-object>))
  variable-type(server, object)
end method environment-object-type;

define method environment-object-type-name
    (object :: <global-variable-object>) => (label :: <string>)
  "Variable"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <thread-variable-object>) => (label :: <string>)
  "Variable"
end method environment-object-type-name;
