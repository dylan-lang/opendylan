Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Name objects

define class <name-object> (<compiler-object>)
end class <name-object>;

define class <module-name-object> (<name-object>)
end class <module-name-object>;

define class <binding-name-object> (<name-object>)
end class <binding-name-object>;

define open generic name-value
    (server :: <server>, name :: <name-object>)
 => (value :: false-or(<environment-object>));

define open generic name-type
    (server :: <server>, name :: <name-object>)
 => (type :: <environment-object>);

define open generic name-exported?
    (server :: <server>, name :: <name-object>)
 => (exported? :: <boolean>);

define open generic name-imported?
    (server :: <server>, name :: <name-object>)
 => (imported? :: <boolean>);


/// Project dispatching methods

define method name-value
    (project :: <project-object>, name :: <name-object>)
 => (value :: false-or(<environment-object>))
  let server = choose-server(project, name);
  if (server)
    name-value(server, name)
  else
    closed-server-error(name)
  end
end method name-value;

define method name-type
    (project :: <project-object>, name :: <name-object>)
 => (type :: <environment-object>)
  let server = choose-server(project, name);
  if (server)
    name-type(server, name)
  else
    closed-server-error(name)
  end
end method name-type;

define method name-exported?
    (project :: <project-object>, name :: <name-object>)
 => (exported? :: <boolean>)
  let server = choose-server(project, name);
  if (server)
    name-exported?(server, name)
  else
    closed-server-error(name)
  end
end method name-exported?;

define method name-imported?
    (project :: <project-object>, name :: <name-object>)
 => (imported? :: <boolean>)
  let server = choose-server(project, name);
  if (server)
    name-imported?(server, name)
  else
    closed-server-error(name)
  end
end method name-imported?;


/// Implementation

define method environment-object-type-name
    (object :: <name-object>) => (label :: <string>)
  "Name"
end method environment-object-type-name;
