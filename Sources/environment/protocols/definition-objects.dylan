Module:    environment-protocols
Synopsis:  Environment Protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Definition objects

define open abstract class <definition-object> 
     (<macro-call-object>,
      <dylan-object>,
      <environment-object-with-id>)
end class <definition-object>;

define open generic definition-modifiers
    (server :: <server>, object :: <definition-object>)
 => (modifiers :: <sequence>);

define open generic definition-interactive-locations
    (server :: <server>, object :: <definition-object>)
 => (locations :: <sequence>);

define open generic definition-known-locations
    (server :: <server>, object :: <definition-object>)
 => (locations :: <sequence>);


/// Project dispatching methods

define method definition-modifiers
    (project :: <project-object>, object :: <definition-object>)
 => (modifiers :: <sequence>)
  let database = ensure-database-server(project, object);
  if (database)
    definition-modifiers(database, object)
  else
    #[]
  end
end method definition-modifiers;

define method definition-interactive-locations
    (project :: <project-object>, object :: <definition-object>)
 => (locations :: <sequence>)
  let server = choose-server(project, object);
  if (server)
    definition-interactive-locations(server, object)
  else
    #[]
  end
end method definition-interactive-locations;

define method definition-known-locations
    (project :: <project-object>, object :: <definition-object>)
 => (locations :: <sequence>)
  let server = choose-server(project, object);
  if (server)
    definition-known-locations(server, object)
  else
    #[]
  end
end method definition-known-locations;


/// Some convenience functions built on these protocols

define function find-named-definition
    (project :: <project-object>, module :: <module-object>, name :: <string>,
     #key imported? = #t)
 => (definition :: false-or(<definition-object>))
  let name = find-name(project, name, module, imported?: imported?);
  name & name-value(project, name)
end function find-named-definition;
