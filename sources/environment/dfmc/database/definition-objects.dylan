Module:    dfmc-environment-database
Synopsis:  DFM compiler definition information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Definition objects

//---*** Temporary definition. We need to import this from the appropriate place
define constant <code-location>
  = type-union(<integer>, one-of(#"entry", #"exit"));

//---*** It would be nice if browser-support gave us this...
define constant <definition> = <source-form>;

define sealed method environment-object-source-location
    (server :: <dfmc-database>, object :: <library-object>)
 => (location :: false-or(<source-location>))
  let source-form = library-definition(object);
  source-form & find-source-form-location(source-form)
end method environment-object-source-location;

define sealed method definition-modifiers
    (server :: <dfmc-database>, object :: <definition-object>)
 => (modifiers :: <sequence>)
  let source-form = object.source-form-proxy;
  source-form-adjectives(source-form) | #[]
end method definition-modifiers;

define sealed method definition-code-locations
    (server :: <dfmc-database>, object :: <definition-object>)
 => (locations :: <sequence>)
  //---*** To be implemented
  #()
end method definition-code-locations;

define sealed method definition-code-location-character-offset
    (server :: <dfmc-database>, object :: <definition-object>,
     code-location :: <code-location>)
 => (character-offset :: <integer>)
  //---*** To be implemented
  0
end method definition-code-location-character-offset;


/// ID handling

define sealed method find-compiler-database-proxy
    (server :: <dfmc-database>, id :: <definition-id>, #key imported? = #f)
 => (definition :: false-or(<definition>))
  let module-id = id-module(id);
  let module-definition :: false-or(<module-definition>)
    = find-compiler-database-proxy(server, module-id);
  if (module-definition)
    find-definition-in-module
      (server, id.id-name, module-definition, imported?: imported?)
  end
end method find-compiler-database-proxy;

//---*** andrewa: stolen from environment-object-home-name, we should
//---*** probably share this code somehow.
define function definition-home-name-and-module
    (server :: <dfmc-database>, definition :: <definition>)
 => (name :: false-or(<string>), module :: false-or(<module-definition>))
  let variable = definition.source-form-variable;
  if (variable)
    let context = browsing-context(server, definition);
    let home-variable = variable-home(context, variable);
    let (variable-name, module-name) = variable-name(home-variable);
    let module-definition = find-module-definition(context, module-name);
    values(name-to-string(variable-name), module-definition)
  else
    values(#f, #f)
  end if
end function definition-home-name-and-module;
