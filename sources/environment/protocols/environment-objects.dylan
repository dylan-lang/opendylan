Module:    environment-protocols
Synopsis:  Environment Protocols
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment objects

define constant <id-or-integer> = type-union(<id>, <integer>);

define open abstract primary class <environment-object> (<object>)
  sealed slot %primitive-name :: false-or(<string>) = #f,
    init-keyword: name:;
end class <environment-object>;

define open abstract primary class <environment-object-with-id>
    (<environment-object>)
  constant sealed slot %id :: false-or(<id-or-integer>),
    required-init-keyword: id:;
end class <environment-object-with-id>;

define open generic note-object-properties-changed
    (client, object :: <environment-object>, type :: <query-type>) => ();

define generic closed-server-error (object :: <environment-object>);

define open generic environment-object-type
    (server :: <server>, object :: <environment-object>)
 => (type :: false-or(<environment-object>));

define open generic environment-object-type-name
    (object :: <environment-object>)
 => (type-name :: <string>);

define open generic environment-object-id
    (server :: <server>, object :: <environment-object>)
 => (id :: false-or(<id-or-integer>));

define open generic environment-object-exists?
    (server :: <server>, object :: <environment-object>)
 => (exists? :: <boolean>);

define open generic environment-object-primitive-name
    (server :: <server>, object :: <environment-object>)
 => (name :: false-or(<string>));

define open generic get-environment-object-primitive-name
    (server :: <server>, object :: <environment-object>)
 => (name :: false-or(<string>));

define open generic environment-object-library
    (server :: <server>, object :: <environment-object>)
 => (library :: false-or(<library-object>));

define open generic environment-object-source
    (server :: <server>, object :: <environment-object>)
 => (source :: false-or(<string>));

define open generic environment-object-source-location
    (server :: <server>, object :: <environment-object>)
 => (location :: false-or(<source-location>));

define open generic source-location-environment-object
    (server :: <server>, location :: <source-location>)
 => (object :: false-or(<environment-object>));


/// Some convenience functions

define method collect-environment-objects
    (function, server :: <server>, object :: <environment-object>, #rest args)
 => (objects :: <sequence>)
  let results = make(<stretchy-vector>);
  apply(function,
        method (object)
          add!(results, object)
        end,
        server,
        object,
        args);
  results
end method collect-environment-objects;


/// Environment object types

define method environment-object-type
    (server :: <server>, object :: <environment-object>)
 => (type :: false-or(<environment-object>))
  #f
end method environment-object-type;

define method environment-object-type-name
    (object :: <environment-object>)
 => (label :: <string>)
  "Object"
end method environment-object-type-name;


/// Environment object naming

define method environment-object-primitive-name
    (server :: <server>, object :: <environment-object>)
 => (name :: false-or(<string>))
  object.%primitive-name
    | begin
        let name = get-environment-object-primitive-name(server, object);
        object.%primitive-name := name
      end
end method environment-object-primitive-name;

define method get-environment-object-primitive-name
    (server :: <server>, object :: <environment-object>)
 => (name :: false-or(<string>))
  #f
end method get-environment-object-primitive-name;


/// ID handling

define method environment-object-id
    (server :: <server>, object :: <environment-object>)
 => (id == #f)
  #f
end method environment-object-id;

define method environment-object-id
    (server :: <server>, object :: <environment-object-with-id>)
 => (id :: false-or(<id-or-integer>))
  object.%id
end method environment-object-id;


/// Library handling

//---*** Hack to make library a valid keyword
define method initialize
    (object :: <environment-object>,
     #key library :: false-or(<library-object>), #all-keys)
  next-method()
end method initialize;

define open abstract class <environment-object-with-library>
    (<environment-object>)
  constant slot %library :: <library-object>,
    required-init-keyword: library:;
end class <environment-object-with-library>;

define method environment-object-library
    (project :: <project-object>, object :: <environment-object>)
 => (library :: false-or(<library-object>))
  let server = choose-server(project, object);
  server & environment-object-library(server, object)
end method environment-object-library;

define method environment-object-library
    (project :: <project-object>, object :: <environment-object-with-library>)
 => (library :: <library-object>)
  ignore(project);
  object.%library
end method environment-object-library;

define method environment-object-library
    (server :: <server>, object :: <environment-object>)
 => (library :: false-or(<library-object>))
  #f
end method environment-object-library;


/// Source

define method environment-object-source-location
    (project :: <project-object>, object :: <environment-object>)
 => (location :: false-or(<source-location>))
  let server = choose-server(project, object);
  server & environment-object-source-location(server, object)
end method environment-object-source-location;

define method environment-object-source-location
    (server :: <server>, object :: <environment-object>)
 => (location :: false-or(<source-location>))
  #f
end method environment-object-source-location;

define method source-location-environment-object
    (project :: <project-object>, location :: <source-location>)
 => (object :: false-or(<environment-object>))
  let database = project-compiler-database(project);
  database & source-location-environment-object(database, location)
end method source-location-environment-object;

define method source-location-environment-object
    (server :: <server>, location :: <source-location>)
 => (object :: false-or(<environment-object>))
  #f
end method source-location-environment-object;

define method environment-object-source
    (server :: <server>, object :: <environment-object>)
 => (source :: false-or(<string>))
  let location = environment-object-source-location(server, object);
  location
    & as(<string>, location.copy-source-location-contents)
end method environment-object-source;


/// Parsing environment objects

define sealed generic parse-environment-object-name
    (name :: <string>, #key, #all-keys)
 => (id :: false-or(<id-or-integer>));

define function tokenize-string
    (string :: <sequence>, separator :: <character>)
 => (tokens :: <sequence>)
  let tokens = make(<stretchy-vector>);
  let old-position :: <integer> = 0;
  let string-size :: <integer> = size(string);
  while (old-position < string-size & string[old-position] == ' ')
    old-position := old-position + 1
  end;
  let position :: <integer> = old-position;
  while (position < string-size)
    while (position < string-size & string[position] ~= separator)
      position := position + 1
    end;
    if (position <= string-size)
      let end-position = position;
      while (end-position > old-position & string[end-position - 1] == ' ')
        end-position := end-position - 1
      end;
      add!(tokens, copy-sequence(string, start: old-position, end: end-position));
      old-position := position + 1;
      while (old-position < string-size & string[old-position] == ' ')
        old-position := old-position + 1
      end;
      position := old-position
    end;
  end;
  if (old-position < string-size - 1)
    add!(tokens, copy-sequence(string, start: old-position))
  end;
  tokens
end function tokenize-string;

define method parse-environment-object-name
    (name :: <string>,
     #key module :: false-or(<module-id>),
          library :: false-or(<library-id>),
     #all-keys)
 => (id :: false-or(<id-or-integer>))
  let (integer, next-index)
    = string-to-integer(name, default: $minimum-integer);
  let space-index = find-key(name, curry(\=, ' '));
  case
    integer & integer > 0 & next-index == name.size =>
      integer;
    space-index =>
      let keyword = as(<symbol>, copy-sequence(name, end: space-index));
      let name-remainder = copy-sequence(name, start: space-index + 1);
      select (keyword)
        #"library" =>
          make(<library-id>, name: name-remainder);
        #"module" =>
          let library = library | (module & id-library(module));
          parse-module-name(name-remainder, library: library);
        #"method" =>
          let library = library | (module & id-library(module));
          parse-method-name(name-remainder, module: module, library: library);
        otherwise =>
          #f;
      end;
    otherwise =>
      parse-definition-name(name, module: module, library: library);
  end
end method parse-environment-object-name;

define method parse-module-name
    (name :: <string>,
     #key library :: false-or(<library-id>))
 => (id :: false-or(<module-id>))
  let tokens = tokenize-string(name, ':');
  if (~any?(empty?, tokens))
    select (size(tokens))
      1 =>
        library & make(<module-id>, name: tokens[0], library: library);
      2 =>
        let library = make(<library-id>, name: tokens[1]);
        make(<module-id>, name: tokens[0], library: library);
      otherwise =>
        #f;
    end
  end
end method parse-module-name;

define method parse-definition-name
    (name :: <string>,
     #key module :: false-or(<module-id>),
          library :: false-or(<library-id>))
 => (id :: false-or(<definition-id>))
  let tokens = tokenize-string(name, ':');
  if (~any?(empty?, tokens))
    select (size(tokens))
      1 =>
        module & make(<definition-id>, name: tokens[0], module: module);
      2 =>
        let library = library | (module & id-library(module));
        if (library)
          let module = make(<module-id>, name: tokens[1], library: library);
          make(<definition-id>, name: tokens[0], module: module)
        end;
      3 =>
        let library = make(<library-id>, name: tokens[2]);
        let module = make(<module-id>, name: tokens[1], library: library);
        make(<definition-id>, name: tokens[0], module: module);
      otherwise =>
        #f;
    end
  end
end method parse-definition-name;

define method parse-method-name
    (name :: <string>,
     #key module :: false-or(<module-id>),
          library :: false-or(<library-id>))
 => (id :: false-or(<method-id>))
  local method local-parse-definition-name
            (name :: <string>) => (id :: false-or(<definition-id>))
          parse-definition-name(name, module: module, library: library)
        end method local-parse-definition-name;
  let tokens = tokenize-string(name, '(');
  let (gf, specializers)
    = if (size(tokens) == 2)
        let gf = tokens[0];
        let rest = tokens[1];
        let specializer-tokens = tokenize-string(rest, ')');
        let specializers
          = if (size(specializer-tokens) <= 2)
              let string = specializer-tokens[0];
              if (empty?(string))
                #[]
              else
                tokenize-string(string, ',')
              end
            end;
        if (specializers)
          values(gf, specializers)
        else
          values(#f, #f)
        end
      else
        values(#f, #f)
      end;
  let (function-id, specializer-ids)
    = if (gf & specializers)
        let function-id = local-parse-definition-name(gf);
        let specializer-ids
          = map-as(<simple-object-vector>, local-parse-definition-name, specializers);
        values(function-id, specializer-ids)
      else
        values(#f, #f)
      end;
  if (function-id & every?(rcurry(instance?, <definition-id>), specializer-ids))
    make(<method-id>,
         generic-function: function-id,
         specializers: specializer-ids)
  end
end method parse-method-name;


/// Finding environment objects by name

define open generic find-environment-object
    (server :: <server>, name :: type-union(<string>, <id-or-integer>),
     #key, #all-keys)
 => (object :: false-or(<environment-object>));

define method find-environment-object
    (server :: <server>, id :: <id>,
     #key, #all-keys)
 => (object :: false-or(<environment-object>))
  //--- Do we really want this?
  #f
end method find-environment-object;

define method find-environment-object
    (server :: <server>, id :: <integer>,
     #key, #all-keys)
 => (object :: false-or(<environment-object>))
  let project = server.server-project;
  lookup-environment-object-by-id(project, id)
end method find-environment-object;

define method find-environment-object
    (server :: <server>, name :: <string>,
     #rest keys,
     #key module :: false-or(<module-object>),
          library :: false-or(<library-object>),
     #all-keys)
 => (object :: false-or(<environment-object>))
  let module-id = if (module) environment-object-id(server, module) end;
  let library-id = if (library) environment-object-id(server, library) end;
  let id
    = parse-environment-object-name
        (name, module: module-id, library: library-id);
  id & apply(find-environment-object, server, id, keys)
end method find-environment-object;
