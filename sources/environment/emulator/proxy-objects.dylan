Module:    emulator-environment-backend
Synopsis:  Emulator Environment Backend
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Proxy objects

define method ensure-server-object
    (server :: <server>, object == #t)
 => (object :: <environment-object>)
  $true-object
end method ensure-server-object;

define method ensure-server-object
    (server :: <server>, object == #f)
 => (object :: <environment-object>)
  $false-object
end method ensure-server-object;

define method ensure-server-object 
    (server :: <server>, object)
 => (object :: <environment-object>)
  let class
    = select (object by instance?)
        <dylan-library>           => <library-object>;
        <dylan-module>            => <module-object>;
        <character>               => <character-object>;
        <string>                  => <string-object>;
        <number>                  => <number-object>;
        <generic-function>        => <generic-function-object>;
        <cl-standard-method>      => <method-object>;
        <function>                => <function-object>;
        <class>                   => <class-object>;
        <pair>                    => <pair-object>;
        <range>                   => <range-object>;
        <collection>              => <collection-object>;
        <lisp-process>            => <thread-object>;
        <symbol> =>
          case
            dylan-symbol?(object)   => <symbol-object>;
            dylan-constant?(object) => <constant-object>;
            otherwise               => <module-variable-object>;
          end;
        otherwise =>
          case
            dylan-slot?(object)   => <slot-object>;
	    otherwise             =>
	      user-object-environment-class(server, object);
          end
      end;
  ensure-server-object-of-class(server, object, class)
end method ensure-server-object;

define method ensure-server-object-of-class
    (server :: <server>, object, class :: subclass(<environment-object>))
 => (object :: <environment-object>)
  make-environment-object(class, 
			  project: server-project(server),
			  application-object-proxy: object, 
			  compiler-object-proxy: object)
end method ensure-server-object-of-class;

define method do-server-environment-objects
    (function :: <function>, server :: <server>, 
     objects :: <sequence>, class :: <class>)
 => ()
  for (proxy in objects)
    function(ensure-server-object-of-class(server, proxy, class))
  end
end method do-server-environment-objects;


/// Object objects

define method application-object-class
    (application :: <emulator-application>, object :: <application-object>)
 => (class :: <class-object>)
  let class = dylan-object-class(application-object-proxy(object));
  class & ensure-server-object-of-class(application, class, <class-object>)
end method application-object-class;

//--- Hack for the mixed-up world of the Dylan emulator!
define method application-object-class
    (application :: <emulator-application>, object :: <compiler-object>)
 => (class :: <class-object>)
  let class = dylan-object-class(compiler-object-proxy(object));
  class & ensure-server-object-of-class(application, class, <class-object>)
end method application-object-class;

define method get-environment-object-primitive-name
    (application :: <emulator-application>, object :: <variable-object>)
 => (name :: <string>)
  let symbol = compiler-object-proxy(object);
  dylan-symbol-name(symbol)
end method get-environment-object-primitive-name;

define method composite-object-contents
    (project :: <emulator-project>, object :: <composite-object>,
     #key inherited? = #t)
 => (names :: <sequence>, values :: <sequence>)
  let local? = if (inherited?) #() else #t end;
  let (names, contents) 
    = object-slot-contents(application-object-proxy(object), local?: local?);
  let ensure-function = curry(ensure-server-object, project);
  values(map(ensure-function, names),
         map(ensure-function, contents))
end method composite-object-contents;

define method composite-object-size
    (application :: <emulator-application>, object :: <composite-object>,
     #key inherited? = #t)
 => (size :: <integer>)
  let (names, contents)
    = object-slot-contents(application-object-proxy(object), local?: #f);
  ignore(contents);
  size(names)
end method composite-object-size;


/// Core objects

define method get-environment-object-primitive-name
    (application :: <emulator-application>, symbol :: <symbol-object>)
 => (name :: <string>)
  dylan-symbol-name(application-object-proxy(symbol))
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (application :: <emulator-application>, number :: <number-object>)
 => (name :: <string>)
  format-to-string("%d", application-object-proxy(number))
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (application :: <emulator-application>, character :: <character-object>)
 => (name :: <string>)
  format-to-string("%s", application-object-proxy(character))
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (application :: <emulator-application>, string :: <string-object>)
 => (name :: <string>)
  application-object-proxy(string)
end method get-environment-object-primitive-name;

define method pair-head
    (application :: <emulator-application>, pair :: <pair-object>)
 => (object :: <environment-object>)
  ensure-server-object(application, head(application-object-proxy(pair)))
end method pair-head;

define method pair-tail
    (application :: <emulator-application>, pair :: <pair-object>)
 => (object :: <environment-object>)
  ensure-server-object(application, tail(application-object-proxy(pair)))
end method pair-tail;


/// Source handling

//---*** There must be a more efficient way of doing this...
define method source-file-contents 
    (file, #key start-line, end-line) => (string :: <string>)
  with-open-file (stream = file)
    start-line := start-line | 0;
    if (start-line > 0 | end-line)
      for (line from 0 to start-line)
        read-line(stream)
      end;
      if (end-line)
        let contents = make(<string>);
        for (line from start-line to end-line)
          let line = read-line(stream);
          contents := format-to-string("%s%s\n", contents, line)
        end;
        contents
      else
        read-to-end(stream)
      end
    else
      read-to-end(stream)
    end
  end
end method source-file-contents;

define method environment-object-source
    (database :: <emulator-database>, object :: <environment-object>)
 => (source :: false-or(<string>))
  let source-location = definition-source-location(database, object);
  if (source-location)
    let source-record = source-location-source-record(source-location);
    let file = source-record-location(source-record);
    source-file-contents
      (file,
       start-line: source-location-start-line(source-location),
       end-line:   #f) // source-location-end-line(source-location))
  end
end method environment-object-source;


/// Ranges

define method range-start
    (application :: <emulator-application>, range :: <range-object>)
 => (start :: <real>)
  let (size, first, last, step)
    = dylan-range-contents(application-object-proxy(range));
  ensure-server-object(application, first)
end method range-start;

define method range-end
    (application :: <emulator-application>, range :: <range-object>)
 => (start :: <real>)
  let (size, first, last, step)
    = dylan-range-contents(application-object-proxy(range));
  ensure-server-object(application, last)
end method range-end;

define method range-by
    (application :: <emulator-application>, range :: <range-object>)
 => (start :: <real>)
  let (size, first, last, step)
    = dylan-range-contents(application-object-proxy(range));
  ensure-server-object(application, step)
end method range-by;


/// Definition objects

define method definition-source-location
    (database :: <emulator-database>, object :: <definition-object>)
 => (source-location :: false-or(<source-location>))
  let (source, start-line, end-line)
    = dylan-object-source-location(compiler-object-proxy(object));
  if (source)
    let project = server-project(database);
    let source-record = find-project-source-record(project, source);
    if (source-record)
      make-line-location(source-record, start-line)
    end
  end
end method definition-source-location;

define method definition-source-location
    (database :: <emulator-database>, library :: <library-object>)
 => (source-location :: false-or(<source-location>))
  let project = library-project(database, library);
  let source-record = project-sources(project)[0];
  if (source-record)
    make-line-location(source-record, 0)
  end
end method definition-source-location;

define method definition-source-location
    (database :: <emulator-database>, object :: <module-object>)
 => (source-location :: false-or(<source-location>))
  let library = module-library(database, object);
  let project = library-project(database, library);
  let source = project-source-location(project, "module");
  let source-record = find-project-source-record(project, source);
  if (source-record)
    make-line-location(source-record, 0)
  end
end method definition-source-location;

define method do-used-definitions
    (function :: <function>, database :: <emulator-database>, 
     definition :: <definition-object>,
     #key modules, libraries, client)
 => ()
  do-dylan-definitions-used-definitions
    (method (proxy)
       function(ensure-server-object(database, proxy))
     end,
     compiler-object-proxy(definition))
end method do-used-definitions;

define method do-client-definitions
    (function :: <function>, database :: <emulator-database>, 
     definition :: <definition-object>,
     #key modules, libraries, client)
 => ()
  do-dylan-definitions-client-definitions
    (method (proxy)
       function(ensure-server-object(database, proxy))
     end,
     compiler-object-proxy(definition))
end method do-client-definitions;


/// Compiler warnings

define method do-compiler-warnings
    (function :: <function>, database :: <emulator-database>,
     object :: <definition-object>,
     #key client) => ()
  do-dylan-compiler-warnings
    (method (object)
       function
         (ensure-server-object-of-class
	    (database, object, <compiler-warning-object>))
     end,
     compiler-object-proxy(object))
end method do-compiler-warnings;

define method get-environment-object-primitive-name
    (database :: <emulator-database>, warning :: <compiler-warning-object>)
 => (message :: <string>)
  dylan-warning-message(compiler-object-proxy(warning))
end method get-environment-object-primitive-name;

define method warning-object-definition
    (database :: <emulator-database>, warning :: <compiler-warning-object>)
 => (definition :: false-or(<definition-object>))
  let lisp-definition
    = dylan-warning-definition(compiler-object-proxy(warning));
  ensure-server-object(database, lisp-definition)
end method warning-object-definition;

define method warning-object-source-location
    (database :: <emulator-database>, warning :: <compiler-warning-object>)
 => (location :: false-or(<source-location>))
  let definition = warning-object-definition(database, warning);
  if (definition)
    definition-source-location(database, definition)
  end
end method warning-object-source-location;

define method environment-object-source
    (database :: <emulator-database>, object :: <compiler-warning-object>)
 => (source :: false-or(<string>))
  let definition = warning-object-definition(database, object);
  if (definition)
    environment-object-source(database, definition)
  end
end method environment-object-source;


/// Collection modeling

define method composite-object-size
    (application :: <emulator-application>, object :: <collection-object>,
     #key inherited? = #t)
 => (size :: <integer>)
  ignore(inherited?);
  size(application-object-proxy(object))
end method composite-object-size;

define method collection-elements
    (application :: <emulator-application>, object :: <collection-object>,
     #key range)
 => (elements :: <sequence>)
  ignore(range);
  map(curry(ensure-server-object, application),
      application-object-proxy(object))
end method collection-elements;

define method collection-keys
    (application :: <emulator-application>, collection :: <collection-object>,
     #key range: collection-range)
 => (keys :: <sequence>)
  ignore(collection-range);
  let proxy = application-object-proxy(collection);
  select (proxy by instance?)
    <sequence> =>
      range(from: 0, to: size(proxy));
    <explicit-key-collection> =>
      map(curry(ensure-server-object, application),
	  key-sequence(proxy))
  end
end method collection-keys;


/// User objects

define method user-object-slot-values
    (application :: <emulator-application>, object :: <user-object>)
 => (functions :: <sequence>, values :: <sequence>)
  let (names, contents) = object-slot-contents(application-object-proxy(object));
  let ensure-function = curry(ensure-server-object, application);
  values(map(ensure-function, names),
         map(ensure-function, contents))
end method user-object-slot-values;

define method user-object-slot-value
    (application :: <emulator-application>, object :: <user-object>,
     id :: <definition-id>,
     #key repeated-value)
 => (value :: false-or(<environment-object>))
  ignore(repeated-value);
  let proxy = application-object-proxy(object);
  let (value, success?) = object-slot-value(proxy, id-name(id));
  if (success?)
    ensure-server-object(application, value)
  end
end method user-object-slot-values;


/// User object class mapping

define method user-object-environment-class
    (server :: <server>, proxy)
 => (class :: subclass(<user-object>))
  let project = server-project(server);
  let application = project-application(project);
  let dummy-object = make(<user-object>, application-object-proxy: proxy);
  let object-class = application-object-class(application, dummy-object);
  lookup-user-object-environment-class(application, object-class)
    | compute-user-object-environment-class(application, object-class)
end method user-object-environment-class;

define method lookup-user-object-environment-class
    (application :: <emulator-application>, class :: <class-object>)
 => (user-object-class :: false-or(subclass(<user-object>)))
  let table = user-object-class-mapping-table(application);
  element(table, class, default: #f)
end method lookup-user-object-environment-class;

define method cache-user-object-environment-class
    (application :: <emulator-application>, class :: <class-object>, 
     user-object-class :: subclass(<user-object>))
 => (user-object-class :: subclass(<user-object>))
  let table = user-object-class-mapping-table(application);
  element(table, class) := user-object-class
end method cache-user-object-environment-class;

define method compute-user-object-environment-class
    (application :: <emulator-application>, class :: <class-object>)
 => (user-object-class :: subclass(<user-object>))
  let user-object-class
    = block (return)
	for (class-info in user-object-class-mappings())
	  let class-id     = user-class-info-id(class-info);
	  let name         = id-name(class-id);
	  let module-id    = id-module(class-id);
	  let module-name  = id-name(module-id);
	  let library-id   = id-library(module-id);
	  let library-name = id-name(library-id);
	  when (user-object-class-subclass?
		  (application, class, name, module-name, library-name))
	    return(user-class-info-class(class-info))
	  end
	end;
	<user-object>
      end;
  cache-user-object-environment-class(application, class, user-object-class);
end method compute-user-object-environment-class;

define method user-object-class-subclass?
    (application :: <emulator-application>, class :: <class-object>,
     name :: <string>, module :: <string>, library :: <string>)
 => (subclass? :: <boolean>)
  ignore(library);
  let project = find-project(library);
  let superclass = dylan-variable-value(name, module);
  if (instance?(superclass, <class>))
    subtype?(application-object-proxy(class), superclass)
  end
end method user-object-class-subclass?;


/// ID handling

define method application-proxy-id
    (application :: <emulator-application>, proxy)
 => (id :: false-or(<id>))
  //---*** Fill this in!
  #f
end method application-proxy-id;

define method find-application-proxy
    (application :: <emulator-application>, id :: <id>)
 => (proxy)
  //---*** Fill this in!
  #f
end method find-application-proxy;

define method compiler-database-proxy-id
    (database :: <emulator-database>, proxy)
 => (id :: false-or(<id>))
  //---*** Fill this in!
  #f
end method compiler-database-proxy-id;

define method compiler-database-proxy-id
    (database :: <emulator-database>, proxy)
 => (id :: false-or(<id>))
  //---*** Fill this in!
  #f
end method compiler-database-proxy-id;
