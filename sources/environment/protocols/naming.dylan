Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Naming protocols

define open generic print-environment-object
    (stream :: <stream>, server :: <server>, object :: <environment-object>,
     #key, #all-keys)
 => ();

define open generic print-environment-object-name
    (stream :: <stream>, server :: <server>, object :: <environment-object>,
     #key, #all-keys)
 => ();

define generic print-anonymous-object
    (stream :: <stream>, server :: <server>, name :: <environment-object>,
     #key namespace)
 => ();

define generic print-module-name-object
    (stream :: <stream>, server :: <server>, name :: <module-name-object>,
     namespace :: false-or(<namespace-object>),
     #key qualify-names? :: <boolean>)
 => ();

define open generic environment-object-basic-name
    (server :: <server>, object :: <environment-object>, #key, #all-keys)
 => (name :: false-or(<string>));

define open generic environment-object-display-name
    (server :: <server>, object :: <environment-object>,
     namespace :: false-or(<namespace-object>),
     #key, #all-keys)
 => (name :: false-or(<string>));

define open generic environment-object-unique-name
    (server :: <server>, object :: <environment-object>,
     namespace :: false-or(<namespace-object>),
     #key, #all-keys)
 => (name :: <string>);


/// Print constants

define constant $default-qualify-names = #t;
define constant $unknown               = "unknown";
define constant $unknown-project       = "unknown-project";
define constant $unknown-library       = "unknown-library";
define constant $unknown-module        = "unknown-module";
define constant $unknown-object-name   = "{unknown object}";
define constant $list-separator        = ", ";
define constant $interactive-record    = "[interactive]";
define constant $warning-max-lines     = 6;


/// Basic names

define method environment-object-basic-name
    (server :: <server>, object :: <environment-object>,
     #key namespace,
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => (name :: false-or(<string>))
  let name = namespace & environment-object-name(server, object, namespace);
  if (name)
    environment-object-primitive-name(server, name)
  else
    let home-name = environment-object-home-name(server, object);
    if (home-name)
      environment-object-basic-name
	(server, home-name,
	 namespace: namespace,
	 qualify-names?: qualify-names?)
    else
      let id = environment-object-id(server, object);
      instance?(id, <named-id>) 
	& id.id-name
    end
  end
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, name :: <module-name-object>,
     #key namespace,
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => (name :: false-or(<string>))
  if (~qualify-names?
	| begin
	    let library
	      = select (namespace by instance?)
		  <library-object> => namespace;
		  <module-object>  => environment-object-library(server, namespace);
		  otherwise        => #f;
		end;
	    library & library == name-namespace(server, name)
	  end)
    environment-object-primitive-name(server, name) | $unknown-module
  end
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, name :: <binding-name-object>,
     #key namespace, 
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => (name :: false-or(<string>))
  if (~qualify-names?
	| (namespace & namespace == name-namespace(server, name)))
    environment-object-primitive-name(server, name)
  end
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, object :: <user-object>, #key, #all-keys)
 => (name == #f)
  #f
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, collection :: <collection-object>, #key, #all-keys)
 => (name == #f)
  #f
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, character :: <character-object>, #key, #all-keys)
 => (name :: <string>)
  concatenate("'", environment-object-primitive-name(server, character), "'")
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, string :: <string-object>, #key, #all-keys)
 => (name :: <string>)
  format-to-string("%=", environment-object-primitive-name(server, string))
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, symbol :: <symbol-object>, #key, #all-keys)
 => (name :: <string>)
  format-to-string("#%=", environment-object-primitive-name(server, symbol))
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, string :: <boolean-object>, #key, #all-keys)
 => (name :: <string>)
  environment-object-primitive-name(server, string)
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, number :: <number-object>, #key, #all-keys)
 => (name :: <string>)
  environment-object-primitive-name(server, number)
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, library :: <library-object>, #key, #all-keys)
 => (name :: <string>)
  environment-object-primitive-name(server, library) | $unknown-library
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, project :: <project-object>, #key, #all-keys)
 => (name :: <string>)
  environment-object-primitive-name(server, project) | $unknown-project
end method environment-object-basic-name;

define method environment-object-basic-name
    (server :: <server>, warning :: <warning-object>,
     #key namespace,
          qualify-names? :: <boolean> = $default-qualify-names,
          full-message? :: <boolean>,
     #all-keys)
 => (name :: false-or(<string>))
  ignore(namespace);
  if (~full-message?)
    case
      full-message? => compiler-warning-full-message(server, warning);
      otherwise     => compiler-warning-short-message(server, warning);
    end
  end
end method environment-object-basic-name;


/// Print methods

define method print-specializers
    (stream :: <stream>, server :: <server>, specializers :: <sequence>,
     #key namespace, 
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => ()
  write(stream, "(");
  let separator = #f;
  for (specializer in specializers,
       separator = "" then $list-separator)
    write(stream, separator);
    print-environment-object-name
      (stream, server, specializer,
       namespace: namespace, qualify-names?: qualify-names?);
  end;
  write(stream, ")")
end method print-specializers;

define method print-module-name-object
    (stream :: <stream>, server :: <server>, name :: <module-name-object>,
     library :: <library-object>,
     #key qualify-names? :: <boolean> = $default-qualify-names)
 => ()
  let name-library = name-namespace(server, name);
  let primitive-name = environment-object-primitive-name(server, name);
  if (qualify-names? & (~library | name-library ~= library))
    format(stream, "%s:%s",
	   primitive-name 
	     | $unknown-module,
	   environment-object-primitive-name(server, name-library)
	     | $unknown-library)
  else
    write(stream, primitive-name | $unknown-module)
  end
end method print-module-name-object;

define method print-module-name-object
    (stream :: <stream>, server :: <server>, name :: <module-name-object>,
     module :: <module-object>,
     #key qualify-names? :: <boolean> = $default-qualify-names)
 => ()
  let library = environment-object-library(server, module);
  print-module-name-object
    (stream, server, name, library, qualify-names?: qualify-names?)
end method print-module-name-object;

define method print-module-name-object
    (stream :: <stream>, server :: <server>, name :: <module-name-object>,
     context-namespace == #f,
     #key qualify-names? :: <boolean> = $default-qualify-names)
 => ()
  let primitive-name = environment-object-primitive-name(server, name);
  if (qualify-names?)
    let name-library = name-namespace(server, name);
    format(stream, "%s:%s",
	   primitive-name | $unknown-module,
	   environment-object-primitive-name(server, name-library)
	     | $unknown-library)
  else
    write(stream, primitive-name | $unknown)
  end
end method print-module-name-object;

define method print-source-location
    (stream :: <stream>, location :: <source-location>,
     #key line-number :: false-or(<integer>) = #f)
 => ()
  let record     = location.source-location-source-record;
  let first-line = record.source-record-start-line;
  let name
    = select (record by instance?)
        <file-source-record> =>
          locator-as-string(<string>, record.source-record-location);
        <source-record> =>
          record.source-record-name | $interactive-record;
      end;
  let (start-line, end-line)
    = if (line-number)
	values(line-number, line-number)
      else
	let start-offset = location.source-location-start-offset;
	let end-offset   = location.source-location-end-offset;
	values(start-offset.source-offset-line,
	       end-offset.source-offset-line)
      end;
  if (start-line == end-line)
    format(stream, "%s:%s", name, 
	   start-line + first-line)
  else
    format(stream, "%s:%s-%s", name, 
	   start-line + first-line,
	   end-line + first-line)
  end
end method print-source-location;


/// print-environment-object

define method print-environment-object
    (stream :: <stream>, server :: <server>, object :: <environment-object>,
     #key namespace)
 => ()
  let name = environment-object-basic-name(server, object);
  if (name)
    write(stream, name)
  else
    print-anonymous-object(stream, server, object, namespace: namespace)
  end
end method print-environment-object;


/// print-anonymous-object

define method print-anonymous-object
    (stream :: <stream>, server :: <server>, object :: <environment-object>,
     #key namespace)
 => ()
  write(stream,
	environment-object-primitive-name(server, object)
	  | $unknown-object-name)
end method print-anonymous-object;

define method print-anonymous-object
    (stream :: <stream>, server :: <server>, object :: <application-object>,
     #key namespace)
 => ()
  let application = ensure-application-server(server.server-project, object);
  let class = application-object-class(server, object);
  let user-object?
    = instance?(object, <user-object>)
        & ~instance?(object, <internal-object>);
  //---*** This is a hack, we should move all of the printing out
  //---*** of the runtime manager
  if (user-object? & class)
    write(stream, "{");
    print-environment-object-name(stream, server, class, qualify-names?: #f);
    print-anonymous-object-contents
      (stream, server, object, namespace: namespace);
    print-environment-object-id(stream, server, object);
    write(stream, "}")
  else
    if (application)
      write(stream, environment-object-primitive-name(application, object))
    else
      next-method()
    end
  end
end method print-anonymous-object;

define method print-anonymous-object-contents
    (stream :: <stream>, server :: <server>, object :: <application-object>,
     #key namespace)
 => ()
  #f
end method print-anonymous-object-contents;

define method print-anonymous-object-contents
    (stream :: <stream>, server :: <server>, condition :: <condition-object>,
     #key namespace)
 => ()
  let (string, arguments) = condition-format-options(server, condition);
  if (string & arguments)
    //---*** Ideally we'd use format, but it doesn't understand
    //---*** collection-objects, plus we need to use print-environment-object-name
    //---*** on every object with the correct context. We should
    //---*** create environment-format or something.
    // apply(format, stream, string, arguments);
    write(stream, ": ");
    print-environment-object-name(stream, server, string, namespace: namespace);
    write(stream, ", arguments: ");
    print-environment-object-name(stream, server, arguments, namespace: namespace)
  end
end method print-anonymous-object-contents;

define method print-anonymous-object-contents
    (stream :: <stream>, server :: <server>, object :: <duim-object>,
     #key namespace)
 => ()
  let name = environment-object-primitive-name(server.server-project, object);
  name & format(stream, ": %=", name)
end method print-anonymous-object-contents;

define method print-environment-object-id
    (stream :: <stream>, server :: <server>, object :: <environment-object>)
 => ()
  let project = server.server-project;
  let id = environment-object-id(project, object);
  //---*** Enable this in all editions when the runtime manager correctly
  //---*** maintains object identity.
  if (instance?(id, <integer>))
    write(stream, ": ");
    write(stream, integer-to-string(id))
  end
end method print-environment-object-id;

define generic print-environment-object-id-name
    (stream :: <stream>, server :: <server>, object :: <environment-object>,
     #key qualify-names? :: <boolean> = $default-qualify-names)
 => (printed? :: <boolean>);
    
define method print-environment-object-id-name
    (stream :: <stream>, server :: <server>, object :: <environment-object>,
     #key qualify-names? :: <boolean> = $default-qualify-names)
 => (printed? :: <boolean>)
  let id = environment-object-id(server, object);
  if (instance?(id, <named-id>))
    write(stream, id.id-name);
    if (qualify-names?)
      select (id by instance?)
	<library-id> =>
	  #f;
	<module-id> =>
	  write(stream, id.id-library.id-name);
	<definition-id> =>
	  let module = id.id-module;
	  write(stream, module.id-name);
	  write(stream, ":");
	  write(stream, module.id-library.id-name);
      end
    end;
    #t
  end
end method print-environment-object-id-name;

define method print-environment-object-id-name
    (stream :: <stream>, server :: <server>, object :: <method-object>,
     #key qualify-names? :: <boolean> = $default-qualify-names)
 => (printed? :: <boolean>)
  let gf = method-generic-function(server, object);
  if (gf)
    print-environment-object-id-name
      (stream, server, gf, qualify-names?: qualify-names?)
  end
end method print-environment-object-id-name;


/// print-environment-object-name methods

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, object :: <environment-object>,
     #rest args,
     #key namespace,
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => ()
  let basic-name = apply(environment-object-basic-name, server, object, args);
  if (basic-name)
    write(stream, basic-name)
  else
    let name = namespace & environment-object-name(server, object, namespace);
    if (name)
      write(stream, environment-object-primitive-name(server, name))
    else
      let home-name = environment-object-home-name(server, object);
      if (home-name)
	print-environment-object-name
	  (stream, server, home-name, namespace: namespace, 
	   qualify-names?: qualify-names?)
      else
	print-environment-object-id-name
	  (stream, server, object, qualify-names?: qualify-names?)
	  | print-environment-object
	      (stream, server, object, namespace: namespace)
      end
    end
  end
end method print-environment-object-name;

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, name :: <module-name-object>,
     #key namespace,
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => ()
  print-module-name-object
    (stream, server, name, namespace, qualify-names?: qualify-names?)
end method print-environment-object-name;

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, name :: <binding-name-object>,
     #key namespace: context-namespace, 
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => ()
  let primitive-name = environment-object-primitive-name(server, name);
  let namespace = name-namespace(server, name);
  if (qualify-names? & (~context-namespace | namespace ~= context-namespace))
    format(stream, "%s:", primitive-name);
    print-environment-object-name(stream, server, namespace,
				  namespace: context-namespace,
				  qualify-names?: qualify-names?)
  else
    write(stream, primitive-name)
  end
end method print-environment-object-name;

/*---*** Something smart like this might be nice, but it is better
  ---*** if the backend does it, because it can be faster.
define variable *collection-print-length* = 10;

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, collection :: <collection-object>,
     #key namespace, #all-keys)
 => ()
  let elements
    = collection-elements(server, collection, 
                          range: range(from: 0, to: *collection-print-length*));
  let separator = #f;
  for (element in elements)
    separator & write(stream, separator);
    print-environment-object-name(stream, server, element, namespace: namespace);
    unless (separator) separator := $list-separator end;
  end
end method print-environment-object-name;
*/

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, method-object :: <method-object>, 
     #key qualify-names? :: <boolean> = $default-qualify-names,
          namespace, 
          show-function-name? :: <boolean> = #t, 
     #all-keys)
 => ()
  local
    method object-name
	(object :: <environment-object>)
     => (name :: false-or(<binding-name-object>))
      if (namespace)
	environment-object-name(server, object, namespace)
      end
	| environment-object-home-name(server, object)
    end method object-name,

    method method-name
	(object :: <method-object>) 
     => (name :: false-or(<binding-name-object>))
      let gf = method-generic-function(server, object);
      (gf & object-name(gf)) | object-name(object)
    end method method-name;

  let project = server.server-project;
  if (project.project-compiler-database)
    if (show-function-name?)
      let name = method-name(method-object);
      if (name)
	print-environment-object-name
	  (stream, server, name, 
	   namespace: namespace, 
	   qualify-names?: qualify-names?);
	write(stream, " ")
      else
	print-environment-object-id-name
	  (stream, server, method-object, qualify-names?: qualify-names?)
      end
    end;
    print-specializers
      (stream, server, method-specializers(server, method-object),
       namespace: namespace,
       qualify-names?: qualify-names?)
  else
    let name = environment-object-primitive-name(project, method-object);
    if (name)
      write(stream, name)
    else
      print-anonymous-object(stream, server, method-object, namespace: namespace)
    end
  end
end method print-environment-object-name;

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, domain-object :: <domain-object>, 
     #key namespace,
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => ()
  next-method();
  write(stream, " ");
  print-specializers
    (stream, server, domain-specializers(server, domain-object),
     namespace: namespace,
     qualify-names?: qualify-names?)
end method print-environment-object-name;

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, object :: <singleton-object>,
     #key namespace,
          qualify-names? :: <boolean> = $default-qualify-names,
     #all-keys)
 => ()
  let value = singleton-value(server, object);
  write(stream, "== ");
  if (value)
    print-environment-object
      (stream, server, value,
       namespace: namespace, qualify-names?: qualify-names?)
  else
    write(stream, $unknown-object-name)
  end
end method print-environment-object-name;

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, 
     warning :: <warning-object>,
     #key namespace,
          full-message? :: <boolean>,
     #all-keys)
 => ()
  ignore(namespace);
  let location = environment-object-source-location(server, warning);
  if (full-message?)
    if (location)
      print-source-location(stream, location);
      write(stream, ": ");
    end;
    format(stream, "%s - ", environment-object-type-name(warning));
  end;
  let message
    = case
	full-message? => compiler-warning-full-message(server, warning);
	otherwise     => compiler-warning-short-message(server, warning);
      end;
  write(stream, message);
  if (full-message? & location)
    new-line(stream);
    let record       = location.source-location-source-record;
    let start-offset = location.source-location-start-offset;
    let first-line = record.source-record-start-line;
    let start-line = first-line + start-offset.source-offset-line;
    let (lines, upper-dec, lower-dec) = extract-lines(location);
    if (~lines)
      print-source-location(stream, location)
    else
      local method output-line
		(lineno :: <integer>, line :: false-or(<string>)) => ()
              format(stream, "%4s  %s\n",
                     if (lines & lines.size > 1) lineno else ' ' end,
                     if (line) line else ' ' end);
	    end method output-line;
      format(stream, "%4s  %s\n", ' ', upper-dec);
      let no-of-lines = lines.size;
      if (no-of-lines <= $warning-max-lines)
	for (line in lines, number from start-line)
	  output-line(number, line)
	end
      else
	let half-count = floor/($warning-max-lines, 2);
	for (index from 0 below half-count)
	  output-line(start-line + index, lines[index])
	end;
        format(stream, "%4s  [...]\n", ' ');
	for (index from (no-of-lines - half-count + 1) below no-of-lines)
	  output-line(start-line + index, lines[index])
	end
      end;
      format(stream, "%4s  %s", ' ', lower-dec);
    end
  end
end method print-environment-object-name;

define method print-environment-object-name
    (stream :: <stream>, server :: <server>, thread :: <thread-object>,
     #key namespace)
 => ()
  let application = ensure-application-server(server.server-project, thread);
  if (application)
    let index = thread-index(application, thread);
    let name = environment-object-primitive-name(application, thread);
    format(stream, "Thread %d: %s", index, name)
  else
    next-method()
  end
end method print-environment-object-name;


/// Displaying names

define method print-environment-object-to-string
    (server :: <server>, object :: <environment-object>,
     #rest args,
     #key namespace, #all-keys)
 => (name :: <string>)
  let stream = make(<byte-string-stream>, direction: #"output");
  apply(print-environment-object, stream, server, object, args);
  as(<byte-string>, stream-contents(stream))
end method print-environment-object-to-string;

define method print-environment-object-name-to-string
    (server :: <server>, object :: <environment-object>,
     #rest args,
     #key namespace, #all-keys)
 => (name :: <string>)
  apply(environment-object-basic-name, server, object, args)
    | begin
	let stream = make(<byte-string-stream>, direction: #"output");
	apply(print-environment-object-name, stream, server, object, args);
	as(<byte-string>, stream-contents(stream))
      end
end method print-environment-object-name-to-string;

define method environment-object-display-name
    (server :: <server>, object :: <environment-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  apply(print-environment-object-name-to-string, server, object, 
	namespace: namespace,
	args)
end method environment-object-display-name;


/// Unique names

define method environment-object-unique-name
    (server :: <server>, object :: <environment-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  apply(environment-object-display-name, server, object, namespace, args)
end method environment-object-unique-name;

define method environment-object-unique-name
    (server :: <server>, object :: <project-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  let name = next-method();
  format-to-string("%s %s",
		   as-lowercase(environment-object-type-name(object)),
		   name)
end method environment-object-unique-name;

define method environment-object-unique-name
    (server :: <server>, object :: <definition-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  let name = namespace & environment-object-name(server, object, namespace);
  if (instance?(name, <binding-name-object>))
    apply(environment-object-display-name, server, name, namespace, args)
  else
    next-method()
  end
end method environment-object-unique-name;

define method environment-object-unique-name
    (server :: <server>, module :: <module-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  environment-object-typed-name(server, module, namespace: namespace)
end method environment-object-unique-name;

define method environment-object-unique-name
    (server :: <server>, library :: <library-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  environment-object-typed-name(server, library, namespace: namespace)
end method environment-object-unique-name;

define method environment-object-unique-name
    (server :: <server>, object :: <domain-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  environment-object-typed-name(server, object, namespace: namespace)
end method environment-object-unique-name;

define method environment-object-unique-name
    (server :: <server>, function :: <method-object>,
     namespace :: false-or(<namespace-object>),
     #rest args,
     #key, #all-keys)
 => (name :: <string>)
  //--- We really shouldn't have concrete superclasses...
  if (instance?(function, <method-constant-object>))
    next-method()
  else
    environment-object-typed-name(server, function, namespace: namespace)
  end
end method environment-object-unique-name;

define function environment-object-typed-name
    (server :: <server>, object :: <definition-object>,
     #key namespace :: false-or(<namespace-object>))
 => (name :: <string>)
  let name = environment-object-display-name(server, object, namespace);
  format-to-string("%s %s",
		   as-lowercase(environment-object-type-name(object)),
		   name)
end function environment-object-typed-name;


/// Application state

define function thread-state-label 
    (project :: <project-object>, state :: <symbol>)
 => (label :: <string>)
  select (state)
    #"frozen" => "frozen";
    #"running" => "running";
    otherwise => as(<string>, state);
  end
end function thread-state-label;

define function application-state-label 
    (project :: <project-object>)
 => (label :: <string>)
  let application = project.project-application;
  let state = application & application.application-state;
  select (state)
    #f, #"uninitialized" => "uninitialized";
    #"running"           => "running";
    #"stopped"           => "paused";
    #"closed"            => "stopped";
    otherwise            => as(<string>, state);
  end
end function application-state-label;


/// Compiler warnings

define constant $newline-code = as(<integer>, '\n');

//--- stolen from dfmc-reader
//--- Should be moved to source records and shared from there.
define function extract-lines 
    (loc :: <source-location>)
 => (line-strings, upper-decorator, lower-decorator)
  let text
    = block ()
	source-record-contents(loc.source-location-source-record)
      exception (<source-record-missing>)
	#f
      end;
  if (~text)
    values(#f, #f, #f);
  else
    let line-count
      = loc.source-location-end-offset.source-offset-line
	  - loc.source-location-start-offset.source-offset-line;
    let line-one-start-index
      = compute-line-start-character
	  (text, loc.source-location-start-offset.source-offset-line);
    collecting (line-strings)
      local method walk-lines (cursor, lines)
	if (lines <= line-count)
	  let (line-string, next-cursor) = copy-next-line(text, cursor);
	  collect-into(line-strings, line-string);
	  walk-lines(next-cursor, lines + 1);
	end;
      end;
      walk-lines(line-one-start-index, 0);
      let line-strings = collected(line-strings);
      let start-col = loc.source-location-start-offset.source-offset-column;
      let end-col = loc.source-location-end-offset.source-offset-column;
      if (line-count = 0)
	values(line-strings,
	       make-closed-upper-decorator(start-col, end-col),
	       make-closed-lower-decorator(start-col, end-col))
      else
	values(line-strings,
	       make-open-upper-decorator(start-col, size(line-strings.first)),
	       make-open-lower-decorator(end-col))
      end;
    end;
  end;
end function extract-lines;

// These functions construct the "decorator" lines used to indicate
// the extent of a source location on a given line.

define function make-closed-upper-decorator 
    (start-col :: <integer>, end-col :: <integer>) => (decorator :: <string>)
  collecting (as <string>)
    for (i from 0 below start-col) collect(' ') end;
    for (i from start-col below end-col) collect('-') end;
  end;
end function make-closed-upper-decorator;

define function make-closed-lower-decorator 
    (start-col :: <integer>, end-col :: <integer>) => (decorator :: <string>)
  make-closed-upper-decorator(start-col, end-col);
end function make-closed-lower-decorator;

define function make-open-upper-decorator 
    (start-col :: <integer>, end-col :: <integer>) => (decorator :: <string>)
  make-closed-upper-decorator(start-col, end-col);
end function make-open-upper-decorator;

define function make-open-lower-decorator 
    (end-col :: <integer>) => (decorator :: <string>)
  make-closed-upper-decorator(0, end-col);
end function make-open-lower-decorator;

define function compute-line-start-character 
    (string, lines :: <integer>) => (character :: <integer>)
  let line-count = 1;
  for (i from 0, char in string, until: line-count == lines)
    if (char == $newline-code)
      line-count := line-count + 1;
    end;
  finally 
    i
  end;
end function;

define function copy-next-line (string, start) => (line-string, next-start)
  let stop = size(string);
  collecting (line-string :: <string>)
    for (i from start below stop, until: string[i] == 10)
      collect-into(line-string, as(<character>, string[i]));
    finally
      values(collected(line-string), i + 1);
    end;
  end;
end function copy-next-line;
