Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Dylan objects
///
/// Dylan objects don't seem to do much yet, but it seems like it should
/// be useful for the environment to be able to work out if an object is
/// part of the language (e.g. a class) or whether it is an abstraction
/// provided by the environment (e.g. a project)

define open abstract class <dylan-object> 
    (<environment-object>)
end class <dylan-object>;

define open abstract class <dylan-application-object>
    (<dylan-object>, <application-object>)
end class <dylan-application-object>;

define open abstract class <immediate-application-object> 
    (<dylan-application-object>)
end class <immediate-application-object>;

define open abstract class <dylan-compiler-object>
    (<dylan-object>, <compiler-object>)
end class <dylan-compiler-object>;


/// Dylan IDs

define constant $dylan-library-id
  = make(<library-id>,
	 name: "dylan");

define constant $dylan-module-id
  = make(<module-id>, 
	 name: "dylan", 
	 library: $dylan-library-id);

define constant $dylan-extensions-module-id
  = make(<module-id>, 
	 name: "dylan-extensions", 
	 library: $dylan-library-id);

define constant $dispatch-engine-module-id
  = make(<module-id>,
	 name: "dispatch-engine", 
	 library: $dylan-library-id);

define constant $<object>-id
  = make(<definition-id>, 
	 name: "<object>",
	 module: $dylan-module-id);

define constant $<class>-id
  = make(<definition-id>, 
	 name: "<class>",
	 module: $dylan-module-id);

define constant $<method>-id
  = make(<definition-id>, 
	 name: "<method>",
	 module: $dylan-module-id);

define constant $<generic-function>-id
  = make(<definition-id>, 
	 name: "<generic-function>",
	 module: $dylan-module-id);

define constant $<boolean>-id
  = make(<definition-id>, 
	 name: "<boolean>",
	 module: $dylan-module-id);


/// Characters

define class <character-object> (<immediate-application-object>)
end class <character-object>;


/// Symbols

define class <symbol-object> (<dylan-application-object>)
end class <symbol-object>;


/// Booleans

define class <boolean-object> (<immediate-application-object>)
  constant slot boolean-object-true? :: <boolean>,
    required-init-keyword: true?:;
end class <boolean-object>;

define constant $true-object  = make(<boolean-object>, true?: #t);
define constant $false-object = make(<boolean-object>, true?: #f);

define method get-environment-object-primitive-name
    (project :: <project-object>, boolean :: <boolean-object>)
 => (name :: <string>)
  if (boolean-object-true?(boolean))
    "#t"
  else
    "#f"
  end
end method get-environment-object-primitive-name;

define method application-object-class
    (project :: <project-object>, object :: <boolean-object>)
 => (class :: false-or(<class-object>))
  make-environment-object(<class-object>, project: project, id: $<boolean>-id)
end method application-object-class;


/// Numbers

define class <number-object> (<immediate-application-object>)
end class <number-object>;

define class <integer-object> (<number-object>)
end class <integer-object>;

define open generic number-object-to-string
    (server :: <server>, number :: <number-object>,
     #key prefix? :: <boolean>,
          format :: false-or(<symbol>))
 => (string :: false-or(<string>));


/// Collections

define class <collection-object> 
    (<composite-object>, <dylan-application-object>)
end class <collection-object>;

define open generic collection-size
    (server :: <server>, collection :: <collection-object>)
 => (size :: false-or(<integer>));

define open generic do-collection-keys
    (function :: <function>, server :: <server>,
     collection :: <collection-object>)
 => ();

define open generic do-collection-elements
    (function :: <function>, server :: <server>,
     collection :: <collection-object>)
 => ();

define open generic collection-keys
    (server :: <server>, collection :: <collection-object>,
     #key range)
 => (keys :: false-or(<sequence>));

define open generic collection-elements
    (server :: <server>, collection :: <collection-object>,
     #key range)
 => (elements :: false-or(<sequence>));

define class <sequence-object> (<collection-object>)
end class <sequence-object>;

define class <string-object> (<sequence-object>)
end class <string-object>;

define class <explicit-key-collection-object> 
    (<internal-object>, <collection-object>)
end class <explicit-key-collection-object>;

// Note: ranges are user objects, not internal objects, because
// the "Contents" page is the only way to browse ranges.
define class <range-object> (<user-object>, <sequence-object>)
end class <range-object>;

define class <array-object> (<sequence-object>)
end class <array-object>;

define open generic range-start
    (server :: <server>, range :: <range-object>)
 => (_start :: false-or(<number-object>));

define open generic range-end
    (server :: <server>, range :: <range-object>)
 => (_end :: false-or(<number-object>));

define open generic range-by
    (server :: <server>, range :: <range-object>)
 => (by :: false-or(<number-object>));


/// This only models non-proper lists, so it isn't a sequence object
define class <pair-object> (<user-object>)
end class <pair-object>;

define open generic pair-head
    (server :: <server>, pair :: <pair-object>)
 => (head :: false-or(<application-object>));

define open generic pair-tail
    (server :: <server>, pair :: <pair-object>)
 => (head :: false-or(<application-object>));


/// Project dispatching methods

define method number-object-to-string
    (project :: <project-object>, number :: <number-object>,
     #key prefix? :: <boolean>,
          format :: false-or(<symbol>) = #f)
 => (string :: false-or(<string>))
  let server = choose-server(project, number, error?: #t);
  if (server)
    number-object-to-string(server, number, prefix?: prefix?, format: format)
  end
end method number-object-to-string;

define method collection-size
    (project :: <project-object>, collection :: <collection-object>)
 => (size :: false-or(<integer>))
  let server = choose-server(project, collection);
  server & collection-size(server, collection)
end method collection-size;

define method do-collection-keys
    (function :: <function>, project :: <project-object>,
     collection :: <collection-object>)
 => ()
  let server = choose-server(project, collection);
  server & do-collection-keys(function, server, collection)
end method do-collection-keys;

define method do-collection-elements
    (function :: <function>, project :: <project-object>,
     collection :: <collection-object>)
 => ()
  let server = choose-server(project, collection);
  server & do-collection-elements(function, server, collection)
end method do-collection-elements;

define method collection-keys
    (project :: <project-object>, collection :: <collection-object>,
     #key range)
 => (keys :: false-or(<sequence>))
  let server = choose-server(project, collection);
  server & collection-keys(server, collection, range: range)
end method collection-keys;

define method collection-elements
    (project :: <project-object>, collection :: <collection-object>,
     #key range)
 => (elements :: false-or(<sequence>))
  let server = choose-server(project, collection);
  server & collection-elements(server, collection, range: range)
end method collection-elements;


define method range-start
    (project :: <project-object>, range :: <range-object>)
 => (_start :: false-or(<number-object>))
  let server = choose-server(project, range);
  server & range-start(server, range)
end method range-start;

define method range-end
    (project :: <project-object>, range :: <range-object>)
 => (_end :: false-or(<number-object>))
  let server = choose-server(project, range);
  server & range-end(server, range)
end method range-end;

define method range-by
    (project :: <project-object>, range :: <range-object>)
 => (by :: false-or(<number-object>))
  let server = choose-server(project, range);
  server & range-by(server, range)
end method range-by;


define method pair-head
    (project :: <project-object>, pair :: <pair-object>)
 => (head :: false-or(<application-object>))
  let server = choose-server(project, pair);
  server & pair-head(server, pair)
end method pair-head;

define method pair-tail
    (project :: <project-object>, pair :: <pair-object>)
 => (tail :: false-or(<application-object>))
  let server = choose-server(project, pair);
  server & pair-tail(server, pair)
end method pair-tail;


/// Type names

define method environment-object-type-name
    (object :: <number-object>) => (name :: <string>)
  "Number"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <integer-object>) => (name :: <string>)
  "Integer"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <character-object>) => (name :: <string>)
  "Character"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <string-object>) => (name :: <string>)
  "String"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <boolean-object>)  => (name :: <string>)
  "Boolean"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <symbol-object>) => (name :: <string>)
  "Symbol"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <collection-object>) => (name :: <string>)
  "Collection"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <pair-object>) => (name :: <string>)
  "Pair"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <range-object>) => (name :: <string>)
  "Range"
end method environment-object-type-name;
