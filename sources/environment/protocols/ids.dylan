Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// IDs

define sealed abstract class <id> (<object>)
end class <id>;

define sealed domain make (subclass(<id>));
define sealed domain initialize (<id>);

define sealed abstract class <unique-id> (<id>)
end class <unique-id>;

define sealed abstract class <named-id> (<unique-id>)
  constant sealed slot id-name :: <string>,
    required-init-keyword: name:;
end class <named-id>;


/// ID interning

define constant $name-separator = ";";

define sealed generic id-table-and-key
    (class :: subclass(<id>), #key, #all-keys)
 => (table :: <string-table>, key :: <string>);

define method make
    (class :: subclass(<id>), #rest args, #key, #all-keys)
 => (id :: <id>)
  let (table, key) = apply(id-table-and-key, class, args);
  element(table, key, default: #f)
    | begin
        let id = next-method();
        element(table, key) := id
      end
end method make;

define function make-id-cache
    () => (cache :: <string-table>)
  make(<string-table>, weak: #"value")
end function make-id-cache;


/// Library IDs

define sealed class <library-id> (<named-id>)
end class <library-id>;

define constant $library-ids = make-id-cache();

define method id-table-and-key
    (class == <library-id>, #key name :: <string>)
 => (table :: <string-table>, key :: <string>)
  values($library-ids, name)
end method id-table-and-key;


/// Module IDs

define sealed class <module-id> (<named-id>)
  constant sealed slot id-library :: <library-id>,
    required-init-keyword: library:;
end class <module-id>;

define constant $module-ids = make-id-cache();

define method id-table-and-key
    (class == <module-id>, #key name :: <string>, library :: <library-id>)
 => (table :: <string-table>, key :: <string>)
  let library-name = id-name(library);
  let mangled-name = concatenate-as(<string>, name, $name-separator, library-name);
  values($module-ids, mangled-name)
end method id-table-and-key;


/// Definition IDs

define sealed class <definition-id> (<named-id>)
  constant sealed slot id-module :: <module-id>,
    required-init-keyword: module:;
end class <definition-id>;

define constant $definition-ids = make-id-cache();

define method id-table-and-key
    (class == <definition-id>, #key name :: <string>, module :: <module-id>)
 => (table :: <string-table>, key :: <string>)
  let module-name = id-name(module);
  let library-name = id-name(id-library(module));
  let mangled-name
    = concatenate-as(<string>,
                     name,         $name-separator,
                     module-name,  $name-separator,
                     library-name);
  values($definition-ids, mangled-name)
end method id-table-and-key;


/// Method IDs

define sealed class <method-id> (<unique-id>)
  constant sealed slot id-generic-function :: <definition-id>,
    required-init-keyword: generic-function:;
  constant sealed slot id-specializers :: <simple-object-vector>,
    required-init-keyword: specializers:;
end class <method-id>;

define constant $method-ids = make-id-cache();

define method id-table-and-key
    (class == <method-id>,
     #key generic-function :: <definition-id>,
          specializers :: <simple-object-vector>)
 => (table :: <string-table>, key :: <string>)
  let (table, key)
    = id-table-and-key(<definition-id>,
                       name: generic-function.id-name,
                       module: generic-function.id-module);
  ignore(table);
  let mangled-name = key;
  for (specializer :: <definition-id> in specializers)
    let (table, key)
      = id-table-and-key(<definition-id>,
                         name: specializer.id-name,
                         module: specializer.id-module);
    ignore(table);
    mangled-name
      := concatenate-as(<byte-string>,
                        mangled-name, $name-separator,
                        key)
  end;
  values($method-ids, mangled-name)
end method id-table-and-key;


/// Object location IDs

define sealed class <object-location-id> (<id>)
  constant sealed slot id-filename :: <file-locator>,
    required-init-keyword: filename:;
  constant sealed slot id-line-number :: <integer>,
    required-init-keyword: line-number:;
end class <object-location-id>;

define constant $object-location-ids = make-id-cache();

define method id-table-and-key
    (class == <object-location-id>,
     #key filename :: <file-locator>,
          line-number :: <integer>)
 => (table :: <string-table>, key :: <string>)
  let filename = as(<string>, filename);
  let mangled-name
    = concatenate-as(<string>,
                     filename, $name-separator,
                     integer-to-string(line-number));
  values($object-location-ids, mangled-name)
end method id-table-and-key;


/// Library object location IDs

define sealed class <library-object-location-id> (<object-location-id>)
  constant sealed slot id-library :: <library-id>,
    required-init-keyword: library:;
end class <library-object-location-id>;

define constant $library-object-location-ids = make-id-cache();

define method id-table-and-key
    (class == <library-object-location-id>,
     #key filename :: <file-locator>,
          line-number :: <integer>,
          library :: <library-id>)
 => (table :: <string-table>, key :: <string>)
  let filename = as(<string>, filename);
  let library-name = library.id-name;
  let mangled-name
    = concatenate-as(<string>,
                     library-name, $name-separator,
                     filename,     $name-separator,
                     integer-to-string(line-number));
  values($library-object-location-ids, mangled-name)
end method id-table-and-key;
