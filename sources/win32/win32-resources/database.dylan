Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Resource tables

define sealed class <resource-table> (<table>)
end class <resource-table>;

define sealed domain make (singleton(<resource-table>));
define sealed domain initialize (<resource-table>);

define sealed method resource-equal
    (id1 :: <raw-resource-id>, id2 :: <raw-resource-id>) => (equal? :: <boolean>)
  pointer-address(id1) = pointer-address(id2)
end method resource-equal;

define sealed method resource-hash
    (id :: <raw-resource-id>, hash-state :: <hash-state>)
 => (hash-value, hash-state)
  let (value, state) = object-hash(decode-resource(id), hash-state);
  values(value, state)
end method resource-hash;

define sealed method table-protocol
    (table :: <resource-table>)
 => (test-function :: <function>, hash-function :: <function>)
  values(resource-equal, resource-hash)
end method table-protocol;


/// Resource databases

define abstract class <resource-database> (<object>)
end class <resource-database>;

define generic processing-type
    (database :: <resource-database>, type :: <resource-type>);

define generic store-resource-name
    (database :: <resource-database>, name :: <resource-id>) => ();

define generic store-resource-details
    (database :: <resource-database>,
     handle :: <HANDLE>, resource-size :: <integer>, language-id :: <integer>) => ();


define sealed class <win32-resource-database> (<resource-database>)
  constant slot %resources :: <resource-table> = make(<resource-table>);
  slot %module :: <HANDLE>;		// the current instance handle
end class <win32-resource-database>;

define sealed domain make (singleton(<win32-resource-database>));
define sealed domain initialize (<win32-resource-database>);


/// Initializing resource databases

// The single resource database for the current application
define variable *resource-database* :: <win32-resource-database>
  = make(<win32-resource-database>);

// These are used for booting the application
define thread variable *current-database*   = #f;
define thread variable *current-type*       = #f;
define thread variable *current-type-table* = #f;
define thread variable *current-resource*   = #f;

define function load-default-resources
    () => (status :: <boolean>)
  let hInstance = application-instance-handle();
  *resource-database*.%module := hInstance;
  enumerate-resources(hInstance, database: *resource-database*)
end function load-default-resources;

define function describe-database ()
  for (database keyed-by type in *resource-database*.%resources)
    let decoded-type = decode-resource(type);
    format-out("Type: %=\n", decoded-type);
    format-out("Database = %=\n", database);
    for (resource keyed-by id in database)
      let resource-id = decode-resource(id);
      format-out("\tResource id %=, decoded %d\n", id, resource-id)
    end
  end
end function describe-database;


// Must be called with the four thread variables above bound...
define sealed method processing-type
    (database :: <win32-resource-database>, type :: <resource-type>)
  let raw-type = encode-resource(type);
  let type-db  = element(database.%resources, raw-type, default: #"not-found");
  when (type-db == #"not-found")
    type-db := make(<resource-table>);
    database.%resources[raw-type] := type-db
  end;
  *current-type*       := raw-type;
  *current-type-table* := type-db;
end method processing-type;


// Must be called with the four thread variables above bound...
define sealed method store-resource-name
    (database :: <win32-resource-database>, name :: <unsigned-int>) => ()
  store-new-resource(database, encode-resource(name))
end method store-resource-name;

define sealed method store-resource-name
    (database :: <win32-resource-database>, name :: <byte-string>) => ()
  store-new-resource(database, encode-resource(name))
end method store-resource-name;

define sealed method store-resource-name
    (database :: <win32-resource-database>, name :: <raw-resource-id>) => ()
  store-new-resource(database, name)
end method store-resource-name;

define sealed method store-new-resource
    (database :: <win32-resource-database>, name :: <raw-resource-id>) => ()
  let resource = make(<resource-description>, 
		      resource-id: name,
		      resource-type: *current-type*);
  let wrapper  = make(<resource-wrapper>,
		      resource: resource);
  *current-type-table*[name] := wrapper;
  *current-resource*         := resource;
end method store-new-resource;


// Must be called with the four thread variables above bound...
define sealed method store-resource-details
    (database :: <win32-resource-database>,
     handle :: <HANDLE>, resource-size :: <integer>, language-id :: <integer>) => ()
  resource-handle(*current-resource*) := handle;
  resource-size(*current-resource*)   := resource-size;
end method store-resource-details;
