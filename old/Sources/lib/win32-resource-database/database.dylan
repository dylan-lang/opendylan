Module:    win32-resource-database-internal 
Synopsis:  resource database
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $Id: database.dylan,v 1.1 2004/03/12 00:41:32 cgay Exp $

define class <resource-table> (<table>)
end;

define method print-object(t :: <resource-table>, stream :: <stream>)
 => ()
  let keys = t.key-sequence;
  format(stream, "table:\n");
  for(k in keys)
    format(stream, "%=,%=\n", decode-resource(k), element(t, k, default: #"no-key"))
  end;
end; 

define method resource-equal (r1 :: <raw-resource-id>, r2 :: <raw-resource-id>)
  r1.pointer-address = r2.pointer-address;
end;

define method resource-hash
    (r :: <raw-resource-id>, hash-state :: <hash-state>)
//  let (hid, hs) = object-hash(r.pointer-address, hash-state);
  let (hid, hs) = object-hash(decode-resource(r), hash-state);
//  debug-out("rid= %= hid= %=\n", decode-resource(r), hid);
  values(hid, hs)
end;

define method table-protocol(t :: <resource-table>)
  => (test-function :: <function>, hash-function :: <function>);
  values(resource-equal, resource-hash)
end;

define abstract class <resource-database> (<object>)
end;

define generic processing-type(db :: <resource-database>, 
			       type :: <resource-type>);

define generic store-resource-name(db :: <resource-database>,
				   name :: <resource-id>);

define generic store-resource-details(db :: <resource-database>,
				      handle :: <HANDLE>,
				      resource-size :: <integer>,
				      language-id :: <integer>);


define class <win32-resource-database> (<resource-database>)
  slot database :: <resource-table> = make(<resource-table>);
  slot win32-module :: <HANDLE>;
  slot accelerator-resources = #f;
  slot bitmap-resources = #f;
  slot dialog-resources = #f;
  slot font-resources = #f;
  slot fontdir-resources = #f;
  slot menu-resources = #f;
  slot rcdata-resources = #f;
  slot string-resources = #f;
  slot messagetable-resources = #f;
  slot cursor-resources = #f;
  slot group-cursor-resources = #f;
  slot icon-resources = #f;
  slot group-icon-resources = #f;
  slot version-resources = #f;
end;

define variable *resource-database* = make(<win32-resource-database>);

define function describe-database()
  for(type in *resource-database*.database.key-sequence)
    let decoded-type = decode-resource(type);
    debug-out("Type: %=\n", decoded-type);
    debug-out("Looking up DB");
    let db = element(*resource-database*.database, type, default: #"not-found");
    debug-out("DB = %=\n", db);
    if(db ~= #"not-found")
      for(res-id in db.key-sequence)
	let rid = decode-resource(res-id);
	debug-out("\tResource id: %=, %d\n", rid, rid);
      end;
    end;
  end;
end;

define variable *current-type-table* = #f;
define variable *current-type* = #f;
define variable *current-resource* = #f;

define method processing-type(db :: <win32-resource-database>, 
			      type :: <resource-type>)
  let raw-type =  encode-resource(type);
  let type-db = element(db.database, raw-type, default: #"not-found");
//  debug-out("processing database table: %=\n", map(decode-resource, db.database.key-sequence));
  unless(type-db ~= #"not-found")
    type-db := make(<resource-table>);
    db.database[raw-type] := type-db;
//    debug-out("\tprocessed database table: %=\n", map(decode-resource, db.database.key-sequence));
//    debug-out("%=\n", db.database);
//    debug-out("Creating DB for type %=\n", decode-resource(raw-type));
//    let new-db = element(db.database, raw-type, default: #"not-found");
//    debug-out("DB created: %=, %=\n", new-db, type-db);
  end;

  *current-type-table* := type-db;
  *current-type* := raw-type;
end;

define method store-new-resource(db :: <win32-resource-database>,
				 name :: <raw-resource-id>)
  let new-resource = make(<resource-description>, 
			  resource-id: name,
			  resource-type: *current-type*);

  debug-out("Adding new resource: %=\n", decode-resource(name));

  *current-type-table*[name] := 
    make(<resource-wrapper>, the-resource: new-resource);
  *current-resource* := new-resource;
end;

define method store-resource-name(db :: <win32-resource-database>,
				  name :: <integer>)
  store-new-resource(db, encode-resource(name));
end;

define method store-resource-name(db :: <win32-resource-database>,
				  name :: <byte-string>)
  store-new-resource(db, encode-resource(name));
end;

define method store-resource-name(db :: <win32-resource-database>,
				  name :: <raw-resource-id>)
  store-new-resource(db, name);
end;

define method store-resource-details(db :: <win32-resource-database>,
				     handle :: <HANDLE>,
				     res-size :: <integer>,
				     language-id :: <integer>)

  *current-resource*.resource-handle := handle;
  *current-resource*.resource-size := res-size;
// we won't deal for now with language-id
// there probably should be seperate db's for each language
end;

define function win32-load-app-instance-resources() => (status :: <boolean>);
  let hInstance = application-instance-handle();
  *resource-database*.win32-module := hInstance;
  let status = enumerate-resources(hInstance, database: *resource-database*);
  if(status = 0)
    #t
  else
    #f
  end;
end;

define method lookup-resource(type :: <predefined-resource-type>,
			      id :: <integer>)
 => (resource :: <resource>);
  let res-table = 
    *resource-database*.database[type]; // doesn't have to be encoded
  let resource-wrapper = res-table[encode-resource(id)];
  resource-wrapper.the-resource 
    := retrieve-resource(resource-wrapper.the-resource, *resource-database*);
end;

define constant *grok-resource-table* = make(<resource-table>);

define method retrieve-resource(r :: <loaded-resource>, 
				db :: <win32-resource-database>)
  => (resource :: <loaded-resource>);
  r
end;

define method retrieve-resource(r :: <resource-description>, 
				db :: <win32-resource-database>)
  => (resource :: <loaded-resource>);
  let groker = element(*grok-resource-table*, r.resource-type, default: #f);
  if(groker)
    groker(r, db.win32-module);
  else
    ErrorHandler("Resource of this type not supported yet");
  end;
end;

define method lookup-control(dialog :: <dialog-resource>, id :: <resource-id>)
 => (resource :: <control-resource>);
  let control = element(dialog.dialog-children, encode-resource(id), default: #f);
  unless(control)
    debug-out("No such control id : %=\n", id);
  end;
  control;
end;
