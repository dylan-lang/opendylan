module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BOOT ON 

define method db-initialize-persistent-association-list
    (connection :: <database-connection>, address :: <disk-address>, 
     db-list-setter :: <function>, db-table :: <function>)
  let database = connection.db-database;
  let persistent-list = 
    read-object-at(connection, db-read-at(connection, address));
  database.db-list  := persistent-list;
  let table = database.db-table;
  clear!(table);
  for (item in persistent-list)
    table[item.persistent-head] := item.persistent-tail;
  end;
end;

define method db-initialize (connection :: <database-connection>)
  let database = connection.db-database;
  let empty-list = 
    read-object-at
      (connection, db-read-at(connection, database.db-nil-pointer-id));
  database.db-nil      := empty-list;
  database.db-symbols  := empty-list;
  database.db-classes  := empty-list;
  database.db-roots    := empty-list;
  db-initialize-persistent-association-list
    (connection, database.db-symbols-id, db-symbols-setter, db-symbol-table);
  db-initialize-persistent-association-list
    (connection, database.db-classes-id, db-classes-setter, db-class-table);

  // create proxy classes

  db-format("INITIALIZING PROXY CLASSES~%");

  let proxy-class-table = database.db-proxy-class-table;
  for (class in key-sequence(database.db-class-table))
    if (subtype?(class, <persistent-object>))
      proxy-class-table[class] := make-proxy-class(connection, class);
    end;
  end;

  db-initialize-persistent-association-list
    (connection, database.db-roots-id, db-roots-setter, db-root-table);
end;

/// BOOT ON CREATION

define method boot-predefines (connection :: <database-connection>)
  let database  = connection.db-database;
  let addresses = clear!(database.db-class-table);
  let swizzled  = clear!(database.db-swizzled);
  local method boot-class (class :: <class>, address :: <disk-address>)
	  addresses[class] := address;  swizzle(connection, class, address);
	end,
        method boot-persistent-class 
            (class :: <class>, address :: <disk-address>)
          boot-class(class, address);
          find-proxy-class(connection, class);
        end;

  boot-class(<byte-string>, database.db-byte-string-id);
  boot-class(<symbol>, database.db-symbol-id);
  boot-class(<reference>, database.db-reference-id);
  boot-class(<class>, database.db-class-id);
  boot-persistent-class(<persistent-empty-list>, database.db-empty-list-id);
  boot-persistent-class(<persistent-pair>, database.db-pair-id);

  swizzle(connection, #T,          database.db-true-id);
  swizzle(connection, #F,          database.db-false-id);
  swizzle(connection, $db-unbound, database.db-unbound-id);
  values();
end;  

define method db-boot-empty-list (connection :: <database-connection>)
  let database = connection.db-database;
  let nil-id = db-allocate(connection, 3);
  write-address-at(connection, nil-id, database.db-nil-pointer-id);
  write-address-at(connection, database.db-empty-list-id, nil-id); 
  write-address-at(connection, nil-id, nil-id + 1); 
  write-address-at(connection, nil-id, nil-id + 2); 
  write-address-at(connection, nil-id, database.db-symbols-id);
  write-address-at(connection, nil-id, database.db-classes-id);
  write-address-at(connection, nil-id, database.db-roots-id);

  let empty-list = 
    make(<persistent-empty-list>, connection: connection, address: nil-id);
  database.db-nil      := empty-list;
  database.db-symbols  := empty-list;
  database.db-classes  := empty-list;
  database.db-roots    := empty-list;
end;

// eof
