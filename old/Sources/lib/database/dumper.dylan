module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method class? (object :: subclass(<object>)) #T end;
define method class? (object :: <class>) #T end;
define method class? (object) #F end;

define method print-address 
    (stream, connection :: <database-connection>, address :: <disk-address>)
  let address = untag(address);
  let database = connection.db-database;
  let object = swizzled(connection, address, default: not-found());
  case
    class?(object) =>
      format-out("%s", class-debug-name(object));
    address = address-of(database.db-nil) => 
      format-out("#()");
    not-found?(object) =>
      format-out("@%d", address);
    address < database.db-header-size =>
      format-out("%s", object);
    otherwise =>
      format-out("@%d", address);
  end;
end;

define method dump-one 
    (connection :: <database-connection>, address :: <disk-address>)
  let database = connection.db-database;
  let value = db-read-at(connection, address);
  format-out("%d:	", address);
  case 
    address?(value)        => 
      print-address(*standard-output*, connection, value);
    integer?(value)        => 
      format-out(" %d", untag(value));
    byte-character?(value) => 
      format-out("'%s'", as(<character>, untag(value)));
  end;
  format-out("\n");
  address + 1;
end;

define method dump (connection :: <database-connection>)
  let old-db-debug? = *db-debug?*;
  *db-debug?* := #f;
  block ()
    iterate dump (address = 0)
      if (address < connection.db-free-address)
	dump(dump-one(connection, address));
      end;
    end;
  cleanup
    *db-debug?* := old-db-debug?;
  end;
  values(); 
end;
