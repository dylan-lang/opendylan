module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <persistent-object> (<object>)
  slot db-connection, required-init-keyword: connection:;
  slot db-address, required-init-keyword: address:;
end;

define method address-of (object :: <persistent-object>)
  object.db-address;
end;

define method number-words-from-bytes (number-bytes :: <integer>)
  let (div, rem) = truncate/(number-bytes, $bytes-per-word);
  if (rem > 0) div + 1 else div end;
end method;

define method sync
    (connection :: <database-connection>, object :: <persistent-object>)
  let class = object.object-class;
  let proxy-class = find-proxy-class(connection, class);
  let number-slot-descriptors =
    proxy-class.db-number-persistent-slot-descriptors;
  let database = connection.db-database;
  let repeated-slot-descriptor = proxy-class.db-repeated-slot-descriptor;
  let repeated-byte-slot? = proxy-class.db-repeated-slot-size == 1;
  // not quite right
  let size = if (repeated-slot-descriptor) object.size else #f end; 

  for (slot-address from address + 1,
       count from 0 below number-slot-descriptors)
    db-write-at
      (connection, make-object(connection, $db-unbound), slot-address);
  end for;

  if (repeated-slot-descriptor)
    let base-address = address + 1 + number-slot-descriptors;
    db-write-at(connection, make-object(connection, size), base-address);
    if (repeated-byte-slot?)
      for (slot-address from (base-address + 1) * $bytes-per-word,
	   count from 0 below size)
	db-byte-write-at
	  (connection, make-object(connection, object[count]), slot-address);
      end for;
    else
      for (slot-address from base-address + 1,
	   count from 0 below size)
        db-write-at
          (connection, make-object(connection, object[count]), slot-address);
      end for;
    end if;
  end if;
end method;

define method attach
    (connection :: <database-connection>, object :: <persistent-object>)
  // maybe create persistent class
  // write class slot

  let class = object.object-class;
  let proxy-class = find-proxy-class(connection, class);
  let number-slot-descriptors =
    proxy-class.db-number-persistent-slot-descriptors;
  let database = connection.db-database;
  let repeated-slot-descriptor = proxy-class.db-repeated-slot-descriptor;
  let repeated-byte-slot? = proxy-class.db-repeated-slot-size == 1;
  // not quite right
  let size = if (repeated-slot-descriptor) object.size else #f end; 
  let address = 
    if (repeated-slot-descriptor)
      db-allocate(connection, 1 + number-slot-descriptors + 1 +
                  number-words-from-bytes
                    (size * proxy-class.db-repeated-slot-size));
    else
      db-allocate(connection, 1 + number-slot-descriptors);
    end;
  db-format("ALLOCATING %= OBJECT @ %d\n", class-debug-name(class), address);
  let class-address = persistent-make-class(connection, proxy-class);
  write-address-at(connection, class-address, address);
  sync(connection, object);
end method;

define method persistent-make 
    (connection :: <database-connection>, 
     class :: subclass(<persistent-object>), #rest all-keys, #all-keys)

  // create in memory version 

  let object = 
    apply(make, class, connection: connection, address: address, all-keys);

  // create persistent version

  attach(connection, object);

  // swizzle it straight away

  connection.db-database.db-swizzled[address] := object;

  object
end;

/*
define method persistent-make 
    (connection :: <database-connection>,
     class :: subclass(<persistent-object>), 
     #rest all-keys, #key size = 0, fill = unsupplied())

  // maybe create persistent class
  // write class slot

  let proxy-class = find-proxy-class(connection, class);
  let number-slot-descriptors =
    proxy-class.db-number-persistent-slot-descriptors;
  let database = connection.db-database;
  let repeated-slot-descriptor = proxy-class.db-repeated-slot-descriptor;
  let repeated-byte-slot? = proxy-class.db-repeated-slot-size == 1;
  let address = 
    if (repeated-slot-descriptor)
      db-allocate(connection, 1 + number-slot-descriptors + 1 +
                  number-words-from-bytes
                    (size * proxy-class.db-repeated-slot-size));
    else
      db-allocate(connection, 1 + number-slot-descriptors);
    end;
  db-format("ALLOCATING %= OBJECT @ %d\n", class-debug-name(class), address);
  let class-address = persistent-make-class(connection, proxy-class);
  write-address-at(connection, class-address, address);

  // initialize required persistent slots

  for (slot-address from address + 1,
       count from 0 below number-slot-descriptors)
    db-write-at
      (connection, make-object(connection, $db-unbound), slot-address);
  end;

  if (repeated-slot-descriptor)
    let base-address = address + 1 + number-slot-descriptors;
    db-write-at(connection, make-object(connection, size), base-address);
    let fill = 
      if (unsupplied?(fill)) 
        if (repeated-byte-slot?) ' ' else #F end 
      else
        fill;
      end;
    let fill-object = make-object(connection, fill);
    if (repeated-byte-slot?)
      for (slot-address from (base-address + 1) * $bytes-per-word,
	   count from 0 below size)
	db-byte-write-at(connection, fill-object, slot-address);
      end;
    else
      for (slot-address from base-address + 1,
	   count from 0 below size)
	db-write-at(connection, fill-object, slot-address);
      end;
    end;
  end;

  // create in memory version 

  let object = 
    apply(make, class, connection: connection, address: address, all-keys);

  // swizzle it straight away

  database.db-swizzled[address] := object;

  object;
end;
*/


// !@#$ simulating repeated slots

define method initialize 
    (object :: <persistent-object>, #rest all-keys, #key size = 0)
  next-method();
  let connection = object.db-connection;
  let proxy-class = find-proxy-class(connection, object.object-class);
  let repeated-slot-descriptor = proxy-class.db-repeated-slot-descriptor;
  let repeated-byte-slot? = proxy-class.db-repeated-slot-size == 1;
  if (repeated-slot-descriptor)
    if (repeated-byte-slot?)
      repeated-slot-descriptor.slot-setter
        (make(<byte-string>, size: size), object);
    else
      repeated-slot-descriptor.slot-setter
        (make(<vector>, size: size, fill: unswizzled()), object);
    end;
  end;
end;

define constant (unswizzled, unswizzled?, swizzled?) =
  begin
    let unswizzled = list("UNSWIZZLED");
    values(method () unswizzled end,
	   method (value) value == unswizzled end,
	   method (value) value ~== unswizzled end);
  end;

define method persistent-slot-value
    (object :: <persistent-object>, 
     access :: <function>, access-setter :: <function>,
     offset :: <integer>)
  let x = object.access;
  let connection = object.db-connection;
  if (connection & unswizzled?(x))
    let new-value = 
      read-object-at
        (connection, db-read-at(connection, address-of(object) + offset + 1));
    object.access := new-value;
    new-value
  else
    x;
  end;
end;

define method persistent-slot-value-setter
    (new-value, object :: <persistent-object>,
     access-setter :: <function>, offset)
  let connection = object.db-connection;
  if (connection)
    db-write-at(connection, make-object(connection, new-value),
                address-of(object) + offset + 1);
  end if;
  object.access := new-value;
end;

define method persistent-slot-element
    (object :: <persistent-object>, 
     access :: <function>, 
     base-offset :: <integer>, offset :: <integer>)
  let v = object.access;
  let x = v[offset];
  let connection = object.db-connection;
  if (connection & unswizzled?(x))
    let new-value = 
      read-object-at
        (connection, 
         db-read-at(connection, address-of(object) + 1 + base-offset + offset));
    v[offset] := new-value;
    new-value
  else
    x;
  end;
end;

define method persistent-slot-element-setter
    (new-value, object :: <persistent-object>,
     access :: <function>, base-offset :: <integer>, offset :: <integer>)
  let connection = object.db-connection;
  let v = object.access;
  if (connection)
    db-write-at(connection, make-object(connection, new-value),
                address-of(object) + 1 + base-offset + offset);
  end if;
  v[offset] := new-value;
end;

define method persistent-byte-slot-element
    (object :: <persistent-object>, 
     access :: <function>, 
     base-offset :: <integer>, offset :: <integer>)
  object.access[offset];
end;

define method persistent-byte-slot-element-setter
    (new-value :: <byte-character>, object :: <persistent-object>,
     access :: <function>, base-offset :: <integer>, offset :: <integer>)
  let connection = object.db-connection;
  let v = object.access;
  if (connection)
    db-byte-write-at
      (connection, as(<integer>, new-value),
       (address-of(object) + 1 + base-offset) * $bytes-per-word + offset);
  end if;
  v[offset] := new-value;
end;

define method read-object-using-class
    (connection :: <database-connection>, 
     class :: subclass(<persistent-object>))
  let proxy-class = find-proxy-class(connection, class);
  let address = position(connection) - 1;
  if (proxy-class.db-repeated-slot-descriptor)
    let repeated-byte-slot? = proxy-class.db-repeated-slot-size == 1;
    let size-address = 
      address + 1 + proxy-class.db-number-persistent-slot-descriptors;
    let size = 
      read-object-at(connection, db-read-at(connection, size-address));
    let object =
      make(class, connection: connection, address: address, size: size);
    if (repeated-byte-slot?)
      let string =
	(proxy-class.db-repeated-slot-descriptor.slot-getter)(object);
      for (address from (size-address + 1) * $bytes-per-word,
	   index from 0 below size)
	string[index] :=
	  as(<byte-character>, db-byte-read-at(connection, address));
      end;
    end;
    object
  else
    make(class, connection: connection, address: address);
  end;
end;

define method make-object 
    (connection :: <database-connection>, object :: <persistent-object>)
  tag-as-address(address-of(object));
end;

// CONVERSIONS BETWEEN PERSISTENT AND EMPHEMERAL REPRESENTATIONS

define method persistent-copy (connection :: <database-connection>, x)
  x
end;

define method persistent-shallow-copy (connection :: <database-connection>, x)
  x
end;

define method ephemeral-copy
    (connection :: <database-connection>, x :: <persistent-object>)
  x
end;

define method ephemeral-shallow-copy
    (connection :: <database-connection>, x :: <persistent-object>)
  x
end;

/// TYPE MAPPING

define method persistent-object-class (object)
  object.object-class
end;

define method ephemeral-object-class (object)
  object.object-class
end;


// eof
