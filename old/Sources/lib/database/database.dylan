module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// DISK-ADDRESS

define constant <disk-address> = <integer>;

//// REFERENCE

define class <reference> (<object>) end;

//// TAGGING

define constant $number-tag-bits = 2;
define constant $tag-mask = 3;
define constant $address-tag = 0;
define constant $integer-tag = 1;
define constant $byte-character-tag = 2;

define method tagged? (object :: <integer>) #T end;
define method tagged? (object :: <byte-character>) #T end;
define method tagged? (object :: <boolean>) #T end;
define method tagged? (object :: <empty-list>) #T end;
define method tagged? (object) #F end;

define method untag (address :: <disk-address>)
  ash(address, - $number-tag-bits)
end;

define method address? (address :: <disk-address>)
  logand(address, $tag-mask) == $address-tag
end;

define method integer? (address :: <disk-address>)
  logand(address, $tag-mask) == $integer-tag
end;

define method byte-character? (address :: <disk-address>)
  logand(address, $tag-mask) == $byte-character-tag
end;

define method tag-as-integer (address :: <disk-address>)
  logior(ash(address, $number-tag-bits), $integer-tag)
end;

define method tag-as-byte-character (address :: <disk-address>)
  logior(ash(address, $number-tag-bits), $byte-character-tag)
end;

define method tag-as-address (address :: <disk-address>)
  logior(ash(address, $number-tag-bits), $address-tag)
end;

/// DATABASE

define constant $bytes-per-word = 4;

define constant $db-unbound = list("UNBOUND");

define class <database> (<object>)
  slot db-name, init-keyword: name:;
  slot db-free-pointer :: <disk-address>;
  slot db-swizzled, init-function: method () make(<object-table>) end;
  slot db-classes;
  slot db-class-table, init-function: method () make(<object-table>) end;
  slot db-proxy-class-table, init-function: method () make(<object-table>) end;
  slot db-symbols;
  slot db-symbol-table, init-function: method () make(<object-table>) end;
  slot db-roots;
  slot db-root-table, init-function: method () make(<object-table>) end;
  slot db-nil, init-value: #F;

  constant slot db-version :: <integer>, init-value: 110961;

  // should be class constants

  // version

  constant slot db-version-id,        init-value:  0;

  // free pointer

  constant slot db-free-pointer-id,   init-value:  1;

  // persistent association lists

  constant slot db-symbols-id,        init-value:  2;
  constant slot db-classes-id,        init-value:  3;
  constant slot db-roots-id,          init-value:  4;

  // booted empty-list

  constant slot db-nil-pointer-id,    init-value:  5;

  // predefines

  constant slot db-predefines-begin,  init-value:  6;

  // predefined classes

  constant slot db-byte-string-id,    init-value:  7;
  constant slot db-symbol-id,         init-value:  8;
  constant slot db-reference-id,      init-value:  9;
  constant slot db-class-id,          init-value: 10;

  // booted persistent classes

  constant slot db-empty-list-id,     init-value: 11;
  constant slot db-pair-id,           init-value: 12;

  // predefined constants

  constant slot db-true-id,           init-value: 13;
  constant slot db-false-id,          init-value: 14;
  constant slot db-unbound-id,        init-value: 15;

  constant slot db-predefines-end,    init-value: 16;

  constant slot db-header-size,       init-value: 16;
end;

// !@#$ the following should be defined in <table>
// !@#$ table.size := 0 should also work but doesn't

define method clear! (table :: <table>) 
  do (curry(remove-key!, table), key-sequence(table));
  table;
end;

define class <database-connection> (<object>)
  slot db-database, init-keyword: database:;
  slot db-stream,   init-keyword: stream:;
end;

define method position (connection :: <database-connection>)
  truncate/(stream-position(connection.db-stream), $bytes-per-word);
end;

define method position-setter (value, connection :: <database-connection>)
  stream-position(connection.db-stream) := value * $bytes-per-word;
end;

define method db-free-address (connection :: <database-connection>)
  connection.db-database.db-free-pointer;
end;

define method db-free-address-setter 
    (address :: <disk-address>, connection :: <database-connection>)
  let database = connection.db-database;
  database.db-free-pointer := address;
  db-write-at
    (connection, make-object(connection, address), database.db-free-pointer-id);
  address
end;

define method db-open-connection
    (database :: <database>, #key initialize? = #t)
  let stream
    = make(<file-stream>, locator: database.db-name, 
           direction: #"input-output", element-type: <byte>);
  /*
  let stream = open
                 (make(<byte-vector>, size: 10000, fill: 0), 
                  element-type: #"byte");
   */
  let connection = 
    make(<database-connection>, database: database, stream: stream);
  boot-predefines(connection);
  if (initialize?)
    db-initialize(connection);
  end;
  connection;
end;

define method db-close (connection :: <database-connection>)
  close(connection.db-stream);
end;

define method db-setup (connection :: <database-connection>)
  let database = connection.db-database;
  db-free-address(connection) := database.db-header-size;
  position(connection) := database.db-predefines-begin;
  for (i from database.db-predefines-begin below database.db-predefines-end)
    db-write(connection, 0);
  end;
  position(connection) := connection.db-database.db-header-size;
  db-boot-empty-list(connection);
end;

define constant <pathname> = <string>;

define method db-create (pathname :: <pathname>)
  let database :: <database> = make(<database>, name: pathname);
  let connection = db-open-connection(database, initialize?: #F);
  db-setup(connection);
  connection
end;

define method db-open (pathname :: <pathname>)
  let database :: <database> = make(<database>, name: pathname);
  let connection = db-open-connection(database);
  database.db-free-pointer := 
    untag(db-read-at(connection, database.db-free-pointer-id));
  connection
end;

define method db-allocate
    (connection :: <database-connection>, size :: <integer>)
  let address = connection.db-free-address;
  connection.db-free-address := address + size;
  db-format("ALLOCATING %d @ %d\n", size, address);
  address
end;

define method db-deallocate
    (connection :: <database-connection>, address :: <disk-address>)
  db-format("DEALLOCATING %d @ %d\n", size, address);
  address
end;

/// READING

/// LOW LEVEL

define method db-byte-read (connection :: <database-connection>)
  read-element(connection.db-stream);
end;

define method db-byte-read-at
    (connection :: <database-connection>, byte-address :: <disk-address>)
  stream-position(connection.db-stream) := byte-address;
  db-byte-read(connection);
end;

define method encode-bytes
   (b1 :: <integer>, b2 :: <integer>, b3 :: <integer>, b4 :: <integer>)
  logior(logior(ash(b1, 24), ash(b2, 16)), logior(ash(b3, 8), b4)); 
end;

define method db-read (connection :: <database-connection>)
  let stream = connection.db-stream;
  let address = position(connection);
  let b1 = read-element(stream);
  let b2 = read-element(stream);
  let b3 = read-element(stream);
  let b4 = read-element(stream);
  let value = encode-bytes(b1, b2, b3, b4);
  db-format("READING %d @ %d\n", value, address);
  value 
end;

define method db-read-at
    (connection :: <database-connection>, address :: <disk-address>)
  position(connection) := address;
  db-read(connection);
end;

define method db-read-address-at
    (connection :: <database-connection>, address :: <disk-address>)
  untag(db-read-at(connection, address));
end;

define method db-read-address (connection :: <database-connection>)
  untag(db-read(connection));
end;

/// HIGH LEVEL

define method read-object-using-class
    (connection :: <database-connection>, class :: subclass(<byte-string>))
  db-format("READING BYTE-STRING\n");
  let size = read-object(connection);
  let string = make(<byte-string>, size: size);
  for (i from 0 below size)
    string[i] := read-object(connection);
  end;
  db-format("READ %=\n", string);
  string;
end;

define method read-object-using-class
    (connection :: <database-connection>, class :: subclass(<symbol>))
  db-format("READING SYMBOL\n");
  let symbol = as(<symbol>, read-object(connection));
  db-format("READ %=\n", symbol);
  symbol; 
end;

define method read-object-using-class
    (connection :: <database-connection>, class :: subclass(<reference>))
  db-format("READING REFERENCE\n");
  let name = read-object(connection);
  let module = read-object(connection);
  let library = read-object(connection);
  variable-value(name, module, library); 
end;

define method read-object-using-class // !@#$ <class>
    (connection :: <database-connection>, class :: subclass(<object>)) 
  db-format("READING CLASS\n");
  read-class(connection); 
end;

define method read-class (connection :: <database-connection>)
  let class = read-object(connection);
  /*
  let number-slot-descriptors = read-object(connection);
  let has-repeated-slots? = read-object(connection);
  let slot-info = make(<vector>, size: 1 + number-slot-descriptors);
  for (i from 1 up-to 1 + number-slot-descriptors)
    slot-info[i] := read-object(connection);
  end;
  */
  db-format("read-class %=\n", class);
  class; 
end;

define method swizzle 
    (connection :: <database-connection>, object, address :: <disk-address>)
  connection.db-database.db-swizzled[address] := object;
end;

define method swizzled
    (connection :: <database-connection>, address :: <disk-address>, 
     #rest all-keys, #key default)
  apply(element, connection.db-database.db-swizzled, address, all-keys);
end;

define method read-object-at
    (connection :: <database-connection>, address :: <disk-address>)
  let saved-position = position(connection);
  block ()
    case
      address?(address) =>
	let address = untag(address);
	let swizzled = connection.db-database.db-swizzled;
	// db-format("ADDRESS = %d\n", address);
	let swizzled-value = element(swizzled, address, default: not-found());
	if (found?(swizzled-value))
	  db-format("SWIZZLED-VALUE %=\n", swizzled-value);
	  swizzled-value;
	else
	  position(connection) := address;
	  let class = read-class(connection);
	  let object = read-object-using-class(connection, class);
          swizzle(connection, object, address);
	  object;
	end;
      integer?(address) =>
	untag(address);
      byte-character?(address) =>
	as(<byte-character>, untag(address));
    end;
  cleanup
    position(connection) := saved-position;
  end; 
end;

define method read-object (connection :: <database-connection>)
  let address = db-read(connection);
  read-object-at(connection, address);
end;
 
/// WRITING

/// LOW LEVEL

define method db-byte-write
    (connection :: <database-connection>, value :: <integer>)
  write-element(connection.db-stream, value);
end;

define method db-byte-write-at
    (connection :: <database-connection>, value :: <integer>,
     byte-address :: <disk-address>)
  stream-position(connection.db-stream) := byte-address;
  db-byte-write(connection, value);
end;

define method decode-bytes (value :: <integer>)
 let b1 = logand(ash(value, -24), 255);
 let b2 = logand(ash(value, -16), 255);
 let b3 = logand(ash(value, -8), 255);
 let b4 = logand(value, 255);
 values(b1, b2, b3, b4); 
end;

define method db-write
    (connection :: <database-connection>, value :: <integer>)
  let stream = connection.db-stream;
  db-format("WRITING %d @ %d\n", value, position(connection));
  let (b1, b2, b3, b4) = decode-bytes(value);
  write-element(stream, b1);
  write-element(stream, b2);
  write-element(stream, b3);
  write-element(stream, b4);
end;

define method db-write-at 
    (connection :: <database-connection>, value :: <integer>, 
     address :: <disk-address>)
  position(connection) := address;
  db-write(connection, value);
end;

define method write-address
    (connection :: <database-connection>, address :: <disk-address>)
  db-write(connection, tag-as-address(address)); 
end;

define method write-address-at
    (connection :: <database-connection>, address :: <disk-address>,
     at-address :: <disk-address>)
  db-write-at(connection, tag-as-address(address), at-address);
end;

/// HIGH LEVEL

define method db-force-output (connection :: <database-connection>)
  force-output(connection.db-stream);
end;

define method db-allocate-instance
    (connection :: <database-connection>, size :: <integer>, 
     class-id :: <disk-address>)
  let address = db-allocate(connection, size);
  write-address-at(connection, class-id, address);
  address;
end;

define method make-object
    (connection :: <database-connection>, object :: <integer>)
  db-format("MAKING INTEGER %=\n", object);
  tag-as-integer(object);
end;

define method make-object 
    (connection :: <database-connection>, object :: <byte-character>)
  db-format("WRITING BYTE-CHARACTER %=\n", object);
  tag-as-byte-character(as(<integer>, object));
end;

define method make-object
    (connection :: <database-connection>, object :: <boolean>)
  db-format("MAKING BOOLEAN %=\n", object);
  let database = connection.db-database;
  tag-as-address
    (if (object) database.db-true-id else database.db-false-id end);
end;

define method make-object
    (connection :: <database-connection>, object == $db-unbound)
  db-format("MAKING UNBOUND\n");
  tag-as-address(connection.db-database.db-unbound-id);
end;

define method make-object
    (connection :: <database-connection>, object :: <byte-string>)
  db-format("MAKE BYTE-STRING %=\n", object);
  let address = 
    db-allocate-instance
      (connection, 2 + object.size, connection.db-database.db-byte-string-id);
  db-write-at(connection, make-object(connection, object.size), address + 1);
  for (character in object, address from address + 2)
    db-write-at(connection, make-object(connection, character), address);
  end;
  tag-as-address(address);
end;

define method db-add-association 
    (connection :: <database-connection>, address :: <disk-address>, 
     key, value, 
     db-list :: <function>, db-list-setter :: <function>, 
     db-table :: <function>)
  let database = connection.db-database;
  database.db-table[key] := value;
  let association = persistent-pair(connection, key, value);
  let old-list = database.db-list;
  let new-list = persistent-pair(connection, association, old-list);
  write-address-at(connection, address-of(new-list), address);
  database.db-list  := new-list;
end;

define method persistent-add-symbol
    (connection :: <database-connection>, 
     symbol :: <symbol>, symbol-address :: <disk-address>)
  db-add-association
    (connection, connection.db-database.db-symbols-id, symbol, symbol-address, 
     db-symbols, db-symbols-setter, db-symbol-table);
end;

define method make-object
    (connection :: <database-connection>, object :: <symbol>)
  db-format("MAKING SYMBOL %=\n", object);
  let database = connection.db-database;
  let symbol-address = element(database.db-symbol-table, object, default: #F);
  if (symbol-address)
    tag-as-address(symbol-address);
  else
    let address = db-allocate-instance(connection, 2, database.db-symbol-id);
    db-write-at
      (connection, make-object(connection, as(<string>, object)), address + 1);
    persistent-add-symbol(connection, object, address);
    tag-as-address(address);
  end;
end;

define method make-reference (connection :: <database-connection>, object)
  let database = connection.db-database;
  let address = db-allocate-instance(connection, 4, database.db-reference-id);
  let (variable-name, module-name, library-name) = locate-variable(object);
  db-format("MAKING REFERENCE @ %d %= %= %=\n", address,
            variable-name, module-name, library-name);
  db-write-at(connection, make-object(connection, variable-name), address + 1);
  db-write-at(connection, make-object(connection, module-name), address + 2);
  db-write-at(connection, make-object(connection, library-name), address + 3);
  tag-as-address(address);
end;

define method make-object // !@#$ <class>
    (connection :: <database-connection>, object :: subclass(<object>)) 
  let database = connection.db-database;
  let address = db-allocate-instance(connection, 2, database.db-class-id);
  db-format("MAKING CLASS %= @ %d\n", object, address);
  db-write-at(connection, make-reference(connection, object), address + 1);
  tag-as-address(address);
end;

// eof
