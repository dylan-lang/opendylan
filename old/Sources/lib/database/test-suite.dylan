module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// EASY CREATION

define variable c = #f;
define variable d = #f;

define method db-reopen (connection :: <database-connection>) 
  db-close(connection);
  db-open(connection.db-database.db-name);
end;

define method mkdb () 
  if (c) 
    c := db-reopen(c);
  else
    c := db-create(as(<pathname>, "d")); 
  end;
  d := c.db-database; 
  c;
end;

define variable *db-debug?* = #F;

define variable *db-dump?* = #F;

define method debug ()
  *db-debug?* := #T; *db-dump?*  := #T;
end;

define method no-debug ()
  *db-debug?* := #F; *db-dump?*  := #F;
end;

define method db-format (format-string :: <string>, #rest arguments)
  if (*db-debug?*)
    apply(format-out, format-string, arguments);
  end;
end;

/// BUILT-IN OBJECTS

define method read-write
    (connection :: <database-connection>, write :: <function>, read :: <function>)
  let swizzled = connection.db-database.db-swizzled;
  let saved-swizzles = key-sequence(swizzled);
  block ()
    address := write(connection);
    // db-format("CREATED AT %d/n", untag(address));
    if (*db-dump?*)
      dump(connection);
    end;
    let object = read(connection, address);
    object
  cleanup
    for (key in key-sequence(swizzled))
      unless (member?(key, saved-swizzles))
        remove-key!(swizzled, key);
      end;
    end;
  end
end;

define method test
    (connection :: <database-connection>, prompt :: <function>,
     write :: <function>, read :: <function>, test :: <function>)
  prompt();
  let result = read-write(connection, write, read);
  if (test(result))
    format-out("SUCCESS\n");
  else
    format-out("FAILURE %=\n", result);
  end;    
end;

define method double-test
    (connection :: <database-connection>, prompt :: <function>,
     write :: <function>, read :: <function>, test :: <function>)
  let fresh-result = #F;
  local double-read (connection, address)
          let object = read(connection, address);
          let new-connection = db-open(connection.db-database.db-name);
          fresh-result := read(new-connection, address);
          db-close(new-connection);
          object;
        end,
        double-test (object)
          test(result) & test(fresh-result);
        end;
  test(connection, prompt, write, double-read, double-test);
end;

define method primitive-test
    (connection :: <database-connection>, object)
  test(connection, 
       method () format-out("TESTING %= ... ", object) end,
       method (c) make-object(c, object) end,
       method (c,addr) read-object-at(c, addr) end,
       method (result) object = result end);
end;

define method primitive-tests (connection :: <database-connection>)
  primitive-test(connection, 1);
  primitive-test(connection, 'a');
  primitive-test(connection, #t);
  primitive-test(connection, #f);
  primitive-test(connection, "xyz");
  primitive-test(connection, #"abc");
  primitive-test(connection, <pair>); 
end;

/// PERSISTENT-PAIRS

define method \= (x :: <pair>, y :: <persistent-pair>)
  x.head = y.persistent-head & x.tail = y.persistent-tail;
end;

define method \= (x :: <empty-list>, y :: <persistent-empty-list>)
  #T;
end;

define method copy-test (connection :: <database-connection>, object)
  let copy = persistent-copy(connection, object);
  test(connection, 
       method () format-out("TESTING %= ... ", object) end,
       method (c) tag-as-address(address-of(copy)) end,
       method (c,addr) read-object-at(c, addr) end,
       method (result) object = result end);
end;

define method pair-tests (connection :: <database-connection>)
  copy-test(connection, pair(1, 2));
  copy-test(connection, pair(1, pair(2, 3)));
end;

/// PERSISTENT-SIMPLE-OBJECT-VECTORS

define method \= (x :: <vector>, y :: <persistent-vector>)
  every?(\=, x, y);
end;

define method vector-tests (connection :: <database-connection>)
  copy-test(connection, vector(1, 2));
  copy-test(connection, vector(1, 2, 3));
end;

/// PERSISTENT-BYTE-STRINGS

define method byte-string-tests (connection :: <database-connection>)
  copy-test(connection, "abc");
  copy-test(connection, "");
end;

/// PERSISTENT VARIABLES

define method root-test
    (connection :: <database-connection>, key :: <symbol>, value)
  let result = #F;
  test(connection, 
       method () format-out("TESTING KEY %= VALUE %= ... ", key, value) end,
       method (c) root(connection, key) := value end,
       method (c,addr) root(connection, key) end,
       method (result) result = value end);
end;

define method root-tests (connection :: <database-connection>)
  root-test(connection, #"Y", 99);
  root-test(connection, #"boy", #"girl");
end;

/// ALL TESTS

define method all-tests (connection :: <database-connection>)
  primitive-tests(connection);
  pair-tests(connection);
  vector-tests(connection);
  byte-string-tests(connection);
  root-tests(connection);
end;

// eof
