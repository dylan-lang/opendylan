module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <persistent-proxy-class> (<object>)
  slot db-connection, required-init-keyword: connection:;
  slot db-class-address :: <integer>, init-keyword: class-address:;
  slot db-memory-class :: <integer>,
       required-init-keyword: memory-class:;
  slot db-number-persistent-slot-descriptors :: <integer>;
  slot db-persistent-slot-descriptors,
       required-init-keyword: persistent-slot-descriptors:;
  slot db-repeated-slot-descriptor, init-keyword: repeated-slot-descriptor:;
  slot db-repeated-slot-size, init-keyword: repeated-slot-size:, init-value: 0;
end;

define method initialize (class :: <persistent-proxy-class>, #rest all-keys)
  next-method();
  class.db-number-persistent-slot-descriptors := 
    class.db-persistent-slot-descriptors.size;
end;

define method compute-repeated-slot-descriptor
    (class :: subclass(<persistent-object>))
  block (return)
    let (repeated-slot-getter, slot-size) = repeated-slot-getter(class);
    for (descriptor in class.slot-descriptors)
      if (descriptor.slot-getter == repeated-slot-getter)
        return(descriptor, slot-size);
      end;
    end;
  end;
end;

define method compute-persistent-slot-descriptors
    (class :: subclass(<persistent-object>))
  let (repeated-slot-descriptor, repeated-slot-size) = 
    compute-repeated-slot-descriptor(class);
  values(choose(method (slot) 
                  slot.slot-allocation == instance: &
                  slot ~== repeated-slot-descriptor // no filter for pslots
                end, 
                class.slot-descriptors.tail.tail),  // remove connection & address
         repeated-slot-descriptor,
         repeated-slot-size);
end;

define method make-proxy-class
    (connection :: <database-connection>, 
     class :: subclass(<persistent-object>))
  db-format("MAKING PROXY-CLASS FOR %=\n", class-debug-name(class));
  let class-table = connection.db-database.db-class-table;
  let (slot-descriptors, repeated-slot-descriptor, repeated-slot-size) = 
     compute-persistent-slot-descriptors(class);
  make(<persistent-proxy-class>, 
       connection: connection, memory-class: class, 
       class-address: element(class-table, class, default: #F),
       persistent-slot-descriptors: slot-descriptors,
       repeated-slot-descriptor: repeated-slot-descriptor,
       repeated-slot-size: repeated-slot-size);
end;

define method find-proxy-class 
    (connection :: <database-connection>, 
     class :: subclass(<persistent-object>))
  let proxy-class-table = connection.db-database.db-proxy-class-table;
  element(proxy-class-table, class, default: #f) |
    (proxy-class-table[class] := make-proxy-class(connection, class));
end;

define method persistent-add-class
    (connection :: <database-connection>, 
     proxy-class :: <persistent-proxy-class>, class-address :: <disk-address>)
  db-format("ADDING CLASS %=\n", 
            class-debug-name(proxy-class.db-memory-class));
  db-add-association
    (connection, connection.db-database.db-classes-id, 
     proxy-class.db-memory-class, class-address, 
     db-classes, db-classes-setter, db-class-table);
end;

define method persistent-make-class
    (connection :: <database-connection>,
     proxy-class :: <persistent-proxy-class>)
  proxy-class.db-class-address |
    begin
      let class = proxy-class.db-memory-class;
      let address = 
        untag(make-object(connection, class));
      persistent-add-class(connection, proxy-class, address);
      proxy-class.db-class-address := address;
      swizzle(connection, class, address);
      address
    end;
end;

