Module:    doss-internals
Author:    Eliot Miranda
Synopsis:  DOSS dumper
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <doss-dumper> (<doss-io-manager>)
  // Matches 'restored' in doss loader
  slot object-ids :: <object-table> = make(<object-table>);
  slot next-id :: <integer> = 0;
  // class -> sequence of (instance: allocation) slot descriptors
  slot class-slot-info :: <object-table> = make(<object-table>);
  slot policy :: <doss-policy> = make(<basic-doss-policy>),
    init-keyword: policy:;
  slot stream :: <stream>, 
    init-keyword: stream:;
end class <doss-dumper>;

// Top-level object dumping entry-point store-object, prepends a header 
// then dumps the object.
define method store-object (o :: <object>, dd :: <doss-dumper>) => ()
  dump-header(dd);
  dump-object(o, dd);
  patch-header(dd)
end method store-object;

define method dump-header (dd :: <doss-dumper>) => ()
  let s = dd.stream;
  let hsize = dd.header-size;
  // assert(dd.doss-version-string.size <= dd.header-size-offset, 
  //       "doss version string too big"); 
  write(s, dd.doss-version-string);
  write-fill(s, 0, dd.header-size-offset - dd.doss-version-string.size);
  write-8b(dd, hsize);
  write-24b(dd, 0);
  write-fill(s, 0, hsize - dd.header-size-offset - 4)
end method dump-header;

define method patch-header (dd :: <doss-dumper>) => ()
  let s = dd.stream;
  stream-position(s) := dd.header-size-offset + 1;
  write-24b(dd, dd.object-ids.size);
end method patch-header;

define function write-8b (dd :: <doss-dumper>, n :: <integer>) => ()
  let s = dd.stream;
  write-element(s, logand(n, 255))
end function write-8b;

define function write-24b (dd :: <doss-dumper>, n :: <integer>) => ()
  let s = dd.stream;
  write-element(s, logand(n, 255));
  write-element(s, logand(ash(n, -8), 255));
  write-element(s, logand(ash(n, -16), 255))
end function write-24b;


/// Low-level dump methods dump data types as raw bytes.

// c.f. get-int
define method dump-int
    (integer :: <integer>, code :: <integer>, dd :: <doss-dumper>) => ()
  let byte-count = integer-bytes(integer);
  let s = dd.stream;
  let max-byte-count = 63;
  // my-format("dumpint: %d code:%d bytes:%d\n", integer, code, byte-count);
  if (byte-count >= max-byte-count)
    // 0 implies byte count encoded in following int
    write-element(s, code + max-byte-count);
    dump-int(byte-count, $integer-start, dd)
  else
    write-element(s, code + byte-count)
  end;
  if (byte-count = 1)		// by far the most common case
    write-element(s, logand(integer, 255))
  elseif (byte-count = 0)
    #f
  else
    iterate dump-bytes
	(n :: <integer> = byte-count, bytes :: <integer> = integer)
      if (n > 0)
	write-element(s, logand(bytes, 255));
	dump-bytes(n - 1, ash(bytes, -8))
      end
    end iterate;
  end
end method dump-int;

// Naive number of bytes in the 2's complement rep of an integer.
define method integer-bytes (n :: <integer>) => (nbytes :: <integer>)
  if (n >= -1 & n <= 1)
    abs(n)
  else
    let normal = if (positive?(n)) ash(n, 1) else ash(-n - 1, 1) end;
    for (count from 0, bytes = normal then ash(bytes, -8),
         until: bytes = 0)
    finally
      count
    end
  end
end method integer-bytes;

/*
define method integer-bytes (integer :: <integer>) => (nbytes :: <integer>)
  if (integer >= -1 & integer <= 1)
    abs(integer)
  else
    ceiling((1 + log(if (positive?(integer))
		       1 + integer
                     else
                       0 - integer
		     end,
		     2))
	      / 8);
  end
end method integer-bytes;
*/

// c.f. get-string
define method dump-string
    (string :: <byte-string>, dd :: <doss-dumper>) => ()
  let s = dd.stream;
  // my-format("dump-string %=\n", string);
  dump-int(string.size, 0, dd);
  // Streams will take care of char->byte conversion...
  write(s, string)
end method dump-string;

// Workhorse "by reference" dumper.  Some object referenced by a module
// variable is being dumped by reference, i.e. by the name of the module
// variable referencing it. Lookup the variable name (via 
// locate-variable-via-policy) and dump the values returned.
define method dump-variable 
    (obj, dd :: <doss-dumper>, dp :: <doss-policy>) => ()
  // Typically returns 3 symbols, although clients may encode the name
  // via the locate-variable protocol as their whim dictates.
  // dump the triple to identify the module variable.
  // Perhaps this should read:
  /* 
     let (#rest identifiers) = locate-variable-via-policy(obj,dp);
     dump-int(identifiers.size, dd);
     do(rcurry(dump-object,dd),identifiers)
   */
  let (name, module, library) = locate-variable-via-policy(obj,dp);
  // my-format("dump-variable %= -> %= %= %=\n", obj, name, module, library);
  if (~ instance?(module,<symbol>)
      | module == #"nil")
    error("DOSS failed to find variable referring to object: %= (%=,%=,%=)",
	  obj,name,module,library)
  end;
  dump-variable-triplet(dd, name, module, library)
end method dump-variable;

// Flexible back-door whereby a policy can supply whatever names it wants.
define method dump-variable-triplet
    (dd :: <doss-dumper>, variable-name, module-name, library-name) => ()
  dump-object(variable-name, dd);
  dump-object(module-name, dd);
  dump-object(library-name, dd)
end method dump-variable-triplet;

// Dump-object methods for special cases (character integer booleans unbound)
define method dump-object
    (character :: <character>, dd :: <doss-dumper>) => ()
  dump-int(as(<integer>, character), $character-start, dd)
end method dump-object;

define method dump-object
    (integer :: <integer>, dd :: <doss-dumper>) => ()
  dump-int(integer, $integer-start, dd)
end method dump-object;

define method dump-object
    (boolean :: <boolean>, dd :: <doss-dumper>) => ()
  write-element(dd.stream, if (boolean) $true-code else $false-code end)
end method dump-object;

define method dump-object
    (empty-list :: <empty-list>, dd :: <doss-dumper>) => ()
  write-element(dd.stream, $empty-list-code)
end method dump-object;

define method dump-object
    (string :: <byte-string>, dd :: <doss-dumper>) => ()
  if (~check-dump-value-object-id(string, $string-code, dd))
    dump-string(string, dd)
  end
end method dump-object;

define method dump-object
    (symbol :: <symbol>, dd :: <doss-dumper>) => ()
  // my-format("dump-object(symbol) %= \n", symbol);
  unless (check-dump-value-object-id(symbol, $symbol-code, dd))
    dump-string(as(<string>, symbol), dd)
  end
end method dump-object;

define method dump-object
    (unbound == $unbound-proxy, dd :: <doss-dumper>) => ()
  write-element(dd.stream, $unbound-code)
end method dump-object;

// Methinks this is better done via dumping an apply.
define method dump-object 
    (void-element == $table-void-element, dd :: <doss-dumper>) => ()
  write-element(dd.stream, $void-code)
end method dump-object;

define method dump-object 
    (pair :: <pair>, dd :: <doss-dumper>) => ()
  unless (check-dump-object-id(pair, $pair-code, dd))
    dump-object(pair.head,dd);
    dump-object(pair.tail,dd)
  end
end method dump-object;

// Either output an object's id (because its already been seen during the
// dump, or assign the object a new id & output the object's code and new id.
define method check-dump-value-object-id
    (object, code :: <integer>, dd :: <doss-dumper>) => (old? :: <boolean>)
  // Has object been encountered earlier in traverse?
  let id = element(dd.object-ids, object, default: #f);
  if (id)
    // If so, dump object's id
    write-element(dd.stream, $object-id-code);
    dump-int(id, 0, dd);
    #t
  else
    // Assign the object an id
    // put object in table of previously encountered objects
    // put the object-definition code followed by the object's id
    // increment the next-id (replace with (output-position stream) soonish)
    // dump & traverse the object
    let new-id = dd.next-id;
    dd.object-ids[object] := new-id;
    // Eliot, do you really need this code?
    write-element(dd.stream, $val-obj-id-code); 
    dump-int(new-id, 0, dd);
    write-element(dd.stream, code);
    dd.next-id := 1 + new-id;
    #f
  end
end method check-dump-value-object-id;

define method check-dump-object-id 
    (object, code :: <integer>, dd :: <doss-dumper>) => (old? :: <boolean>)
  // Has object been encountered earlier in traverse?
  let id = element(dd.object-ids, object, default: #f);
  if (id)
    // If so, dump object's id
    write-element(dd.stream, $object-id-code);
    dump-int(id, 0, dd);
    #t
  else
    // assign the object an id
    // put object in table of previously encountered objects
    // put the object-definition code followed by the object's id
    // increment the next-id (replace with (output-position stream) soonish)
    // dump & traverse the object
    let new-id = dd.next-id;
    dd.object-ids[object] := new-id;
    write-element(dd.stream, code);
    dump-int(new-id, 0, dd);
    dd.next-id := 1 + new-id;
    #f
  end
end method check-dump-object-id;

define method dump-object
    (object, dd :: <doss-dumper>) => ()
  // Has object been encountered earlier in traverse?
  let id = element(dd.object-ids, object, default: #f);
  // my-format("dump-object a(n) %= id %=\n", object.object-class, id);
  if (id)
    // Seen the object before, dump its id
    write-element(dd.stream, $object-id-code);
    dump-int(id, 0, dd)
  // Encountering object for first time.  Should it be dumped by reference?
  // (by an expression)
  elseif (~ put-specially(object, dd.policy, dd))
      // Apparently not. So assign the object an id
      // put object in table of previously encountered objects
      // put the object-definition code followed by the object's id
      // increment the next-id (replace with (output-position stream) soonish)
      // dump & traverse the object
      let new-id = dd.next-id;
      dd.object-ids[object] := new-id;
      write-element(dd.stream, $object-code);
      dump-int(new-id, 0, dd);
      dd.next-id := 1 + new-id;
      store-and-traverse(object, dd)
  end
end method dump-object;


/// special storage methods

define method put-reference (object, dd :: <doss-dumper>) => ()
  /*
  let id = element(dd.object-ids, object, default: #f);
  // my-format("put-reference %=  id %d\n", object, id);
  if (id)
    // If so, dump object's id
    write-element(dd.stream, $object-id-code);
    dump-int(id, 0, dd)
  else
    let new-id = dd.next-id;
    write-element(dd.stream, $variable-code);
    dd.object-ids[object] := new-id;
    dump-int(new-id, 0, dd);
    dd.next-id := 1 + new-id;
    dump-variable(object, dd, dd.policy)
  end;
  */
  unless (check-dump-object-id(object, $variable-code, dd))
    dump-variable(object, dd, dd.policy)
  end
end method put-reference;

define method put-variable
    (object, dd :: <doss-dumper>, variable-name, module-name, library-name) => ()
  /*
  let id = element(dd.object-ids, object, default: #f);
  // my-format("put-variable %=  id %d\n", object, id);
  if (id)
    // If so, dump object's id
    write-element(dd.stream, $object-id-code);
    dump-int(id, 0, dd)
  else
    let new-id = dd.next-id;
    write-element(dd.stream, $variable-code);
    dd.object-ids[object] := new-id;
    dump-int(new-id, 0, dd);
    dd.next-id := 1 + new-id;
    dump-variable-triplet(dd, variable-name, module-name, library-name)
  end
  */
  unless (check-dump-object-id(object, $variable-code, dd))
    dump-variable-triplet(dd, variable-name, module-name, library-name)
  end
end method put-variable;

// N.B. Typing the function parameter to be <function> is too restrictive.
// In particular it prevents us passing through proxies for functions.
define method put-apply
    (object, dd :: <doss-dumper>, function /*:: <function>*/, #rest args) => ()
  /*
  let id = element(dd.object-ids, object, default: #f);
  // my-format("put-apply %=  id %d\n", object, id);
  if (id)
    // If so, dump object's id
    write-element(dd.stream, $object-id-code);
    dump-int(id, 0, dd)
  else
    let new-id = dd.next-id;
    write-element(dd.stream, $apply-code);
    dd.object-ids[object] := new-id;
    dump-int(new-id, 0, dd);
    dd.next-id := 1 + new-id;
    dump-int(args.size, 0, dd);
    dump-object(function, dd);
    for (arg in args)
      dump-object(arg, dd)
    end;
  end;
  */
  unless (check-dump-object-id(object, $apply-code, dd))
    dump-int(args.size, 0, dd);
    dump-object(function, dd);
    for (arg in args)
      dump-object(arg, dd)
    end
  end;
end method put-apply;

define method put-object (object, dd :: <doss-dumper>) => ()
  dump-object(object,dd)
end method put-object;

define method put-header (dd :: <doss-dumper>) => ()
  dump-header(dd)
end method put-header;

define method put-footer (dd :: <doss-dumper>) => ()
  write-element(dd.stream, $footer-code)
end method put-footer;

define method dump-repeated-object
    (obj, dd :: <doss-dumper>, repeated-obj, repeat-count :: <integer>)
 => (obj, new-repeat-count :: <integer>)
  // my-format("dump repeated %= count:%d\n", obj, repeat-count);
  if (repeated-obj == obj)
    values(obj, repeat-count + 1)	// same object so bump the repeat count
  else
    if (repeated-obj ~== $unbound-proxy)
      end-repeat(dd, repeated-obj, repeat-count)
    end;
    dump-object(obj, dd);
    values(obj, 0)			// new object so reset the repeat count
  end
end method dump-repeated-object;

// Simple run-length-encoding scheme for repeated slot contents.
// If a run of more than one object occurs in consecutive repeated slots
// then the run is compressed into the object followed by the repeat code
// followed by the repeat count.  The repeat count is the number of times
// to repeat the object, so if an object occurs twice in consecutive
// repeated slots the repeat count is 1.
define method end-repeat
    (dd :: <doss-dumper>, repeated-obj, repeat-count :: <integer>) => ()
  /*
  my-format("end-repeat n:%d o:%=\n",
	    repeat-count,
	    if (repeated-object == $unbound-proxy) "unbound" else repeated-obj end);
  */
  if ((repeated-obj ~== $unbound-proxy) & repeat-count >= 1)
    write-element(dd.stream, $repeat-code);
    dump-int(repeat-count, 0, dd)
  end
end method end-repeat;

define method store-and-traverse (obj, dd :: <doss-dumper>) => ()
  let class = obj.object-class;
  put-class-description(class, dd);
  for (sd in slot-info-for-class(class, dd))
    dump-object(doss-slot-value(sd.slot-getter, obj, dd), dd)
  end;
  if (has-repeated-slots?(class))
    let limit        = number-of-repeated-slots(obj);
    let repeated-obj = $unbound-proxy; // simple run-length encoding scheme
    let repeat-count = 0;
    dump-int(limit, 0, dd);
    for (i from 0 below limit)
      let (o,c) = dump-repeated-object(doss-repeated-slot-element(obj, i, dd),
				       dd,
				       repeated-obj,
				       repeat-count);
      repeated-obj := o;
      repeat-count := c;
    end;
    end-repeat(dd, repeated-obj, repeat-count)
  end
end method store-and-traverse;


// Problems in the emulator with built-in classes (e.g.
// <simple-object-vector>) Specialising on <class> won't catch these
// classes.  Its too tedious to fix this by overriding in
// emulator-doss-out.dylan.  Will change this when appropriate.

// N.B. Its _not_ appropriate to type this as <class> since this prevents 
// us from using proxy objects for classes.
define method put-class-description 
    (class /*:: <class>*/, dd :: <doss-dumper>) => ()
  let id = element(dd.object-ids, class, default: #f);
  // my-format("put-class-description %=  id %d\n", class, id);
  if (id)
    // If so, dump object's id
    write-element(dd.stream, $object-id-code);
    dump-int(id, 0, dd)
  else
    let slot-descs = slot-info-for-class(class, dd);
    let new-id = dd.next-id;
    // Syntax of a class definition:
    // {class-code}
    //   class's object-id (encoded int)
    //   class name (variable)
    //   has repeated slots?
    //   number of slot descriptors
    //   slot descriptors
    write-element(dd.stream, $class-code);
    dd.object-ids[class] := new-id;
    dump-int(new-id, 0, dd);
    dd.next-id := 1 + new-id;
    dump-variable(class, dd, dd.policy);
    dump-object(has-repeated-slots?(class), dd);
    dump-int(size(slot-descs), 0, dd);
    for (sd in slot-descs)
      dump-variable(sd.slot-setter, dd, dd.policy)
    end
  end
end method put-class-description;

define function dumpable-slot? (sd) => (dumpable? :: <boolean>)
  let allocation = sd.slot-allocation;
  (allocation == #"instance" | allocation == #"constant")
end function dumpable-slot?;

define method all-dumpable-slot-descriptors
    (class /*:: <class>*/) => (slot-descs :: <vector>)
  as(<vector>, choose(dumpable-slot?, class.slot-descriptors))
end method all-dumpable-slot-descriptors;

define method doss-dumpable-slots
    (class /*:: <class>*/, policy :: <doss-policy>) => (slots :: <sequence>)
  all-dumpable-slot-descriptors(class)
end method doss-dumpable-slots;

define method slot-info-for-class 
    (class /*:: <class>*/, dd :: <doss-dumper>) => (slot-info :: <sequence>)
  let info = element(dd.class-slot-info, class, default: #f);
  info | (dd.class-slot-info[class] := doss-dumpable-slots(class, dd.policy))
end method slot-info-for-class;

define method has-repeated-slots? (object) => (result :: <boolean>)
  #f
end method has-repeated-slots?;

define method has-repeated-slots? 
    (string-class :: subtype(<string>)) => (result :: <boolean>)
  #t
end method has-repeated-slots?;

define method has-repeated-slots? 
    (vector-class :: subtype(<vector>)) => (result :: <boolean>)
  #t
end method has-repeated-slots?;

define method has-repeated-slots? 
    (class-byte-vector == <byte-vector>) => (result :: <boolean>)
  #t
end method has-repeated-slots?;

define method number-of-repeated-slots
    (sequence :: <mutable-sequence>) => (number :: <integer>)
  sequence.size
end method number-of-repeated-slots;

define method doss-slot-value 
    (getter, obj, dd :: <doss-dumper>) => (slot-contents :: <object>)
  if (slot-initialized?(obj, getter))
    getter(obj)
  else
    dd.unbound-proxy
  end
end method doss-slot-value;

define method doss-repeated-slot-element 
    (obj, i :: <integer>, dd :: <doss-dumper>) => (slot-contents :: <object>)
  obj[i]
end method doss-repeated-slot-element;

// eof
