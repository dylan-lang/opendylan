Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method read-object-using-class-at
    (dood :: <dood>, class :: subclass(<table>), address :: <address>)
 => (res :: <table>)
  let size = read-object(dood);
  let tbl  = make(class, size: size);
  dood-register-read-object(dood, tbl, address); 
  for (i :: <integer> from 0 below size)
    let value = read-object(dood);
    let key   = read-object(dood);
    tbl[key] := value;
  end for;
  tbl
end method;

define inline function walk-table-slots
    (dood :: <dood>, info :: <walk-info>, object :: <table>, class :: <class>)
  walk-slot(dood, info, object, class);
  walk-slot(dood, info, object, size(object));
  for (value keyed-by key in object using dood-lazy-forward-iteration-protocol,
       i :: <integer> from 0)
    if (instance?(object, <dood-lazy-table>))
      local method mark-lazy-slot-using
		(dood :: <dood>, x, address :: <address>, 
		 slotd :: <dood-slot-descriptor>, offset :: <integer>, 
		 force? :: <boolean>)
	      make-address-proxy(dood, address, offset);
	    end method,
  	    method force-lazy-slot (table :: <dood-lazy-table>, x :: <dood-address-proxy>)
	      // FULLY RESOLVED AT THIS POINT BECAUSE OF REPEATED-SIZE
	      // THUS THIS WILL NEVER BE CALLED
	      dood-force-address-proxy(x) 
	    end method;
      let offset = i * 2 + 1;
      walk-lazy-slot
	(dood, info, object, value, offset, 
	 force-lazy-slot, mark-lazy-slot-using);
    else
      walk-slot(dood, info, object, value);
    end if;
    walk-slot(dood, info, object, key);
  end for;
end function;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <table>)
  walk-table-slots(dood, info, object, object-class(object));
end method;

define method dood-repeated-size 
    (dood :: <dood>, object :: <table>) => (res :: <integer>)
  size(object) * 2
end method;

define method dood-repeated-slot? (dood :: <dood>, class :: subclass(<table>))
  #t
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object :: subclass(<table>)) => (address :: <address>)
  1
end method;

///
/// DOOD-LAZY-TABLE
///

define dood-class <dood-lazy-table>
    (<dood-mapped-and-owned-object>, 
     <mutable-explicit-key-collection>, <stretchy-collection>)
  weak slot dood-lazy-table-resolved? = #t, 
    reinit-expression: #f;
  weak slot dood-lazy-table-source :: false-or(<dood-lazy-table>) = #f,
    init-keyword: source:;
  constant slot dood-lazy-table-data :: <table> = make(<table>), 
    init-keyword: data:;
end dood-class;
    
define method key-test (table :: <dood-lazy-table>)
  => test :: <function>;
  \=
end method;

define sealed inline method dood-lazy-table-ensure-copied (table :: <dood-lazy-table>)
  let source = dood-lazy-table-source(table);
  when (source)
    let data = dood-lazy-table-data(table);
    for (value keyed-by key in source
           using dood-lazy-forward-iteration-protocol)
      data[key] := value
    end for;
    dood-lazy-table-source(table)     := #f;
  end when;
end method;

define sealed method element
    (table :: <dood-lazy-table>, key, #key default = unsupplied()) => (value)
  dood-lazy-table-ensure-copied(table);
  let value = element(dood-lazy-table-data(table), key, default: $unfound);
  if (unfound?(value))
    if (supplied?(default))
      default
    else
      error("%= is not present as a key for %=.", key, table);
    end if
  elseif (lazy-value?(value))
    let new-value = dood-force-address-proxy(value);
    // format-out("FORCING %= @ %= = %=\n", key, proxy-address(value), new-value);
    dood-lazy-table-data(table)[key] := new-value;
  else
    value
  end if
end method element;

define sealed inline method element-setter
    (new-value, table :: <dood-lazy-table>, key) => (new-value)
  dood-lazy-table-ensure-copied(table);
  dood-lazy-table-data(table)[key] := new-value;
end method element-setter;

define sealed method remove-key! 
    (table :: <dood-lazy-table>, key) => (present? :: <boolean>)
  dood-lazy-table-ensure-copied(table);
  remove-key!(dood-lazy-table-data(table), key)
end method;

define sealed method remove-all-keys! (table :: <dood-lazy-table>)
  remove-all-keys!(dood-lazy-table-data(table))
end method;

define sealed inline method size (table :: <dood-lazy-table>) => (res :: <integer>)
  dood-lazy-table-ensure-copied(table);
  size(dood-lazy-table-data(table))
end method;

define method dood-force-table-value (table :: <dood-lazy-table>, key) 
  table[key] // TOUCH IT
end method;

define inline function default-forward-iteration-protocol 
  (table :: <dood-lazy-table>)
    => (initial-state, limit,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>)
  let (initial-state, limit, next-state, finished-state?,
       current-key, current-element, current-element-setter, copy-state) 
    = forward-iteration-protocol(dood-lazy-table-data(table));
  values(initial-state,
	 limit,
	 method (table :: <dood-lazy-table>, state) 
	   next-state(dood-lazy-table-data(table), state) 
	 end method,
	 method (table :: <dood-lazy-table>, state, limit) 
	   finished-state?(dood-lazy-table-data(table), state, limit) 
	 end method,
	 method (table :: <dood-lazy-table>, state) 
	   current-key(dood-lazy-table-data(table), state) 
	 end method,
	 method (table :: <dood-lazy-table>, state) 
	   current-element(dood-lazy-table-data(table), state) 
	 end method,
	 method (new-value, table :: <dood-lazy-table>, state) 
	   current-element-setter(new-value, dood-lazy-table-data(table), state) 
	 end method,
	 method (table :: <dood-lazy-table>, state) 
	   copy-state(dood-lazy-table-data(table), state)
	 end method)
end function default-forward-iteration-protocol;

define sealed inline method forward-iteration-protocol 
  (table :: <dood-lazy-table>)
    => (initial-state, limit,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>)
  dood-lazy-table-ensure-copied(table);
  dood-lazy-table-ensure-resolved(table);
  default-forward-iteration-protocol(table);
end method forward-iteration-protocol;

define sealed inline method dood-lazy-table-ensure-resolved (table :: <dood-lazy-table>)
  unless (dood-lazy-table-resolved?(table))
    dood-lazy-table-resolve(table);
    dood-lazy-table-resolved?(table) := #t;
  end unless;
end method;

define sealed method dood-lazy-table-resolve (table :: <dood-lazy-table>)
  for (key in key-sequence(dood-lazy-table-data(table)))
    dood-force-table-value(table, key); // resolve it
  end for;
end method;

define sealed inline method dood-lazy-forward-iteration-protocol 
  (table :: <table>)
    => (initial-state, limit,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>)
  forward-iteration-protocol(table)
end method;

define sealed inline method dood-lazy-forward-iteration-protocol 
  (table :: <dood-lazy-table>)
    => (initial-state, limit,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>)
  default-forward-iteration-protocol(table);
end method dood-lazy-forward-iteration-protocol;

define method read-object-using-class-at
    (dood :: <dood>, class :: subclass(<dood-lazy-table>), address :: <address>)
 => (res :: <dood-lazy-table>)
  let size = read-object(dood);
  let data = make(<object-table>, size: size);
  let tbl  = make(class, data: data);
  dood-register-read-object(dood, tbl, address); 
  for (i :: <integer> from 0 below size)
    let offset = i * 2 + 1;
    let value  = make-address-proxy(dood, address, offset);
    let key    = read-object-at(dood, address + offset + 2);
    // format-out("READING %= @ %=\n", key, address + offset + 1);
    data[key] := value;
  end for;
  dood-lazy-table-resolved?(tbl) := #f;
  tbl
end method;

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, object :: <dood-lazy-table>)
  walk-table-slots
    (dood, info, dood-lazy-table-data(object), object-class(object))
end method;

define method dood-repeated-size 
    (dood :: <dood>, object :: <dood-lazy-table>) => (res :: <integer>)
  // TODO: NEED THIS TO BE CONDITIONAL ON WALK-INFO 
  //       SO THAT WE CAN GET ACCURATE STATISTICS
  dood-lazy-table-ensure-copied(object);
  dood-lazy-table-ensure-resolved(object);
  dood-repeated-size(dood, dood-lazy-table-data(object))
end method;

define method dood-repeated-slot? 
    (dood :: <dood>, class :: subclass(<dood-lazy-table>)) 
 => (well? :: <boolean>)
  #t
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object :: subclass(<dood-lazy-table>))
 => (address :: <address>)
  1
end method;

/// 
/// LAZY-SYMBOL-TABLE
/// 

define dood-class <dood-lazy-key-table> (<dood-lazy-table>)
  weak slot dood-lazy-table-address :: <address> = 0;
  weak slot dood-lazy-table-size :: <integer> = 0;
  weak slot dood-lazy-table-loaded-keys :: <simple-object-vector> = #[];
  weak slot dood-lazy-table-dood-state :: false-or(<dood-state>) = #f;
end dood-class;
    
define method read-object-using-class-at
    (dood :: <dood>, class :: subclass(<dood-lazy-key-table>), 
     address :: <address>)
 => (res :: <dood-lazy-key-table>)
  let size :: <integer>            = read-object(dood);
  let tbl :: <dood-lazy-key-table> = make(class);
  dood-register-read-object(dood, tbl, address); 
  dood-lazy-table-address(tbl)     := address + 1 + 1;
  dood-lazy-table-size(tbl)        := size;
  dood-lazy-table-loaded-keys(tbl) := make(<simple-object-vector>, size: size);
  dood-lazy-table-dood-state(tbl)  := dood-state(dood);
  dood-lazy-table-resolved?(tbl)   := #f;
  tbl
end method;

define sealed inline method dood-lazy-table-ensure-copied
    (tbl :: <dood-lazy-key-table>)
  let src = dood-lazy-table-source(tbl);
  when (src)
    dood-lazy-table-resolved?(tbl) := dood-lazy-table-resolved?(src);
    let data = dood-lazy-table-data(tbl);
    for (value keyed-by key in src using dood-lazy-forward-iteration-protocol)
      data[key] := value;
    end for;
    dood-lazy-table-dood-state(tbl)  := dood-lazy-table-dood-state(src);
    dood-lazy-table-address(tbl)     := dood-lazy-table-address(src);
    dood-lazy-table-size(tbl)        := dood-lazy-table-size(src);
    dood-lazy-table-loaded-keys(tbl) := copy-sequence(dood-lazy-table-loaded-keys(src));
    dood-lazy-table-source(tbl)      := #f;
  end when;
end method;

///
/// FIRST-LAZY-TABLE
///

define dood-class <dood-first-lazy-table> (<dood-lazy-key-table>)
end dood-class;
    
define sealed inline method element
    (table :: <dood-first-lazy-table>, key, #rest all-keys, #key default) 
 => (value)
  dood-lazy-table-ensure-copied(table);
  dood-lazy-table-ensure-resolved(table);
  apply(element, dood-lazy-table-data(table), key, all-keys)
end method element;

define sealed inline method element-setter
    (new-value, table :: <dood-first-lazy-table>, key :: <symbol>) 
 => (new-value)
  dood-lazy-table-ensure-copied(table);
  dood-lazy-table-ensure-resolved(table);
  dood-lazy-table-data(table)[key] := new-value;
end method element-setter;

define sealed inline method dood-lazy-table-resolve
    (tbl :: <dood-first-lazy-table>)
  let state :: <dood-state> = dood-lazy-table-dood-state(tbl);
  let dood :: <dood>        = dood-dood-state(state);
  with-dood-state (dood, state)
    with-saved-position (dood)
      dood-position(dood) := dood-lazy-table-address(tbl);
      let data = dood-lazy-table-data(tbl);
      for (i :: <integer> from 0 below dood-lazy-table-size(tbl))
	let value = read-object(dood);
	let key   = read-object(dood);
	data[key] := value;
      end for;
      tbl
    end with-saved-position;
  end with-dood-state;
  dood-lazy-table-dood-state(tbl) := #f;
end method;

define sealed inline method dood-lazy-table-ensure-copied
    (tbl :: <dood-first-lazy-table>)
  let src = dood-lazy-table-source(tbl);
  when (src)
    dood-lazy-table-resolved?(tbl)  := #t;
    dood-lazy-table-dood-state(tbl) := #f;
    let data = dood-lazy-table-data(tbl);
    for (value keyed-by key in src)
      data[key] := value;
    end for;
    dood-lazy-table-address(tbl) := dood-lazy-table-address(src);
    dood-lazy-table-size(tbl)    := dood-lazy-table-size(src);
    dood-lazy-table-source(tbl)  := #f;
  end when;
end method;

///
/// LAZY-SYMBOL-TABLE
///

define dood-class <dood-lazy-symbol-table> (<dood-lazy-key-table>)
end dood-class;

define sealed method case-insensitive-less-than?
    (string-1 :: <byte-string>, string-2 :: <byte-string>)
 => (well? :: <boolean>)
  let min-size :: <integer> = min(string-1.size, string-2.size);
  without-bounds-checks
    iterate grovel (index :: <integer> = 0)
      if (index >= min-size)
	string-1.size < string-2.size
      else
	let x :: <byte-character> = as-lowercase(element(string-1, index));
	let y :: <byte-character> = as-lowercase(element(string-2, index));
	if (x == y)
	  grovel(index + 1)
	else
	  x < y
	end if
      end if
    end iterate
  end without-bounds-checks;
end method;

define inline function symbol-less-than?
    (x :: <symbol>, y :: <symbol>) => (well? :: <boolean>)
  case-insensitive-less-than?
    (as(<byte-string>, x), as(<byte-string>, y))
end function;

define inline function symbol-equal?
    (x :: <symbol>, y :: <symbol>) => (well? :: <boolean>)
  x = y
end function;

define function key-sequence-vector
    (x :: <table>) => (res :: <simple-object-vector>)
  let keys :: <simple-object-vector> = make(<simple-object-vector>, size: size(x));
  for (i from 0 below size(keys),
       val keyed-by key in x)
    keys[i] := key;
  end for;
  keys
end function;

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, object :: <dood-lazy-symbol-table>)
  // ALREADY ENSURE-RESOLVED DURING REPEATED-SIZE
  walk-slot(dood, info, object, object-class(object));
  walk-slot(dood, info, object, size(object));
  let keys :: <simple-object-vector>
    = key-sequence-vector(dood-lazy-table-data(object));
  sort!(keys, test: symbol-less-than?);
  for (key in keys)
    walk-slot(dood, info, object, key);
  finally
    for (key in keys)
      walk-slot(dood, info, object, object[key])
    end for;
  end for;
end method;

/*
define inline method dood-lazy-table-key-loaded?
    (table :: <dood-lazy-symbol-table>, key :: <symbol>) => (result)
  dood-lazy-table-resolved?(table) |
    // element(dood-lazy-table-loaded-keys(table), key, default: #f)
    element(dood-lazy-table-data(table), key, default: #f)
end method;
*/

define inline method dood-lazy-table-register-key-loaded
    (table :: <dood-lazy-symbol-table>, key :: <symbol>) => (result)
  // dood-lazy-table-resolved?(table) |
  //   add!(dood-lazy-table-loaded-keys(table), key)
end method;

define inline method binary-search 
    (keys, key, #key
     number-keys :: <function> = size,
     lookup :: <function> = element,
     equal? :: <function> = \=, less-than? :: <function> = \<)
 => (index :: false-or(<integer>))
  let size = number-keys(keys);
  iterate search (low :: <integer> = -1, high :: <integer> = size)
    let mid :: <integer> = floor/(low + high, 2);
    unless (low = mid)
      let try = lookup(keys, mid);
      case
	equal?(try, key)     => mid;
	less-than?(key, try) => search(low, mid);
	otherwise            => search(mid, high);
      end case;
    end unless;
  end iterate;
end method;

define inline method dood-lazy-table-key-loaded-at?
    (table :: <dood-lazy-symbol-table>, index :: <integer>) => (result)
  let keys = dood-lazy-table-loaded-keys(table);
  element(keys, index)
end method;

define inline method dood-lazy-table-register-key-loaded-at
    (key :: <symbol>, table :: <dood-lazy-symbol-table>, index :: <integer>) => (result)
  let keys = dood-lazy-table-loaded-keys(table);
  element(keys, index) := key;
end method;

define inline method dood-lazy-table-load-at
    (table :: <dood-lazy-symbol-table>, index :: <integer>) => (result)
  let state :: <dood-state> = dood-lazy-table-dood-state(table);
  let dood :: <dood>        = dood-dood-state(state);
  // with-dood-state (dood, state)
    with-saved-position (dood)
      let address = dood-lazy-table-address(table) + index;
      read-object-at(dood, address);
    end with-saved-position
  // end with-dood-state;
end method;

define inline method dood-lazy-table-load-key-at
    (table :: <dood-lazy-symbol-table>, index :: <integer>) => (result)
  dood-lazy-table-key-loaded-at?(table, index)
    | begin
        let key = dood-lazy-table-load-at(table, index);
        dood-lazy-table-register-key-loaded-at(key, table, index);
	key
      end 
end method;

define inline method dood-lazy-table-load-value-at
    (table :: <dood-lazy-symbol-table>, index :: <integer>) => (result)
  dood-lazy-table-load-at(table, index + dood-lazy-table-size(table));
end method;

define inline method dood-lazy-table-binary-search
    (table :: <dood-lazy-symbol-table>, key) => (result)
  binary-search
    (table, key, 
     number-keys: dood-lazy-table-size, 
     lookup: dood-lazy-table-load-key-at,
     equal?: symbol-equal?, less-than?: symbol-less-than?)
end method;

define sealed method element
    (table :: <dood-lazy-symbol-table>, key :: <symbol>, 
     #key default = unsupplied()) 
 => (value)
  dood-lazy-table-ensure-copied(table);
  let value = element(dood-lazy-table-data(table), key, default: $unfound);
  local method defaulted-value ()
	  if (supplied?(default))
	    default
	  else
	    error("%= is not present as a key for %=.", key, table);
	  end if
        end method;
  if (unfound?(value))
    if (dood-lazy-table-resolved?(table))
      defaulted-value();
    else
      let state :: <dood-state> = dood-lazy-table-dood-state(table);
      let dood :: <dood>        = dood-dood-state(state);
      with-dood-state (dood, state)
	let index = dood-lazy-table-binary-search(table, key);
	if (index)
	  table[key] := dood-lazy-table-load-value-at(table, index);
	else
	  defaulted-value();
	end if
      end with-dood-state;
    end if
  else
    value
  end if
end method element;

define sealed inline method element-setter
    (new-value, table :: <dood-lazy-symbol-table>, key :: <symbol>) 
 => (new-value)
  dood-lazy-table-ensure-copied(table);
  dood-lazy-table-register-key-loaded(table, key);
  dood-lazy-table-data(table)[key] := new-value;
end method element-setter;

define sealed method dood-lazy-table-resolve
    (table :: <dood-lazy-symbol-table>)
  let data = dood-lazy-table-data(table);
  let keys = dood-lazy-table-loaded-keys(table);
  // STEP KEYS FIRST CAUSE THEY'RE SEQUENTIAL ON DISK
  // DETERMINE KEYS NOT CORRESPONDING TO ALREADY PRESENT VALUES
  without-bounds-checks
    let state :: <dood-state> = dood-lazy-table-dood-state(table);
    let dood :: <dood>        = dood-dood-state(state);
    with-dood-state (dood, state)
      for (key in keys, i from 0)
	if (key) // loaded?
	  when (found?(element(table, key, default: $unfound))) 
	    keys[i] := #f; // value loaded already
	  end when;
	else
	  keys[i] := dood-lazy-table-load-key-at(table, i);
	end if;
      end for;
      // STEP VALUES SECOND CAUSE THEY'RE NEXT SEQUENTIALLY ON DISK
      for (key in keys, i from 0)
	when (key) // value not loaded already?
	  data[key] := dood-lazy-table-load-value-at(table, i);
	end when;
      end for;
    end with-dood-state;
  end without-bounds-checks;
  dood-lazy-table-dood-state(table)  := #f;
  dood-lazy-table-loaded-keys(table) := #[];
end method;

///
/// SET
///

define method read-object-using-class-at
    (dood :: <dood>, class :: subclass(<set>), address :: <address>)
 => (res :: <set>)
  let size = read-object(dood);
  let set  = make(class, size: size);
  dood-register-read-object(dood, set, address); 
  for (i :: <integer> from 0 below size)
    let key = read-object(dood);
    add!(set, key);
  end for;
  set
end method;

define inline function walk-set-slots
    (dood :: <dood>, info :: <walk-info>, object :: <set>, class :: <class>)
  walk-slot(dood, info, object, class);
  walk-slot(dood, info, object, size(object));
  for (value keyed-by key in object)
    walk-slot(dood, info, object, key);
  end for;
end function;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <set>)
  walk-set-slots(dood, info, object, object-class(object));
end method;

define method dood-repeated-size 
    (dood :: <dood>, object :: <set>) => (res :: <integer>)
  size(object)
end method;

define method dood-repeated-slot? (dood :: <dood>, class :: subclass(<set>))
  #t
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object :: subclass(<set>)) => (address :: <address>)
  1
end method;
