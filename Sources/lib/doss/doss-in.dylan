Module:    doss-internals
Author:    Eliot Miranda
Synopsis:  DOSS loader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $not-yet-restored = #("not yet restored");

define sealed class <loaded-object-proxy> (<object>)
  slot loaded-object = $not-yet-restored;
  slot fixups :: <stretchy-vector> = make(<stretchy-vector>);
end class <loaded-object-proxy>;

define sealed class <doss-loader> (<doss-io-manager>)
  // Dumped from 'object-ids' in doss dumper
  // Slow stretchy vector for DOSS 0.2, faster for DOSS 0.3 and higher
  slot restored :: <vector> = #[];
  slot restored-base :: <integer> = 0;
  // class -> simple-object-vector of setters
  slot class-slot-info :: <table> = make(<table>);
  slot repeated-object;
  slot repeat-count :: <integer> = 0;
  // circularity hack. if a class (e.g. indirectly) refers to an
  // instance of itself we can substitute a proxy for the instance
  // and patch references up later.
  // proxy (<object>) -> rehabilitee (<object>)
  slot proxy-table :: <table-set> = make(<table-set>);
  slot stream :: <stream>,
    init-keyword: stream:;
end class <doss-loader>;

define sealed class <end-of-doss-stream> (<condition>)
end class <end-of-doss-stream>;


/// external methods

define method post-load-cleanup (object) => ()
  #f
end method post-load-cleanup;

define method post-load-cleanup (object :: <hashed-collection>) => ()
  rehash!(object)
end method post-load-cleanup;


/// interface methods for reconstructing an object from a stream.

define method add-fixup!
    (object :: <object>, fn, obj, arg) => () 
end method add-fixup!;

define method add-fixup! 
    (proxy :: <loaded-object-proxy>, fn, obj, arg) => ()
  let fixups = proxy.fixups;
  let index = fixups.size;
  fixups.size := index + 3;
  fixups[index + 0] := fn;
  fixups[index + 1] := obj;
  fixups[index + 2] := arg
end method add-fixup!;

define method do-fixups (proxy :: <loaded-object-proxy>) => ()
  let fixups = proxy.fixups;
  let index = fixups.size;
  for (i from 0 below index by 3)
    let fn  = fixups[i + 0];
    let obj = fixups[i + 1];
    let arg = fixups[i + 2];
    fn(obj, arg)
  end
end method do-fixups;

define method fetch-next-object
    (dl :: <doss-loader>) => (#rest objects)
  let (#rest objects) = dl.get-next-object;
  let restored-vector = dl.restored;
  let base            = dl.restored-base;
  let index           = restored-vector.size;
  unless (empty?(dl.proxy-table))
    let proxies = as(<vector>, dl.proxy-table);
    for (proxy in proxies)
      if (proxy.loaded-object ~== $not-yet-restored)
        // my-format("Fixing up %=(n = %=)\n", proxy.loaded-object, proxy.fixups.size);
        do-fixups(proxy);
        set-remove!(dl.proxy-table, proxy)
      end
    end
  end;
  while (index > base)
    index := index - 1;
    post-load-cleanup(restored-vector[index])
  end;
  dl.restored-base := restored-vector.size;
  apply(values, objects)
end method fetch-next-object;

define method fetch-object
    (dl :: <doss-loader>) => (#rest objects)
  check-and-skip-header(dl);
  fetch-next-object(dl)
end method fetch-object;


/// header checker

define method check-and-skip-header (dl :: <doss-loader>) => ()
  let s = dl.stream;
  let raw-version-string = read(s, dl.header-size-offset);
  case
    (copy-sequence(raw-version-string, end: $doss-0-3-version.size)
       = $doss-0-3-version) =>
      let hsize = dl.read-8b;
      let restored-object-count = dl.read-24b;
      if (hsize ~= dl.header-size)
	error("DOSS header format error - erroneous header size field")
      end;
      dl.doss-version := 3;
      dl.restored := make(<vector>, size: restored-object-count, fill: $not-yet-restored);
      read-skip(s, hsize - dl.header-size-offset - 4);
    (copy-sequence(raw-version-string, end: $doss-0-2-version.size)
       = $doss-0-2-version) =>
      let check-byte = s.peek;
      if ((check-byte < $integer-start) | (check-byte >= $character-start))
	error("DOSS header format error - erroneous header size field")
      end;
      let hsize = dl.get-next-object;
      if (hsize ~= dl.header-size)
	error("DOSS header format error - erroneous header size field")
      end;
      dl.doss-version := 2;
      dl.restored := make(<stretchy-vector>, fill: $not-yet-restored);
      read-skip(s, hsize - dl.header-size-offset - hsize.integer-bytes - 1);
    (copy-sequence(raw-version-string, end: "DOSS ".size)
       ~= as(<byte-vector>,"DOSS ")) =>
      error("Attempt to read object from non-DOSS stream");
    otherwise =>
      error("DOSS stream has different version number")
  end;
end method check-and-skip-header;

define function read-8b (dl :: <doss-loader>) => (n :: <integer>)
  let s = dl.stream;
  read-element(s)
end function read-8b;

define function read-24b (dl :: <doss-loader>) => (n :: <integer>)
  let s = dl.stream;
  read-element(s)
  + ash(read-element(s), 8)
  + ash(read-element(s), 16)
end function read-24b;


/// primary doss byte-code decoder/object reconstruction driver

define constant $byte-code-names
  = #["()", "#t", "#f",
      "object-id", "unbound", "object-def", "class-def",
      "keyword", "symbol", "variable", "string", "apply",
      "tanble-void", "value-object-def", "repeated object",
      "pair", "float", "double", "extended"];

define method get-next-object
    (dl :: <doss-loader>) => (#rest objects)
  if (dl.repeat-count > 0)
    dl.repeat-count := dl.repeat-count - 1;
    dl.repeated-object
  else
    let byte-code = read-element(dl.stream);
    /*
    my-format( "byte %D action %S\n", 
               // stream-position(dl.stream) - 1,
               byte-code,
               element($byte-code-names, byte-code, 
		       default: if (byte-code >= $integer-start)
				  if (byte-code >= $character-start)
				    "character"
				  else
				    "integer"
				  end
				else
				  "unknown!?"
				end));
    */
    if (byte-code >= $integer-start)
      if (byte-code < $character-start)
        dl.repeated-object := get-int(byte-code, dl)
      else
        dl.repeated-object := as(<character>, get-int(byte-code, dl))
      end
    elseif (byte-code = $apply-code)
      let (#rest objects) = dl.get-apply;
      unless (objects.empty?)
        dl.repeated-object := objects[0];
      end;
      apply(values, objects)
    else
      dl.repeated-object :=
	(select (byte-code) 
           $empty-list-code => #();
	   $true-code       => #t;
	   $false-code      => #f;
	   $object-id-code  => dl.get-object-id;
	   $unbound-code    => dl.unbound-proxy;
	   $object-code     => dl.get-object-definition;
	   $class-code      => dl.get-class-definition;
	   $keyword-code    => dl.get-keyword;
	   $symbol-code     => dl.get-symbol;
	   $variable-code   => dl.get-variable;
	   $string-code     => dl.get-string;
	   $void-code       => $table-void-element;
	   $val-obj-id-code => dl.get-value-object-definition;
	   $repeat-code =>
	     begin
	       dl.repeat-count := dl.get-next-int;
	       dl.get-next-object;
	     end;
	   $pair-code       => dl.get-pair;
	   // NYI:
	   // $float-code      => dl.get-float;
	   // $double-code     => dl.get-double;
	   // $extended-code   => dl.get-extended;
	   $footer-code     => signal(make(<end-of-doss-stream>));
	   otherwise =>
	     format(*standard-output*, "\nERROR: Doss Loader: corrupt doss stream\n");
	     signal(make(<end-of-doss-stream>));
	 end);
    end
  end;
end method get-next-object;


/// low-level load methods create integers, characters & strings from raw bytes

// c.f. dump-int
define method get-int
    (byte-count-code :: <integer>, dl :: <doss-loader>) => (int :: <integer>)
  let byte-count = logand(63, byte-count-code);
  if (byte-count = 1)		// by far the most common case
    let byte = read-element(dl.stream);
    if (byte >= 128) byte - 256 else byte end
  elseif (byte-count = 0)
    0
  else
    let value = 0;
    let s = dl.stream;
    iterate get-bytes (count :: <integer> = 1)
      let byte = read-element(s);
      if (byte-count = count)
	// Ugh, this is slow! big-endian faster to load
	if (byte >= 128)
	  byte := byte - 256
	end;
	// but easy to strength reduce the 8 * (count - 1)s
	logior(ash(byte, 8 * (count - 1)), value)
      else
	value := logior(ash(byte, 8 * (count - 1)), value);
	get-bytes(count + 1)
      end
    end iterate;
  end
end method get-int;

define method get-next-int
    (dl :: <doss-loader>) => (int :: <integer>)
  // my-format("get-next-int");
  let result = get-int(read-element(dl.stream), dl);
  // my-format(" => %D\n", result);
  result
end method get-next-int;

// c.f. dump-string
define method get-string
    (dl :: <doss-loader>) => (s :: <byte-string>)
  let s = dl.get-next-int;
  let string = make(<byte-string>, size: s);
  read-into!(dl.stream, s, string);
  // my-format("get-string: %s\n", string);
  string
end method get-string;

// get-keyword is obsolete (only there for emulator's sake) - 
// this method is overriden by the emulator patches.
define method get-keyword
    (dl :: <doss-loader>) => (s :: <symbol>)
  get-symbol(dl)
end method get-keyword;

define method get-symbol
    (dl :: <doss-loader>) => (s :: <symbol>)
  as(<symbol>, dl.get-string)
end method get-symbol;

// restore inserts an object in the id->object map, growing the map if necessary.
define method restore 
    (o, id :: <integer>, dl :: <doss-loader>) => (obj :: <object>)
  let restored-vector = dl.restored;
  // avoid growing restored too often by growing it in lumps
  if (size(restored-vector) <= id)
    if (dl.doss-version > 2)
      error("Dumped object count was incorrect!")
    else
      let current-size = restored-vector.size;
      size(restored-vector) := 32 + id;
      fill!(restored-vector, $not-yet-restored, start: current-size)
    end
  end;
  if (restored-vector[id] ~== $not-yet-restored)
    // must be a proxy (unless we've objects with the same id)
    // my-format("found a proxy for id %=\n", id);
    restored-vector[id].loaded-object := o
  end;
  restored-vector[id] := o
end method restore;

// c.f. dump-variable
define method get-variable
    (dl :: <doss-loader>) => (obj :: <object>)
  let id      = dl.get-next-int;
  let name    = dl.get-next-object;
  let module  = dl.get-next-object;
  let library = dl.get-next-object;
  // my-format("get-variable %d %= %= %=\n", id, name, module, library);
  restore(variable-value(name, module, library), id, dl)
end method get-variable;

// c.f. dump-variable
define method get-anonymous-variable
    (dl :: <doss-loader>) => (obj :: <object>)
  let name    = dl.get-next-object;
  let module  = dl.get-next-object;
  let library = dl.get-next-object;
  // my-format("get-anonymous-variable %= %= %=\n", name, module, library);
  variable-value(name, module, library)
end method get-anonymous-variable;

// Potential circularity problem if the result of the apply is one of its (indirect) arguments.
define method get-apply
    (dl :: <doss-loader>) => (#rest objects)
  let id        = dl.get-next-int;
  let arg-count = dl.get-next-int;
  let function  = dl.get-next-object;
  let args      = make(<vector>, size: arg-count);
  local method fixer (val, obj, index) obj[index] := val end;
  iterate get-args (i :: <integer> = 0)
    if (i < arg-count)
      let next-object = dl.get-next-object;
      add-fixup!(next-object, fixer, args, i);
      args[i] := next-object;
      get-args(i + 1)
    end
  end iterate;
  let (#rest vals) = apply(function, args);
  unless (vals.empty?)
    restore(vals[0], id, dl)
  end;
  apply(values, vals)
end method get-apply;


// c.f. dump-pair
define method get-pair
    (dl :: <doss-loader>) => (p :: <pair>)
  local method head-fixer (val, obj, index) obj.head := val end;
  local method tail-fixer (val, obj, index) obj.tail := val end;
  let id          = dl.get-next-int;
  let the-pair    = list(#());
  let next-object = #f;
  restore(the-pair, id, dl);
  next-object := dl.get-next-object;
  add-fixup!(next-object, head-fixer, the-pair, #f);
  the-pair.head := next-object;
  next-object := dl.get-next-object;
  add-fixup!(next-object, head-fixer, the-pair, #f);
  the-pair.tail := next-object;
  the-pair
end method get-pair;

// c.f. check-put-value-object
define method get-value-object-definition
    (dl :: <doss-loader>) => (obj :: <object>)
  let id     = dl.get-next-int;
  let object = dl.get-next-object;
  restore(object, id, dl)
end method get-value-object-definition;

// c.f. store-and-traverse
define method get-object-definition
    (dl :: <doss-loader>) => (obj :: <object>)
  let id = dl.get-next-int;
  let class = dl.get-next-object;
  let rpt = if (repeated-slots-stored(class, dl))
              dl.get-next-int
	    end;
  let obj = if (rpt)  
              doss-allocate-instance(class, size: rpt)
            else
              doss-allocate-instance(class)
            end;
  if (rpt)
   doss-fill-repeated-slot(obj, class, rpt)
  end;
  let unbound = dl.unbound-proxy;
  restore(obj, id, dl);
  local method fixer (val, obj, setter-func) setter-func(val, obj) end;
  let setters = dl.class-slot-info[class];
  for (i from 0 below size(setters), setter-func in setters)
    unless (i = 0)
      let next-obj = dl.get-next-object;
      add-fixup!(next-obj, fixer, obj, setter-func);
      if (unbound ~== next-obj)
        /* doss-slot-value(obj, setter-func) := next-obj; */
        setter-func(next-obj, obj)
      end
    end
  end;
  local method rpt-fixer (val, obj, index) obj[index] := val end;
  if (rpt)
    for (index from 0 below rpt)
      /* doss-repeated-slot-element(obj, index) := dl.get-next-object; */
      let next-obj = dl.get-next-object;
      add-fixup!(next-obj, rpt-fixer, obj, index);
      obj[index] := next-obj
    end
  end;
  obj
end method get-object-definition;

define method get-object-id
    (dl :: <doss-loader>) => (obj :: <object>)
  let obj-id = dl.get-next-int;
  let obj    = dl.restored[obj-id];
  if (obj == $not-yet-restored)
    // my-format("Warning id %s not yet restored.  Making proxy!\n", obj-id);
    let proxy = make(<loaded-object-proxy>);
    set-add!(dl.proxy-table, dl.restored[obj-id] := proxy);
    proxy
  else
    obj
  end
end method get-object-id;

// c.f. put-class-description
define method get-class-definition
    (dl :: <doss-loader>) => (class :: <class>)
  let class               = dl.get-variable;
  let has-repeated-slots? = dl.get-next-object;
  let num-slot-descs      = dl.get-next-int;
  let slot-info           = make(<vector>, size: num-slot-descs + 1);
  dl.class-slot-info[class] := slot-info;
  slot-info[0] := has-repeated-slots?;
  for (i from 1 below num-slot-descs + 1)
    slot-info[i] := dl.get-anonymous-variable
  end;
  class
end method get-class-definition;

define method repeated-slots-stored
    (class, dl :: <doss-loader>) => (has-repeated-slots? :: <boolean>)
  dl.class-slot-info[class][0]
end method repeated-slots-stored;

// cf put-specially (obj :: <slot-descriptor>, policy :: <basic-doss-policy>, dd :: <doss-dumper>)
define method resolve-slot-descriptor
    (getter :: <function>, class :: <class>) => (sd :: <slot-descriptor>)
  block (return)
    for (sd in class.slot-descriptors)
      if (sd.slot-getter == getter)
        return(sd)
      end
    end;
    error("DOSS could not find slot descriptor with getter %= in class %=", getter, class)
  end
end method resolve-slot-descriptor;


/// Fast(er) sets...

define constant <table-set> = <table>;

define method set-member?
    (x, set :: <table-set>) => (true? :: <boolean>)
  element(set, x, default: #f)
end method set-member?;

define method set-add!
    (set :: <table-set>, x) => (set :: <table-set>)
  set[x] := x;
  set
end method set-add!;

define method set-remove!
    (set :: <table-set>, x) => (set :: <table-set>)
  remove-key!(set, x);
  set
end method set-remove!;

// eof
