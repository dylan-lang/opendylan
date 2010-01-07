module:    dood-test-suite
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// EASY CREATION

define variable d = #f;

define method force-mkdb ()
  // dood-world-reset(dood-world-default());
  make(<dood>, locator: "d", direction: #"input-output", 
       if-exists: #"replace");
end method;

define method do-store
    (object, 
     #key dood, name, reopen? = dood, close? = ~dood, buffer-size = 100,
     #all-keys)
  let dood
    = if (dood)
        if (dood == d)
          d := force-mkdb()
        else
          dood
        end if
      else
        make(<dood>, locator: as(<string>, name), buffer-size: buffer-size,
             direction: #"input-output", if-exists: #"replace");
      end if;
  block ()
    dood-root(dood) := object;
    dood-commit(dood);
    // dump(dood);
    values(object, dood)
  cleanup
    if (close?)
      dood-close(dood);
    end if;
  end block;
end method;

define method do-load 
    (#key name, dood, flush? = ~name, close? = ~dood, buffer-size = 100, #all-keys)
  let dood
    = if (dood)
        dood
      else
	make(<dood>, locator: as(<string>, name), direction: #"input-output",
	     buffer-size: buffer-size);
      end if;
  block ()
    if (flush?)
      dood-flush(dood);
    end if;
    values(dood-root(dood), dood)
  cleanup
    if (close?)
      dood-close(dood, abort: #t);
    end if;
  end block;
end method;

define method do-store-load
    (object, #rest all-keys, #key dood, name, close?, reopen?, flush?)
  apply(do-store, object, all-keys);
  apply(do-load, all-keys)
end method;

define method store-load
    (object, #rest all-keys, #key dood, name, close?, reopen?, flush?)
  apply(do-store-load, object, all-keys);
end method;

define method store (object)
  do-store(object, dood: d)
end method;

define method load ()
  do-load(dood: d)
end method;

/// THINGS TO TEST FOR
///   FLUSH
///   COMMIT
///   WALK
///   PROXIES

define test primitives ()
  check-equal("INTEGER", store(1), load());
  check-equal("NEG-INTEGER", store(-1), load());
  check-equal("MAX-INTEGER", store($max-dood-integer + 1), load());
  check-equal("MIN-INTEGER", store($min-dood-integer - 1), load());
  check-equal("FLOAT", store(1.23), load());
  check-equal("CHARACTER", store('a'), load());
  check-equal("TRUE", store(#t), load());
  check-equal("FALSE", store(#f), load());
  check-equal("STRING", store("ABC"), load());
  check-equal("EMPTY STRING", store(""), load());
  check-equal("SYMBOL", store(#"ABC"), load());
end test;

define test program-bindings ()
  check-equal("BUILTIN CLASS", store(<pair>), load());
  check-equal("CLASS", store(<stretchy-vector>), load());
  // check-equal("GENERIC", store(size), load());
end test;

define class <fslot> (<object>)
  slot fs = size;
end class;

define test function-slots ()
  check-equal("FUNCTION-SLOT", fs(store(make(<fslot>))), fs(load()));
end test;

define method as-explicit-key-collection
    (class :: subclass(<explicit-key-collection>), key-values :: <list>)
  let c = make(class);
  for (key-value in key-values)
    c[head(key-value)] := tail(key-value);
  end for;
  c
end method;

define method stretchy-vector (#rest elts)
  as(<stretchy-vector>, elts)
end method;

define method deque (#rest elts)
  as(<deque>, elts)
end method;

define test collections ()
  check-equal("EMPTY-LIST", store(#()), load());
  check-equal("PAIR", store(pair(1, 2)), load());
  check-equal("LIST", store(list(1, 2, 3)), load());
  check-equal("EMPTY-VECTOR", store(#[]), load());
  check-equal("VECTOR", store(vector(1, 2, 3)), load());
  check-equal("NESTED VECTOR", store(vector(1, 2, vector(3))), load());
  check-equal("STRETCHY-VECTOR", store(stretchy-vector(1, 2, 3)), load());
  check-equal("NESTED STRETCHY-VECTOR", 
              store(stretchy-vector(stretchy-vector(1), 2, 3)), load());
  check-equal("DEQUE", store(deque(1, 2, 3)), load());
  check-equal("NESTED DEQUE", store(deque(deque(1), 2, 3)), load());
  let tbl = as-explicit-key-collection(<table>, list(pair(1, 2), pair(3, 4)));
  check-equal("TABLE", store(tbl), load());
end test;

define constant $reinit-value = 55;

define dood-class <weak-object> (<object>)
  weak slot weak-object, 
    reinit-expression: $reinit-value,
    init-value: 0, init-keyword: object:;
end dood-class;

define test weak-slots ()
  check-true("WEAK REINITED", 
             begin 
               store(make(<weak-object>, object: $reinit-value - 1));
               weak-object(load()) = $reinit-value
             end);
end test;

define dood-class <lazy-object> (<object>)
  lazy slot lazy-object, init-keyword: object:;
end dood-class;

define method lazy-slot-checks 
    (class :: <class>, name :: <byte-string>, 
     getter :: <function>, private-getter :: <function>)
  check-true(concatenate(name, " SHALLOW LAZY"),
             begin 
               store(make(class, object: "ABC")); 
               instance?(private-getter(load()), <dood-slot-value-proxy>)
             end);
  check-equal(concatenate(name, " FULFILLS SHALLOW PROMISE"),
              begin 
                store(make(class, object: "ABC")); 
                getter(load())
              end,
              "ABC");
  local method make-deep-lazy-object (value) 
          list(list(make(class, object: value)))
        end method;
  local method deep-lazy-object (start)
          head(head(start))
        end method;
  check-true(concatenate(name, " DEEPLY LAZY"),
             begin 
               store(make-deep-lazy-object("ABC")); 
               instance?(private-getter(deep-lazy-object(load())), 
                         <dood-slot-value-proxy>)
             end);
  check-equal(concatenate(name, " FULFILLS DEEP PROMISE"),
              begin 
                store(make-deep-lazy-object("ABC")); 
                getter(deep-lazy-object(load()))
              end,
              "ABC");
  check-true(concatenate(name, " DOUBLY DEEPLY LAZY ONCE"),
             begin 
               store(make-deep-lazy-object(make-deep-lazy-object("ABC"))); 
               instance?(private-getter(deep-lazy-object(load())), 
                         <dood-slot-value-proxy>)
             end);
  check-true(concatenate(name, " DOUBLY DEEPLY LAZY TWICE"),
             begin 
               store(make-deep-lazy-object(make-deep-lazy-object("ABC"))); 
               instance?(private-getter
                          (deep-lazy-object
                            (getter(deep-lazy-object(load())))), 
                         <dood-slot-value-proxy>)
             end);
  check-equal(concatenate(name, " FULFILLS DOUBLY DEEP PROMISE"),
              begin 
                store(make-deep-lazy-object(make-deep-lazy-object("ABC"))); 
                getter(deep-lazy-object(getter(deep-lazy-object(load()))))
              end,
              "ABC");
end method;

define test lazy-slots ()
  lazy-slot-checks(<lazy-object>, "LAZY", lazy-object, private-lazy-object);
  check-true("LAZY DOES WRITE BACK", 
             begin 
               store(make(<lazy-object>, object: "ABC")); 
               let obj = load();
               lazy-object(obj);
               private-lazy-object(obj) = "ABC"
             end);
end test;

define test lazy-table ()
  let tbl
    = as-explicit-key-collection
        (<dood-lazy-table>, list(pair(1, 2), pair(3, 4)));
  check-true("LAZY TABLE IS LAZY", 
             begin
               store(tbl);
               every?(rcurry(instance?, <dood-address-proxy>), 
                      dood-lazy-table-data(load()))
             end);
  check-equal("LAZY TABLE READS BACK", store(tbl), load());
end test;

define dood-class <disk-object> (<object>)
  disk slot disk-object, 
    init-value: 0, init-keyword: object:;
end dood-class;

define test disk-slots ()
  lazy-slot-checks(<disk-object>, "DISK", disk-object, private-disk-object);
  check-true("DOESN'T WRITE BACK", 
             begin 
               store(make(<disk-object>, object: "ABC"));
               let obj = load();
               disk-object(obj);
               instance?(private-disk-object(obj), <dood-slot-value-proxy>)
             end);
end test;

define dood-class <mapped-object> (<dood-mapped-object>)
  slot mapped-left  = 0, init-keyword: left:;
  slot mapped-right = 0, init-keyword: right:;
end dood-class;

define method match? (x, y, visited? :: <table>)
  x = y
end method;

define method match?
    (x :: <mapped-object>, y :: <mapped-object>, visited? :: <table>)
  element(visited?, x, default: #f)
    | begin
	element(visited?, x) := #t;
	match?(mapped-left(x),  mapped-left(y), visited?)
	  & match?(mapped-right(x), mapped-right(y), visited?)
      end
end method;

define test mapped-objects ()
  check-true("MAPPED TREE", 
             begin 
	       let tree
		 = make(<mapped-object>, 
			left:  make(<mapped-object>, left: #(1, 2), right: 3),
			right: make(<mapped-object>, left: 4, right: #(1, 2)));
               store(tree);
               match?(tree, load(), make(<table>))
             end);
  check-true("MAPPED GRAPH", 
             begin 
	       let shared-tree
		 = make(<mapped-object>, left: #(1, 2), right: #(3, 4));
	       let tree
		 = make(<mapped-object>, 
			left:  make(<mapped-object>, left: shared-tree, right: 5),
			right: make(<mapped-object>, left: 6, right: shared-tree));
               store(tree);
               match?(tree, load(), make(<table>))
             end);
end test;

define dood-class <mapped-and-owned-object> (<dood-mapped-and-owned-object>)
  slot mapped-value = 0, init-keyword: value:;
end dood-class;

define test mapped-and-owned-objects ()
  check-true("MAPPED AND OWNED", 
             begin 
               store(make(<mapped-and-owned-object>, 
			  value: make(<mapped-and-owned-object>, 
                                      value: list(1, 2))));
               mapped-value(mapped-value(load())) = #(1, 2)
             end);
end test;

define method store-load-test (name, dood, object)
  check-equal(name, 
              do-store(object, dood: dood), 
              do-load(dood: dood, flush?: #f));
end method;

define test rewrites ()
  let dood = 
    make(<dood>, locator: "RRR", direction: #"input-output", 
         if-exists: #"replace");
  block ()
    for (i from 1 below 10,
         object = #() then reverse!(pair(i, object)))
      store-load-test(format-to-string("REWRITE %d", i), dood, object);
    end for;
  cleanup
    dood & dood-close(dood);
  end block;
end test;

define dood-class <reinit-object> (<object>)
  slot reinit-object, 
    init-value: 0, init-keyword: object:;
end dood-class;

define method dood-reinitialize (dood :: <dood>, object :: <reinit-object>) => ()
  next-method();
  reinit-object(object) := reinit-object(object) + 1;
end method;

define test reinitialization ()
  check-true("USER REINITIALIZATION", 
             begin 
               store(make(<reinit-object>, object: 1)); 
               reinit-object(load()) = 1 + 1
             end);
end test;

define test reusing ()
  check-equal("REUSING A DOOD THROUGH WORLD LOOKUP",
              make(<dood>, locator: "XXX", direction: #"input-output", 
                   if-exists: #"replace"),
              begin 
                let d = make(<dood>, locator: "XXX");
                block ()
                  d
                cleanup
                  dood-close(d)
                end;
              end);
end test;

define method fill-vector (size)
  let vector = make(<vector>, size: size);
  for (i from 0 below size)
    vector[i] := i;
  end for;
  vector
end method;

define test reopening ()
  check-equal("REOPENING A DOOD AND RECOVERING DATA",
              do-store(list(1, 2, 3), name: "YYY"),
              do-load(name: "YYY"));
  let big-fill-vector = fill-vector(100);
  check-equal("REOPENING A DOOD AND RECOVERING LOTS O DATA",
              do-store(big-fill-vector, name: "YYY"),
              do-load(name: "YYY"));
end test;

/*
define class <external-object> (<object>)
  slot external-name,      required-init-keyword: name:;
  slot external-value,     required-init-keyword: value:;
end class;

define method \= (x :: <external-object>, y :: <external-object>)
  external-name(x) == external-name(y) &
    external-value(x) = external-value(y)
end method;

define class <dood-cross-binding-proxy> (<dood-cross-proxy>)
  slot proxy-name, required-init-keyword: name:;
end class;

define method dood-make-cross-proxy
    (dood :: <dood>, object, external-dood :: <dood>) => (object)
  make(<dood-cross-binding-proxy>, 
       dood-name: dood-name(external-dood),
       name:      external-name(object))
end method;

define method dood-external-object (dood :: <dood>, name)
  block (return)
    for (object in dood-root(dood))
      if (external-name(object) == name)
        return(object)
      end if;
    end for;  
  end block;
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-binding-proxy>) => (object)
  let external-dood = make(<dood>, locator: dood-proxy-dood-name(proxy));
  dood-external-object(external-dood, proxy-name(proxy))
end method;

define test cross-proxies ()
  // dood-world-reset(dood-world-default());
  let a = make(<external-object>, name: #"A", value: list(1));
  let b = make(<external-object>, name: #"B", value: list(2));
  do-store(list(a, b), name: "EXT", close?: #f);
  do-store(list(a, b), name: "INT");
  check-equal("CROSS PROXIES EXT RECOVERS A PROPERLY",
              first(do-load(name: "EXT")), a);
  check-equal("CROSS PROXIES EXT RECOVERS B PROPERLY",
              second(do-load(name: "EXT")), b);
  check-equal("CROSS PROXIES INT RECOVERS A PROPERLY",
              first(do-load(name: "INT")), a);
  check-equal("CROSS PROXIES INT RECOVERS B PROPERLY",
              second(do-load(name: "INT")), b);
end test;
*/

///
/// 
///

define class <external-dooded-object> (<object>)
  slot external-dood-name, required-init-keyword: dood-name:;
  slot external-name,      required-init-keyword: name:;
  slot external-value,     required-init-keyword: value:;
end class;

define method \= (x :: <external-dooded-object>, y :: <external-dooded-object>) => (res :: <boolean>)
  external-dood-name(x) == external-dood-name(y) &
    external-name(x) == external-name(y) &
    external-value(x) = external-value(y)
end method;

define class <external-dooded-proxy> (<dood-proxy>)
  slot proxy-dood-name, required-init-keyword: dood-name:;
  slot proxy-name, required-init-keyword: name:;
end class;

define method make-external-dooded-proxy
    (dood :: <dood>, object :: <external-dooded-object>) => (proxy)
  format-out("MAKING EXT DOODED PROXY %= %=\n", dood-name(dood), external-name(object));
  make(<external-dooded-proxy>, 
       dood-name: dood-name(dood),
       name:      external-name(object))
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <external-dooded-object>)
 => (proxy :: type-union(<external-dooded-proxy>, <external-dooded-object>))
  if (dood-name(dood) == external-dood-name(object))
    next-method();
  else
    format-out("MAKING EXT PROXY\n");
    let res = dood-as-proxy(dood, object, make-external-dooded-proxy);
    format-out("DONE\n");
    res
  end if
end method;

define method external-dooded-object (dood :: <dood>, name)
  block (return)
    for (object in dood-root(dood))
      if (external-name(object) == name)
        return(object)
      end if;
    end for;  
  end block;
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <external-dooded-proxy>) => (object)
  external-dooded-object(dood, proxy-name(proxy))
end method;

define test external-dooded-proxies ()
  /*
  dood-world-reset(dood-world-default());
  let i = make(<external-dooded-object>, 
	       dood-name: #"INT", name: #"I", value: list(1));
  let e = make(<external-dooded-object>, 
	       dood-name: #"EXT", name: #"E", value: list(2));
  let (obj, ext-dood) = do-store(list(e), name: "EXT", close?: #f);
  let (obj, int-dood) = do-store(list(i, e), name: "INT", close?: #f);
  check-equal("CROSS PROXIES EXT RECOVERS E PROPERLY",
              first(do-load(name: "EXT")), e);
  check-equal("CROSS PROXIES INT RECOVERS I PROPERLY",
              first(do-load(name: "INT")), i);
  check-equal("CROSS PROXIES INT RECOVERS E PROPERLY",
              second(do-load(name: "INT")), e);
  check-equal("CROSS PROXIES I IN INT",
              object-dood(i), int-dood);
  check-equal("CROSS PROXIES E IN EXT",
              object-dood(e), ext-dood);
  dood-close(ext-dood, abort?: #t);
  dood-close(int-dood, abort?: #t);
  */
  // TRY SAME WITHOUT FIRST COMMITTING E TO EXT
  // dood-world-reset(dood-world-default());
  let i = make(<external-dooded-object>, 
	       dood-name: #"INT", name: #"I", value: list(1));
  let e = make(<external-dooded-object>, 
	       dood-name: #"EXT", name: #"E", value: list(2));
  let (obj, ext-dood) = do-store(#(), name: "EXT", close?: #f);
  let (obj, int-dood) = do-store(list(i, e), name: "INT", close?: #f);
  check-equal("CROSS PROXIES I IN INT",
              object-dood(i), int-dood);
  format-out("OBJECT-DOOD(E) = %=\n", object-dood(e));
  check-equal("CROSS PROXIES E IN EXT",
              object-dood(e), ext-dood);
  dood-root(ext-dood) := list(e);
  dood-commit(ext-dood);
  check-equal("CROSS PROXIES EXT RECOVERS E PROPERLY",
              first(do-load(name: "EXT")), e);
  check-equal("CROSS PROXIES INT RECOVERS I PROPERLY",
              first(do-load(name: "INT")), i);
  check-equal("CROSS PROXIES INT RECOVERS E PROPERLY",
              second(do-load(name: "INT")), e);
  dood-close(ext-dood, abort?: #t);
  dood-close(int-dood, abort?: #t);
end test;

define suite dood-test-suite 
    (setup-function:   method () d := force-mkdb() end, 
     cleanup-function: method () dood-close(d) end)
  test primitives;
  test program-bindings;
  test collections;
  test weak-slots;
  test lazy-slots;
  test disk-slots;
  test lazy-table;
  test mapped-objects;
  test rewrites;
  test reinitialization;
  test reusing;
  test reopening;
  test external-dooded-proxies;
end suite;

define method print-object (dood :: <dood>, stream :: <stream>)
  format-out("[DOOD %=]", dood-name(dood));
end method;

define method print-object (x :: <external-dooded-object>, stream :: <stream>)
  format-out("[OBJ %=]", external-name(x));
end method;

run-test-application(dood-test-suite);
