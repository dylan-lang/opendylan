module: dfmc-back-end
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic heap-required-instance-size (class) => (res :: <integer>);

define method heap-required-instance-size
    (class :: <&class>) => (res :: <integer>)
  ^instance-storage-size(class)
end method;

define method heap-required-instance-size 
    (class == <simple-object-vector>) => (res :: <integer>)
  2
end method;

define method heap-required-instance-size 
    (class == <byte-string>) => (res :: <integer>)
  2
end method;

define method heap-required-instance-size 
    (class == <uninterned-symbol>) => (res :: <integer>)
  2
end method;

define method heap-required-instance-size 
    (class :: subclass(<list>)) => (res :: <integer>)
  3
end method;

define method heap-required-instance-size 
    (class :: <class>) => (res :: <integer>)
  1 // size(slot-descriptors(class))
end method;

define method heap-required-instance-size 
    (class == <boolean>) => (res :: <integer>)
  1
end method;

define generic heap-instance-size (element) => (res :: <integer>);

define method heap-instance-size (element) => (res :: <integer>)
  heap-required-instance-size(&object-class(element))
end method;

define method heap-instance-size
    (element :: <byte-string>) => (res :: <integer>)
  round/(size(element), 4) + 2
end method;

define method heap-instance-size 
    (element :: <simple-object-vector>) => (res :: <integer>)
  size(element) + 2
end method;

define method heap-instance-size (element :: <&iep>) => (res :: <integer>)
  0
end method;

define method heap-instance-class (element :: <&object>) => (res)
  &object-class(element)
end method;

define method heap-instance-class (element) => (res)
  object-class(element)
end method;

define method heap-debug-name (element :: <class>) => (res)
  debug-name(element)
end method;

define method heap-debug-name (element :: <&class>) => (res)
  ^debug-name(element)
end method;

define method heap-stats (ld :: <library-description>)
  without-dependency-tracking
  with-library-context (ld)
    let heaps
      = map(compilation-record-model-heap, compilation-context-records(ld));
    let all-stats
      = map(rcurry(single-heap-stats, #f), heaps);
    let stats
      = reduce1(method (x, y) walker-merge-statistics(x, y, \+) end, all-stats);
    walker-display-statistics
      (0, #t, heap-debug-name, heap-required-instance-size, stats);
  end with-library-context;
  end without-dependency-tracking;
end method;

define method all-heap-stats 
    (ld :: <library-description>, #rest all-keys, #key, #all-keys)
  let total-count = 0;
  let total-size  = 0;
  for (ld in reverse(all-library-descriptions(ld)))
    let (count, size)
      = apply(heap-stats, ld, all-keys);
    total-count := total-count + count;
    total-size  := total-size  + size;
  end for;
  format-out("TOTAL SUMMARY\n");
  format-out("%d count, size = %d words\n", total-count, total-size);
end method;

define variable *filter-set*    = vector(<collection>);
define variable *aggregate-set* = #[];

define inline function compile-time-only? (object)
  instance?(object, <&iep>)
end function;

define function single-heap-stats (heap :: <model-heap>, display?)
  let parents 
    = heap-back-pointers(heap);
  let compile-time-only-objects
    = collecting ()
	for (rents keyed-by object in parents)
	  when (compile-time-only?(object))
	    collect(object)
	  end when;
	end for;
      end collecting;
  do(curry(remove-key!, parents), compile-time-only-objects);
  let (total-count, total-size, stats)
    = walker-instance-statistics
        (display?, identity, heap-instance-class, heap-debug-name,
	 heap-instance-size, heap-required-instance-size,
	 parents,
	 aggregate-set: *aggregate-set*,
	 filter-set:    *filter-set*);
  stats
end function;

define method diff-heap-stats
    (ld :: <project-library-description>, #rest all-keys, #key force?)
  /*
  with-library-context (ld)
    let dood = library-description-dood(ld);
    diff-last-two-statistics(dood);
  end with-library-context;
  */
end method;

// define function do-defined-objects 
//     (fn :: <function>, ld :: <library-description>)
//   let heaps
//      = map(compilation-record-model-heap, compilation-context-records(ld));
//   for (heap in heaps)
//     do(fn, heap-defined-objects(heap))
//   end for;
// end function;
