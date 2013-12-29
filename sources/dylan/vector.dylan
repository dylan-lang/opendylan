Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <vector> ... end;
// BOOTED: define ... class <simple-vector> ... end;
// BOOTED: define ... class <simple-object-vector> ... end;


////////////
// INTERFACE
////////////


// Functions on <vector>

define inline function vector (#rest arguments)
 => (vector :: <simple-object-vector>)
  arguments
end function;

// this is just a marker for optimization, to say that the vector can be
// constant-folded
define not-inline function immutable-vector (#rest arguments)
 => (vector :: <simple-object-vector>)
  arguments
end function;

// this is just a marker for optimization, to say that the vector can be
// constant-folded
define not-upgrade not-inline function immutable-type-vector (#rest types)
 => (vector :: <simple-object-vector>)
  for (type in types)
    check-type(type, <type>)
  end for;
  types
end function;


//
// LIMITED-VECTOR
//

define open generic limited-vector
     (of :: false-or(<type>), fill, size :: false-or(<integer>)) => (type :: <type>);


/////////////////
// IMPLEMENTATION
/////////////////


//
// Specialized inherited generic methods
//


//
// MAKE
//

define sealed inline method make (class == <vector>, #key size = 0, fill)
 => (vector :: <simple-object-vector>)
  make(<simple-object-vector>, size: size, fill: fill)
end method make;

//
// AS
//

define open method as
    (class == <vector>, collection :: <vector>)
 => (vector :: <vector>)
  collection
end method as;

//
// SHALLOW-COPY
//

define inline method shallow-copy (vector :: <vector>) => (result :: <vector>)
  vector.copy-sequence
end method shallow-copy;


//
// TYPE-FOR-COPY
//

define inline method type-for-copy (vector :: <vector>) => (type :: <type>)
  <simple-object-vector>
end method type-for-copy;


//
// EMPTY?
//

define inline method empty? (vector :: <vector>) => (b :: <boolean>)
  vector.size = 0
end method empty?;


//
// EMPTY
//

// This method returns a shared sequence of the given type with the default fill.
define open generic empty
    (class :: <sequence-type>) => (res :: <sequence>);

define inline method empty
    (class :: <vector-type>) => (res :: <simple-object-vector>)
  empty(<simple-object-vector>)
end method empty;

//
// DIMENSIONS
//

define inline method dimensions (vector :: <vector>) => (l :: <list>)
  list(vector.size)
end method dimensions;


//
// ADD
//

define method add (vector :: <vector>, object) => (v :: <vector>)
  let new-vector :: <vector>
    = make(vector.type-for-copy, size: vector.size + 1, fill: object);
  without-bounds-checks
    for (i :: <integer> from 0 below vector.size)
      new-vector[i] := vector[i]
    end;
    new-vector[vector.size] := object;
  end without-bounds-checks;
  new-vector
end method add;


//
// REVERSE!
//

define method reverse! (vector :: <vector>) => (v :: <vector>)
  let stopping-index = floor/(vector.size, 2);
  without-bounds-checks
    for (index :: <integer> from 0 below stopping-index,
         size-index :: <integer> from vector.size - 1 by -1)
      let tmp = vector[index];
      vector[index] := vector[size-index];
      vector[size-index] := tmp;
    end for
  end without-bounds-checks;
  vector
end method reverse!;


//
// REVERSE
//

define method reverse (vector :: <vector>) => (v :: <vector>)
  let size = size(vector);
  if (size = 0)
    make(vector.type-for-copy, size: 0)
  else
    // For compatibility, use fill: rather than relying on element-type-fill.
    let new-vector :: <vector>
      = make(vector.type-for-copy, size: size, fill: vector[0]);
    without-bounds-checks
      for (from :: <integer> from 0, to from size - 1 to 0 by -1)
        new-vector[to] := vector[from]
      end
    end without-bounds-checks;
    new-vector
  end if
end method reverse;


//
// ROW-MAJOR-INDEX
//

define method row-major-index (vector :: <vector>, #rest indices)
 => (i :: <integer>)
  unless (indices.size = 1)
    error(make(<subscript-out-of-bounds-error>,
               format-string: "Number of subscripts %= not equal to "
                   "rank of array %=",
               format-arguments: list(indices, vector)))
  end unless;
  indices.first
end method row-major-index;


//
// REPLACE-ELEMENTS!
//

define method replace-elements!(vector :: <vector>,
                                predicate :: <function>,
                                new_value_fn :: <function>,
                                #key count: count = #f) => vec :: <vector>;
  let count :: <integer> = count | vector.size;
  without-bounds-checks
    for (key :: <integer> from 0 below vector.size,
         until: count = 0)
      let this_element = vector[key];
      if (predicate(this_element))
        vector[key] := new_value_fn(this_element);
        count := count - 1;
      end if;
    end for
  end without-bounds-checks;
  vector;
end method replace-elements!;


//
// AREF
//

define method aref (vector :: <vector>, #rest indices) => elt :: <object>;
  if (indices.size == 1)
    vector[indices[0]];
  else
    error(make(<subscript-out-of-bounds-error>,
               format-string: "Invalid number of indices for %=.  "
                              "Expected 1, got %d",
               format-arguments: list(vector, indices.size)))
  end;
end;


//
// AREF-SETTER
//

define method aref-setter (new, vector :: <vector>, #rest indices)
 => new :: <object>;
  if (indices.size == 1)
    vector[indices[0]] := new;
  else
    error(make(<subscript-out-of-bounds-error>,
               format-string: "Invalid number of indices for %=.  "
                              "Expected 1, got %d",
               format-arguments: list(vector, indices.size)))
  end;
end;

//
// COPY-SEQUENCE
//

define sealed method copy-sequence
    (source :: <vector>,
     #key start: first :: <integer> = 0, end: last = unsupplied())
 => (result-sequence :: <vector>);
  let last :: <integer> = check-start-compute-end(source, first, last);
  let sz   = last - first;
  if (sz <= 0)
    make(type-for-copy(source), size: 0)
  else
    // For compatibility, use fill: rather than relying on element-type-fill.
    let fill = source[0];
    let result :: <vector>
      = make(type-for-copy(source), size: sz, fill: fill);
    without-bounds-checks
      for (j :: <integer> from 0 below sz,
           i :: <integer> from first)
        result[j] := source[i];
      end for;
    end without-bounds-checks;
    result
  end if
end method;

//
// SUBSEQUENCE-POSITION
//

define method subsequence-position
    (big :: <vector>, pat :: <vector>,
     #key test :: <function> = \==, count :: <integer> = 1)
 => (index :: false-or(<integer>));
  let sz = size(big);
  let pat-sz = size(pat);
  without-bounds-checks
    select (pat-sz)
      0 =>
        count - 1;
      1 =>
        let ch = pat[0];
        for (key :: <integer> from 0 below sz,
             until: test(big[key], ch) & (count := count - 1) <= 0)
        finally
          if (key < sz) key end if;
        end for;
      2 =>
        let ch1 = pat[0];
        let ch2 = pat[1];
        for (key :: <integer> from 0 below sz - 1,
             until: test(big[key], ch1) & test(big[key + 1], ch2)
               & (count := count - 1) <= 0)
        finally
          if (key < (sz - 1)) key end if;
        end for;
      otherwise =>
        local method search(index, big-key, pat-key, count)
                case
                  pat-key >= pat-sz =>
                    if (count = 1) index
                    else search(index + 1, index + 1, 0, count - 1);
                    end if;
                  big-key = sz =>
                    #f;
                  test(big[big-key], pat[pat-key]) =>
                    search(index, big-key + 1, pat-key + 1, count);
                  otherwise =>
                    search(index + 1, index + 1, 0, count);
                end case;
              end method search;
        search(0, 0, 0, count);
    end select;
  end without-bounds-checks
end method subsequence-position;

//
// SORT
//

define method sort (sequence :: <vector>, #key test = \<, stable: stable)
 => new-seq :: <sequence>;
  sort!(copy-sequence(sequence), test: test, stable: stable);
end method sort;


//
// MEMBER
//

define method member?
    (value, collection :: <vector>, #key test = \==)
 => (boolean :: <boolean>)
  (without-bounds-checks
    let n :: <integer> = collection.size;
    if (test == \== & ~(value-object?(value)))
      iterate grovel (index :: <integer> = 0)
        if (index = n) #f elseif (pointer-id?(collection[index], value)) #t else grovel(index + 1) end
      end iterate
    else
      iterate grovel (index :: <integer> = 0)
        unless (index = collection.size)
          (test(value, element(collection, index)) & #t) | grovel(index + 1)
        end unless
      end iterate
    end if
  end without-bounds-checks);
end method member?;


//
// ADD-NEW
//

define method add-new
    (vector :: <vector>, new-element,
     #key test :: <function> = \==)
 => (new-vector :: <vector>);
  if (any? (method (el) test(el, new-element) end, vector))
    vector
  else
    add(vector, new-element)
  end if
end method add-new;


//
// ADD-NEW!
//

define method add-new!
    (vector :: <vector>, new-element,
     #key test :: <function> = \==)
 => (new-vector :: <vector>);
  if (any? (method (el) test(el, new-element) end, vector))
    vector
  else
    add!(vector, new-element)
  end if
end method add-new!;


//
// AS
//

define constant <vector-type>
  = type-union(subclass(<vector>), <limited-vector-type>);

define method as
    (class :: <vector-type>, collection :: <collection>)
 => (vector :: <vector>)
  if (collection.object-class == class)
    collection
  else
    let new-size
      = collection.size;
    without-bounds-checks
      if (new-size = 0)
        make(class, size: new-size)
      else
        // For compatibility, use fill: rather than relying on element-type-fill.
        let new-vector
          = with-fip-of collection
              let fill = current-element(collection, initial-state);
              make(class, size: new-size, fill: fill);
            end with-fip-of;
        for (index :: <integer> from 0 below new-size, item in collection)
          element(new-vector, index) := item;
        end for;
        new-vector
      end if
    end without-bounds-checks;
  end if
end method as;


//
// CONCATENATE-AS
//

define method concatenate-as
    (type :: <vector-type>, vector :: <vector>, #rest more-vectors)
 => (result :: <vector>)
  block (return)
    let total-sz :: <integer> = vector.size;
    let num-non-empty :: <integer> = if (total-sz = 0) 0 else 1 end;
    let fill = unsupplied();

    for (v in more-vectors)
      unless (instance?(v, type))
        return(next-method())
      end unless;
      let sz :: <integer> = v.size;
      unless (sz = 0)
        total-sz := total-sz + sz;
        num-non-empty := num-non-empty + 1;
        when (unsupplied?(fill))
          fill := v[0];
        end when;
      end unless;
    end for;

    without-bounds-checks
      select (num-non-empty)
        0 => make(type);
        1 => if (vector.size > 0)
               as(type, vector)
             else
               for (i :: <integer> from 0 below more-vectors.size,
                    while: more-vectors[i].size = 0)
               finally
                 as(type, more-vectors[i])
               end for
             end;
        otherwise =>
          // For compatibility, use fill: rather than relying on element-type-fill.
          let result = make(type, size: total-sz, fill: fill);
          for (i :: <integer> from 0 below size(vector))
            result[i] := vector[i];
          finally
            let result-index :: <integer> = i;
            for (v :: <vector> in more-vectors)
              for (i :: <integer> from 0 below size(v),
                   j :: <integer> from result-index)
                result[j] := v[i];
              end for;
              result-index := result-index + size(v);
            end for;
          end for;
          result;
      end select;
    end without-bounds-checks;
  end block
end method concatenate-as;

//
// REDUCE1
//

define inline method reduce1
    (fn :: <function>, collection :: <vector>) => (object)
  if (empty?(collection))
    // Is there a more informative error class that's appropriate here?
    error(make(<empty-collection-error>,
               format-string: "Reduce1 undefined for empty collections"))
  else
    without-bounds-checks
      for (index :: <integer> from 1 below collection.size,
           result = collection[0] then fn(result, collection[index]))
      finally
        result
      end for
    end without-bounds-checks
  end
end method reduce1;


//
// REPLACE-SUBSEQUENCE!
//

define method replace-subsequence!
    (target :: <vector>, insert :: <vector>,
     #key start :: <integer> = 0, end: last = unsupplied())
 => (result-sequence :: <vector>);
  let target-size :: <integer> = target.size;
  let insert-size :: <integer> = insert.size;
  let last :: <integer> = check-start-compute-end(target, start, last);
  let delete-size :: <integer> = last - start;
  without-bounds-checks
    if (delete-size = insert-size)
      for (index :: <integer> from start below last,
           e in insert)
        target[index] := e
      end for;
      target
    else
      let new-size :: <integer> = target-size - delete-size + insert-size;
      let new-target = make(target.type-for-copy, size: new-size);
      let new-end :: <integer> = start + insert-size;
      for (index :: <integer> from 0 below start)
        new-target[index] := target[index]
      end;
      for (index :: <integer> from start below new-end, e in insert)
        new-target[index] := e end;
      for (from :: <integer> from last below target-size,
           to from new-end below new-size)
        new-target[to] := target[from] end;
      new-target
    end if;
  end without-bounds-checks
end method;

/// NEED
///   CONCATENATE-AS
///   FILL!


//
// <SIMPLE-VECTOR>
//


//
// MAKE
//

define sealed inline method make (class == <simple-vector>, #key size = 0, fill)
 => (vector :: <simple-object-vector>)
  make(<simple-object-vector>, size: size, fill: fill)
end method make;


//
// TYPE-FOR-COPY
//

define inline method type-for-copy (vector :: <simple-vector>)
 => (type :: <type>)
  object-class(vector)
end method type-for-copy;



///
/// LIMITED VECTOR
///

define macro limited-vector-shared-definer
  { define limited-vector-shared "<" ## ?:name ## ">" }
    => { define inline sealed method element-no-bounds-check
             (vector :: "<simple-" ## ?name ## "-vector>", index :: <integer>,
              #key default)
          => (object :: "<" ## ?name ## ">")
           ?name ## "-vector-element"(vector, index)
         end method element-no-bounds-check;

         define inline sealed method element-no-bounds-check-setter
             (new-value :: "<" ## ?name ## ">",
              vector :: "<simple-" ## ?name ## "-vector>", index :: <integer>)
           => (object)
           ?name ## "-vector-element"(vector, index) := new-value;
           new-value
         end method element-no-bounds-check-setter;

         define inline function ?name ## "-vector-current-element"
             (vector :: "<simple-" ## ?name ## "-vector>", state :: <integer>)
          => (res :: "<" ## ?name ## ">")
           ?name ## "-vector-element"(vector, state)
         end function;

         define inline function ?name ## "-vector-current-element-setter"
             (new-value :: "<" ## ?name ## ">",
              vector :: "<simple-" ## ?name ## "-vector>", state :: <integer>)
           ?name ## "-vector-element"(vector, state) := new-value
         end function;

         define sealed inline method forward-iteration-protocol
           (sequence :: "<simple-" ## ?name ## "-vector>")
             => (initial-state :: <integer>, limit :: <integer>,
                 next-state :: <function>, finished-state? :: <function>,
                 current-key :: <function>,
                 current-element :: <function>,
                 current-element-setter :: <function>,
                 copy-state :: <function>)
           values(0,
                  sequence.size,
                  sequence-next-state,
                  sequence-finished-state?,
                  sequence-current-key,
                  ?name ## "-vector-current-element",
                  ?name ## "-vector-current-element-setter",
                  identity-copy-state)
         end method forward-iteration-protocol;

         define sealed inline method backward-iteration-protocol
           (sequence :: "<simple-" ## ?name ## "-vector>")
             => (final-state :: <integer>,
                 limit :: <integer>,
                 previous-state :: <function>,
                 finished-state? :: <function>,
                 current-key :: <function>,
                 current-element :: <function>,
                 current-element-setter :: <function>,
                 copy-state :: <function>)
           values(sequence.size - 1,
                  -1,
                  sequence-previous-state,
                  sequence-finished-state?,
                  sequence-current-key,
                  ?name ## "-vector-current-element",
                  ?name ## "-vector-current-element-setter",
                  identity-copy-state)
         end method backward-iteration-protocol;
         /// SEALED DOMAINS
         define sealed domain make (singleton("<simple-" ## ?name ## "-vector>"));
         define sealed domain initialize ("<simple-" ## ?name ## "-vector>");
         define sealed domain type-for-copy ("<simple-" ## ?name ## "-vector>");
         define sealed domain shallow-copy ("<simple-" ## ?name ## "-vector>");
         define sealed domain size ("<simple-" ## ?name ## "-vector>");
         define sealed domain empty? ("<simple-" ## ?name ## "-vector>");
         define sealed domain add ("<simple-" ## ?name ## "-vector>", <object>);
         define sealed domain add! ("<simple-" ## ?name ## "-vector>", <object>);
         define sealed domain add-new ("<simple-" ## ?name ## "-vector>", <object>);
         define sealed domain add-new! ("<simple-" ## ?name ## "-vector>", <object>);
         define sealed domain member? (<object>, "<simple-" ## ?name ## "-vector>");
         define sealed domain fill! ("<simple-" ## ?name ## "-vector>");
         define sealed domain remove ("<simple-" ## ?name ## "-vector>", <object>);
         define sealed domain remove! ("<simple-" ## ?name ## "-vector>", <object>);
         define sealed domain sort ("<simple-" ## ?name ## "-vector>");
         define sealed domain sort! ("<simple-" ## ?name ## "-vector>");
         define sealed domain copy-sequence ("<simple-" ## ?name ## "-vector>");
         define sealed domain reduce (<function>, <object>, "<simple-" ## ?name ## "-vector>");
         define sealed domain reduce1 (<function>, "<simple-" ## ?name ## "-vector>");
         define sealed domain choose (<function>, "<simple-" ## ?name ## "-vector>");
         define sealed domain replace-subsequence!
           ("<simple-" ## ?name ## "-vector>", "<simple-" ## ?name ## "-vector>");
         define sealed domain subsequence-position
           ("<simple-" ## ?name ## "-vector>", "<simple-" ## ?name ## "-vector>");
         define sealed domain as
           (singleton("<simple-" ## ?name ## "-vector>"), <collection>);
         define sealed domain concatenate-as
           (singleton("<simple-" ## ?name ## "-vector>"), "<simple-" ## ?name ## "-vector>");
         }
end macro;

define macro limited-vector-element-setter-definer
  { define limited-vector-element-setter "<" ## ?:name ## ">" }
    => { define inline sealed method element-setter
             (new-value :: "<" ## ?name ## ">",
              vector :: "<simple-" ## ?name ## "-vector>", index :: <integer>)
          => (object :: "<" ## ?name ## ">")
           if (element-range-check(index, size(vector)))
             element-no-bounds-check(vector, index) := new-value
           else
             element-range-error(vector, index)
           end if
         end method element-setter; }
end macro;

define macro limited-vector-shared+element-setter-definer
  { define limited-vector-shared+element-setter "<" ## ?:name ## ">" }
    => { define limited-vector-shared "<" ## ?name ## ">";
         define limited-vector-element-setter "<" ## ?name ## ">"; }
end macro;

define macro limited-vector-minus-constructor-definer
  { define limited-vector-minus-constructor "<" ## ?:name ## ">" (?superclasses:*)
      (#key ?fill:expression) }
    => { define limited-vector-shared "<" ## ?name ## ">";
    
         define sealed concrete primary class "<simple-" ## ?name ## "-vector>" (?superclasses)
           repeated sealed inline slot ?name ## "-vector-element" :: "<" ## ?name ## ">",
             init-value:        ?fill,
             init-keyword:      fill:,
             size-getter:       size,
             size-init-keyword: size:,
             size-init-value:   0;
         end class;
         
         define inline sealed method element
             (vector :: "<simple-" ## ?name ## "-vector>", index :: <integer>,
              #key default = unsupplied())
          => (object)
           if (element-range-check(index, size(vector)))
             element-no-bounds-check(vector, index)
           else
             if (unsupplied?(default))
               element-range-error(vector, index)
             else
               default
             end if
           end if
         end method element;
         }
end macro;

define macro limited-vector-minus-selector-definer
  { define limited-vector-minus-selector "<" ## ?:name ## ">" (?superclasses:*) (#key ?fill:expression) }
    => { define limited-vector-minus-constructor "<" ## ?name ## ">" (?superclasses) (fill: ?fill);
         define limited-vector-element-setter "<" ## ?name ## ">";
         
         define constant "$empty-<simple-" ## ?name ## "-vector>"
           = system-allocate-repeated-instance
               ("<simple-" ## ?name ## "-vector>", "<" ## ?name ## ">", unbound(), 0, ?fill);

         define sealed inline method empty
             (class == "<simple-" ## ?name ## "-vector>")
          => (res :: "<simple-" ## ?name ## "-vector>")
           "$empty-<simple-" ## ?name ## "-vector>"
         end method empty;
         
         define sealed inline method element-type
             (t :: "<simple-" ## ?name ## "-vector>") => (type :: <type>)
           "<" ## ?name ## ">"
         end method;

         // This method is not inline, because the typist needs to find it
         // in order to propagate limited collection type information.
         define method make
             (class == "<simple-" ## ?name ## "-vector>",
               #key fill = ?fill, size :: <integer> = 0,
                    element-type-fill: default-fill = ?fill)
          => (vector :: "<simple-" ## ?name ## "-vector>")
           unless (size = 0)
             check-type(fill, "<" ## ?name ## ">")
           end unless;
           let instance = system-allocate-repeated-instance
             ("<simple-" ## ?name ## "-vector>", "<" ## ?name ## ">", unbound(), size, fill);
           instance.element-type-fill := default-fill;
           instance
         end method;
         
         define sealed inline method type-for-copy
             (vector :: "<simple-" ## ?name ## "-vector>")
          => (type :: <type>)
           limited-vector(element-type(vector), element-type-fill(vector), #f)
         end method type-for-copy
         }
end macro;

define macro limited-vector-definer
  { define limited-vector "<" ## ?:name ## ">" (#key ?fill:expression) }
    => { define limited-vector-minus-selector "<" ## ?name ## ">"
             (<limited-fillable-collection>, <simple-vector>) (fill: ?fill);
    
         define sealed inline method concrete-limited-vector-class
             (of == "<" ## ?name ## ">", default-fill :: "<" ## ?name ## ">")
          => (type :: singleton("<simple-" ## ?name ## "-vector>"), fully-specified?)
           values("<simple-" ## ?name ## "-vector>", default-fill = ?fill)
         end method }
end macro;

define limited-vector-shared+element-setter <object>;
define constant object-vector-element = vector-element;
define constant object-vector-element-setter = vector-element-setter;

define inline method concrete-limited-vector-class
     (of :: <type>, default-fill)
 => (res :: <class>, fully-specified?)
  values(<simple-element-type-vector>, #f)
end method;

define method limited-vector
    (of :: <type>, default-fill :: <object>, size :: false-or(<integer>))
 => (type :: <vector-type>)
  let (concrete-class, fully-specified?)
    = concrete-limited-vector-class(of, default-fill);
  if (size | ~fully-specified?)
    make(<limited-vector-type>,
         class:          <simple-vector>,
         element-type:   of,
         default-fill:   default-fill,
         concrete-class: concrete-class,
         size:           size)
  else
    concrete-class
  end if;
end method;


//
// <SIMPLE-OBJECT-VECTOR>
//


//
// MAKE
//

// COULD BE A COPY-DOWN

define sealed method make (class == <simple-object-vector>,
                           #key fill = #f, size :: <integer> = 0)
 => (vector :: <simple-object-vector>)
  if (size = 0)
    #[]  // canonicalize empty vector
  else
    system-allocate-repeated-object-instance
      (<simple-object-vector>, unbound(), size, fill);
  end if
end method;

//
// LIMITED-VECTOR
//

define method limited-vector
    (of == <object>, default-fill == #f, size :: false-or(<integer>))
 => (res :: <class>)
  <simple-object-vector>
end method;

//
// ELEMENT
//

define /* inline */ sealed method element
    (vector :: <simple-object-vector>, index :: <integer>,
     #key default = unsupplied()) => (object)
  if (element-range-check(index, size(vector)))
    vector-element(vector, index)
  else
    if (unsupplied?(default))
      element-range-error(vector, index)
    else
      default
    end if
  end if
end method element;

//
// EMPTY
//

define inline method empty
    (class == <simple-object-vector>) => (res :: <simple-object-vector>)
  #[]
end method empty;

//
// AS
//

define inline sealed method as
    (class == <vector>, collection :: <simple-object-vector>)
 => (vector :: <simple-object-vector>)
  collection
end method as;


//
// FILL!
//

define method fill!
    (target :: <simple-object-vector>, value,
     #key start :: <integer> = 0, end: last = unsupplied())
 => (target :: <simple-object-vector>)
  let last :: <integer> = check-start-compute-end(target, start, last);
  primitive-fill!
    (target, primitive-repeated-slot-offset(target), integer-as-raw(start),
     integer-as-raw(last - start), value);
  target
end;


//
// CONCATENATE-AS
//

define sealed method concatenate-as
    (class == <simple-object-vector>, vector :: <simple-object-vector>,
     #rest more-vectors) => (result :: <simple-object-vector>)
  block (return)
    let total-sz :: <integer> = vector.size;
    let num-non-empty :: <integer> = if (total-sz = 0) 0 else 1 end;

    for (v in more-vectors)
      if (~instance?(v, <simple-object-vector>)) return(next-method()) end;
      let sz :: <integer> = v.size;
      if (sz ~= 0)
        total-sz := total-sz + sz;
        num-non-empty := num-non-empty + 1;
      end;
    end for;

    select (num-non-empty)
      0 => #[];
      1 => if (vector.size > 0) vector
           else
             for (i :: <integer> from 0 below more-vectors.size,
                  while: more-vectors[i].size = 0) finally more-vectors[i] end
           end;
      otherwise =>
        let result = make(<simple-object-vector>, size: total-sz);
        let sz :: <raw-integer> = integer-as-raw(vector.size);
        primitive-replace!
          (result, primitive-repeated-slot-offset(result), integer-as-raw(0),
           vector, primitive-repeated-slot-offset(vector), integer-as-raw(0), sz);
        let result-index :: <raw-integer> = sz;
        for (v :: <simple-object-vector> in more-vectors)
          let vsz :: <raw-integer> = integer-as-raw(v.size);
          primitive-replace!
            (result, primitive-repeated-slot-offset(result), result-index,
             v,      primitive-repeated-slot-offset(v),      integer-as-raw(0), vsz);
          result-index := primitive-machine-word-add(result-index, vsz);
        end;
        result
    end select;
  end block
end method concatenate-as;

//
// COPY-SEQUENCE
//

define sealed method copy-sequence
    (source :: <simple-object-vector>,
     #key start: first :: <integer> = 0, end: last = unsupplied())
 => (result-sequence :: <simple-object-vector>);
  let last :: <integer> = check-start-compute-end(source, first, last);
  let sz = last - first;
  let result :: <simple-object-vector> = make(<simple-object-vector>, size: sz);
  primitive-replace!
    (result, primitive-repeated-slot-offset(result), integer-as-raw(0),
     source, primitive-repeated-slot-offset(source), integer-as-raw(first),
     integer-as-raw(sz));
  result
end method;
