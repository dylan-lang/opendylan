Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//
// STRETCHY-SEQUENCES
//

// Should these classes really be defined?  Not in the DRM...
// But used by deques for now?

define open abstract class <stretchy-sequence> (<stretchy-collection>, <sequence>)
end class <stretchy-sequence>;

define open abstract class <stretchy-mutable-sequence>
    (<stretchy-sequence>, <mutable-sequence>)
end class <stretchy-mutable-sequence>;

//
// REPLACE-SUBSEQUENCE!
//

define method replace-subsequence!
    (dst :: <stretchy-mutable-sequence>, src :: <sequence>,
     #key start = 0, end: last = dst.size)
 => (s :: <stretchy-mutable-sequence>)
  let dst-size :: <integer> = dst.size;
  let new-size :: <integer> = start + (src.size + (dst-size - last));
  case
    new-size < dst-size =>
      for (di :: <integer> from start, e in src)
        dst[di] := e;
      finally
        for (di :: <integer> from di,
             si :: <integer> from last below dst-size)
          dst[di] := dst[si];
        end for;
      end for;
      dst.size := new-size;
    new-size > dst-size =>
      dst.size := new-size;
      for (si :: <integer> from dst-size - 1 to last by -1,
           di :: <integer> from new-size - 1 by -1)
        dst[di] := dst[si];
      end for;
      for (di :: <integer> from start, e in src)
        dst[di] := e;
      end for;
    otherwise =>
      for (di :: <integer> from start, e in src)
        dst[di] := e;
      end for
  end case;
  dst
end method replace-subsequence!;

////////////
// INTERFACE
////////////

define open abstract class <stretchy-vector> (<stretchy-sequence>, <vector>)
end class <stretchy-vector>;


define open generic limited-stretchy-vector
     (of :: false-or(<type>), fill) => (type :: <type>);


/////////////////
// IMPLEMENTATION
/////////////////


//
// MAKE
//

define sealed inline method make
    (class == <stretchy-vector>, #rest all-keys, #key size, capacity, fill)
 => (vector :: <stretchy-object-vector>);
  apply(make, <stretchy-object-vector>, all-keys)
end method make;


//
// AS
//

define method as (class == <stretchy-vector>, coll :: <collection>)
 => (vector :: <stretchy-vector>)
  as(<stretchy-object-vector>, coll)
end method as;

define sealed inline method as (class == <stretchy-vector>, coll :: <stretchy-vector>)
 => (vector :: <stretchy-vector>)
  coll
end method as;


//
// TYPE-FOR-COPY
//

define method type-for-copy (vector :: <stretchy-vector>) => (type :: <type>)
  <stretchy-object-vector>
end method type-for-copy;


define class <capacity-size-error> (<simple-error>) end;

//
// SHARED-STRETCHY-VECTOR
//

define open class <limited-stretchy-vector> (<limited-fillable-collection>, <stretchy-vector>)
end class;

define open primary class <limited-stretchy-vector-representation> (<object>)
  slot %size :: <integer>, required-init-keyword: size:;
end class;

define open generic stretchy-representation
    (x :: <limited-stretchy-vector>)
 => (res :: <limited-stretchy-vector-representation>);
define open generic stretchy-representation-setter
    (v, x :: <limited-stretchy-vector>)
 => (res :: <limited-stretchy-vector-representation>);
define open generic stretchy-representation-type
    (x :: <limited-stretchy-vector>)
 => (res :: subclass(<limited-stretchy-vector-representation>));


define open generic stretchy-vector-element
    (rep :: <limited-stretchy-vector-representation>, key :: <integer>)
 => (res :: <object>);

define open generic stretchy-vector-element-setter
    (value :: <object>, rep :: <limited-stretchy-vector-representation>, key :: <integer>)
 => (value :: <object>);


//
// EMPTY?
//

define method empty? (vector :: <limited-stretchy-vector>)
 => (well? :: <boolean>)
  vector.stretchy-representation.%size = 0
end method empty?;


//
// ADD!
//

define inline method add! (vector :: <limited-stretchy-vector>, new-element)
 => (v :: <limited-stretchy-vector>)
  let old-size = vector.size;
  trusted-size(vector) := old-size + 1;
  check-type(new-element, element-type(vector));
  without-bounds-checks
    vector[old-size] := new-element;
  end without-bounds-checks;
  vector
end method add!;


//
// SIZE-SETTER
//

define method trusted-size-setter
    (new-size :: <integer>, vector :: <limited-stretchy-vector>)
         => (new-size :: <integer>)
  // TODO: could remove fills and do this in size-setter
  let f = element-type-fill(vector);
  let v = vector.stretchy-representation;
  let v-capacity = v.size;
  let v-size = v.%size;
  if (new-size > v-capacity)
    let nv :: <limited-stretchy-vector-representation>
      = make(stretchy-representation-type(vector),
             capacity: new-size.power-of-two-ceiling,
             size:     new-size);
    for (i :: <integer> from 0 below v-size)
      stretchy-vector-element(nv, i) := stretchy-vector-element(v, i)
    finally
      for (j :: <integer> from i below new-size)
        stretchy-vector-element(nv, j) := f
      end for;
    end for;
    vector.stretchy-representation := nv;
  else
    for (i :: <integer> from v-size below new-size)
      stretchy-vector-element(v, i) := f
    end for;
    v.%size := new-size;
  end if;
  new-size
end method trusted-size-setter;

define method size-setter
    (new-size :: <integer>, vector :: <limited-stretchy-vector>)
         => (new-size :: <integer>)
  check-nat(new-size);
  trusted-size(vector) := new-size;
end method size-setter;


//
// REMOVE!
//

define method remove!
    (vector :: <limited-stretchy-vector>, target,
     #key test = \==, count = unsupplied())
 => (vector :: <limited-stretchy-vector>)
  let count :: <integer> =
    if (unsupplied?(count)) vector.size else count end;
  let src = vector.stretchy-representation;
  let src-size = src.%size;
  iterate grovel (count :: <integer> = count,
                  src-index :: <integer> = 0,
                  dst-index :: <integer> = 0)
    if (src-index < src-size)
      let item = stretchy-vector-element(src, src-index);
      case
        count > 0 & test(item, target) =>
          grovel(count - 1, src-index + 1, dst-index);
        otherwise =>
          stretchy-vector-element(src, dst-index) := item;
          grovel(count, src-index + 1, dst-index + 1)
      end case
    else
      for (i :: <integer> from dst-index below src-index)
        stretchy-vector-element(src, i) := element-type-fill(vector)
      end;
      src.%size := dst-index
    end if
  end iterate;
  vector
end method remove!;



//
// ACTUAL LIMITED STRETCHY VECTORS
//

define macro limited-stretchy-vector-minus-constructor-definer
  { define limited-stretchy-vector-minus-constructor "<" ## ?:name ## ">" (?superclasses:*)
             (#key ?fill:expression) }
    => { define sealed class "<stretchy-" ## ?name ## "-vector>" (?superclasses)
           slot stretchy-representation :: "<stretchy-" ## ?name ## "-vector-representation>",
             init-value: "$empty-<stretchy-" ## ?name ## "-vector-representation>";
         end class "<stretchy-" ## ?name ## "-vector>";

         define sealed domain stretchy-representation
           ("<stretchy-" ## ?name ## "-vector>");
         define sealed domain stretchy-representation-setter
           ("<stretchy-" ## ?name ## "-vector-representation>", "<stretchy-" ## ?name ## "-vector>");

         define constant "$empty-<stretchy-" ## ?name ## "-vector-representation>"
                            :: "<stretchy-" ## ?name ## "-vector-representation>" =
           make("<stretchy-" ## ?name ## "-vector-representation>", capacity: 0, size: 0);

         define sealed primary class "<stretchy-" ## ?name ## "-vector-representation>"
             (<limited-stretchy-vector-representation>)
           repeated slot "stretchy-" ## ?name ## "-vector-element" :: "<" ## ?name ## ">",
             init-keyword:      fill:,
             init-value:        ?fill,
             size-getter:       size,
             size-init-keyword: capacity:,
             size-init-value:   0;
         end class "<stretchy-" ## ?name ## "-vector-representation>";

         define sealed domain size("<stretchy-" ## ?name ## "-vector-representation>");
         define sealed domain make(singleton("<stretchy-" ## ?name ## "-vector-representation>"));
         define sealed domain initialize("<stretchy-" ## ?name ## "-vector-representation>");

         define sealed inline method stretchy-vector-element
             (rep :: "<stretchy-" ## ?name ## "-vector-representation>", key :: <integer>)
                 => (res :: "<" ## ?name ## ">");
           "stretchy-" ## ?name ## "-vector-element"(rep, key)
         end method;

         define sealed inline method stretchy-vector-element-setter
             (new-value :: "<" ## ?name ## ">",
              rep :: "<stretchy-" ## ?name ## "-vector-representation>", key :: <integer>)
                 => (new-value :: "<" ## ?name ## ">");
           "stretchy-" ## ?name ## "-vector-element"(rep, key) := new-value;
         end method;

         define sealed inline method as
             (class == "<stretchy-" ## ?name ## "-vector>",
              vector :: "<stretchy-" ## ?name ## "-vector>")
          => (vector :: "<stretchy-" ## ?name ## "-vector>")
           vector
         end method as;

         define sealed inline method stretchy-representation-type
             (vector :: "<stretchy-" ## ?name ## "-vector>")
          => (res :: singleton("<stretchy-" ## ?name ## "-vector-representation>"))
           "<stretchy-" ## ?name ## "-vector-representation>"
         end method;

         define sealed inline method size
             (vector :: "<stretchy-" ## ?name ## "-vector>")
          => (sz :: <integer>)
           vector.stretchy-representation.%size
         end method size;

         define sealed inline method element
             (collection :: "<stretchy-" ## ?name ## "-vector>",
              index :: <integer>, #key default = unsupplied())
          => (object)
           let v = collection.stretchy-representation;
           if (element-range-check(index, v.%size))
             "stretchy-" ## ?name ## "-vector-element"(v, index)
           else
             if (unsupplied?(default))
               element-range-error(collection, index)
             else
               default
             end if
           end if
         end method element;

         // We assume here that the underlying vector only grows.
         // If this ceases to be true the following code will need to be changed.

         define inline sealed method element-no-bounds-check
             (collection :: "<stretchy-" ## ?name ## "-vector>",
              index :: <integer>, #key default)
          => (object :: "<" ## ?name ## ">")
           "stretchy-" ## ?name ## "-vector-element"
             (collection.stretchy-representation, index)
         end method element-no-bounds-check;

         define inline sealed method element-no-bounds-check-setter
             (new-value :: "<" ## ?name ## ">",
              collection :: "<stretchy-" ## ?name ## "-vector>", index :: <integer>)
                 => (new-value :: "<" ## ?name ## ">");
           "stretchy-" ## ?name ## "-vector-element"
             (collection.stretchy-representation, index) := new-value
         end method element-no-bounds-check-setter;

         // We assume the representation vector is only ever replaced by a larger
         // vector.  If this assumption ceases to hold the following code will need
         // to be altered.

         define inline function "stretchy-" ## ?name ## "-vector-current-element"
             (vector :: "<stretchy-" ## ?name ## "-vector>", state :: <integer>)
          => (res :: "<" ## ?name ## ">")
           "stretchy-" ## ?name ## "-vector-element"(vector.stretchy-representation, state)
         end function;

         define inline function "stretchy-" ## ?name ## "-vector-current-element-setter"
             (new-value :: "<" ## ?name ## ">",
              vector :: "<stretchy-" ## ?name ## "-vector>", state :: <integer>)
          => (res :: "<" ## ?name ## ">")
           "stretchy-" ## ?name ## "-vector-element"
             (vector.stretchy-representation, state) := new-value
         end function;

         /* THESE ARE CURRENTLY DEFINED ON STRETCHY-VECTOR AND COPIED DOWN
         define sealed method size-setter
             (new-size :: <integer>, vector :: "<stretchy-" ## ?name ## "-vector>")
                  => (new-size :: <integer>)
           check-nat(new-size);
           let v = vector.stretchy-representation;
           if (new-size > v.size)
             let nv :: "<stretchy-vector-" ## ?name ## "-representation>"
               = make("<stretchy-vector-" ## ?name ## "-representation>",
                      capacity: new-size.power-of-two-ceiling, size: new-size,
                      fill: ?fill);
             for (i :: <integer> from 0 below v.%size)
               "stretchy-" ## ?name ## "-vector-element"(nv, i)
                 := "stretchy-" ## ?name ## "-vector-element"(v, i)
             end;
             vector.stretchy-representation := nv;
             new-size;
           elseif (new-size < v.%size)
             let s = v.%size;
             v.%size := new-size;
             for (i :: <integer> from new-size below s)
               "stretchy-" ## ?name ## "-vector-element"(v, i) := ?fill
             end;
             new-size;
           else
             v.%size := new-size
           end if;
         end method size-setter;

         define sealed method remove!
             (vector :: "<stretchy-" ## ?name ## "-vector>", target,
              #key test = \==, count = unsupplied())
                 => (vector :: "<stretchy-" ## ?name ## "-vector>")
           let count :: <integer> =
             if (unsupplied?(count)) vector.size else count end;
           let src = vector.stretchy-representation;
           let src-size = src.%size;
           iterate grovel (count :: <integer> = count,
                           src-index :: <integer> = 0,
                           dst-index :: <integer> = 0)
             if (src-index < src-size)
               let item = "stretchy-" ## ?name ## "-vector-element"(src, src-index);
               case
                 count > 0 & test(item, target) =>
                   grovel(count - 1, src-index + 1, dst-index);
                 otherwise =>
                   "stretchy-" ## ?name ## "-vector-element"(src, dst-index) := item;
                   grovel(count, src-index + 1, dst-index + 1)
               end case
             else
               for (i :: <integer> from dst-index below src-index)
                 "stretchy-" ## ?name ## "-vector-element"(src, i) := ?fill
               end;
               src.%size := dst-index
             end if
           end iterate;
           vector
         end method remove!;
         */

         define sealed inline method forward-iteration-protocol
             (sequence :: "<stretchy-" ## ?name ## "-vector>")
             => (initial-state :: <integer>, limit :: <integer>,
                 next-state :: <function>, finished-state? :: <function>,
                 current-key :: <function>,
                 current-element :: <function>, current-element-setter :: <function>,
                 copy-state :: <function>)
           values(0,
                  sequence.size,
                  sequence-next-state,
                  sequence-finished-state?,
                  sequence-current-key,
                  "stretchy-" ## ?name ## "-vector-current-element",
                  "stretchy-" ## ?name ## "-vector-current-element-setter",
                  identity-copy-state)
         end method forward-iteration-protocol;

         define sealed inline method backward-iteration-protocol
             (sequence :: "<stretchy-" ## ?name ## "-vector>")
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
                  "stretchy-" ## ?name ## "-vector-current-element",
                  "stretchy-" ## ?name ## "-vector-current-element-setter",
                  identity-copy-state)
         end method backward-iteration-protocol;

         define sealed domain make (singleton("<stretchy-" ## ?name ## "-vector>"));
         define sealed domain initialize ("<stretchy-" ## ?name ## "-vector>");

         define sealed copy-down-method choose
             (test :: <function>,
              sequence :: "<stretchy-" ## ?name ## "-vector>")
          => (result :: "<stretchy-" ## ?name ## "-vector>");
         define sealed copy-down-method remove
             (sequence :: "<stretchy-" ## ?name ## "-vector>",
              value :: "<" ## ?name ## ">",
              #key test :: <function> = \==, count :: false-or(<integer>) = #f)
          => (new-sequence :: "<stretchy-" ## ?name ## "-vector>");

         // SHOULD BE COPY DOWNS BUT FAILS TO WORK

         define sealed /* copy-down- */ method as
             (class  == "<stretchy-" ## ?name ## "-vector>",
              collection :: <collection>)
          => (sv :: "<stretchy-" ## ?name ## "-vector>");
           let new-vector :: "<stretchy-" ## ?name ## "-vector>"
             = make("<stretchy-" ## ?name ## "-vector>");
           for (item in collection)
             new-vector := add!(new-vector, item);
           end for;
           new-vector
         end method;

         // SHOULD BE COPY DOWNS BUT FAILS TO WORK

         define sealed /* copy-down- */ method as
             (class == "<stretchy-" ## ?name ## "-vector>",
              collection :: <array>)
          => (sv :: "<stretchy-" ## ?name ## "-vector>");
           let size = size(collection);
           if (size = 0)
             make("<stretchy-" ## ?name ## "-vector>", size: 0);
           else
             let new-vector :: "<stretchy-" ## ?name ## "-vector>"
               = make("<stretchy-" ## ?name ## "-vector>", size: size);
             let d = new-vector.stretchy-representation;
             without-bounds-checks
               for (item in collection, index :: <integer> from 0)
                 stretchy-vector-element(d, index) := item
               end;
             end without-bounds-checks;
             new-vector
             end if
         end method;

         define sealed copy-down-method trusted-size-setter
             (new-size :: <integer>,
              vector :: "<stretchy-" ## ?name ## "-vector>")
          => (new-size :: <integer>);

         define sealed copy-down-method size-setter
             (new-size :: <integer>,
              vector :: "<stretchy-" ## ?name ## "-vector>")
          => (new-size :: <integer>);

         define sealed copy-down-method empty?
             (vector :: "<stretchy-" ## ?name ## "-vector>")
          => (well? :: <boolean>);

         define sealed copy-down-method add!
             (vector :: "<stretchy-" ## ?name ## "-vector>",
              new-element :: "<" ## ?name ## ">")
          => (v :: "<stretchy-" ## ?name ## "-vector>");

         define sealed copy-down-method remove!
             (vector :: "<stretchy-" ## ?name ## "-vector>",
              target :: "<" ## ?name ## ">",
              #key test = \==, count = unsupplied())
          => (vector :: "<stretchy-" ## ?name ## "-vector>");
         }
end macro;

define inline method stretchy-initialize
    (vector :: <stretchy-vector>, capacity :: <integer>, size :: <integer>, fill)
 => ()
  check-nat(size);
  check-nat(capacity);
  if (capacity < size)
    error(make(<capacity-size-error>,
               format-string: "capacity %= < size %=",
               format-arguments: list(capacity, size)))
  end if;
  if (capacity > 0) // otherwise empty-stretchy-vector
    vector.stretchy-representation
      := make(stretchy-representation-type(vector),
              capacity: capacity,
              size:     size,
              fill:     fill);
  end if;
end method;

define macro limited-stretchy-vector-minus-selector-definer
  { define limited-stretchy-vector-minus-selector "<" ## ?:name ## ">" (?superclasses:*)
             (#key ?fill:expression) }
    => { define limited-stretchy-vector-minus-constructor "<" ## ?name ## ">" (?superclasses)
             (fill: ?fill);

         define method initialize
             (vector :: "<stretchy-" ## ?name ## "-vector>",
              #key size :: <integer> = 0, capacity :: <integer> = size,
                   fill :: "<" ## ?name ## ">" = ?fill,
                   element-type-fill: default-fill = ?fill)
          => ()
           ?=next-method();
           vector.element-type-fill := default-fill;
           stretchy-initialize(vector, capacity, size, fill);
         end method initialize;

         define sealed inline method element-type
             (t :: "<stretchy-" ## ?name ## "-vector>") => (type :: <type>)
           "<" ## ?name ## ">"
         end method;

         define sealed method element-setter
             (new-value :: "<" ## ?name ## ">",
              collection :: "<stretchy-" ## ?name ## "-vector>",
              index :: <integer>)
          => (object :: "<" ## ?name ## ">")
           if (index < 0)
             element-range-error(collection, index)
           end if;
           if (index >= collection.size)
             if (index = collection.size)
               trusted-size(collection) := index + 1;
             else
               collection.size := index + 1
             end if
           end if;
           // We assume here that the underlying vector only grows.
           // If this ceases to be true the following code will need to be changed.
           "stretchy-" ## ?name ## "-vector-element"
             (collection.stretchy-representation, index) := new-value
         end method element-setter;

         define sealed inline method type-for-copy
             (vector :: "<stretchy-" ## ?name ## "-vector>")
          => (type :: <type>)
           limited-stretchy-vector(element-type(vector), element-type-fill(vector))
         end method type-for-copy;
        }
end macro;

define macro limited-stretchy-vector-definer
  { define limited-stretchy-vector "<" ## ?:name ## ">" (#key ?fill:expression) }
    => { define limited-stretchy-vector-minus-selector "<" ## ?name ## ">"
             (<limited-stretchy-vector>) (fill: ?fill);
           
         define method concrete-limited-stretchy-vector-class
             (of == "<" ## ?name ## ">", default-fill)
          => (res :: <class>, fully-specified?)
           values("<stretchy-" ## ?name ## "-vector>", default-fill = ?fill)
         end method }
end macro;

define limited-stretchy-vector <object>         (fill: #f);

define method limited-stretchy-vector
    (of :: <type>, default-fill :: <object>) => (type :: <vector-type>)
  let (concrete-class, fully-specified?)
    = concrete-limited-stretchy-vector-class(of, default-fill);
  if (~fully-specified?)
    make(<limited-stretchy-vector-type>,
         class:          <stretchy-vector>,
         element-type:   of,
         default-fill:   default-fill,
         concrete-class: concrete-class);
  else
    concrete-class
  end if;
end method;

define inline copy-down-method map-into-stretchy-one
  (fun :: <function>, target :: <array>, coll :: <stretchy-object-vector>) =>
  (target :: <mutable-collection>);

define inline copy-down-method map-into-rigid-one
  (fun :: <function>, target :: <mutable-collection>, coll :: <stretchy-object-vector>) =>
  (target :: <mutable-collection>);

define inline copy-down-method map-into-rigid-one
  (fun :: <function>, target :: <mutable-sequence>, coll :: <stretchy-object-vector>) =>
  (target :: <mutable-collection>);
