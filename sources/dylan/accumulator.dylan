Module:    internal
Author:   Kevin Mitchell (based on code from Apple's dylan library)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// When we don't know in advance how big a result collection will be we use
// accumulators.   The basic idea is very simple.  We just allocate storage
// in fixed-size blocks, chaining them together when required.  We then
// provide specialized versions of "as" to coerce them to a "real" collection
// when we have finished accumulating the values.

define constant $accumulator-size = 25; // Must be odd.

define sealed abstract class <accumulator> (<mutable-collection>)
  constant slot key-test :: <function> = \==,
    init-keyword: key-test:;
  slot acc-buffer :: <simple-object-vector> =
    make(<simple-object-vector>, size: $accumulator-size, fill: #f);
    // acc-buffer[0] is used to chain the buffers
  slot acc-index :: <integer> = $accumulator-size - 1;
  slot acc-size :: false-or(<integer>) = #f;
end class <accumulator>;


// The following method can be used to extend an accumulator when it becomes
// full.

define function extend-accumulator (accumulator :: <accumulator>)
  let buff = make(<simple-object-vector>, size: $accumulator-size, fill: #f);
  buff[0] := accumulator.acc-buffer;
  accumulator.acc-buffer := buff;
  accumulator.acc-index := $accumulator-size - 1;
  values();
end function extend-accumulator;


// There are two kinds of accumulators, one for building sequences and the
// other one for explicitly keyed collections.  In the explicit key case we
// hold keys and values in adjacent locations in the buffer.

define class <keyed-accumulator> (<accumulator>, <explicit-key-collection>)
end class <keyed-accumulator>;

define class <sequence-accumulator> (<accumulator>, <sequence>)
end class <sequence-accumulator>;


// We use assignment to add entries to a keyed accumulator.

define method element-setter
    (value :: <object>, accumulator :: <keyed-accumulator>,  key :: <object>)
 => (value :: <object>);
  unless (accumulator.acc-index > 0) extend-accumulator(accumulator) end unless;

  let index = accumulator.acc-index;
  accumulator.acc-buffer[index] := value;
  accumulator.acc-buffer[index - 1] := key;
  accumulator.acc-index := index - 2;
end method element-setter;


// We use add! to add entries to the end of a sequence accumulator.

define method add!
    (accumulator :: <sequence-accumulator>, new-element :: <object>)
 => (accumulator :: <sequence-accumulator>);
  unless (accumulator.acc-index > 0) extend-accumulator(accumulator) end unless;

  let index = accumulator.acc-index;
  accumulator.acc-buffer[index] := new-element;
  accumulator.acc-index := index - 1;
  accumulator;
end method add!;


// We only need to iterate over an accumulator once so we just define a
// cutdown version of the iteration protocol for speed.
// First some auxiliary functions for use by the protocol.

define inline function next-state-key-acc
    (accumulator :: <keyed-accumulator>, state :: <integer>)
 => (new-state :: <integer>);
  accumulator.acc-index := accumulator.acc-index - 2;
  if (accumulator.acc-index = 0)
    accumulator.acc-index := $accumulator-size - 1;
    let next-buf = accumulator.acc-buffer[0];
    if (next-buf) accumulator.acc-buffer := next-buf end if;
  end if;
  state + 1
end function next-state-key-acc;

define inline function next-state-seq-acc
    (accumulator :: <sequence-accumulator>, state :: <integer>)
 => (new-state :: <integer>);
  accumulator.acc-index := accumulator.acc-index - 1;
  if (accumulator.acc-index = 0)
    accumulator.acc-index := $accumulator-size - 1;
    let next-buf = accumulator.acc-buffer[0];
    if (next-buf) accumulator.acc-buffer := next-buf end if;
  end if;
  state + 1
end function next-state-seq-acc;

define inline function finished-state?-acc
    (acc :: <accumulator>, state :: <integer>,
     limit :: <integer>)
 => (finished? :: <boolean>);
  state = limit
end function finished-state?-acc;

define inline function current-key-key-acc
    (accumulator :: <keyed-accumulator>, state :: <integer>)
 => (key :: <object>);
  accumulator.acc-buffer[accumulator.acc-index - 1]
end function current-key-key-acc;

define inline function current-key-seq-acc
    (accumulator :: <sequence-accumulator>, state :: <integer>)
 => (key :: <object>);
  state
end function current-key-seq-acc;

define inline function current-element-acc
    (accumulator :: <accumulator>, state :: <integer>)
 => (element :: <object>);
  accumulator.acc-buffer[accumulator.acc-index]
end function current-element-acc;

define inline function current-element-setter-acc
    (value :: <object>, accumulator :: <accumulator>, state :: <integer>)
  error(make(<immutable-error>,
             format-string: "Accumulator is immutable during iteration"))
end function current-element-setter-acc;

define inline function copy-state-acc
    (accumulator :: <accumulator>, state :: <integer>)
  error(make(<iteration-error>,
             format-string: "Accumulator state cannot be copied during iteration"))
end function copy-state-acc;


// The buffers making up the accumulator are chained together "backwards" and
// so need to be reversed before iterating over them.  The method also
// computes the size of the accumulator during the traversal.

define function invert-accumulator(accumulator :: <accumulator>)
 => (size :: <integer>);
  let buff = accumulator.acc-buffer;
  let size = $accumulator-size - accumulator.acc-index - 1;
  let prev = #f;
  let next = buff[0];
  buff[0] := prev;

  while (next)
    size := size + $accumulator-size - 1;
    prev := buff;
    buff := next;
    next := buff[0];
    buff[0] := prev;
  end while;

  accumulator.acc-buffer := buff;
  accumulator.acc-index := $accumulator-size - 1;
  accumulator.acc-size := size
end function invert-accumulator;

define method size(accumulator :: <keyed-accumulator>)
 => (sz :: <integer>);
  ash(accumulator.acc-size | invert-accumulator(accumulator), -1)
end method size;

define method size(accumulator :: <sequence-accumulator>)
 => (sz :: <integer>);
  accumulator.acc-size | invert-accumulator(accumulator)
end method size;


// The iteration protocol for the accumulator classes is non-standard because
// it destructively alters the buffer during the iteration, i.e. it can only
// be used once.  The accumulator classes are private so this should not be a
// problem.  It's not really clear whether it is worth defining a fip on these
// classes but it makes uses of the accumulator neater.

define inline method forward-iteration-protocol
    (accumulator :: <keyed-accumulator>)
 => (init :: <object>, limit :: <object>, next :: <function>,
     finished? :: <function>, key :: <function>, elem :: <function>,
     elem-setter :: <function>, copy :: <function>);
  let sz = accumulator.size;
  values (
    0, sz, next-state-key-acc, finished-state?-acc, current-key-key-acc,
    current-element-acc, current-element-setter-acc, copy-state-acc)
end method forward-iteration-protocol;


define inline method forward-iteration-protocol
    (accumulator :: <sequence-accumulator>)
 => (init :: <object>, limit :: <object>, next :: <function>,
     finished? :: <function>, key :: <function>, elem :: <function>,
     elem-setter :: <function>, copy :: <function>);
  let sz = accumulator.size;
  values (
    0, sz, next-state-seq-acc, finished-state?-acc, current-key-seq-acc,
    current-element-acc, current-element-setter-acc, copy-state-acc)
end method forward-iteration-protocol;


// Once we have added all the elements to the accumulator we must build
// a real collection.  We can do this efficiently at this point as we
// know the size of the collection.  There are four variants as we
// have two different kinds of accumulator and two different kinds of
// target collection, <mutable-sequence> and <mutable-explicit-key-collection>.

define method check-key-test-eq(x, y)
  unless (x.key-test == y.key-test)
    error(make(<key-test-error>,
               format-string: "Collection %= and %= have different key tests",
               format-arguments: list(x,y)))
  end unless
end;


define generic convert-accumulator-as
    (type :: <mutable-collection-type>,
     acc :: <accumulator>)
 => (result :: <mutable-collection>);  // actually :: type;


define method convert-accumulator-as
    (type :: <mutable-sequence-type>, acc :: <sequence-accumulator>)
 => (result :: <mutable-sequence>);  // actually :: type;
  if (size(acc) = 0)
    let target = make-sequence(type, size: 0);
    check-key-test-eq(target, acc);
    target
  else
    // For compatibility, use fill: rather than relying on element-type-fill.
    let target =
      make-sequence(type, size: acc.acc-size, fill: acc.acc-buffer[acc.acc-index]);
    check-key-test-eq(target, acc);
    with-fip-of target /* with-setter? */
      for (e in acc,
           state = initial-state then next-state(target, state))
        current-element(target, state) := e
      end for;
    end with-fip-of;
    target
  end if
end method convert-accumulator-as;


define method convert-accumulator-as
    (type :: <mutable-explicit-key-collection-type>,
     acc :: <sequence-accumulator>)
 => (result :: <mutable-explicit-key-collection>);  // actually :: type;
  if (size(acc) = 0)
    let target = make(type, size: 0);
    check-key-test-eq(target, acc);
    target
  else
    // For compatibility, use fill: rather than relying on element-type-fill.
    let target =
      make(type, size: acc.acc-size, fill: acc.acc-buffer[acc.acc-index]);
    check-key-test-eq(target, acc);
    for (e in acc, i from 0) target[i] := e end;
    target
  end if
end method convert-accumulator-as;


define method convert-accumulator-as
    (type :: <mutable-sequence-type>, acc :: <keyed-accumulator>)
 => (result :: <mutable-sequence>);  // actually :: type;
  let sz = size(acc);

  if (sz = 0)
    let target = make-sequence(type, size: 0);
    check-key-test-eq(target, acc);
    target
  else // Use a temp for fast random update and coerce when done
    let temp = make(<simple-object-vector>, size: sz);

    for (e keyed-by k in acc)
      if (instance?(k, <integer>) & k >= 0 & k < sz)
        temp[k] := e
      else
        error(make(<invalid-index-error>,
                   format-string: "Cannot add an element with key %= "
                       "to a sequence of type %=",
                   format-arguments: list(k, type)))
      end if
    end for;

    let target = as(type, temp);
    check-key-test-eq(target, acc);
    target
  end
end method convert-accumulator-as;


define method convert-accumulator-as
    (type :: <mutable-explicit-key-collection-type>,
     acc :: <keyed-accumulator>)
 => (result :: <mutable-explicit-key-collection>);  // actually :: type;
  let result = make(type, size: size(acc));
  check-key-test-eq(result, acc);
  for (e keyed-by k in acc) result[k] := e end;
  result
end method convert-accumulator-as;
