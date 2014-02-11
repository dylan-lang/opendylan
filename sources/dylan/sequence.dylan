Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <sequence> ... end;

define constant <sequence-type>
  = type-union(subclass(<sequence>), <limited-sequence-type>);

define constant <mutable-sequence-type>
  = type-union(subclass(<mutable-sequence>), <limited-mutable-sequence-type>);


// Instances of <array> must have the dimensions: init-keyword, but generic
// sequence methods won't know to provide that. This function creates an
// <array> or other sequence with the appropriate init-keywords.
//
// The shaped-like: argument creates a sequence with the same dimensions or size
// as the given one. The size: argument creates a sequence with the given size;
// in the case of an <array>, that sequence will be a <vector>. If both are
// given, the sequence will have the same dimensions/size as the shaped-like:
// argument so long as that doesn't conflict with the size: argument.

define generic make-sequence
    (type :: <sequence-type>,
     #key shaped-like :: false-or(<sequence>), size :: false-or(<integer>),
     #all-keys)
 => (new-instance :: <sequence>);

define method make-sequence
    (type :: <sequence-type>, #rest all-keys,
     #key shaped-like: template :: false-or(<sequence>),
          size: desired-size :: false-or(<integer>))
 => (new-instance :: <sequence>)
  if (template)
    if (~desired-size | desired-size = template.size)
      apply(make, type, size: template.size, all-keys)
    else
      apply(make, type, size: desired-size, all-keys)
    end if
  else
    apply(make, type, all-keys)
  end if
end method;

define method make-sequence
    (type :: <array-type>, #rest all-keys,
     #key shaped-like: template :: false-or(<sequence>),
          size: desired-size :: false-or(<integer>))
 => (new-instance :: <array>)
  let all-keys = remove-keyword-arguments(all-keys, #[ size: ]);
  if (instance?(template, <array>))
    if (~desired-size | desired-size = template.size)
      apply(make, type, dimensions: template.dimensions, all-keys)
    else
      apply(make, type, dimensions: vector(desired-size), all-keys)
    end if
  elseif (template)
    if (~desired-size | desired-size = template.size)
      apply(make, type, dimensions: vector(template.size), all-keys)
    else
      apply(make, type, dimensions: vector(desired-size), all-keys)
    end if
  elseif (desired-size)
    apply(make, type, dimensions: vector(desired-size), all-keys)
  else
    apply(make, type, all-keys)
  end if
end method;

define method make-sequence
    (type :: <vector-type>, #rest all-keys,
     #key shaped-like: template :: false-or(<sequence>),
          size: desired-size :: false-or(<integer>))
 => (new-instance :: <vector>)
  if (template)
    if (~desired-size | desired-size = template.size)
      apply(make, type, size: template.size, all-keys)
    else
      apply(make, type, size: desired-size, all-keys)
    end if
  else
    apply(make, type, all-keys)
  end if
end method;


////////////
// INTERFACE
////////////

// Functions on <sequence>

define sealed generic concatenate
    (sequence1 :: <sequence>, #rest sequences :: <sequence>)
 => (result-sequence :: <sequence>);

// type should be subtype of <mutable-sequence>. That is almost expressible by
// saying <mutable-sequence-type>, but the "subclass" used therein is not quite
// the same as "subtype?".
define sealed generic concatenate-as
    (type :: <type>,
     sequence1 :: <sequence>, #rest more-sequences :: <sequence>)
 => (result-sequence :: <mutable-sequence>);

define sealed generic first
  (sequence :: <sequence>, #key default) => (value);

define sealed generic second
  (sequence :: <sequence>, #key default) => (value);

define sealed generic third
  (sequence :: <sequence>, #key default) => (value);


// Open generics on <collection>

define open generic add
  (sequence :: <sequence>, new-element) => (new-sequence :: <sequence>);

//define open generic add!
//  (sequence :: <sequence>, new-element) => (new-sequence :: <sequence>);

define open generic add-new
    (sequence :: <sequence>, new-element, #key test)
 => (new-sequence :: <sequence>);

define open generic add-new!
    (sequence :: <sequence>, new-element, #key test)
 => (new-sequence :: <sequence>);

define open generic remove
    (sequence :: <sequence>, value, #key test, count)
 => (new-sequence :: <sequence>);

//define open generic remove!
//    (sequence :: <sequence>, value, #key test, count)
// => (new-sequence :: <sequence>);

define open generic choose
    (predicate :: <function>, sequence :: <sequence>)
 => (result :: <sequence>);

define open generic choose-by
    (predicate :: <function>,
     test-sequence :: <sequence>, value-sequence :: <sequence>)
 => (result-sequence :: <sequence>);

define open generic intersection
    (sequence1 :: <sequence>, sequence2 :: <sequence>, #key test)
 => (result-sequence :: <sequence>);

define open generic union
    (sequence1 :: <sequence>, sequence2 :: <sequence>, #key test)
 => (result-sequence :: <sequence>);

define open generic remove-duplicates
    (sequence :: <sequence>, #key test)
 => (result-sequence :: <sequence>);

define open generic remove-duplicates!
  (sequence :: <sequence>, #key test) => (result-sequence :: <sequence>);

define open generic copy-sequence
    (source :: <sequence>, #key start, end: last)
 => (result-sequence :: <sequence>);

define open generic replace-subsequence!
    (target-sequence :: <sequence>, insert-sequence :: <sequence>,
     #key start, end: last)
 => (result-sequence :: <sequence>);

define open generic reverse
  (sequence :: <sequence>) => (new-sequence :: <sequence>);

define open generic reverse!
  (sequence :: <sequence>) => (new-sequence :: <sequence>);

define open generic sort
  (sequence :: <sequence>, #key test, stable) => (new-sequence :: <sequence>);

define open generic sort!
  (sequence :: <sequence>, #key test, stable) => (new-sequence :: <sequence>);

define open generic last
  (sequence :: <sequence>, #key default) => object;

define open generic subsequence-position
    (big :: <sequence>, pat :: <sequence>, #key test, count)
 => (index :: false-or(<integer>));



/////////////////
// IMPLEMENTATION
/////////////////


//
// CONCATENATE
//

define inline method concatenate(
    first-seq :: <sequence>, #rest rest-seqs :: <sequence>)
 => (result-seq :: <sequence>);
  apply(concatenate-as, type-for-copy(first-seq), first-seq, rest-seqs)
end;


//
// CONCATENATE-AS
//

// We should try to share as much structure as possible with the original
// arguments.  Note that the DRM allows us to do this, even though the sequence
// may be mutable.

define method concatenate-as(
    type :: <mutable-sequence-type>,
    first-seq :: <sequence>,
    #rest rest-seqs :: <sequence>)
 => (result-seq :: <mutable-sequence>); // Actually :: type
  let num-rests = rest-seqs.size;
  select (num-rests)
    0 => as(type, first-seq);
    1 => concatenate-as-two(type, first-seq, rest-seqs[0]);
    otherwise =>
      let total-sz = first-seq.size;
      let num-non-empty = if (empty?(first-seq)) 0 else 1 end;
      let non-empty-index = num-non-empty - 1;
      for (s in rest-seqs, index from 1)
        let sz = s.size;
        if (sz ~= 0)
          total-sz := total-sz + sz;
          num-non-empty := num-non-empty + 1;
          if (num-non-empty = 1)
            // get the index of the *first* non-empty
            non-empty-index := index;
          end;
        end;
      end;

      select (num-non-empty)
        0 => as(type, #());
        1 => if (non-empty-index = 0)
               as(type, first-seq)
             else
               as(type, rest-seqs[non-empty-index - 1])
             end;
        otherwise =>
          without-bounds-checks
            let fill = if (non-empty-index = 0) first-seq[0]
                       else rest-seqs[non-empty-index - 1][0] end;
            let result = make-sequence(type, size: total-sz, fill: fill);
            with-fip-of result
              let state = initial-state;
              for (val in first-seq)
                current-element(result, state) := val;
                state := next-state(result, state)
              end;

              for (s in rest-seqs)
                for (val in s)
                  current-element(result, state) := val;
                  state := next-state(result, state)
                end
              end;
            end with-fip-of;
            result
          end without-bounds-checks
      end select
  end select
end method;


define method concatenate-as-two
    (type :: <mutable-sequence-type>,
     first-seq :: <sequence>, second-seq :: <sequence>)
 => (result-seq :: <sequence>); // Actually :: type
  case
    empty?(first-seq) => as(type, second-seq);
    empty?(second-seq) => as(type, first-seq);
    otherwise =>
      let result = make-sequence(type, size: first-seq.size + second-seq.size,
                                 fill: first-seq[0]);
      without-bounds-checks
        for (val in first-seq, key from 0)
          result[key] := val;
        finally
          for (val in second-seq, key from key) result[key] := val end
        end
      end without-bounds-checks;
      result
  end case
end;



//
// FIRST
//

define sealed inline method first(sequence :: <sequence>, #rest all-keys, #key default)
 => object;
  apply(element, sequence, 0, all-keys)
end method;

//
// SECOND
//

define sealed inline method second(sequence :: <sequence>, #rest all-keys, #key default)
 => object;
  apply(element, sequence, 1, all-keys)
end method;


//
// THIRD
//

define sealed inline method third(sequence :: <sequence>, #rest all-keys, #key default)
 => object;
  apply(element, sequence, 2, all-keys)
end method;


//
// ADD
//

define method add   // Default version
    (sequence :: <sequence>, new-element) => (new-sequence :: <sequence>);
  concatenate-as(type-for-copy(sequence), list(new-element), sequence)
end;


//
// ADD!
//

define method add!   // Default version
    (sequence :: <sequence>, new-element) => (new-sequence :: <sequence>);
  add(sequence, new-element)
end;


//
// ADD-NEW
//

define method add-new
    (sequence :: <sequence>, new-element, #key test :: <function> = \==)
 => (new-sequence :: <sequence>);
  if (any? (method (el) test(el, new-element) end, sequence))
    sequence
  else
    add(sequence, new-element)
  end if
end method add-new;


//
// ADD-NEW!
//

define method add-new!
    (sequence :: <sequence>, new-element, #key test :: <function> = \==)
 => (new-sequence :: <sequence>);
  if (any? (method (el) test(el, new-element) end, sequence))
    sequence
  else
    add!(sequence, new-element)
  end if
end method add-new!;


//
// REMOVE
//

define method remove
    (sequence :: <sequence>, value,
     #key test :: <function> = \==,
          count :: false-or(<integer>) = #f)
 => (new-sequence :: <sequence>)
  let new-sequence = #();
  let changed? = #f;
  if (count)
    let count :: <integer> = count;
    for (item in sequence)
      if (count > 0 & test(item, value))
        count := count - 1; changed? := #t
      else
        new-sequence := pair(item, new-sequence)
      end if;
    end for
  else
    for (item in sequence)
      if (test(item, value))
        changed? := #t
      else
        new-sequence := pair(item, new-sequence)
      end if
    end for
  end if;
  if (changed?)
    as(sequence.type-for-copy, reverse!(new-sequence))
  else
    sequence
  end
end method remove;


//
// REMOVE!
//

define method remove!
  (sequence :: <sequence>, value,
   #key test :: <function> = \==, count :: false-or(<integer>) = #f)
      => (new-sequence :: <sequence>);
  remove(sequence, value, test: test, count: count)
end method remove!;


//
// CHOOSE
//

define method choose
    (test :: <function>, sequence :: <sequence>)
 => (result :: <sequence>);
  for (result = #() then if (test(item)) pair(item,result) else result end,
       item in sequence)
  finally
    as(sequence.type-for-copy, reverse!(result))
  end for
end method choose;


//
// CHOOSE-BY
//

define method choose-by
    (test :: <function>,
     test-sequence :: <sequence>, value-sequence :: <sequence>)
 => (result-sequence :: <sequence>);
  for (result = #()
         then if (test(test-item)) pair(value-item,result) else result end,
       test-item in test-sequence,
       value-item in value-sequence)
  finally
    as(value-sequence.type-for-copy, reverse!(result))
  end for
end method choose-by;


//
// INTERSECTION
//

define method intersection
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result-sequence :: <list>);
  for (result = #() then if (member?(item, sequence2, test: test))
                           pair(item,result)
                         else result
                         end,
       item in sequence1)
  finally
    result // DRM doesn't force us to use type-for-copy here
  end for
end method intersection;


//
// UNION
//

define method union
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result-sequence :: <list>);
  for (new = as (<list>, sequence2)
         then if (member?(item, sequence2, test: test))
                new
              else
                pair(item,new)
              end,
       item in sequence1)
  finally
    new // DRM doesn't force us to use type-for-copy here
  end for
end method union;


//
// REMOVE-DUPLICATES
//

define method remove-duplicates
    (sequence :: <sequence>, #key test :: <function> = \==)
 => (result-sequence :: <sequence>);
  let duplicates = #f;
  for (new = #()
         then if (any?(method (el) test(el, item) end, new))
                duplicates := #t; new
              else pair(item,new) end,
       item in sequence)
  finally
    if (duplicates) // Not forced to preserve order or type here, but it's
                    // probably natural to do so.
      as(sequence.type-for-copy, reverse!(new))
    else
      sequence
    end
  end for
end method remove-duplicates;


//
// REMOVE-DUPLICATES!
//

define method remove-duplicates!
    (sequence :: <sequence>, #key test :: <function> = \==)
 => (result-sequence :: <sequence>);
  remove-duplicates(sequence, test: test)
end method remove-duplicates!;


//
// COPY-SEQUENCE
//

// We assume that start: and end: are exact bounds rather than limits, as
// implied by the DRM, even though there are arguments against this.
// We assume there will be specialized methods for sequences with expensive
// size methods, e.g. lists and some implementations of deques.

define function invalid-sequence-bounds-error
    (s :: <sequence>, start :: <integer>, finish :: <integer>)
 => (will-never-return :: <bottom>)
  error(make(<subscript-out-of-bounds-error>,
             format-string: "Invalid bounds for %=: start: %d, end: %d",
             format-arguments: list(s, start, finish)))
end function invalid-sequence-bounds-error;

define function invalid-sequence-start-error
    (s :: <sequence>, start :: <integer>)
 => (will-never-return :: <bottom>)
  error(make(<subscript-out-of-bounds-error>,
             format-string: "Invalid start: value of %= for %=",
             format-arguments: list(start, s)))
end function invalid-sequence-start-error;

define function invalid-sequence-end-error
    (s :: <sequence>, finish :: <integer>)
 => (will-never-return :: <bottom>)
  error(make(<subscript-out-of-bounds-error>,
             format-string: "Invalid end: value of %= for %=",
             format-arguments: list(finish, s)))
end function invalid-sequence-end-error;

define method check-start-compute-end
  (seq :: <sequence>, start :: <integer>, last)
      => (real-last :: <integer>);
  let seq-size = seq.size;
  let last :: <integer> = if (unsupplied?(last)) seq-size else last end;

  if (start < 0) invalid-sequence-start-error(seq, start) end;

  case
    last > seq-size =>  invalid-sequence-end-error(seq, last);
    last < start =>     invalid-sequence-bounds-error(seq, start, last);
    otherwise => last;
  end
end method check-start-compute-end;


define method copy-sequence
    (source :: <sequence>,
     #key start: first :: <integer> = 0, end: last = unsupplied())
 => (result-sequence :: <sequence>);
  let last :: <integer> = check-start-compute-end(source, first, last);

  if (first = last) as(type-for-copy(source), #())
  else
    let result =
      make-sequence(type-for-copy(source), shaped-like: source,
                    size: last - first, fill: source[0]);

    with-fip-of source
      for (index from 0 below first,
           state = initial-state then next-state(source, state))
      finally /* Use with-setter? */
        with-fip-of result with prefix r-
          for (index from first below last,
               state = state then next-state(source, state),
               r-state = r-initial-state then r-next-state(result, r-state))
            r-current-element(result, r-state) :=
              current-element(source, state)
          end for
        end
      end
    end;
    result
  end
end method copy-sequence;


//
// REPLACE-SUBSEQUENCE!
//

define method replace-subsequence!
    (target :: <sequence>, insert-sequence :: <sequence>,
     #key start :: <integer> = 0, end: last = unsupplied())
 => (result-sequence :: <sequence>);
  // Let's just give a simple definition and concentrate on efficiency in
  // the mutable cases.
  let last :: <integer> = check-start-compute-end(target, start, last);

  concatenate(copy-sequence(target, start: 0, end: start),
              insert-sequence,
              copy-sequence(target, start: last))
end method replace-subsequence!;


//
// REVERSE
//

define method reverse
  (sequence :: <sequence>) => (new-sequence :: <sequence>);
  let new-sequence :: <list> = #();
  for (item in sequence,
       new-sequence = #() then pair(item, new-sequence))
  finally
    as(sequence.type-for-copy, new-sequence)
  end for;
end method reverse;


//
// REVERSE!
//

define method reverse!
    (sequence :: <sequence>) => (new-sequence :: <sequence>);
  sequence.reverse
end method reverse!;


//
// SORT
//

define method sort (sequence :: <sequence>, #key test = \<, stable: stable)
 => (new-seq :: <sequence>);
  // sort! takes a copy of the sequence if it's not a vector, so don't take
  // a copy here.  But we must ensure that we specialize on vector...
  sort!(sequence, test: test, stable: stable);
end method sort;




//
// LAST
//

define method last (sequence :: <sequence>, #key default = unsupplied())
 => object;
  if (sequence.empty?)
    if (unsupplied?(default))
      error(make(<subscript-out-of-bounds-error>,
                 format-string: "Attempting to retrieve last element "
                     "of empty sequence",
                 format-arguments: #()))
    else
      default
    end if
  else
    sequence[sequence.size - 1] // Not used by <list>
  end if
end method last;


//
// SUBSEQUENCE-POSITION
//

define method subsequence-position
    (big :: <sequence>, pat :: <sequence>,
     #key test :: <function> = \==, count :: <integer> = 1)
 => (index :: false-or(<integer>));
  if (empty?(pat))
    let n = size(big);
    if (count > n) n else count - 1 end;
  else
    with-fip-of pat with prefix pat-
      with-fip-of big
        iterate search(index = 0,
                       index-state = initial-state,
                       big-state = copy-state(big, initial-state),
                       pat-state = pat-copy-state(pat, pat-initial-state),
                       count = count)
          case
            pat-finished-state?(pat, pat-state, pat-limit) =>
              if (count = 1)
                index
              else
                let next = next-state(big, index-state);
                search(index + 1, next, copy-state(big, next),
                       pat-copy-state(pat, pat-initial-state), count - 1);
              end if;
            finished-state?(big, big-state, limit) =>
              #f;
            test(current-element(big, big-state),
                 pat-current-element(pat, pat-state)) =>
              search(index, index-state, next-state(big, big-state),
                     pat-next-state(pat, pat-state), count);
            otherwise =>
              let next = next-state(big, index-state);
              search(index + 1, next, next & copy-state(big, next),
                     pat-copy-state(pat, pat-initial-state), count);
          end case;
        end iterate;
      end with-fip-of
    end with-fip-of
  end if;
end method subsequence-position;


//
// Specialized inherited generic methods
//


//
// SHALLOW-COPY
//

define method shallow-copy (sequence :: <sequence>) => (new :: <sequence>)
  copy-sequence(sequence)
end method shallow-copy;


//
// FIND-KEY
//

define method find-key
    (collection :: <sequence>, fn :: <function>,
     #key skip :: <integer> = 0, failure = #f) => (key)
  for (e in collection,
       found = #f then fn(e) & ((skip := skip - 1) < 0),
       index = -1 then index + 1,
       until: found)
  finally
    if (found) index else failure end if
  end for
end method;


//
// KEY-SEQUENCE
//

define method key-sequence (sequence :: <sequence>) => (keys :: <sequence>)
  let the-size = sequence.size;
  if (the-size)
    range(from: 0, below: the-size)
  else
    range(from: 0)
  end if
end method key-sequence;


//
// KEY-TEST
//

define sealed method key-test (collection :: <sequence>) => (tst :: <function>)
  \==
end method key-test;

//
// ELEMENT-RANGE-CHECK
//

define inline sealed method element-range-check
    (index :: <integer>, below :: <integer>) => (well? :: <boolean>)
  // optimize because of the property of negative signed integers
  // being greater than all positive signed integers
  machine-word-unsigned-less-than?
    (interpret-integer-as-machine-word(index),
     interpret-integer-as-machine-word(below))
  // index >= 0 & index < below
end method;

//
// ITERATION PROTOCOL
//

define inline function identity-copy-state
    (collection :: <sequence>, state :: <object>) => (result :: <object>)
  state
end function;

define inline function sequence-next-state
    (sequence :: <sequence>, state :: <integer>) => (state :: <integer>)
  state + 1
end function;

define inline function sequence-previous-state
    (sequence :: <sequence>, state :: <integer>) => (state :: <integer>)
  state - 1
end function;

define inline function sequence-finished-state?
    (sequence :: <sequence>, state :: <integer>, limit :: <integer>)
 => (finished? :: <boolean>)
  state = limit
end function;

define inline function sequence-current-key
    (sequence :: <sequence>, state :: <integer>) => (key :: <integer>)
  state
end function;

define inline function sequence-current-element-setter
    (new-value, sequence :: <sequence>, state :: <integer>)
 => (will-never-return :: <bottom>)
  error(make(<immutable-error>,
             format-string: "Sequence %= is immutable",
             format-arguments: list(sequence)))
end function;


define inline method forward-iteration-protocol (sequence :: <sequence>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  values(0,
         sequence.size,
         sequence-next-state,
         sequence-finished-state?,
         sequence-current-key,
         element,
         sequence-current-element-setter,
         identity-copy-state)
end method forward-iteration-protocol;


define inline method backward-iteration-protocol (sequence :: <sequence>)
 => (final-state :: <integer>, limit :: <integer>,
     previous-state :: <function>,   finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,  current-element-setter :: <function>,
     copy-state :: <function>);
  values(sequence.size - 1,
         -1,
         sequence-previous-state,
         sequence-finished-state?,
         sequence-current-key,
         element,
         sequence-current-element-setter,
         identity-copy-state)
end method backward-iteration-protocol;


//
// MAXIMUM-SEQUENCE-KEY
//

define method maximum-sequence-key(collection :: <sequence>)
 => (key :: <integer>);
  collection.size - 1
end method maximum-sequence-key;
