Module:       common-dylan-internals
Author:       Jonathan Bachrach, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $unfound = #("NOT FOUND");
define inline constant unfound    = method () $unfound end;
define inline constant unfound?   = method (x) x == $unfound; end;
define inline constant found?     = method (x) ~(x == $unfound); end;

define constant $unsupplied = #("UNSUPPLIED");
define inline constant unsupplied  = method () $unsupplied end;
define inline constant unsupplied? = method (x) x == $unsupplied; end;
define inline constant supplied?   = method (x) ~(x == $unsupplied); end;


/// Basic utilities

define inline function false?
    (object :: <object>) => (false? :: <boolean>)
  object == #f
end function false?;

define inline function true?
    (object :: <object>) => (true? :: <boolean>)
  object ~== #f
end function true?;


define open generic concatenate!
    (sequence :: <sequence>, #rest more-sequences)
 => (result-sequence :: <sequence>);

define method concatenate! 
    (s :: <sequence>, #rest more) => (result-sequence :: <sequence>)
  apply(concatenate, s, more);
end method concatenate!;

define method concatenate!
    (vector :: <stretchy-vector>, #rest more)
 => (vector :: <stretchy-vector>)
  for (sv in more)
    for (e in sv)
      add!(vector, e);
    end for;
  end for;
  vector
end method concatenate!;

// from jonathan, 97nov21
define method concatenate! (x :: <list>, #rest more) => (z :: <list>)
  // find first non-empty arg., to be destructively updated and returned
  iterate find-result (r :: <list> = x, i :: <integer> = 0)
    if (i = size(more))
      r
    elseif (empty?(r)) 
      find-result(as(<list>, more[i]), i + 1) // skip empty arg prefix
    else
      // p points into r (which is growing).  p is non-empty.
      iterate connect (p :: <list> = r, i :: <integer> = i)
        if (i = size(more))
          r
	else
	  // find next non-empty arg. to add
          let x = as(<list>, more[i]);
          if (empty?(x))
            connect(p, i + 1)      // skip empty arg
	  else
	    // cdr to end of p and side-effect tail
 	    iterate find-tail (p :: <list> = p) 
	      if (empty?(tail(p)))
	        tail(p) := x;      // DESTRUCTIVE UPDATE
                connect(x, i + 1)  // connect next arg
	      else
	        find-tail(tail(p))
	      end if
	    end iterate
          end if
        end if;
      end iterate 
    end if
  end iterate
end method;

define open generic difference
    (sequence-1 :: <sequence>, sequence-2 :: <sequence>, 
     #key test :: <function>)
 => (result-sequence :: <sequence>);

define method difference
    (sequence-1 :: <sequence>, sequence-2 :: <sequence>,
     #key test :: <function> = \==)
 => (result-sequence :: <sequence>)
 choose(method (item) ~member?(item, sequence-2, test: test) end,
        sequence-1);
end method difference;


define open generic position
    (sequence :: <sequence>, target,
     #key test :: <function>,
          start: _start :: <integer>,
          end: _end,
          skip :: <integer>,
          //---*** For compatibility: remove this eventually
          count)
 => (position :: false-or(<integer>));

define method position
    (sequence :: <sequence>, target, 
     #key test :: <function> = \==, 
          start: _start :: <integer> = 0,
          end: _end,
          skip :: <integer> = 0,
          count)
 => (position :: false-or(<integer>))
  with-fip-of sequence
    // Skip indices before _start
    for (index :: <integer> from 0 below _start,
         state = initial-state then next-state(sequence, state))
    finally
      // Search for a matching index
      iterate loop (index :: <integer> = _start,
                    skip :: <integer> = if (count) count - 1 else skip end,
                    state = state)
        if (finished-state?(sequence, state, limit) | (_end & index >= _end))
          #f
        elseif (test(target, current-element(sequence, state)))
          if (skip = 0)
            index
          else
            loop(index + 1, skip - 1, next-state(sequence, state))
          end if
        else
          loop(index + 1, skip, next-state(sequence, state))
        end if
      end iterate
    end for
  end with-fip-of
end method position;

// Specialization for random-access sequences (subclasses of <vector>)
define method position
    (sequence :: <vector>, target, 
     #key test :: <function> = \==, 
          start: _start :: <integer> = 0,
          end: _end :: <integer> = sequence.size,
          skip :: <integer> = 0,
          count)
 => (position :: false-or(<integer>))
  // Search for a matching index
  iterate loop (index :: <integer> = _start,
                skip :: <integer> = if (count) count - 1 else skip end)
    if (index >= _end)
      #f
    elseif (test(target, sequence[index]))
      if (skip = 0)
        index
      else
        loop(index + 1, skip - 1)
      end if
    else
      loop(index + 1, skip)
    end if
  end iterate;
end method position;

define sealed copy-down-method position
    (sequence :: <simple-object-vector>, target, 
     #key test :: <function> = \==, 
          start: _start :: <integer> = 0,
          end: _end :: <integer> = sequence.size,
          skip :: <integer> = 0,
          count)
 => (position :: false-or(<integer>));

define sealed copy-down-method position
    (sequence :: <byte-string>, target, 
     #key test :: <function> = \==, 
          start: _start :: <integer> = 0,
          end: _end :: <integer> = sequence.size,
          skip :: <integer> = 0,
          count)
 => (position :: false-or(<integer>));

// Split a sequence into parts at each occurrance of the 'separator'
// and return a sequence containing the parts.  The sequence is
// searched from beginning to end for the given 'separator' and stops
// when it reaches the end of 'sequence' or when the size of the
// result reaches 'count' elements.  The meaning of the 'start' and
// 'end' parameters may differ for different methods, but the intent
// is that it be the same as if you passed in the subsequence delimited
// by 'start' and 'end'.  See the individual methods for details.
//
define open generic split
    (sequence :: <sequence>, separator :: <object>,
     #key start :: <integer>,
          end: epos :: <integer>,
          count :: <integer>,
          remove-if-empty :: <boolean>)
 => (parts :: <sequence>);

// This is in some sense the most basic method, since others can be
// implemented in terms of it.  The 'separator' function must accept
// three arguments: (1) the sequence in which to search for a
// separator, (2) the start index in that sequence at which to begin
// searching, and (3) the index at which to stop searching.  The
// 'separator' function must return #f to indicate that no separator
// was found, or two values: the start and end indices of the
// separator in the given sequence.  The initial start and end
// indices passed to the 'separator' function are the same as the
// 'start' and 'end' arguments passed to this method.  The
// 'separator' function should stay within the given bounds whenever
// possible.  (In particular it may not always be possible when the
// separator is a regex.)
define method split
    (seq :: <sequence>, find-separator :: <function>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = seq.size,
          count :: <integer> = epos + 1,
          remove-if-empty :: <boolean> = #f)
 => (parts :: <sequence>)
  reverse!(iterate loop (bpos :: <integer> = start,
                         parts :: <list> = #(),
                         nparts :: <integer> = 1)
             let (sep-start, sep-end) = find-separator(seq, bpos, epos);
             if (sep-start & sep-end & (sep-end <= epos) & (nparts < count))
               let part = copy-sequence(seq, start: bpos, end: sep-start);
               let remove? = remove-if-empty & empty?(part);
               loop(sep-end,
                    if (remove?) parts else pair(part, parts) end,
                    if (remove?) nparts else nparts + 1 end)
             else
               pair(copy-sequence(seq, start: bpos, end: epos), parts)
             end
           end)
end method split;

// Splits seq around occurrances of the separator subsequence.
// Works for the relatively common case where seq and separator
// are both <string>s.
define method split
    (seq :: <sequence>, separator :: <sequence>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = seq.size,
          count :: <integer> = epos + 1,
          test :: <function> = \==,
          remove-if-empty :: <boolean> = #f)
 => (parts :: <sequence>)
  // Is there a function that does this already?
  local method looking-at? (pattern :: <sequence>, big :: <sequence>,
                            bpos :: <integer>)
          block (return)
            let len :: <integer> = big.size;
            for (thing in pattern, pos from bpos)
              if (pos >= len | ~test(thing, big[pos]))
                return(#f)
              end if;
            end for;
            #t
          end
        end method looking-at?;
  local method find-subseq (seq :: <sequence>,
                            bpos :: <integer>,
                            epos :: false-or(<integer>))
          // Note that this only splits on the separator sequence if it is
          // entirely contained between the start and end positions.
          let epos :: <integer> = epos | seq.size;
          let max-separator-start :: <integer> = epos - separator.size;
          block (exit-loop)
            for (seq-index from bpos to max-separator-start)
              if (looking-at?(separator, seq, seq-index))
                exit-loop(seq-index, seq-index + separator.size);
              end;
            end;
            #f      // separator not found
          end
        end;
  split(seq, find-subseq, start: start, end: epos, count: count,
        remove-if-empty: remove-if-empty)
end method split;

// Split on a given object.
// This handles the common (<string>, <character>) case.
define method split
    (seq :: <sequence>, separator :: <object>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = seq.size,
          count :: <integer> = epos + 1,
          test :: <function> = \==,
          remove-if-empty :: <boolean> = #f)
 => (parts :: <sequence>)
  local method find-pos (seq :: <sequence>,
                         bpos :: <integer>,
                         epos :: false-or(<integer>))
          // Unfortunately the position function doesn't accept
          // start and end parameters so we have to write our own.
          block (exit-loop)
            for (i from bpos below epos)
              if (test(seq[i], separator))
                exit-loop(i, i + 1)
              end;
            end;
            #f
          end block
        end method;
  split(seq, find-pos, start: start, end: epos, count: count,
        remove-if-empty: remove-if-empty)
end method split;

// Join several sequences together, including a separator between each
// sequence.  If the first argument is empty, an empty sequence of type
// type-for-copy(separator) is returned.
//
define open generic join
    (items :: <sequence>, separator :: <sequence>, #key key, conjunction)
 => (joined :: <sequence>);

// join(range(from: 1, to: 3), ", ",
//      key: integer-to-string,
//      conjunction: " and ");
// => "1, 2 and 3"
//
define method join
    (sequences :: <sequence>, separator :: <sequence>,
     #key key :: <function> = identity,
          conjunction :: false-or(<sequence>))
 => (joined :: <sequence>)
  if (key ~== identity)
    // This allocates a new list, but allows us to calculate the result-size
    // correctly when key is passed.  Seems the lesser of two evils.
    sequences := map(key, sequences);
  end;
  let length :: <integer> = sequences.size;
  if (length == 0)
    make(type-for-copy(separator), size: 0)
  elseif (length == 1)
    sequences[0]
  else
    let result-size :: <integer>
      = (reduce(method (len, seq)
                  len + seq.size
                end,
                0,
                sequences)
           + (separator.size * (length - 1))
           + if (conjunction)
               // the last separator is replaced by the conjunction
               conjunction.size - separator.size
             else
               0
             end);
    let first = sequences[0];
    let result = make(object-class(first), size: result-size);
    let result-index :: <integer> = 0;
    local method copy-to-result (seq :: <sequence>)
            result := replace-subsequence!(result, seq, start: result-index);
            result-index := result-index + seq.size;
          end;
    copy-to-result(first);
    let max-index :: <integer> = length - 1;
    for (i :: <integer> from 1 to max-index)
      let seq :: <sequence> = sequences[i];
      copy-to-result(if(conjunction & i == max-index)
                       conjunction
                     else
                       separator
                     end);
      copy-to-result(seq);
    end;
    result
  end if
end method join;
    
define open generic find-element
    (collection :: <collection>, predicate :: <function>,
     #key skip :: <integer>, failure)
 => (element);

define method find-element
    (collection :: <collection>, predicate :: <function>,
     #key skip :: <integer> = 0,
          failure = #f)
 => (element)
  let key = find-key(collection, predicate, skip: skip, failure: unfound());
  if (unfound?(key))
    failure
  else
    element(collection, key, default: failure);
  end;
end method find-element;

//---*** This should be removed at some point
define method find-value
    (collection :: <collection>, predicate :: <function>,
     #key skip :: <integer> = 0,
          failure = #f)
 => (value)
  find-element(collection, predicate, skip: skip, failure: failure)
end method find-value;


define function fill-table!
    (table :: <table>, keys-and-elements :: <sequence>)
 => (table :: <table>)
  let key = #f;
  for (object in keys-and-elements)
    if (key)
      table[key] := object;
      key := #f;
    else
      key := object
    end
  end;
  table
end function fill-table!;


define method default-last-handler (condition :: <serious-condition>, next-handler :: <function>)
  block()
    format-out("%s\n", condition);
  exception ( print-error :: <error> )
    format-out("%=\nsignalled while trying to print an instance of %=\n",
	       print-error, object-class(condition));
  end block;
  next-handler();
end method;

define last-handler <serious-condition> = default-last-handler;

define sideways method default-handler (condition :: <warning>)
  debug-message("Warning: %s", condition-to-string(condition) | condition);  
end method;


define function exit-application (code :: <integer>) => ()
  primitive-exit-application(integer-as-raw(code))
end function exit-application;
