Module:       common-dylan-internals
Author:       Jonathan Bachrach, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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
          skip :: <integer>,
          //---*** For compatibility: remove this eventually
          count)
 => (position :: false-or(<integer>));

define method position
    (sequence :: <sequence>, target, 
     #key test :: <function> = \==, 
          skip :: <integer> = 0,
          count)
 => (position :: false-or(<integer>))
  if (count)
    skip := count - 1
  end;
  block (return)
    for (index :: <integer> from 0, item in sequence)
      let matched? = test(target, item);
      if (matched?)
        if (skip = 0)
	  return(index);
        end;
	skip := skip - 1;
      end;
    end;
  end;
end method position;

// Copy-down method for simple object vectors...
define sealed method position
    (sequence :: <simple-object-vector>, target, 
     #key test :: <function> = \==, 
          skip :: <integer> = 0,
          count)
 => (position :: false-or(<integer>))
  if (count)
    skip := count - 1
  end;
  block (return)
    for (index :: <integer> from 0, item in sequence)
      let matched? = test(target, item);
      if (matched?)
        if (skip = 0)
	  return(index);
        end;
	skip := skip - 1;
      end
    end
  end
end method position;


define open generic split
    (collection :: <string>, character :: <character>,
     #key start :: <integer>, 
          end: _end :: <integer>,
          trim? :: <boolean>)
 => (strings :: <sequence>);

define sealed method split
    (string :: <byte-string>, character :: <byte-character>,
     #key start :: <integer> = 0,
          end: _end :: <integer> = string.size,
          trim? :: <boolean> = #t)
 => (strings :: <stretchy-object-vector>)
  let old-position :: <integer> = start;
  let end-position :: <integer> = _end;
  let new-position :: <integer> = old-position;
  let results :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  local method add-substring
	    (start :: <integer>, _end :: <integer>, #key last? :: <boolean> = #f) => ()
	  if (trim?)
	    while (start < _end & string[start] = ' ')
	      start := start + 1
	    end;
	    while (start < _end & string[_end - 1] = ' ')
	      _end := _end - 1
	    end
	  end;
	  // Don't ever return just a single empty string
	  if (~last? | start ~== _end | ~empty?(results))
	    add!(results, copy-sequence(string, start: start, end: _end))
	  end
	end method add-substring;
  while (new-position < end-position)
    if (string[new-position] = character)
      add-substring(old-position, new-position);
      new-position := new-position + 1;
      old-position := new-position
    else
      new-position := new-position + 1;
    end
  end;
  add-substring(old-position, new-position, last?: #t);
  results
end method split;


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
