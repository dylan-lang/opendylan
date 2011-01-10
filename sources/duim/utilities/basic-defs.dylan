Module:       duim-utilities
Synopsis:     DUIM utilities
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Debug support

define variable *debug-duim-function* :: false-or(<function>) = #f;

define function duim-debug-message
    (message :: <string>, #rest args) => ()
  let message-function = *debug-duim-function*;
  if (message-function)
    apply(message-function, message, args)
  end
end function duim-debug-message;


/// Numeric constants

// Various forms of pi, as single floats
define constant $pi   :: <single-float> = $single-pi;
define constant $2pi  :: <single-float> = $single-pi * 2;
define constant $pi/2 :: <single-float> = $single-pi / 2;


/// Simpler table accessors

define function gethash
    (table :: <table>, key, #key default = #f) => (value, found? :: <boolean>)
  let value = element(table, key, default: $unfound);
  if (value == $unfound)
    values(default, #f)
  else
    values(value, #t)
  end
end function gethash;

define inline function gethash-setter
    (value, table :: <table>, key) => (value)
  table[key] := value
end function gethash-setter;

define inline function remhash (table :: <table>, key) => ()
  remove-key!(table, key)
end function remhash;


/// New table subclass

// This hashes objects as normal, except for <string>s, which compare and
// hash as for <string-table>.
define class <string-or-object-table> (<table>)
end class <string-or-object-table>;

define sealed method table-protocol
    (table :: <string-or-object-table>)
 => (test-function :: <function>, hash-function :: <function>)
  values(method (obj1, obj2) => (equal? :: <boolean>)
	   if (instance?(obj1, <string>) & instance?(obj1, <string>))
	     obj1 = obj2
	   else
	     obj1 == obj2
	   end
	 end,
	 method
	     (key, initial-state :: <hash-state>)
	  => (id :: <integer>, result-state :: <hash-state>)
	   if (instance?(key, <string>))
	     string-hash(key, initial-state)
	   else
	     object-hash(key, initial-state)
	   end
	 end)
end method table-protocol;


/// Sequence hacking

define inline function substitute! (sequence, old, new) => (sequence)
  replace-elements!(sequence, curry(\=, old), method (old) ignore(old); new end)
end function substitute!;

define inline function substitute (sequence, old, new) => (sequence)
  substitute!(copy-sequence(sequence), old, new)
end function substitute;


define inline function range-check
    (sequence :: <sequence>, _size :: <integer>, _start :: <integer>, _end :: <integer>) => ()
  when (_start < 0 | _start > _size)
    element-range-error(sequence, _start)
  end;
  when (_end < 0 | _end > _size)
    element-range-error(sequence, _end)
  end
end function range-check;


define method insert-at!
    (sv :: <stretchy-object-vector>, item, index)
 => (sv :: <stretchy-object-vector>)
  local method move-up
	    (sv :: <stretchy-object-vector>, index :: <integer>) => ()
	  without-bounds-checks
	    for (i :: <integer> from (size(sv) - 1) to (index + 1) by -1)
	      sv[i] := sv[i - 1]
	    end
	  end
	end method;
  select (index)
    #"start" =>
      sv.size := sv.size + 1;
      move-up(sv, 0);
      sv[0] := item;
    #"end" =>
      add!(sv, item);
    otherwise =>
      sv.size := sv.size + 1;
      move-up(sv, index);
      sv[index] := item;
  end;
  sv
end method insert-at!;

define method remove-at!
    (sv :: <stretchy-object-vector>, index)
 => (sv :: <stretchy-object-vector>)
  local method move-down
	    (sv :: <stretchy-object-vector>, index :: <integer>) => ()
	  without-bounds-checks
	    for (i :: <integer> from index to (size(sv) - 2))
	      sv[i] := sv[i + 1]
	    end
	  end
	end method;
  select (index)
    #"start"  => move-down(sv, 0);
    #"end"    => #f;
    otherwise => move-down(sv, index);
  end;
  sv.size := sv.size - 1;
  sv
end method remove-at!;


define method find-pair
    (sequence :: <sequence>, item, #key test = \==) => (result)
  block (return)
    for (pair in sequence)
      when (pair & test(pair[0], item))
	return(pair)
      end
    end;
    #f
  end
end method find-pair;


define method count
    (predicate, sequence :: <sequence>) => (count :: <integer>)
  let c = 0;
  for (item in sequence)
    when (predicate(item))
      inc!(c)
    end
  end;
  c
end method count;


define inline function primitive-position
    (sequence :: <sequence>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence))
 => (index :: false-or(<integer>))
  range-check(sequence, size(sequence), _start, _end);
  block (return)
    without-bounds-checks
      for (i :: <integer> = _start then i + 1,
	   until: i = _end)
	when (test(item, sequence[i]))
	  return(i)
	end
      end
    end;
    #f
  end
end function primitive-position;

define method position
    (sequence :: <sequence>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence))
 => (index :: false-or(<integer>))
  primitive-position(sequence, item, test: test, start: _start, end: _end)
end method position;

define sealed method position
    (vector :: <simple-object-vector>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(vector))
 => (index :: false-or(<integer>))
  primitive-position(vector, item, test: test, start: _start, end: _end)
end method position;

define sealed method position
    (vector :: <stretchy-object-vector>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(vector))
 => (index :: false-or(<integer>))
  primitive-position(vector, item, test: test, start: _start, end: _end)
end method position;

define sealed method position
    (string :: <byte-string>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (index :: false-or(<integer>))
  primitive-position(string, item, test: test, start: _start, end: _end)
end method position;


/// Array hacking

// Makes and fills a 2d-array from contents, which is a sequence of sequences
define method make-array-from-contents
    (contents :: <sequence>) => (array :: <array>)
  let nrows :: <integer> = size(contents);
  let ncols :: <integer> = reduce(method (v, x) max(v, size(x)) end, 0, contents);
  let array :: <array>   = make(<array>, dimensions: list(nrows, ncols));
  for (row :: <integer> from 0 below nrows)
    let subcontents = contents[row];
    for (col :: <integer> from 0 below min(size(subcontents), ncols))
      array[row,col] := subcontents[col]
    end
  end;
  array
end method make-array-from-contents;

// Fills a 2d-array from a sequence
define method fill-array!
    (array :: <array>, sequence :: <sequence>) => (array :: <array>)
  let ncols :: <integer> = dimension(array, 1);
  let row :: <integer> = 0;
  let col :: <integer> = 0;
  fill!(array, #f);
  for (item in sequence)
    array[row, col] := item;
    inc!(col);
    when (col = ncols)
      col := 0;
      inc!(row)
    end
  end;
  array
end method fill-array!;


/// Conditions

define function warn (format-string :: <string>, #rest format-args) => ()
  dynamic-extent(format-args);
  apply(duim-debug-message,
	concatenate("Warning: ", format-string), format-args)
end function warn;
