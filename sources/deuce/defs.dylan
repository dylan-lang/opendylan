Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Various useful definitions

define inline function true
    (#rest args) => (true :: singleton(#t))
  ignore(args);
  #t
end function true;

define inline function false
    (#rest args) => (false :: singleton(#f))
  ignore(args);
  #f
end function false;


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


/// Sequence hacking

define inline function range-check
    (sequence :: <sequence>, _size :: <integer>, _start :: <integer>, _end :: <integer>) => ()
  when (_start < 0 | _start > _size)
    element-range-error(sequence, _start)
  end;
  when (_end < 0 | _end > _size)
    element-range-error(sequence, _end)
  end
end function range-check;


define inline function primitive-position
    (sequence :: <sequence>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence), from-end?)
 => (index :: false-or(<integer>))
  range-check(sequence, size(sequence), _start, _end);
  block (return)
    let (_start :: <integer>, _end :: <integer>, increment :: <integer>)
      = if (from-end?)
          values(_end - 1, _start - 1, -1)
        else
          values(_start, _end, 1)
        end;
    without-bounds-checks
      for (i :: <integer> = _start then i + increment,
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
          start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence), from-end?)
 => (index :: false-or(<integer>))
  primitive-position(sequence, item,
                     test: test, start: _start, end: _end, from-end?: from-end?)
end method position;

define sealed method position
    (vector :: <simple-object-vector>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(vector), from-end?)
 => (index :: false-or(<integer>))
  primitive-position(vector, item,
                     test: test, start: _start, end: _end, from-end?: from-end?)
end method position;

define sealed method position
    (vector :: <stretchy-object-vector>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(vector), from-end?)
 => (index :: false-or(<integer>))
  primitive-position(vector, item,
                     test: test, start: _start, end: _end, from-end?: from-end?)
end method position;

define sealed method position
    (string :: <byte-string>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(string), from-end?)
 => (index :: false-or(<integer>))
  primitive-position(string, item,
                     test: test, start: _start, end: _end, from-end?: from-end?)
end method position;


define inline function primitive-position-if
    (sequence :: <sequence>, predicate :: <function>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence), from-end?)
 => (index :: false-or(<integer>))
  range-check(sequence, size(sequence), _start, _end);
  block (return)
    let (_start :: <integer>, _end :: <integer>, increment :: <integer>)
      = if (from-end?)
          values(_end - 1, _start - 1, -1)
        else
          values(_start, _end, 1)
        end;
    without-bounds-checks
      for (i :: <integer> = _start then i + increment,
           until: i = _end)
        when (predicate(sequence[i]))
          return(i)
        end
      end
    end;
    #f
  end
end function primitive-position-if;

define method position-if
    (sequence :: <sequence>, predicate :: <function>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence), from-end?)
 => (index :: false-or(<integer>))
  primitive-position-if(sequence, predicate,
                        start: _start, end: _end, from-end?: from-end?)
end method position-if;

define sealed method position-if
    (vector :: <simple-object-vector>, predicate :: <function>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(vector), from-end?)
 => (index :: false-or(<integer>))
  primitive-position-if(vector, predicate,
                        start: _start, end: _end, from-end?: from-end?)
end method position-if;

define sealed method position-if
    (vector :: <stretchy-object-vector>, predicate :: <function>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(vector), from-end?)
 => (index :: false-or(<integer>))
  primitive-position-if(vector, predicate,
                        start: _start, end: _end, from-end?: from-end?)
end method position-if;

define sealed method position-if
    (string :: <byte-string>, predicate :: <function>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string), from-end?)
 => (index :: false-or(<integer>))
  primitive-position-if(string, predicate,
                        start: _start, end: _end, from-end?: from-end?)
end method position-if;


define inline function primitive-count
    (sequence :: <sequence>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence))
 => (index :: false-or(<integer>))
  range-check(sequence, size(sequence), _start, _end);
  let n :: <integer> = 0;
  without-bounds-checks
    for (i :: <integer> from _start below _end)
      when (test(item, sequence[i]))
        inc!(n)
      end
    end
  end;
  n
end function primitive-count;

define method count
    (sequence :: <sequence>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(sequence))
 => (index :: false-or(<integer>))
  primitive-count(sequence, item,
                  test: test, start: _start, end: _end)
end method count;

define sealed method count
    (vector :: <simple-object-vector>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(vector))
 => (index :: false-or(<integer>))
  primitive-count(vector, item,
                  test: test, start: _start, end: _end)
end method count;

define sealed method count
    (vector :: <stretchy-object-vector>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(vector))
 => (index :: false-or(<integer>))
  primitive-count(vector, item,
                  test: test, start: _start, end: _end)
end method count;

define sealed method count
    (string :: <byte-string>, item,
     #key test = \==,
          start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (index :: false-or(<integer>))
  primitive-count(string, item,
                  test: test, start: _start, end: _end)
end method count;


// Inserts the new item at the given index, effectively discarding the very
// last item in the vector
define method insert-at!
    (v :: type-union(<vector>, <string>), item, index)
 => (v :: type-union(<vector>, <string>))
  local method expand (v, index :: <integer>) => ()
          without-bounds-checks
            for (i :: <integer> from (size(v) - 1) to (index + 1) by -1)
              v[i] := v[i - 1]
            end
          end
        end method;
  select (index)
    #"start" =>
      expand(v, 0);
      v[0] := item;
    #"end" =>
      add!(v, item);
    otherwise =>
      expand(v, index);
      v[index] := item;
  end;
  v
end method insert-at!;

// Like the above, but grows the stretchy vector
define method insert-at!
    (sv :: <stretchy-object-vector>, item, index)
 => (sv :: <stretchy-object-vector>)
  local method expand (sv, index :: <integer>) => ()
          without-bounds-checks
            for (i :: <integer> from (size(sv) - 1) to (index + 1) by -1)
              sv[i] := sv[i - 1]
            end
          end
        end method;
  select (index)
    #"start" =>
      sv.size := sv.size + 1;
      expand(sv, 0);
      sv[0] := item;
    #"end" =>
      add!(sv, item);
    otherwise =>
      sv.size := sv.size + 1;
      expand(sv, index);
      sv[index] := item;
  end;
  sv
end method insert-at!;


define method remove-at!
    (v :: type-union(<vector>, <string>), index)
 => (v :: type-union(<vector>, <string>))
  local method contract (v, index :: <integer>) => ()
          without-bounds-checks
            for (i :: <integer> from index to (size(v) - 2))
              v[i] := v[i + 1]
            end
          end
        end method;
  select (index)
    #"start"  => contract(v, 0);
    #"end"    => #f;
    otherwise => contract(v, index);
  end;
  v
end method remove-at!;

define method remove-at!
    (sv :: <stretchy-object-vector>, index)
 => (sv :: <stretchy-object-vector>)
  local method contract (sv, index :: <integer>) => ()
          without-bounds-checks
            for (i :: <integer> from index to (size(sv) - 2))
              sv[i] := sv[i + 1]
            end
          end
        end method;
  select (index)
    #"start"  => contract(sv, 0);
    #"end"    => #f;
    otherwise => contract(sv, index);
  end;
  sv.size := sv.size - 1;
  sv
end method remove-at!;


/// File hacking

define method get-file-property
    (pathname :: <pathname>, property, #key default = $unsupplied) => (value)
  if (unsupplied?(default))
    file-property(pathname, property)
  else
    block ()
      let value = file-property(pathname, property);
      value
    exception (<condition>)
      default                // if there's an error, return the default
    end
  end
end method get-file-property;


/// Ticks

// With 29 bits of positive integer, it will take 125 days to wrap this
// around, even if you work 12 hours days making 100 changes per second
define locked variable *tick* :: <integer> = 0;

define inline function tick () => (tick :: <integer>)
  atomic-increment!(*tick*)
end function tick;
