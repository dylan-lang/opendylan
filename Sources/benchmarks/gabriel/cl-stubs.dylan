Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: Dylan equivalents to some Common Lisp built-in functions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline-only function null? (x)
  x == #()
end;

define inline-only function atom? (x)
  ~instance?(x, <pair>)
end;

define inline-only function fourth (x)
  tail(third(x))
end;

define function assoc (x, alist, #key test = \==)
  if (alist == #())
    #() // nil
  else
    let p = first(alist);
    if (instance?(p, <pair>) & test(first(p), x))
      p
    else
      assoc(x, tail(alist), test: test)
    end if
  end if
end function assoc;

// Used to implement "get" and "(setf get)"
define constant $symbol-plist-table = make(<table>);

// Similar to Common Lisp's MEMBER function.  i.e. returns the first
// tail of the given list whos car is element.
define function find-tail (list :: <list>, element)
  if (list == #())
    #f
  else
    if (first(list) == element)
      list
    else
      find-tail(tail(list), element)
    end if
  end if
end function find-tail;

define function get (symbol, key)
  let plist = element($symbol-plist-table, symbol, default: #f);
  if (plist)
    let it = find-tail(plist, key);
    it & second(it)
  else
    #()
  end if
end function get;

define method get-setter (new-value, symbol, key)
  let plist = element($symbol-plist-table, symbol, default: #());
  let it = find-tail(plist, key);
  if (it)
    second(it) := new-value;
  else
    $symbol-plist-table[symbol] := pair(key, pair(new-value, plist));
  end if;
  new-value
end method get-setter;

define inline-only function rplaca (p :: <pair>, new-head) => (p :: <pair>)
  head(p) := new-head;
  p
end;

define inline-only function rplacd (p :: <pair>, new-tail) => (p :: <pair>)
  tail(p) := new-tail;
  p
end;

define function nconc (a :: <list>, b :: <list>) => (a-or-b :: <list>)
  if (null?(a))
    b
  else
    for (x = a then tail(x),
         until: null?(x))
      if (null?(tail(x)))
        tail(x) := b;
        x := #(); // stop loop
      end if;
    end for;
    a
  end if
end function nconc;

