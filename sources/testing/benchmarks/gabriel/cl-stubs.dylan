Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: Dylan equivalents to some Common Lisp built-in functions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro push!
  { push! (?list:expression, ?item:expression) }
    => { ?list := add!(?list, ?item) }
end macro push!;

define inline-only function null? (x)
  x == #()
end;

define inline-only function atom? (x)
  ~instance?(x, <pair>)
end;

define inline method fourth (sequence :: <sequence>, #rest all-keys, #key default)
 => object;
  apply(element, sequence, 3, all-keys)
end method;

define function assoc
    (x, alist :: <list>, #key test = \==) => (result :: <list>)
  if (alist == #())
    #() // nil
  else
    let p = head(alist);
    if (instance?(p, <pair>) & test(head(p), x))
      p
    else
      assoc(x, tail(alist), test: test)
    end if
  end if
end function assoc;

define variable *gensym-counter* :: <integer> = 0;

define function generate-symbol (#key string)
  as(<symbol>, concatenate("G", integer-to-string(*gensym-counter* := *gensym-counter* + 1)))
end function;

// Used to implement "get" and "(setf get)"
define constant $symbol-plist-table :: <object-table> = make(<object-table>);

// Similar to Common Lisp's MEMBER function.  i.e. returns the first
// tail of the given list whose car is element.
define function find-tail (list :: <list>, element)
  if (list == #())
    #f
  else
    if (head(list) == element)
      list
    else
      find-tail(tail(tail(list)), element)
    end if
  end if
end function find-tail;

define function get (symbol, key :: <symbol>)
  let plist = element($symbol-plist-table, symbol, default: #f);
  if (plist)
    let it = find-tail(plist, key);
    it & second(it)
  else
    #()
  end if
end function get;

define method get-setter (new-value, symbol, key :: <symbol>)
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

define function copy-tree (a)
  if (instance?(a, <pair>))
    pair(copy-tree(a.head), copy-tree(a.tail))
  else
    a
  end if
end function;
