Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// BOOTED: define ... class <list> ... end;
// BOOTED: define ... class <pair> ... end;
// BOOTED: define ... class <empty-list> ... end;


/////////////////
// IMPLEMENTATION
/////////////////


/*
define inline function head (list :: <list>) => (object)
  head-slot(list)
end;

define inline function tail (list :: <list>) => (object)
  tail-slot(list)
end;

define inline function head-setter (object :: <object>, pair :: <pair>)
 => (object)
  head-slot(pair) := object
end;

define inline function tail-setter (object :: <object>, pair :: <pair>)
 => (object)
  tail-slot(pair) := object
end;
*/


//
// PAIR
// 
 
define inline function pair(head, tail) => (result :: <pair>)
  make(<pair>, head: head, tail: tail)
  // let instance :: <pair> = system-allocate-simple-instance(<pair>, fill: head);
  // instance.tail := tail;
  // instance
end function;


//
// LIST
// 
 
define function list (#rest objects) => (result :: <list>)
  %dynamic-extent(objects);
  iterate build (index :: <integer> = objects.size - 1,
                 result :: <list> = #())
    if (index < 0)
      result
    else
      build(index - 1, pair(objects[index], result))
    end if
  end iterate
end function;


//
// Specialized inherited generic methods
//


//
// TYPE-FOR-COPY
// 
 
define inline sealed method type-for-copy (object :: <list>) => (type :: <type>)
  <list>
end method type-for-copy;


//
// EMPTY?
// 
 
define inline sealed method empty? (list :: <list>) => (result :: <boolean>)
  list == #()
end method empty?;


//
// AS
// 
 
define inline sealed method as (class == <list>, list :: <list>) => (list :: <list>)
  list
end method as;

define method as (class == <list>, collection :: <collection>) 
    => (l :: <list>)
  let result :: <list> = #();
  for (item in collection)
    result := pair(item, result);
  end for;
  reverse!(result)
end method as;

define sealed method as (class == <list>, v :: <simple-object-vector>) 
    => (l :: <list>)
  for (result = #() then pair(vector-element(v, index), result),
       index :: <integer> from v.size - 1 to 0 by -1)
  finally
    result
  end
end;

define inline sealed method as (class == <pair>, collection :: <pair>) => (p :: <pair>)
  collection
end method as;


//
// MAKE
// 
 
define sealed method make (class == <list>, #key size = 0, fill) 
    => (l :: <list>)
  for (i :: <integer> from 0 below size,
       result :: <list> = #() then pair(fill, result))
  finally
    result
  end for
end method make;

define inline sealed method make (class == <empty-list>, #key)
    => (l :: <empty-list>)
  #()
end method make;

define sealed method first-setter
    (value :: <object>, sequence :: <empty-list>) => (value :: <object>) 
  element-range-error(sequence, 0)    
end method;

define sealed method second-setter
    (value :: <object>, sequence :: <empty-list>) => (value :: <object>) 
  element-range-error(sequence, 1)    
end method;

define sealed method third-setter
    (value :: <object>, sequence :: <empty-list>) => (value :: <object>) 
  element-range-error(sequence, 2)    
end method;

define sealed domain make (subclass(<list>));
define sealed domain initialize (<list>);

//
// SIZE
// 
 
//define sealed method size (list :: <list>) => (s :: false-or(<integer>))
//  if (list == #()) 
//    0
//  else
//    iterate sum (count :: <integer> = 0, 
//                 fast :: <pair> = list, slow :: <pair> = list)
//      let fast-tail = fast.tail;
//      if (fast-tail == #())
//        count + 1
//      elseif (fast == slow & count > 0)
//        #f
//      elseif (fast-tail.object-class == <pair>)
//        let fast-tail-tail = fast-tail.tail;
//        select (fast-tail-tail.object-class)
//          <empty-list> => count + 2;
//          <pair> => sum(count + 2, fast-tail-tail, slow.tail);
//          otherwise => error(make(<improper-list-error>, format-string: "Taking size of an improper list"));
//        end
//      else
//        error(make(<improper-list-error>, format-string: "Taking size of an improper list"))
//      end if
//    end iterate
//  end
//end method size;
define sealed method size (list :: <list>) => (s :: false-or(<integer>))
  if (list == #()) 
    0
  else
    let list :: <pair> = list;
    iterate sum (count :: <integer> = 0, 
                 fast :: <pair> = list, slow :: <pair> = list)
      let fast-tail = fast.tail;
      if (fast-tail == #())
        count + 1
      elseif (fast == slow & count > 0)
        #f
      elseif (instance?(fast-tail, <pair>))   // fast-tail.object-class == <pair>
        let fast-tail :: <pair> = fast-tail;
	let fast-tail-tail = fast-tail.tail;
        case					// select (fast-tail-tail.object-class) ...
	  fast-tail-tail == #() => count + 2;
	  instance?(fast-tail-tail,<pair>) => 
	    let fast-tail-tail :: <pair> = fast-tail-tail;
	    let slowtail :: <pair> = tail(slow);
	    sum(count + 2, fast-tail-tail, slowtail);
          otherwise => error(make(<improper-list-error>, format-string: "Taking size of an improper list"));
        end
      else
        error(make(<improper-list-error>, format-string: "Taking size of an improper list"))
      end if
    end iterate
  end
end method size;


//
// ELEMENT
// 
 
//define sealed method element
//    (lst :: <list>, key :: <integer>, #key default = unsupplied())
//        => (o :: <object>)
//  let remain = lst;

//  for (k :: <integer> from 0 below key,
//       while: remain ~== #())
//    if (remain.object-class == <pair>)
//      remain := remain.tail
//    else
//      remain := #()
//    end if
//  end for;

//  case
//    key >= 0 & remain.object-class == <pair> =>
//      remain.head;
//    unsupplied?(default) =>
//      element-range-error(lst, key);
//    otherwise =>
//      default
//  end case
//end method element;      
define sealed method element
    (lst :: <list>, key :: <integer>, #key default = unsupplied())
        => (o :: <object>)
  if (key < 0)
    if (unsupplied?(default)) element-range-error(lst, key) else default end
  else
    iterate loop (l = lst, i :: <integer> = 0)
      if (instance?(l, <pair>))			// object-class(l) == <pair>
	let l :: <pair> = l;
	if (i == key) head(l) else loop(tail(l), i + 1) end;
      elseif (unsupplied?(default))
	element-range-error(lst, key)
      else
	default
      end if
    end iterate
  end if
end method element;      


//
// ELEMENT-NO-BOUNDS-CHECK
// 
 
define sealed method element-no-bounds-check
    (lst :: <list>, key :: <integer>, #key default) => (result :: <object>)
  for (k :: <integer> from 0 below key,
       remain :: <list> = lst then remain.tail)
  finally 
    remain.head
  end for
end method element-no-bounds-check;      


//
// ELEMENT-SETTER
// 
 
//define sealed method element-setter
//    (new-value, lst :: <list>, key :: <integer>) => (new-value :: <object>)
//  let remain = lst;
//  for (k :: <integer> from 0 below key,
//       while: remain ~== #())
//    if (remain.object-class == <pair>)
//      remain := remain.tail
//    else
//      remain := #()
//    end if
//  end for;

//  if (key >= 0 & remain.object-class == <pair>)
//    remain.head := new-value
//  else
//    element-range-error(lst, key)
//  end if
//end method element-setter;      
define sealed method element-setter
    (new-value, lst :: <list>, key :: <integer>) => (new-value :: <object>)
  iterate loop (l = lst, i :: <integer> = 0)
    if (instance?(l, <pair>))			// object-class(l) == <pair>
      let l :: <pair> = l;
      if (i == key) head(l) := new-value else loop(tail(l), i + 1) end;
    else
      element-range-error(lst, key)
    end if
  end iterate
end method element-setter;      


//
// ELEMENT-NO-BOUNDS-CHECK-SETTER
// 
 
define sealed method element-no-bounds-check-setter
    (new-value, lst :: <list>, key :: <integer>) => new-value;
  for (k :: <integer> from 0 below key,
       remain :: <list> = lst then remain.tail)
  finally
    remain.head := new-value
  end for
end method element-no-bounds-check-setter;      


//
// MEMBER?
// 
 
define sealed method member? (value, list :: <list>, #key test = \==)
 => (result :: <boolean>)
  ~list.empty?
  & if (test == \==)
      member-eql?(value, list)
    else
      let test :: <function> = test;
      (iterate there? (first = list.head, remaining :: <list> = list.tail)
	 if (test(value, first))
	   #t
	 elseif (~remaining.empty?)
	   there?(remaining.head, remaining.tail)
	 end if
      end iterate)
    end if
end method member?;


define function member-eql? (value, list :: <list>) => (result :: <boolean>)
  ~list.empty?
  & if (value-object?(value))
      let ic :: <implementation-class> = indirect-object-implementation-class(value);
      block (return)
	for (itm in list)
	  if (indirect-object?(itm) 
		& indirect-object-implementation-class(itm) == ic
		& value = itm)
	    return(#t)
	  end if
	end for
      end block
    else
      block (return)
	for (itm in list)
	  if (pointer-id?(value, itm))
	    return(#t)
	  end if
	end for
      end block
    end if
end function member-eql?;


define function xmember? (list :: <list>, value, test :: <function>) => (result :: <boolean>)
  ~list.empty?
  & with-factored-equality (value, test, testit)
      local method loop (x)
	      instance?(x, <pair>) & if (testit(head(x))) #t else loop(tail(x)) end
	    end method;
      loop(list)
    end with-factored-equality
end function xmember?;

//
// ADD
// 
 
define inline sealed method add (list :: <list>, value) => (result :: <pair>)
  pair(value, list)
end method add;


//
// ADD!
// 
 
define inline sealed method add! (list :: <list>, value) => (result :: <pair>)
  pair(value, list)
end method add!;


//
// ADD-NEW
// 
 
define sealed method add-new
    (list :: <list>, new-element, #key test :: <function> = \==) 
        => (new-list :: <list>);
  if (xmember?(list, new-element, test))
    list
  else
    add(list, new-element)
  end if
end method add-new;


//
// ADD-NEW!
// 

define sealed method add-new!
    (list :: <list>, new-element, #key test :: <function> = \==) 
        => (new-list :: <list>);
  if (xmember?(list, new-element, test))
    list
  else
    add!(list, new-element)
  end if
end method add-new!;


//
// REMOVE
// 
 
define class <improper-list-error> (<simple-error>) end;

define sealed method remove (l :: <list>, value, #key test: test = \==,
			     count: count) => new-l :: <list>;
  let rev-accumulator = #();
  let remaining = l;
  let result = #();
  let count :: <integer> = if (count) check-nat(count) else -1 end;

  if (count == 0) l
  else
    while ( instance?(remaining, <pair>) )	// remaining.object-class == <pair>
      if ( test (head (remaining), value) )
        remaining := tail (remaining);
        count     := count - 1;
        if (count = 0)
          result := remaining;
          remaining := #()
        end;                               
      else
        rev-accumulator    := pair (head (remaining), rev-accumulator);
        remaining := tail (remaining);
      end if;
    end while;

    unless (remaining == #()) 
      error(make(<improper-list-error>,
                 format-string: "Improper list %= in call to remove",
                 format-arguments: list(l)))
    end;
 
    until ( rev-accumulator == #() )
      let t = tail (rev-accumulator);
      tail (rev-accumulator) := result;
      result := rev-accumulator;
      rev-accumulator := t;
    end until;

    result;
  end;
end method remove;


//define sealed method remove (l :: <list>, value, #key test: test = \==, count: count)
// => new-l :: <list>;
 
//  let count :: <integer> = if (count) check-nat(count) else -1 end;

//  if (count == 0) 
//    l
//  else
//    with-factored-equality (value, test, testit)
//      local method loop (l, cnt :: <integer>) => new-l :: <list>;
//	      if (cnt == 0 | ~instance?(l, <pair>))
//		l
//	      elseif (testit(head(l)))
//		loop(tail(l), cnt - 1)
//	      else
//		let ans :: <pair> = pair(head(l), #());
//		local method loop2 (l, prev :: <pair>, cnt :: <integer>)
//			if (cnt == 0 | ~instance?(l, <pair>)) 
//			  tail(prev) := l;
//			  ans
//			elseif (testit(head(l)))
//			  loop2(tail(l), prev, cnt - 1)
//			else
//			  let npair :: <pair> = pair(head(l), #());
//			  tail(prev) := npair;
//			  loop2(tail(l), npair, cnt)
//			end if
//		      end method;
//		loop2(tail(l), ans, cnt)
//	      end if
//	    end method;
//      loop(l, cnt)
//    end with-factored-equality
//  end if
//end method;
      

//
// REMOVE!
// 
 
define sealed method remove! 
    (l :: <list>, value, #key test: test :: <function> = \==, count: count) 
        => new-l :: <list>;
  let result    = l;
  let prev      = #f;
  let remaining = l;
  let count :: <integer> = if (count) check-nat(count) else -1 end;

  unless (count == 0)
    while ( instance?(remaining, <pair>) )	// remaining.object-class == <pair>
      if (~ (test (head (remaining), value)))
        prev := remaining;
      else
        if (prev)
          tail (prev) := tail (remaining);
        else
          result      := tail (remaining);
        end if;
        count := count - 1;
        if (count = 0) remaining := #() end;
      end if;
      remaining := tail (remaining);
    end while;

    unless (remaining == #()) 
      error(make(<improper-list-error>,
                 format-string: "Improper list %= in call to remove!",
                 format-arguments: list(l)))
    end;
  end unless;

  result;
end method remove!;


//
// REMOVE-DUPLICATES
// 

define sealed method remove-duplicates (l :: <list>, #key test: test = \== )
 => new-l :: <list>;
  let result    = #();
  let prev      = #f;
  let remaining = l;

  while ( instance?(remaining, <pair>) )	// remaining.object-class == <pair>
    if (member? (head (remaining), tail (remaining), test: test))
      remaining   := tail (remaining);
    elseif (prev)
      let next = list (head (remaining));
      tail (prev) := next;
      prev        := next;
      remaining   := tail (remaining);
    else
      let new = list (head (remaining));
      result      := new;
      prev        := new;
      remaining   := tail (remaining);
    end if;
  end while;

  unless (remaining == #()) 
    error(make(<improper-list-error>,
               format-string: "Improper list %= in call to remove-duplicates",
               format-arguments: list(l)))
  end;

  result;
end method remove-duplicates;


//
// REMOVE-DUPLICATES!
// 

define sealed method remove-duplicates! ( l :: <list>, #key test: test = \== )
    => new-l :: <list>;
  let result    = l;
  let prev      = #f;
  let remaining = l;
  
  while ( instance?(remaining, <pair>) )	// remaining.object-class == <pair>
    if ( ~ member? (head (remaining), tail (remaining), test: test))
      prev        := remaining;
      remaining   := tail (remaining);
    elseif (prev)
      tail (prev) := tail (remaining);
      remaining   := tail (remaining);
    else
      result      := tail (remaining);
      remaining   := tail (remaining);
    end if;
  end while;

  unless (remaining == #()) 
    error(make(<improper-list-error>,
               format-string: "Improper list %= in call to remove-duplicates!",
               format-arguments: list(l)))
  end;

  result;
end method remove-duplicates!;


//
// LAST
// 

define sealed method last (lst :: <list>, #key default = unsupplied())
    => (object)
  if (lst.empty?)
    if (unsupplied?(default))
      element-range-error(lst, 0)
    else
      default
    end if
  else
    for (sub-list :: <list> = lst then sub-list.tail,
         while: instance?(sub-list.tail, <pair>))
    finally
      sub-list.head;
    end for
  end if
end method last;


//
// LAST-SETTER
// 

define sealed method last-setter (new-value, lst :: <list>) => (object)
  if (lst.empty?)
    element-range-error(lst,0)
  else
    for (sub-list :: <list> = lst then sub-list.tail,
         until: empty?(sub-list.tail))
    finally
      sub-list.head := new-value;
    end for
  end if
end method last-setter;


//
// ITERATION PROTOCOL
// 

define inline function list-next-state
    (collection :: <list>, state :: <list>) => (l :: <list>)
  state.tail
end function;

define inline function list-finished-state?
    (collection :: <list>, state :: <list>, limit) => (result :: <boolean>)
  state == #()
end function;

define inline function list-current-key
    (collection :: <list>, state :: <list>) => (result :: <integer>)
  iterate search (l :: <list> = collection, k :: <integer> = 0)
    if (l == state)
      k
    else
      search(l.tail, k + 1)
    end if
  end iterate
end function;

define inline function list-current-element
    (collection :: <list>, state :: <list>) => (result)
  state.head
end function;

define inline function list-current-element-setter
    (new-value, collection :: <list>, state :: <list>) => (result)
  state.head := new-value
end function;

define sealed inline method forward-iteration-protocol (collection :: <list>)
    => (initial-state :: <list>, limit :: <list>,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>);
  values(collection,
	 #(),
	 list-next-state,
	 list-finished-state?,
	 list-current-key,
	 list-current-element,
	 list-current-element-setter,
	 identity-copy-state)
end method forward-iteration-protocol;


//
// COPY-SEQUENCE
// 

define sealed method copy-sequence
    (source :: <list>, 
     #key start: first :: <integer> = 0, end: last = unsupplied()) 
        => (result-sequence :: <list>);
  if (first = 0 & unsupplied?(last))
    for (v in source,  result = #() then pair(v, result))
    finally
      reverse!(result)
    end for
  else 
    if (first < 0) invalid-sequence-start-error(source, first) end;
    for (l = source then l.tail,
         i from 0,
         while: i < first & l ~== #())
    finally
      if (i < first) invalid-sequence-start-error(source, first) end;
      if (supplied?(last))
        if (last < first) 
          invalid-sequence-bounds-error(source, first, last) end;
        for (l = l then l.tail,
             i from i,
             result = #() then pair(l.head, result),
             while: i < last & l ~== #())
        finally
          if (i < last) invalid-sequence-end-error(source, last) end;
          reverse!(result)
        end
      else
        for (v in l, result = #() then pair(v, result))
        finally
          reverse!(result)
        end
      end if
    end for
  end if
end method copy-sequence;
    

//
// REPLACE-SUBSEQUENCE!
// 

define sealed method replace-subsequence!
    (target :: <list>, insert :: <sequence>, 
     #key start :: <integer> = 0, end: last) 
        => (result-sequence :: <sequence>);
  let result = pair (#f, target);
  let prev   = result;

  for (i from 1 to start)
    prev := tail (prev);
  end for;

  if (~ last)
    last := start + size (insert);
  end if;

  let after-hole = 
    for (after-hole = tail (prev) then tail (after-hole),
         index = start then index + 1,
         until: index = last)
    finally 
      after-hole;
    end for;

  for (elt in insert)
    let next = pair (elt, #f);
    tail (prev) := next;
    prev        := next;
  end for;
  
  tail (prev) := after-hole;
  tail (result);
end method replace-subsequence!;


//
// REVERSE
// 

//define sealed method reverse  (l :: <list>) => l :: <list>;
//  let result = #();
//  let remaining = l;

//  until ( remaining == #() )
//    result := pair (head (remaining), result);
//    remaining := tail (remaining);
//  end until;

//  result;
//end method reverse;

define sealed method reverse (l :: <list>) => l :: <list>;
  iterate loop (l = l, ans :: <list> = #())
    if (instance?(l, <pair>))
      let l :: <pair> = l;
      loop(tail(l), pair(head(l), ans))
    else
      ans
    end if
  end iterate
end method reverse;


//
// REVERSE!
// 

//define sealed method reverse! (l :: <list>) => l :: <list>;
//  let result    = #();
//  let remaining = l;

//  until ( remaining == #() )
//    let t = tail (remaining);
//    tail (remaining) := result;
//    result           := remaining;
//    remaining        := t;
//  end until;

//  result;
//end method reverse!;

define sealed method reverse! (l :: <list>) => l :: <list>;
  iterate loop (l = l, result = #())
    if (instance?(l, <pair>))			// object-class(l) == <pair>
      let l :: <pair> = l;
      let nxt = tail(l);
      tail(l) := result;
      loop(nxt, l)
    else
      result
    end if
  end iterate
end method reverse!;


//
// CONCATENATE-AS-TWO
//

define sealed method concatenate-as-two
    (type :: subclass(<list>), first-seq :: <sequence>, second-seq :: <sequence>)
        => result-seq :: <list>; 
  if (empty?(second-seq))
    as(<list>, first-seq)
  else 
    let l = as(<list>, second-seq);
    if (empty?(first-seq))
      l
    else
      let revcpy = #();
      for (e in first-seq, revcpy = #() then pair(e, revcpy))
      finally
        let result = l;
        while (revcpy ~== #()) 
          let p = revcpy; revcpy := p.tail; p.tail := result;
          result := p;
        end;
        result
      end for
    end
  end
end;


//
// CONCATENATE-AS 
//

define sealed method concatenate-as
    (type == <list>, first-seq :: <sequence>, #rest rest-seqs :: <sequence>)
        => result-seq :: <list>; 
  let acc = #();
  for (s in rest-seqs using backward-iteration-protocol)
    acc := concatenate-as-two(<list>, s, acc)
  end;
  concatenate-as-two(<list>, first-seq, acc)
end;


//
// REDUCE
// 
 
define sealed method reduce 
    (fn :: <function>, init-value, collection :: <list>)
        => (object)
  for (result = init-value then fn(result, item),
       item in collection)
  finally
    result
  end for
end method reduce;


// Seal everything else.

define domain fill! (<list>);
define domain sort! (<list>);
define domain sort  (<list>);


// TODO: OBSOLETE?

/*

define sealed method list* (#rest args) => (result :: <list>)
  %dynamic-extent(args);
  /// NEEDED BY SYNTAX-CASE
  select (args.size)
    0 =>
      as(<list>, args);
    1 =>
      args.first;
    otherwise =>
      let args = as(<list>, args);
      let result = list(args.first);
      iterate grovel (result = result,
                      arg = args.second,
                      args = tail(args.tail))
        if (args.empty?)
          result.tail := arg
        else
          grovel(result.tail := arg.list, args.head, args.tail)
        end if
      end iterate;
      result
  end select
end method list*;

*/


// HACK: some copy-downs (if only copy downs worked for keyword methods
//   currently, all-keys is not tracked in copy-downs so they are not
//   passed in).

define sealed method first-setter
    (value :: <object>, sequence :: <pair>) => (value :: <object>) 
  head (sequence) := value
end;

define inline function tail-or-range-error 
    (sequence :: <list>, key :: <integer>) => (res :: <pair>)
  let tail = tail(sequence);
  if (instance?(tail, <pair>))
    tail
  else
    element-range-error(tail, key)    
  end if
end function;

define sealed method second-setter
    (value :: <object>, sequence :: <pair>) => (value :: <object>) 
  head(tail-or-range-error(sequence, 1)) := value
end;

define sealed method third-setter 
    (value :: <object>, sequence :: <pair>) => (value :: <object>)
  head(tail-or-range-error(tail(sequence), 2)) := value
end;

// eof
