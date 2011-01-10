Module:    CL-internals
Author:    Scott McKay
Synopsis:  Implementation of useful Common Lisp sequence functions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Returns the index at which ITEM was found
define generic cl-position
    (sequence :: <sequence>, item,
     #key test, key, start, end: finish, from-end?)
 => (index :: false-or(<integer>));

define method cl-position
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish - 1, start - 1, -1)
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      if (test(tkey, item))
        return(i)
      end;
    finally #f;
    end
  end
end method cl-position;


// Returns the index at which PREDICATE returned true
define generic cl-position-if
    (sequence :: <sequence>, predicate,
     #key key, start, end: finish, from-end?)
 => (index :: false-or(<integer>));

define method cl-position-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish - 1, start - 1, -1)
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      if (predicate(tkey))
        return(i)
      end;
    finally #f;
    end
  end
end method cl-position-if;


// Returns the whole value in which ITEM was found
define generic cl-find
    (sequence :: <sequence>, item,
     #key test, key, start, end: finish, from-end?)
 => (item :: <object>);

define method cl-find
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?)
 => (item :: <object>)
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish - 1, start - 1, -1)
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      if (test(tkey, item))
        return(telt)
      end;
    finally #f;
    end
  end
end method cl-find;


// Returns the whole value for which PREDICATE returned true
define generic cl-find-if
    (sequence :: <sequence>, predicate,
     #key key, start, end: finish, from-end?)
 => (item :: <object>);

define method cl-find-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?)
 => (item :: <object>)
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish - 1, start - 1, -1)
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      let tkey = if (key) key(telt) else telt end;
      if (predicate(tkey))
        return(telt)
      end;
    finally #f;
    end
  end
end method cl-find-if;


// Like FIND, but the sequence is a CL-style alist
define generic cl-assoc
    (sequence :: <sequence>, item,
     #key test, key, start, end: finish, from-end?)
 => (pair :: <object>);

define method cl-assoc
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?)
 => (pair :: <object>)
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish - 1, start - 1, -1)
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      if (telt)
        // skip null items
        let tkey = if (key) key(telt[0]) else telt[0] end;
        if (test(tkey, item))
          return(telt)
        end
      end;
    finally #f;
    end
  end
end method cl-assoc;


// Ditto
define generic cl-assoc-if
    (sequence :: <sequence>, item,
     #key key, start, end: finish, from-end?)
 => (pair :: <object>);

define method cl-assoc-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?)
 => (pair :: <object>)
  block (return)
    assert(start >= 0 & (~finish | finish <= size(sequence)));
    unless (finish)
      finish := size(sequence)
    end;
    let (start, finish, increment)
      = if (from-end?)
          values(finish - 1, start - 1, -1)
        else
          values(start, finish, 1)
        end;
    for (i = start then i + increment,
         until: i = finish)
      let telt = sequence[i];
      if (telt)
        // skip null items
	let tkey = if (key) key(telt[0]) else telt[0] end;
        if (predicate(tkey))
          return(telt)
        end
      end;
    finally #f;
    end
  end
end method cl-assoc-if;


// Counts each occurrence of ITEM in the sequence
define generic cl-count
    (sequence :: <sequence>, item,
     #key test, key, start, end: finish, from-end?)
 => (count :: <integer>);

define method cl-count
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?)
 => (count :: <integer>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
	values(finish - 1, start - 1, -1)
      else
        values(start, finish, 1)
      end;
  let n = 0;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    if (test(tkey, item))
      n := n + 1
    end;
  finally n;
  end
end method cl-count;


// Counts each occurrence in the sequence for which PREDICATE returned true
define generic cl-count-if
    (sequence :: <sequence>, item,
     #key key, start, end: finish, from-end?)
 => (count :: <integer>);

define method cl-count-if
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?)
 => (count :: <integer>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
	values(finish - 1, start - 1, -1)
      else
        values(start, finish, 1)
      end;
  let n = 0;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    if (predicate(tkey))
      n := n + 1
    end;
  finally n;
  end
end method cl-count-if;


// Like CL REMOVE
define generic cl-remove
    (sequence :: <sequence>, item,
     #key test, key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-remove
    (sequence :: <sequence>, item, #rest keys,
     #key test = \==, key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-remove!, copy-sequence(sequence), item, keys)
end method cl-remove;


// Like CL REMOVE-IF
define generic cl-remove-if
    (sequence :: <sequence>, item,
     #key key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-remove-if
    (sequence :: <sequence>, predicate, #rest keys,
     #key key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-remove-if!, copy-sequence(sequence), predicate, keys)
end method cl-remove-if;


// Like CL DELETE
define generic cl-remove!
    (sequence :: <sequence>, item,
     #key test, key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-remove!
    (sequence :: <list>, item,
     #key test = \==, key, start = 0, end: finish, from-end?, count)
 => (result :: <list>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-remove!;

define method cl-remove!
    (sequence :: <sequence>, item,
     #key test = \==, key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let output-index = start;
  let (start, finish, increment)
    = if (from-end?)
	values(finish - 1, start - 1, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    unless (test(tkey, item))
      if (~count
	  | begin
	      let _value = positive?(count);
	      count := count - 1;
	      _value
	    end)
        //--- not correct
        sequence[output-index] := telt;
        output-index := output-index + 1
      end
    end;
  finally
    begin
      size(sequence) := output-index;
      sequence
    end;
  end
end method cl-remove!;


// Like CL DELETE-IF
define generic cl-remove-if!
    (sequence :: <sequence>, item,
     #key key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-remove-if!
    (sequence :: <list>, predicate,
     #key key, start = 0, end: finish, from-end?, count)
 => (result :: <list>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-remove-if!;

define method cl-remove-if!
    (sequence :: <sequence>, predicate,
     #key key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let output-index = start;
  let (start, finish, increment)
    = if (from-end?)
	values(finish - 1, start - 1, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    unless (predicate(tkey))
      if (~count
          | begin
	      let _value = positive?(count);
	      count := count - 1;
	      _value
	    end)
        //--- not correct
        sequence[output-index] := telt;
        output-index := output-index + 1
      end
    end;
  finally
    begin
      size(sequence) := output-index;
      sequence
    end;
  end
end method cl-remove-if!;


define generic cl-substitute
    (sequence :: <sequence>, newitem, olditem,
     #key test, key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-substitute
    (sequence :: <sequence>, newitem, olditem, #rest keys,
     #key test = \==, key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-substitute!, copy-sequence(sequence), newitem, olditem, keys)
end method cl-substitute;


define generic cl-substitute-if
    (sequence :: <sequence>, newitem, predicate,
     #key key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-substitute-if
    (sequence :: <sequence>, newitem, predicate, #rest keys,
     #key key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?, count;
  apply(cl-substitute-if!, copy-sequence(sequence), newitem, predicate, keys)
end method cl-substitute-if;


define generic cl-substitute!
    (sequence :: <sequence>, newitem, olditem,
     #key test, key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-substitute!
    (sequence :: <list>, newitem, olditem,
     #key test = \==, key, start = 0, end: finish, from-end?, count)
 => (result :: <list>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-substitute!;

define method cl-substitute!
    (sequence :: <sequence>, newitem, olditem,
     #key test = \==, key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
	values(finish - 1, start - 1, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    if (test(tkey, olditem))
      if (~count
          | begin
	      let _value = positive?(count);
	      count := count - 1;
	      _value
	    end)
        sequence[i] := newitem
      end
    end;
  finally sequence;
  end
end method cl-substitute!;


define generic cl-substitute-if!
    (sequence :: <sequence>, newitem, predicate,
     #key key, start, end: finish, from-end?, count)
 => (result :: <sequence>);

define method cl-substitute-if!
    (sequence :: <list>, newitem, predicate,
     #key key, start = 0, end: finish, from-end?, count)
 => (result :: <list>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-substitute-if!;

define method cl-substitute-if!
    (sequence :: <sequence>, newitem, predicate,
     #key key, start = 0, end: finish, from-end?, count)
 => (result :: <sequence>)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  let (start, finish, increment)
    = if (from-end?)
	values(finish - 1, start - 1, -1)
      else
        values(start, finish, 1)
      end;
  for (i = start then i + increment,
       until: i = finish)
    let telt = sequence[i];
    let tkey = if (key) key(telt) else telt end;
    if (predicate(tkey))
      if (~count
          | begin
	      let _value = positive?(count);
	      count := count - 1;
	      _value
	    end)
        sequence[i] := newitem
      end
    end;
  finally sequence;
  end
end method cl-substitute-if!;


// Like CL REMOVE-DUPLICATES
define method cl-remove-duplicates
    (sequence :: <sequence>, #rest keys,
     #key test = \==, key, start = 0, end: finish, from-end?)
  // declare dynamic-extent keys;
  // declare ignore test, key, start, finish, from-end?;
  apply(cl-remove-duplicates!, copy-sequence(sequence), keys)
end method cl-remove-duplicates;

// Like CL DELETE-DUPLICATES
define method cl-remove-duplicates!
    (sequence :: <list>, #key test = \==, key, start = 0, end: finish, from-end?)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  //--- Do this
end method cl-remove-duplicates!;

define method cl-remove-duplicates!
    (sequence :: <sequence>,
     #key test = \==, key, start = 0, end: finish, from-end?, replace?)
  assert(start >= 0 & (~finish | finish <= size(sequence)));
  unless (finish)
    finish := size(sequence)
  end;
  begin
    let result-index = start;
    for (index from start below size(sequence))
      let test-element = sequence[index];
      let test-key = if (key) key(test-element) else test-element end;
      case
        index >= finish
	| block (return)            // never duplicated?
	    if (from-end?)
	      for (tindex from start below result-index)
		let elt = sequence[tindex];
		let tkey = if (key) key(elt) else elt end;
		if (test(tkey, test-key))
		  // TEST-ELEMENT is an earlier duplicate of element
		  if (replace?)
		    sequence[tindex] := test-element
		  end;
		  return(#f)
		end;
	      finally return(#t);
	      end
	    else
	      for (tindex from index + 1 below finish)
		let elt = sequence[tindex];
		let tkey = if (key) key(elt) else elt end;
		if (test(tkey, test-key))
		  return(#f)
		end;
	      finally return(#t);
	      end
	    end
	  end =>
	  // Not a duplicate
          sequence[result-index] := test-element;
          result-index := result-index + 1
      end;
    finally
      size(sequence) := result-index;
    end
  end;
  sequence
end method cl-remove-duplicates!;


define generic cl-search
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test, key, start1, start2, end1, end2, from-end?)
 => (index :: false-or(<integer>));

define method cl-search
    (sequence1 :: <list>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    if (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end;
      end1 := end1 - start1;
      start1 := 0
    end;
    if (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    if (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    if (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2,
         until: (end2 & i = end2) | empty?(sequence2))
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i2 from i + 1, i1 from start1 + 1,
		   telt1 in tail(sequence1),
		   telt2 = pop!(rest2) then pop!(rest2),
		   until: ((end2 & i2 = end2) | empty?(rest2)) & return(#f))
		if (key)
		  telt1 := key(telt1);
		  telt2 := key(telt2)
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <list>, sequence2 :: <list>,
     start1, start2, end1, end2, test, key)
 => (index :: false-or(<integer>))
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    if (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let last-found = #f;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 to end2 - len1)
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i1 from start1 + 1,
		   telt1 in tail(sequence1),
		   telt2 = pop!(rest2) then pop!(rest2),
		   until: i1 >= end1)
		if (key)
		  telt1 := key(telt1);
		  telt2 := key(telt2)
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          last-found := i
        end
      end;
    finally return(last-found);
    end
  end
end method cl-reverse-search;

define method cl-search
    (sequence1 :: <list>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    if (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end;
      end1 := end1 - start1;
      start1 := 0
    end;
    if (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    if (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 below end2)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i2 from i + 1, i1 from start1 + 1,
		   telt1 in tail(sequence1),
		   until: i2 = end2 & return(#f))
		let telt2 = sequence2[i2];
		if (key)
		  telt1 := key(telt1);
		  telt2 := key(telt2)
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <list>, sequence2 :: <sequence>,
     start1, start2, end1, end2, test, key)
 => (index :: false-or(<integer>))
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    if (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let telt1 = head(sequence1);
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from end2 - len1 to start2 by -1)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i2 from i + 1, i1 from start1 + 1,
		   telt1 in tail(sequence1),
		   until: i1 >= end1)
		let telt2 = sequence2[i2];
		if (key)
		  telt1 := key(telt1);
		  telt2 := key(telt2)
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          return(i)
        end
      end;
    end
  end
end method cl-reverse-search;

define method cl-search
    (sequence1 :: <sequence>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    if (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    if (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    if (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2, 
	 until: (end2 & i = end2) | empty?(sequence2))
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i2 from i + 1, i1 from start1 + 1 below end1,
		   until: ((end2 & i2 = end2) | empty?(rest2)) & return(#f))
		let telt1 = sequence1[i1];
		let telt2 = pop!(rest2);
		if (key)
		  telt1 := key(telt1);
		  telt2 := key(telt2)
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <sequence>, sequence2 :: <list>,
     start1, start2, end1, end2, test, key)
 => (index :: false-or(<integer>))
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    if (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let last-found = #f;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 to end2 - len1)
      let telt2 = head(sequence2);
      let rest2 = tail(sequence2);
      let tkey2 = if (key) key(telt2) else telt2 end;
      pop!(sequence2);
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i1 from start1 + 1 below end1)
		let telt1 = sequence1[i1];
		let telt2 = pop!(rest2);
		if (key)
		  telt1 := key(telt1);
		  telt2 := key(telt2)
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          last-found := i
        end
      end;
    finally return(last-found);
    end
  end
end method cl-reverse-search;

define method cl-search
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    if (from-end?)
      return
        (cl-reverse-search
           (sequence1, sequence2, start1, start2, end1, end2, test, key))
    end;
    if (empty?(sequence1) | (end1 & start1 >= end1))
      return(start2)
    end;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from start2 below end2)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i2 from i + 1, i1 from start1 + 1 below end1,
		   until: i2 = end2 & return(#f))
		let telt1 = sequence1[i1];
		let telt2 = sequence2[i2];
		if (key)
		  telt1 := key(telt1);
		  telt2 := key(telt2)
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          return(i)
        end
      end;
    end
  end
end method cl-search;

define method cl-reverse-search
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     start1, start2, end1, end2, test, key)
 => (index :: false-or(<integer>))
  block (return)
    let len1 = end1 - start1;
    let len2 = end2 - start2;
    if (len1 > len2)
      return(#f)
    end;
    unless (len1 > 0)
      return(end2)
    end;
    let telt1 = sequence1[start1];
    let tkey1 = if (key) key(telt1) else telt1 end;
    for (i from end2 - len1 to start2 by -1)
      let telt2 = sequence2[i];
      let tkey2 = if (key) key(telt2) else telt2 end;
      if (test(tkey1, tkey2))
        if (block (break)
	      for (i2 from i + 1, i1 from start1 + 1 below end1)
		let telt1 = sequence1[i1];
		let telt2 = sequence2[i2];
		if (key)
		  begin
		    telt1 := key(telt1);
		    telt2 := key(telt2)
		  end
		end;
		unless (test(telt1, telt2))
		  break(#f)
		end;
	      finally break(#t);
	      end
	    end)
          return(i)
        end
      end;
    end
  end
end method cl-reverse-search;

/*|
cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 4); ==> #f
cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 5); ==> fail
cl-search(#(4), #(1,2,3,4,5), start2: 2, end2: 5); ==> 3

cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 2); ==> 3
cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 1); ==> 3
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 2); => 0
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 3); => 4

cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 4, from-end?: #t); ==> #f
cl-search(#(1,2,5,4), #(1,2,3,4,5), start1: 2, end1: 5, from-end?: #t); ==> fail
cl-search(#(4), #(1,2,3,4,5), start2: 2, end2: 5, from-end?: #t); ==> 3

cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 2, from-end?: #t); ==> 3
cl-search(#(3,4,5), #(1,2,3,4,5), start1: 1, end1: 2, start2: 1, from-end?: #t); ==> 3
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 2, from-end?: #t); => 10
cl-search(#[3,4,5], #(1,2,3,4,5,2,3,4,5,6), start1: 2, end1: 3, from-end?: #t); => 8
cl-search(#(1), #(1)); => 0
cl-search("", "abcde"); => 0
cl-search("abcde", "abcde", start1: 3, end1: 3); => 0
cl-search(#(1), #(1), from-end?: #t); => 0
cl-search("", "abcde", from-end?: #t); => 5
cl-search("abcde", "abcde", start1: 3, end1: 3, from-end?: #t); => 5
cl-search(#(1), #(1), end1: 0); => 0
cl-search(#(1), #(1), end1: 0, from-end?: #t); => 1
|*/


define generic cl-mismatch
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test, key, start1, start2, end1, end2, from-end?)
 => (index :: false-or(<integer>));

define method cl-mismatch
    (sequence1 :: <list>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    if (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end
    end;
    if (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    let last-difference = #f;
    case
      from-end? =>
        unless (end1)
          end1 := start1 + size(sequence1)
        end;
        unless (end2)
          end2 := start2 + size(sequence2)
        end;
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence1);
              end;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence2);
              end
          end
        end;
        until (start1 = end1)
          begin
            let elt1 = pop!(sequence1);
            let elt2 = pop!(sequence2);
            if (key)
              elt1 := key(elt1);
              elt2 := key(elt2)
            end;
            unless (test(elt1, elt2))
              last-difference := start1 + 1
            end
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = pop!(sequence1);
          let elt2 = pop!(sequence2);
          if (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;

define method cl-mismatch
    (sequence1 :: <list>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    if (positive?(start1))
      for (i from 0 below start1)
        pop!(sequence1);
      end
    end;
    let last-difference = #f;
    case
      from-end? =>
        unless (end1)
          end1 := start1 + size(sequence1)
        end;
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence1);
              end;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1
          end
        end;
        until (start1 = end1)
          begin
            let elt1 = pop!(sequence1);
            let elt2 = sequence2[start2];
            if (key)
              elt1 := key(elt1);
              elt2 := key(elt2)
            end;
            unless (test(elt1, elt2))
              last-difference := start1 + 1
            end
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = pop!(sequence1);
          let elt2 = sequence2[start2];
          if (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;

define method cl-mismatch
    (sequence1 :: <sequence>, sequence2 :: <list>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    if (positive?(start2))
      for (i from 0 below start2)
        pop!(sequence2);
      end
    end;
    let last-difference = #f;
    case
      from-end? =>
        unless (end2)
          end2 := start2 + size(sequence2)
        end;
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1;
              for (i from 0 below diff)
                pop!(sequence2);
              end
          end
        end;
        until (start1 = end1)
          begin
            let elt1 = sequence1[start1];
            let elt2 = pop!(sequence2);
            if (key)
              elt1 := key(elt1);
              elt2 := key(elt2)
            end;
            unless (test(elt1, elt2))
              last-difference := start1 + 1
            end
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = sequence1[start1];
          let elt2 = pop!(sequence2);
          if (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;

define method cl-mismatch
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test = \==, key, start1 = 0, start2 = 0, end1, end2, from-end?)
 => (index :: false-or(<integer>))
  block (return)
    assert(start1 >= 0 & (~end1 | end1 <= size(sequence1)));
    unless (end1)
      end1 := size(sequence1)
    end;
    assert(start2 >= 0 & (~end2 | end2 <= size(sequence2)));
    unless (end2)
      end2 := size(sequence2)
    end;
    let last-difference = #f;
    case
      from-end? =>
        begin
          let len1 = end1 - start1;
          let len2 = end2 - start2;
          case
            len1 > len2 =>
              let diff = len1 - len2;
	      start1 := start1 + diff;
              last-difference := start1;
            len2 > len1 =>
              let diff = len2 - len1;
	      start2 := start2 + diff;
              last-difference := start1
          end
        end;
        for (i1 from end1 - 1 to start1 by -1,
             i2 from end2 - 1 to start2 by -1)
	  let elt1 = sequence1[i1];
	  let elt2 = sequence2[i2];
          if (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(i1 + 1)
          end;
        finally return(last-difference);
        end;
        last-difference;
      otherwise =>
        while (#t)
          case
            empty?(sequence1) | (end1 & start1 = end1) =>
              return
                (if (empty?(sequence2) | (end2 & start2 = end2))
                   #f
                 else
                   start1
                 end);
            empty?(sequence2) | (end2 & start2 = end2) =>
              return(start1)
          end;
          let elt1 = sequence1[start1];
          let elt2 = sequence2[start2];
          if (key)
            elt1 := key(elt1);
            elt2 := key(elt2)
          end;
          unless (test(elt1, elt2))
            return(start1)
          end;
          start1 := start1 + 1;
          start2 := start2 + 1;
        end
    end
  end
end method cl-mismatch;


define method cl-merge
    (result-type, sequence1 :: <sequence>, sequence2 :: <sequence>, predicate,
     #key key)
  //--- Just forget about this?
end method cl-merge;
