Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sorting

define method keyed-sort
    (sequence :: <sequence>, #key key :: <function>, test :: <function> = \<)
 => (ordered-sequence :: <sequence>)
  let vector :: <simple-object-vector> = as(<simple-object-vector>, sequence);
  let vector-copy :: <simple-object-vector>
    = if (vector == sequence) copy-sequence(vector) else vector end;
  keyed-sort!(vector-copy, key: key, test: test)
end method keyed-sort;

// General case for non simple object vectors
define method keyed-sort!
    (sequence :: <sequence>,
     #key key :: <function>, test :: <function> = \<)
 => (ordered-sequence :: <sequence>)
  for (element in sequence,
       i from 0)
    sequence[i] := pair(key(element), element)
  end;
  sort!(sequence,
	test: method (object1, object2)
		test(head(object1), head(object2))
	      end);
  for (element in sequence,
       i from 0)
    sequence[i] := tail(element)
  end;
  sequence
end method keyed-sort!;

define method keyed-sort!
    (vector :: <simple-object-vector>,
     #key key :: <function>, test :: <function> = \<)
 => (ordered-vector :: <simple-object-vector>)
  for (element in vector,
       i from 0)
    vector[i] := pair(key(element), element)
  end;
  sort!(vector,
	test: method (object1, object2)
		test(head(object1), head(object2))
	      end);
  for (element in vector,
       i from 0)
    vector[i] := tail(element)
  end;
  vector
end method keyed-sort!;

//--- This is much faster if you supply a test, rather than forcing
//--- the very generic name test.
//---*** andrewa: remove this once we've stopped calling it...
define method frame-order-sequence 
    (frame :: <frame>, sequence :: <sequence>, 
     #key label-key = identity,
          test)
 => (ordered-sequence :: <sequence>)
  if (label-key == identity)
    sort(sequence, test: test | curry(frame-object<, frame))
  else
    keyed-sort(sequence,
	       key: label-key,
	       test: test | curry(frame-object<, frame))
  end
end method frame-order-sequence;

define method frame-object<
    (frame :: <frame>, object1, object2) => (less? :: <boolean>)
  frame-object-name(frame, object1) < frame-object-name(frame, object2)
end method frame-object<;

define method frame-object<
    (frame :: <frame>, object1 :: <string>, object2 :: <string>)
 => (less? :: <boolean>)
  object1 < object2
end method frame-object<;

define method frame-object<
    (frame :: <frame>, object1 :: <number>, object2 :: <number>)
 => (less? :: <boolean>)
  object1 < object2
end method frame-object<;
