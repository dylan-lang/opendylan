module: sorted-sequence 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of the sorted sequence library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define class <sorted-sequence> (<deque>)
  // The vector where the elements of the sorted sequence are stored, in order.
  slot data :: <stretchy-vector> = make(<stretchy-vector>, size: 0);
  // The function used to extract the comparison value from an element.
  constant slot value-function :: <function> = identity,
    init-keyword: value-function:;
  // The function used to determine if one comparison value is smaller 
  // than another comparison value.
  constant slot comparison-function :: <function> = \<,
    init-keyword: comparison-function:;
end class <sorted-sequence>; 

define method size (sorted-sequence :: <sorted-sequence>) 
    => (sorted-sequence-size :: <integer>)
  sorted-sequence.data.size;
end method size; 

define method shallow-copy (sorted-sequence :: <sorted-sequence>) 
    => (copy :: <sorted-sequence>)
  let copy 
    = make(<sorted-sequence>, value-function: sorted-sequence.value-function,
           comparison-function: sorted-sequence.comparison-function);
  // The map-into function replaces the elements of the copy's data array
  // to be the identical elements of the data array of sorted sequence.
  copy.data.size := sorted-sequence.data.size;
  map-into(copy.data, identity, sorted-sequence.data);
  copy;
end method shallow-copy; 

define constant $unsupplied = list(#f); 

define method element 
    (sorted-sequence :: <sorted-sequence>, key :: <integer>, 
     #key default = $unsupplied) 
 => (element :: <object>);
  if (key < sorted-sequence.data.size) 
    sorted-sequence.data[key];
  elseif (default = $unsupplied)
    error("Attempt to access key %= which is outside of %=.", key, 
          sorted-sequence);
  else default;
  end if;
end method element; 

// Add an element to the sorted sequence.
define method add! 
    (sorted-sequence :: <sorted-sequence>, new-element :: <object>) 
 => (sorted-sequence :: <sorted-sequence>)
  let element-value = sorted-sequence.value-function;
  let compare = sorted-sequence.comparison-function;
  add!(sorted-sequence.data, new-element);
  sorted-sequence.data 
    := sort!(sorted-sequence.data, 
             test: method (e1, e2) 
                     compare(element-value(e1), 
                             element-value(e2))
                   end);
  sorted-sequence;
end method add!; 

// Remove the item at the top of the sorted sequence. 
define method pop (sorted-sequence :: <sorted-sequence>) 
    => (top-of-sorted-sequence :: <object>)
  let data-vector = sorted-sequence.data;
  let top-of-sorted-sequence = data-vector[0];
  let sorted-sequence-size = data-vector.size;
  if (empty?(sorted-sequence))
    error("Trying to pop empty sorted-sequence %=.", sorted-sequence); 

  else
    // Shuffle up existing data, removing the top element from the 
    // sorted sequence.
    for (i from 0 below sorted-sequence-size - 1)
      data-vector[i] := data-vector[i + 1];
    end for;
    // Decreasse the size of the data vector, and return the top element.
    data-vector.size := sorted-sequence-size - 1;
    top-of-sorted-sequence;
  end if;
end method pop; 

// Remove a particular element from the sorted sequence.
define method remove!
    (sorted-sequence :: <sorted-sequence>, value :: <object>, 
     #key test = \==, count = #f)
 => (sorted-sequence :: <sorted-sequence>)
  let data-vector = sorted-sequence.data;
  let sorted-sequence-size = data-vector.size;
  for (deletion-point from 0,
        // If we have reached the user-specified limit, we are done.
       until: (deletion-point >= sorted-sequence-size) | (count & count = 0)) 
    // Otherwise, if we found a matching element, remove it from the 
    // sorted sequence.
    if (test(data-vector[deletion-point], value))
      for (i from deletion-point below sorted-sequence-size - 1)
        data-vector[i] := data-vector[i + 1]
      end for;
      sorted-sequence-size 
         := (data-vector.size := sorted-sequence-size - 1);
      if (count) count := count - 1 end;
    end if;
  end for;
  sorted-sequence;
end method remove!; 

// This method enables many standard and user-defined collection operations.
define method forward-iteration-protocol 
    (sorted-sequence :: <sorted-sequence>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>, current-element :: <function>,
     current-element-setter :: <function>, copy-state :: <function>)
  values(
         // Initial state
         0, 

         // Limit
         sorted-sequence.size, 

          // Next state
         method (collection :: <sorted-sequence>, state :: <integer>)
           state + 1
	 end,

         // Finished state?
         method (collection :: <sorted-sequence>, state :: <integer>,
                 limit :: <integer>)
           state = limit;
         end, 

         // Current key
         method (collection :: <sorted-sequence>, state :: <integer>)
           state
         end, 

         // Current element
         element, 

         // Current element setter
         method (value :: <object>, collection :: <sorted-sequence>, 
                 state :: <integer>)
           error("Setting an element of a sorted sequence is not allowed.");
         end, 

         // Copy state
         identity);
end method forward-iteration-protocol; 
