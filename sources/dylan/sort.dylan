module:	internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================
//
//  This file contains definitions for sorting utilities for Dylan
//  sequences.
//
//  These are the default methods for sorting sequences.  The way they work
//  is to coerce the sequence to be sorted to a vector.  This allows easier
//  access to the elements of the sequence and the use of typical sorting
//  algorithms.  When the sorting is complete, the vector is coerced back
//  to the class for copy of the original sequence.
//
//  A simple insertion sort is defined first.  This algorithm works well for
//  small sequences, but is too inefficient for large tasks.  Two more
//  efficient algorithms are also implemented: merge sort and quick sort.
//  The more efficient sorts can use the simple sorting algorithm for
//  small subsequences.  (This is controlled by the $SMALL-SORT-SIZE
//  constant.)
//
//  One common feature of the sort functions which sort in place is the
//  keyword arguments START and END.  These keywords tell the sort function
//  which portion of the vector to operate upon.  Thus recursive calls or
//  calls to other sort functions can sort different segments of the same
//  vector through use of keys.  The START key is always an inclusive bound
//  for the beginning of the subsequence; the END key is always an
//  exclusive bound for the end of the subsequence.
//
//  Written by David Pierce
//  Tightened up a bit by Scott McKay, February 1999



//// Simple Sorting Algorithms

define inline function sort-range-check
    (sequence :: <sequence>, _size :: <integer>, _start :: <integer>, _end :: <integer>)
  when (_start < 0 | _start > _size)
    element-range-error(sequence, _start)
  end;
  when (_end < 0 | _end > _size)
    element-range-error(sequence, _end)
  end
end function sort-range-check;


// swap-elements! -- internal
//
// Swaps two elements in a vector.
//
define inline-only function primitive-swap-elements!
    (vector :: <vector>, key1 :: <integer>, key2 :: <integer>)
  without-bounds-checks
    let elt1 = vector[key1];
    let elt2 = vector[key2];
    vector[key1] := elt2;
    vector[key2] := elt1;
  end
end function primitive-swap-elements!;

define inline method swap-elements!
    (vector :: <vector>, key1 :: <integer>, key2 :: <integer>)
  primitive-swap-elements!(vector, key1, key2)
end method swap-elements!;

define inline method swap-elements!
    (vector :: <simple-object-vector>, key1 :: <integer>, key2 :: <integer>)
  primitive-swap-elements!(vector, key1, key2)
end method swap-elements!;


// insertion-sort! -- internal
//
// Insertion sort maintains the invariant that the vector is sorted
// up to a current position.  The next element after this position is
// inserted into the sorted part of the vector, pushing larger elements up
// if necessary.
//
// Insertion sort is stable, and this method sorts the vector in place.
//
define inline-only function primitive-insertion-sort!
    (vector :: <vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
	  end:   _end   :: <integer> = size(vector))
  sort-range-check(vector, size(vector), _start, _end);
  without-bounds-checks
    for (current-key :: <integer> from _start + 1 below _end)
      let current-element = vector[current-key];
      for (insert-key :: <integer> from current-key - 1 to _start by -1,
	   while: test(current-element, vector[insert-key]))
	vector[insert-key + 1] := vector[insert-key];
      finally
	vector[insert-key + 1] := current-element;
      end
    end
  end;
  vector
end function primitive-insertion-sort!;

define method insertion-sort!
    (vector :: <vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
	  end:   _end   :: <integer> = size(vector))
  primitive-insertion-sort!(vector,
			    test: test,
			    start: _start, end: _end)
end method insertion-sort!;

define sealed method insertion-sort!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
	  end:   _end   :: <integer> = size(vector))
  primitive-insertion-sort!(vector,
			    test: test,
			    start: _start, end: _end)
end method insertion-sort!;


//// Recursive Sorting Algorithms

// $small-sort-size -- internal
//
// The simple sorts can be used to sort the small subsequences generated
// by the recursive algorithms.  This parameter defines how small the
// subsequence should be before the simple sorts are called.  (The simple
// sorts can be turned off by setting this to 0.)
//
define constant $small-sort-size :: <integer> = 10;


// Merge Sort
//
// Merge sort is a divide-and-conquer algorithm.  It divides the vector in
// half and recursively calls merge sort on the halves.  When the calls
// return, the halves are sorted, and they are merged together.
//
// Merge sort is stable.  There is a version that sorts in place, and
// modifies the original vector.  This uses a small amount of extra space
// in the process (it merges the sorted halves into a new vector and then
// copies back to the original).  There is also a version that uses as
// much extra space as it needs, and sorts non-destructively.

// merge! -- internal
//
// This function merges two contiguous sorted subsequences of a vector.
// It accepts four keyword arguments in addition to a vector.  TEST
// specifies the ascending order for the sort/merge.  START and MIDDLE
// give the beginnings of the two subsequences, and END is the end of the
// second subsequence.  (Again, START and MIDDLE are inclusive bounds for
// the subsequences, and MIDDLE and END are exclusive end bounds.  (The
// subsequences must be contiguous in the vector.))
//
// Again, merging assumes the subsequences are sorted.  Two pointers run
// down each subsequence.  The smallest of the two elements is copied to a
// merge vector and the pointer for its subsequence is incremented.  This
// continues until both pointers reach the end of the subsequences.
// Finally the merge vector is copied into the original vector in place.
//
define inline-only function primitive-merge!
    (vector :: <vector>,
     _start :: <integer>, middle :: <integer>, _end :: <integer>,
     #key test :: <function> = \<)
  let merge-size :: <integer> = _end - _start;
  let start-key  :: <integer> = _start;
  let middle-key :: <integer> = middle;
  let merge-vector :: <simple-object-vector> = make(<vector>, size: merge-size);
  without-bounds-checks
    for (merge-key :: <integer> from 0 below merge-size)
      case
	start-key >= middle =>
	  merge-vector[merge-key] := vector[middle-key];
	  middle-key := middle-key + 1;
	middle-key >= _end =>
	  merge-vector[merge-key] := vector[start-key];
	  start-key := start-key + 1;
	test(vector[middle-key], vector[start-key]) =>
	  merge-vector[merge-key] := vector[middle-key];
	  middle-key := middle-key + 1;
	otherwise =>
	  merge-vector[merge-key] := vector[start-key];
	  start-key := start-key + 1;
      end
    end;
    for (merge-key :: <integer> from 0 below merge-size,
	 copy-key :: <integer> from _start)
      vector[copy-key] := merge-vector[merge-key]
    end
  end
end function primitive-merge!;

define method merge!
    (vector :: <vector>,
     #key test :: <function> = \<, 
          start:  _start :: <integer>, 
          middle: middle :: <integer>, 
	  end:    _end   :: <integer>)
  primitive-merge!(vector, _start, middle, _end, test: test)
end method merge!;

define sealed method merge!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, 
          start:  _start :: <integer>, 
          middle: middle :: <integer>, 
	  end:    _end   :: <integer>)
  primitive-merge!(vector, _start, middle, _end, test: test)
end method merge!;


// merge-sort! -- internal
//
// Sorts a vector in place using merge sort.  Computes the middle of the
// vector and recursively calls MERGE-SORT! on both halves.  Merges the
// halves when both calls return.  If the vector is smaller than
// $SMALL-SORT-SIZE, however, INSERTION-SORT! is used instead.  Recursive
// calls to MERGE-SORT! terminate (by doing nothing) when the vector to be
// sorted contains only one element (or when insertion sort is used).
//
// Three keywords are accepted by this function.  The TEST specifies the
// ascending order for the sort, and START and END give the bounds of the
// subvector to be operated on in VECTOR.
//
define inline-only function primitive-merge-sort!
    (vector :: <vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
	  end:   _end   :: <integer> = size(vector))
  sort-range-check(vector, size(vector), _start, _end);
  without-bounds-checks
    let length :: <integer> = _end - _start;
    case
      length < $small-sort-size =>
	insertion-sort!(vector, test: test, start: _start, end: _end);
      length > 1 =>
	let (div, mod) = floor/(length, 2);
	let middle :: <integer> = _start + div;
	merge-sort!(vector, test: test, start: _start, end: middle);
	merge-sort!(vector, test: test, start: middle, end: _end);
	merge!(vector, start: _start, middle: middle, end: _end, test: test);
      otherwise => #f;
    end
  end;
  vector;
end function primitive-merge-sort!;

define method merge-sort!
    (vector :: <vector>,
     #key test :: <function> = \<, 
	  start: _start :: <integer> = 0, 
	  end:   _end   :: <integer> = size(vector))
  primitive-merge-sort!(vector,
			test: test,
			start: _start, end: _end)
end method merge-sort!;

define sealed method merge-sort!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, 
	  start: _start :: <integer> = 0, 
	  end:   _end   :: <integer> = size(vector))
  primitive-merge-sort!(vector,
			test: test,
			start: _start, end: _end)
end method merge-sort!;


// Quick Sort
//
// Quick sort is also a divide-and-conquer algorithm.  It partitions the
// vector by choosing a pivot, and separating elements smaller than the
// pivot from elements larger than the pivot.  Then quick sort is called
// recursively on the two subsequences to sort them in place.  When the
// recursive calls return, the vector is sorted, because all the elements
// in the first subsequence are smaller than those in the second.
//
// Quick sort sorts in place, destructively, but it is not stable.

// median-of-three -- internal
//
// Pick the index of the pivot point by picking the index corresponding to
// median(vec[start], vec[middle], vec[end - 1]).  Note: In accordance with
// convention, "end" is an exclusive bound.
//
define inline-only function primitive-median-of-three
    (vector :: <vector>, _start :: <integer>, _end :: <integer>,
     less-than :: <function>)
 => (pivot-index :: <integer>)
  without-bounds-checks
    let middle :: <integer> = truncate/(_start + _end, 2);
    let start-elt  = vector[_start];
    let end-elt    = vector[_end - 1];
    let middle-elt = vector[middle];
    if (less-than(start-elt, end-elt))
      if (less-than(middle-elt, end-elt))
	middle
      else 
	_end
      end
    else	// end-elt <= start-elt
      if (less-than(middle-elt, start-elt))
	middle
      else
	_start
      end
    end
  end
end function primitive-median-of-three;

define method median-of-three
    (vector :: <vector>, _start :: <integer>, _end :: <integer>,
     less-than :: <function>)
 => (pivot-index :: <integer>)
  primitive-median-of-three(vector, _start, _end, less-than)
end method median-of-three;

define sealed method median-of-three
    (vector :: <simple-object-vector>, _start :: <integer>, _end :: <integer>,
     less-than :: <function>)
 => (pivot-index :: <integer>)
  primitive-median-of-three(vector, _start, _end, less-than)
end method median-of-three;


// partition! -- internal
//
// Partitions a vector and returns the partition position.  The pivot
// element is chosen by the median-of-three method.  Pointers are
// started at the beginning and end of the vector.  The "small" pointer
// moves forward over elements smaller than the pivot element, and stops
// at those larger.  The "large" point moves backward over elements larger
// than the pivot element, and stops at those smaller.  The two elements
// at the places where the pointers stop are swapped.  This continues
// until the pointers cross each other.  Then the small pointer is
// returned as the partition position.
//
// PARTITION! takes the usual keyword arguments TEST, START, and END.
//
define inline-only function primitive-partition!
    (vector :: <vector>, _start :: <integer>, _end :: <integer>,
     #key test :: <function> = \<)
  without-bounds-checks
    let pivot-key :: <integer> = median-of-three(vector, _start, _end - 1, test);
    let small-key :: <integer> = _start;
    let large-key :: <integer> = _end - 1;
    let pivot-element = vector[pivot-key];
    block (break)
      while (#t)
	while (test(vector[small-key], pivot-element))
	  small-key := small-key + 1;
	end;
	while (test(pivot-element, vector[large-key]))
	  large-key := large-key - 1;
	end;
	unless (small-key < large-key)
	  break();
	end;
	swap-elements!(vector, small-key, large-key);
	small-key := small-key + 1;
	large-key := large-key - 1;
      end
    end;
    small-key
  end
end function primitive-partition!;

define method partition!
    (vector :: <vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
          end: _end :: <integer> = size(vector))
  primitive-partition!(vector, _start, _end, test: test)
end method partition!;

define sealed method partition!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
          end: _end :: <integer> = size(vector))
  primitive-partition!(vector, _start, _end, test: test)
end method partition!;


// quick-sort! -- internal
//
// Sorts a vector in place using quick sort.  The vector is partitioned by
// PARTITION!.  The two subsequences formed by START up to the partition
// position and from there to END are sorted recursively.  The recursion
// terminates if the vector has less than two elements, and nothing is
// done; or if the size of the subvector is small and INSERTION-SORT! is
// called on it.
//
// QUICK-SORT! takes the usual keyword arguments TEST, START, and END.
//
define inline-only function primitive-quick-sort!
    (vector :: <vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
          end: _end :: <integer> = size(vector))
  sort-range-check(vector, size(vector), _start, _end);
  let length :: <integer> = _end - _start;
  case
    length < $small-sort-size =>
      insertion-sort!(vector, test: test, start: _start, end: _end);
    length > 1 =>
      let middle = partition!(vector, test: test, start: _start, end: _end);
      quick-sort!(vector, test: test, start: _start, end: middle);
      quick-sort!(vector, test: test, start: middle, end: _end);
    otherwise => #f;
  end;
  vector
end function primitive-quick-sort!;

define method quick-sort!
    (vector :: <vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
          end: _end :: <integer> = size(vector))
  primitive-quick-sort!(vector,
			test: test,
			start: _start, end: _end)
end method quick-sort!;

define sealed method quick-sort!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, 
          start: _start :: <integer> = 0, 
          end: _end :: <integer> = size(vector))
  primitive-quick-sort!(vector,
			test: test,
			start: _start, end: _end)
end method quick-sort!;

define method sort!(vector :: <vector>, #key test = \<, stable: stable)
    => sequence :: <sequence>;
  if (stable) 
    merge-sort!(vector, test: test);
  else 
    quick-sort!(vector, test: test);
  end if;
  vector
end method sort!;


define method sort!(sequence :: <sequence>, #key test = \<, stable: stable)
    => sequence :: <sequence>;
  let vector = as(<vector>, sequence);
  let result = if (stable) merge-sort!(vector, test: test);
	       else quick-sort!(vector, test: test);
	       end if;
  as(type-for-copy(sequence), result);
end method sort!;
