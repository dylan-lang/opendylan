module: Sequence-diff
author: Nick Kramer (nkramer@cs.cmu.edu)

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// This file implements an algorithm that accomplishes something
// similar to the Unix diff utility.  (Your actual diff utility may or
// may not use this algorithm, but it does something similar)

// Algorithm is by Webb Miller and Eugene W. Myers, published as "A
// File Comparison Program", p.  1025-1040 of Software--Practice and
// Experience, November 1985.  Quite frankly the algorithm is rather
// incomprehensible in source code form, so you might want to think
// about getting the paper.

define constant <script> = <list>;  // list of script-entries

define abstract class <script-entry> (<object>)
  slot element-count, init-value: 1, init-keyword: #"count";
  constant slot dest-index, required-init-keyword: #"dest-index";
end class <script-entry>;

// Inserts immediately after dest-start
//
define class <insert-entry> (<script-entry>)
  constant slot source-index, required-init-keyword: #"source-index";
end class <insert-entry>;

define class <delete-entry> (<script-entry>)
end class <delete-entry>;

// Returns the min(index such that seq1[index + 1] ~= seq2[index + 1]
// -1 if seq1[0] ~= seq2[0]
//
define method last-common-elt (seq1 :: <sequence>, seq2 :: <sequence>) 
 => index :: <integer>;
  block (return)
    for (elt1 in seq1, elt2 in seq2, index from -1)
      if (elt1 ~= elt2)
	return(index);
      end if;
    finally
      index + 1;
    end for;
  end block;
end method last-common-elt;

// Returns a list of edit-nodes that describe how to turn seq1 into
// seq2.  The count slot on all the entries is 1.  Call merge-dups to
// change that..
//
define method internal-diff (seq1 :: <sequence>, seq2 :: <sequence>) 
 => edit-script :: <script>;
  block (return)
    let row = last-common-elt(seq1, seq2);
    // The next two are the upper and lower ranges of valid diagonals to try
    let lower = if (row = seq1.size) 1 else -1  end if;
    let upper = if (row = seq2.size) -1 else 1  end if;
    if (lower > upper)   // sequences are identical
      return(#());
    end if;
    if (lower = 1 & upper = 1)
      return(list(make(<insert-entry>,
                       source-index: row,
                       dest-index: row,
                       count: seq2.size - row)));
    elseif (lower = -1 & upper = -1)
      return(list(make(<delete-entry>,
                       dest-index: row,
                       count: seq1.size - row)))
    end;
    // For each diagonal k, last-distance[k] is the last row that
    // contains the desired distance.
    //
    let last-d = make(<sde-vector>);
    last-d[0] := row;
    let script = make(<sde-vector>);
    script[0] := #();

    // With each iteration, compute all potentially useful scripts
    // that require at most distance changes.  Keep computing until
    // you find one that works.
    //
    for (distance from 1)   // for each edit distance
      // For each relevant diagonal ("diagonal" is the number of the
      // diagonal, with the main diagonal being 0, the left negative,
      // and the right positive.
      for (diagonal from lower to upper by 2)
	if (diagonal = -distance 
	      | (diagonal ~= distance 
		   & (last-d[diagonal + 1] >= last-d[diagonal - 1])))
	  // Move down
	  row := last-d[diagonal + 1] + 1;
	  script[diagonal] := pair(make(<delete-entry>, dest-index: row), 
				   script[diagonal + 1]);
	else
	  // Move right
	  row := last-d[diagonal - 1];
	  script[diagonal] := pair(make(<insert-entry>, 
					source-index: row + diagonal,
					dest-index: row),
				   script[diagonal - 1]);
	end if;
	let col = row + diagonal;  // column where row intersects the diagonal

	// Move down diagonal as far as you can
	while (row + 1 < seq1.size & col + 1 < seq2.size 
		 & seq1[row + 1] = seq2[col + 1])
	  row := row + 1;
	  col := col + 1;
	end while;
	last-d[diagonal] := row;
	if (row + 1 = seq1.size & col + 1 = seq2.size)
	  return(reverse(script[diagonal]));
	end if;
	if (row = seq1.size)  // Hit last row
	  lower := diagonal + 2;
	end if;
	if (col = seq2.size)   // Hit last column
	  upper := diagonal - 2;
	end if;
      end for;
      lower := lower - 1;
      upper := upper + 1;
    end for;
  end block;
end method internal-diff;

define method merge-dups-helper (d1 == #(), diffs == #()) 
 => new-diffs :: <script>;
  #();
end method merge-dups-helper;

define method merge-dups-helper (d1 :: <script-entry>, diffs == #()) 
 => new-diffs :: <script>;
  list(d1);
end method merge-dups-helper;

define method merge-dups-helper (d1 :: <script-entry>, diffs :: <script>) 
 => new-diffs :: <script>;
  let d2 = diffs.head;
  let relevant-index 
    = if (d1.object-class = <insert-entry>) source-index else dest-index end;
  if (d1.object-class = d2.object-class
	& d2.relevant-index = d1.relevant-index + d1.element-count)
    d1.element-count := d1.element-count + 1;
    merge-dups-helper(d1, diffs.tail);
  else
    pair(d1, merge-dups-helper(d2, diffs.tail));
  end if;
end method merge-dups-helper;

// The script is assumed to have come from the diff program, which has
// a few known properties
//
define method merge-dups (diffs :: <script>) => new-diffs :: <script>;
  if (diffs.empty?)
    #();
  else
    merge-dups-helper(diffs.head, diffs.tail);
  end if;
end method merge-dups;

define method sequence-diff 
    (s1 :: <sequence>, s2 :: <sequence>) => script :: <script>;
  merge-dups(internal-diff(s1, s2));
end method sequence-diff;

