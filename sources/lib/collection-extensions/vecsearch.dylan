module: 	vector-search
author: 	Robert Stockton (rgs@cs.cmu.edu)
synopsis:	Provides a small assortment of specialized operations for
		searching and modifying <vector>s.  These
		operations are analogous to existing collection operations but
		provide keywords and efficiency improvements which are
		meaningful only within the more limited domain.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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

//======================================================================
// The "vector-search" module provides basic search and replace capabilities
// upon restricted subsets of <sequence> -- primarily <vector>.
// Exploiting the known properties of these types yields
// substantially better performance than can be achieved for sequences in
// general.  
// 
// The following functions are supplied:  
// 
// find-first-key vector predicate? #key start end failure => key 
//     Find the index of first element (after start but before end) of a
//     vector which satisfies the given predicate.  If no matching element is
//     found, return failure.  The defaults for start, end and failure are,
//     respectively,  0, size(vector), and #f.  This function is like
//     find-key, but accepts start: and end: rather than skip:.) 
// 
// find-last-key vector predicate? #key start end failure => key 
//     This is like find-first-key, but goes backward from end.  
//======================================================================

// Find the index of first element (after "from") of a vector which
// satisfies the given predicate.  (Like find-key, but accepts start: and end:
// rather than skip:.)
define method find-first-key(seq :: <vector>, pred? :: <function>, 
			     #key start = 0, end: last, failure: fail)
  block (return)
    let sz = size(seq);
    let last = if (last & last < sz) last else sz end if;
    for (i :: <integer> from start below last)
      if (pred?(seq[i])) return(i) end if;
    finally fail
    end for
  end block 
end method find-first-key;

// Like find-first-key, but goes backward from the end (or from before end:).
define method find-last-key(seq :: <vector>, pred? :: <function>,
			    #key start = 0, end: last, failure: fail)
  block (return)  
    let sz = size(seq);
    let last = if (last & last < sz) last else sz end if;
    for (i from last - 1 to start by -1) 
      if (pred?(seq[i])) return(i) end if;
    finally fail 
    end for
  end block 
end method find-last-key;

