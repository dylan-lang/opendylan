module: 	string-search
rcs-header:	$Header&
author: 	Robert Stockton (rgs@cs.cmu.edu)
RCS-header:     $Header: /scm/cvs/fundev/Sources/lib/collection-extensions/strsearch.dylan,v 1.1 2004/03/12 00:08:43 cgay Exp $
synopsis:	Provides a small assortment of specialized operations for
		searching and modifying <vector>s and <byte-string>s.  These
		operations are analogous to existing collection operations but
		provide keywords and efficiency improvements which are
		meaningful only within the more limited domain.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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

//======================================================================
// The "string-search" module provides basic search and replace capabilities
// upon restricted subsets of <sequence> -- primarily <vector> and
// <byte-string>.  Exploiting the known properties of these types yields
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
// 
// substring-position big pattern #key compiled start => index 
//     This is a specialized version of subsequence-position which works only
//     on <byte-strings>.  Since this routine only handles byte-characters and
//     \== tests, it can do a "Boyer-Moore-ish" search.  (If the pattern is
//     too small for B-M to pay off, substring-position will fall back upon a
//     simpler search strategy -- this function should never be slower than
//     subsequence-position.) 
// 
//     Note that this function takes a start: keyword (which defaults to 0)
//     instead of skip:.  
// 
//     As a further optimization, you may pre-compute a "compiled" dispatch
//     table for the pattern with compile-substring and pass it in (along with
//     the pattern itselef) via the compiled: keyword.  This will save both
//     time and space if you are searching for the same pattern repeatedly.  
// 
// compile-substring pattern => compiled 
//     Produce a skip table for Boyer-Moore-ish searching.  By splitting this
//     off into a separate routine we allow people to pre-compile heavily used
//     strings, thus avoiding one of the more expensive parts of the search.  
// 
// replace-substring big pattern goal #key count compiled => result 
//     Replaces all (or up to count) occurences of pattern in big with goal.
//     As in substring-position all three arguments must be <byte-string>s.
//     Accepts the compiled: keyword as described above.  Returns a new string
//     iff it finds at least one match to replace.  
//======================================================================

// Find the index of first element (after "from") of a vector which
// satisfies the given predicate.  (Like find-key, but accepts start: and end:
// rather than skip:.)
define method find-first-key(seq :: <vector>, pred?, 
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
define method find-last-key(seq :: <vector>, pred?,
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

// Specialized version of "subsequence-position" specialized for byte-strings.
// Since this routine only handles byte-characters and "==" tests, it can do
// a Boyer-Moore-ish search.  As a further optimization, you may pre-compile
// the pattern with "compile-substring" and pass it in as the "compiled:"
// keyword.  This will save both time and space if you are searching for the
// same pattern repeatedly.
define method substring-position(big :: <byte-string>,
				 pattern :: <byte-string>,
				 #key compiled, start = 0)
  let sz = size(big);
  let pat-sz = size(pattern);
  
  select (pat-sz)
    0 =>			// empty string always matches
      start;
    1 =>			// simple character search
      let ch = pattern[0];
      for (key from start below sz,
	   until big[key] == ch)
      finally
	if (key < sz) key end if;
      end for;
    2 =>			// pairs of characters -- starting to get
      let ch1 = pattern[0];	// marginal 
      let ch2 = pattern[1];
      for (key from start below sz - 1,
	   until big[key] == ch1 & big[key + 1] == ch2)
      finally
	if (key < (sz - 1)) key end if;
      end for;
    otherwise =>		// It's worth doing something Boyer-Moore-ish
      let pat-last = pat-sz - 1;
      let last-char = pattern[pat-last];
      let skip = compiled | compile-substring(pattern);
      local method search(index)
	      if (index >= sz)	// past end of big -- it's not here
		#f;
	      else 
		let char = big[index];
		if (char == last-char) // maybe it's here -- we'd better check
		  for (pat-key from 0 below pat-last,
		       big-key from index - pat-last,
		       while big[big-key] == pattern[pat-key])
		  finally
		    if (pat-key == pat-last) // fell off end -- found it.
		      index - pat-last;
		    else
		      search(index + 1) // no luck -- try further down
		    end if;
		  end for;
		else		// last character didn't match, so we can use
		  // the "skip table" to optimize
		  search(index + skip[as(<integer>, char)]);
		end if;
	      end if;
	    end method;
      search(start + pat-last);
  end select;
end method substring-position;

// Produce a skip table for Boyer-Moore-ish searching.  By splitting this off
// into a separate routine we allow people to pre-compile heavily used
// strings, thus avoiding one of the more expensive parts of the search.
define method compile-substring(pattern :: <byte-string>) => (compiled);
  let sz = size(pattern);
  if (sz < 3)
    #();
  else
    let result = make(<vector>, size: 256, fill: sz);
    for (index from 0 below sz - 1, skip from sz - 1 by -1)
      result[as(<integer>, pattern[index])] := skip;
    end for;
    result;
  end if;
end method;

// Replaces all (or up to count:) occurences of pattern in big with goal.
// Accepts the "compiled:" keyword as described above.  Returns a new string
// or the unmodified original.
define method
    replace-substring(big :: <byte-string>, pattern :: <byte-string>, 
		      goal :: <byte-string>, 
		      #key count: max = size(big),
		           compiled) => (result :: <string>);
  let compiled = compiled | compile-substring(pattern);
  let index = substring-position(big, pattern, compiled: compiled);
  if (index)
    let sz = size(pattern);
    let pieces = #();
    for (start-index = 0 then index + sz,
	 index = index
	   then substring-position(big, pattern, start: index + sz,
				   compiled: compiled),
	 count :: <integer> from 1 to max,
	 while index)
      pieces := pair(goal,
		     pair(subsequence(big, start: start-index, end: index),
			  pieces));
    finally
      apply(concatenate-as, <byte-string>,
	    reverse!(pair(subsequence(big, start: start-index), pieces)))
    end for;
  else
    big;
  end if;
end method replace-substring;

