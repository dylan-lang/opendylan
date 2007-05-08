module: 	substring-search
author: 	Robert Stockton (rgs@cs.cmu.edu)
synopsis:	Provides a small assortment of specialized operations for
		searching and modifying <byte-string>s.  These
		operations are analogous to existing collection operations but
		provide keywords and efficiency improvements which are
		meaningful only within the more limited domain.
                (used to be strsearch.dylan in module string-search library 
		collection-extensions)
copyright: see below

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
// The "string-search" module provides basic search and replace
// capabilities upon <byte-string>.  Exploiting the known properties
// of these types yields substantially better performance than can be
// achieved for sequences in general.
//======================================================================

//     This is a specialized version of subsequence-position which works only
//     on <byte-strings>.  Since this routine only handles byte-characters and
//     \== tests, it can do a "Boyer-Moore-ish" search.  (If the pattern is
//     too small for B-M to pay off, substring-position will fall back upon a
//     simpler search strategy -- this function should never be slower than
//     subsequence-position.) 
//
define sealed inline method substring-position 
    (big :: <string>, pattern :: <byte-string>, #key start = 0,
     end: big-end = size(big), case-sensitive = #f)
 => (position :: false-or(<integer>), #rest end-position :: <integer>);
  let compiled-pattern = compile-substring(pattern, case-sensitive);
  find-substring(big, pattern, start, big-end, case-sensitive,
		 compiled-pattern);
end method substring-position;

// KJP: BUG!!! Removed inline because of crash compiling test case
define sealed /* inline */ method make-substring-positioner 
    (pattern :: <byte-string>, #key case-sensitive = #f)
 => positioner :: <function>;
  let compiled-pattern = compile-substring(pattern, case-sensitive);
  local method compiled-matcher
	    (big :: <string>, #key start = 0, end: big-end = size(big))
	 => (position :: false-or(<integer>), #rest end-position :: <integer>);
	  find-substring(big, pattern, start, big-end, case-sensitive,
			 compiled-pattern);
	end method compiled-matcher;
  compiled-matcher;
end method make-substring-positioner;

define inline function equal?
    (char1 :: <character>, char2 :: <character>, case-sensitive :: <boolean>)
 => (result :: <boolean>);
  if (case-sensitive)
    char1 == char2;
  else
    case-insensitive-equal(char1, char2);
  end if;
end function equal?;

// Does the real work of substring-position.  Not exported.
//
// Specialized version of "subsequence-position" specialized for byte-strings
// patterns.  Since this routine only handles byte-characters and "==" tests,
// it can do a Boyer-Moore-ish search.  As a further optimization, you may
// pre-compile the pattern with "compile-substring" and pass it in as the
// "compiled:" keyword.  This will save both time and space if you are
// searching for the same pattern repeatedly.
//
// Note: By specializing on <string> instead of <byte-string>, we increase our
// generality while decreasing efficiency.  This may be a good candidate for
// providing an "out-lined" implementation on <byte-string>.
//
define sealed method find-substring
    (big :: <string>, pattern :: <byte-string>, start :: <integer>,
     big-end :: <integer>, case-sensitive :: <boolean>,
     compiled-pattern :: <simple-object-vector>)
 => (position :: false-or(<integer>), #rest end-position :: <integer>);
  let pat-sz = size(pattern);
  let start-of-occurence
    = select (pat-sz)
	0 =>			// empty string always matches
	  start;
	1 =>			// simple character search
	  let ch = pattern[0];
	  for (key from start below big-end,
	       until: equal?(big[key], ch, case-sensitive))
	  finally
	    if (key < big-end) key else #f end;
	  end for;
	2 =>			// pairs of characters -- starting to get
	  let ch1 = pattern[0];	// marginal 
	  let ch2 = pattern[1];
	  for (key from start below big-end - 1,
	       until: equal?(big[key], ch1, case-sensitive)
		 & equal?(big[key + 1], ch2, case-sensitive))
	  finally
	    if (key < (big-end - 1)) key else #f end;
	  end for;
	otherwise =>		// It's worth doing something Boyer-Moore-ish
	  let pat-last = pat-sz - 1;
	  let last-char = pattern[pat-last];
	  let skip = compiled-pattern;
	  local method search (index :: <integer>)
		  if (index >= big-end)	// past end of big -- it's not here
		    #f;
		  else 
		    let char = big[index];
		    if (equal?(char, last-char, case-sensitive)) 
		      // maybe it's here -- we'd better check
		      for (pat-key from 0 below pat-last,
			   big-key from index - pat-last,
			   while: equal?(big[big-key], pattern[pat-key],
					 case-sensitive))
		      finally
			if (pat-key == pat-last) // fell off end -- found it.
			  index - pat-last;
			else
			  search(index + 1) // no luck -- try further down
			end if;
		      end for;
		    else    // last character didn't match, so we can use
		            // the "skip table" to optimize
		      let incr :: <integer>
			= element(skip, as(<integer>, char), default: pat-sz);
		      search(index + incr);
		    end if;
		  end if;
		end method;
	  search(start + pat-last);
      end select;
  if (start-of-occurence)
    values(start-of-occurence, start-of-occurence + pat-sz);
  else
    #f;
  end if;
end method find-substring;


// Used by positioners.  Not exported.
//
// Produce a skip table for Boyer-Moore-ish searching.  By splitting this off
// into a separate routine we allow people to pre-compile heavily used
// strings, thus avoiding one of the more expensive parts of the search.
//
define method compile-substring
    (pattern :: <byte-string>, case-sensitive :: <boolean>)
 => (compiled :: <simple-object-vector>);
  let sz = size(pattern);
  if (sz < 3)
    make(<simple-object-vector>, size: 0)
  else
    let result = make(<simple-object-vector>, size: 256, fill: sz);
    for (index from 0 below sz - 1, skip from sz - 1 by -1)
      if (case-sensitive == #f)
	result[as(<integer>, as-lowercase(pattern[index]))] := skip;
	result[as(<integer>, as-uppercase(pattern[index]))] := skip;
      else
	result[as(<integer>, pattern[index])] := skip;
      end if;
    end for;
    result;
  end if;
end method compile-substring;

define method substring-replace 
    (big :: <string>, search-for :: <byte-string>, 
     replace-with :: <string>, #key count, start = 0, 
     end: input-end = size(big))
 => replaced-string :: <string>;
  let positioner = make-substring-positioner(search-for);
  do-replacement(positioner, replace-with, big, start, input-end, count, #f);
end method substring-replace;


define method make-substring-replacer
    (search-for :: <byte-string>, #key replace-with, case-sensitive = #f)
 => replacer :: <function>;
  let positioner = make-substring-positioner(search-for, 
					     case-sensitive: case-sensitive);
  if (replace-with)
    method (big :: <string>, #key count, start = 0, 
	    end: input-end = size(big))
     => new-string :: <byte-string>;
      do-replacement(positioner, replace-with, big, start, 
		     input-end, count, #f);
    end method;
  else
    method (big :: <string>, replace-with :: <string>, 
	    #key count, start = 0, end: input-end = size(big))
     => new-string :: <byte-string>;
      do-replacement(positioner, replace-with, big, start, 
		     input-end, count, #f);
    end method;
  end if;
end method make-substring-replacer;
