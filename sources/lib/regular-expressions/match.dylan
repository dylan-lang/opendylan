module:   regular-expressions
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: This takes a parsed regular expression and tries to find a match
          for it.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /scm/cvs/fundev/Sources/lib/regular-expressions/match.dylan,v 1.1 2004/03/12 00:08:52 cgay Exp $

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

define constant <non-local-exit> = <function>;

// Details of match:

// This whole thing is rather hairy.  Basically, it creates a "path"
// through the regexp parse tree that corresponds to a match of the
// string.  A path is a round trip through a parse tree that starts
// and ends at the root. The part of the path already travelled is the
// call stack, and hints about the untravelled part of the path are
// stored as a list of functions called the up-proc-list.  (Whenever
// you want to go "up" the parse tree, you call the first function in
// the up-proc-list)

// Match-root? declares a few non-local exits to pass around, and then
// calls descend-re to get things moving.  If the appropriate method
// of descend-re is recursive (and most are), it puts its "up-proc" on
// the up-proc-list, and makes a recursive call.  When the recursive
// call is "done", it'll call the first function on the up-proc-list,
// which happens to be the function we just put there.  This up-proc
// will generally do some work, and then will either call descend-re
// or will itself call the first thing on its up-proc-list.

// If descend-re determines this path is a dead end, it'll invoke a
// backtrack function.  Each descend-re invocation generally sets up
// its own non-local exit so that it can try to match its part
// differently.

// As an example, a <union> is "regexp #1 or regexp #2".  When
// descend-re(<union>...) is called, it'll set up a non-local exit and
// then descend-re on regexp #1.  If someone backtracks out of regexp
// #1, descend-re(<union>) will try regexp #2.  If someone backtracks
// out of that, descend-re(<union>) will give up and backtrack itself.

// When this chain of functions completes a match, it'll stumble upon
// the succeed up-proc that match-root? sets up.  Otherwise, it'll
// backtrack until it gets to match-root?'s "fail" non-local exit.

define class <substring> (<object>)
  // KJP: Constant slots.
  constant slot entire-string :: <string>, required-init-keyword: #"string";
  constant slot start-index :: <integer>, required-init-keyword: #"start";
  constant slot end-index :: <integer>, required-init-keyword: #"end";
end class <substring>;

define sealed domain make(singleton(<substring>));
define sealed domain initialize(<substring>);

// Match-root?: Set things up and call descend-re.
//
define method match-root?
    (re :: <parsed-regexp>, target :: <substring>,
     case-sensitive? :: <boolean>, num-groups :: <integer>,
     searcher :: false-or(<function>))
 => (answer :: <boolean>, marks :: <sequence>);
  let marks = make(<vector>, size: num-groups * 2, fill: #f);
  let answer
    = block (succeed)
	local method up-proc (index :: <integer>, 
			      backtrack :: <non-local-exit>, 
			      up-list :: <list>);
		succeed(#t);
	      end method up-proc;

	let string = target.entire-string;
	let end-index = target.end-index;

	// Try each possible starting point.  As soon as a match is
	// found, it'll quit via the success non-local exit.
	// (and yes, that's *to* size(string), not *below* size(string))
	if (searcher)
	  for (index
		 = searcher(string, start: target.start-index, end: end-index)
		 then searcher(string, start: index + 1, end: end-index),
	       while: index)
	    block (fail)
	      descend-re(re, target, case-sensitive?, index,
			 marks, fail, list(up-proc));
	      error("A regexp should either match or not match. Why did it "
		      "reach this piece of code?");
	    end block;
	  end for;
	  values(#f);      // Failure
	else
	  for (index from target.start-index to end-index)
	    block (fail)
	      descend-re(re, target, case-sensitive?, index,
			 marks, fail, list(up-proc));
	      error("A regexp should either match or not match. Why did "
		      "it reach this piece of code?");
	    end block;
	  end for;
	  values(#f);      // Failure
	end if;
      end block;         // success block
  values(answer, marks);
end method match-root?;

// Anchored-match-root?: Handles the simple case where the search string
// starts with "^".
//
define method anchored-match-root?
    (re :: <parsed-regexp>, target :: <substring>,
     case-sensitive? :: <boolean>, num-groups :: <integer>,
     searcher :: false-or(<function>))
 => (answer :: <boolean>, marks :: <sequence>);
  let marks = make(<vector>, size: num-groups * 2, fill: #f);
  let answer
    = block (succeed)
	local method up-proc (index :: <integer>, 
			      backtrack :: <non-local-exit>, 
			      up-list :: <list>);
		succeed(#t);
	      end method up-proc;

	block (fail)
	  descend-re(re, target, case-sensitive?, target.start-index,
		     marks, fail, list(up-proc));
	  error("A regexp should either match or not match. Why did it "
		  "reach this piece of code?");
	end block;
	values(#f);      // Failure
      end block;         // success block
  values(answer, marks);
end method anchored-match-root?;

define generic descend-re
    (re :: false-or(<parsed-regexp>), target :: <substring>,
     case-sensitive? :: <boolean>, start-index :: <integer>,
     marks :: <mutable-sequence>, backtrack-past-me :: <non-local-exit>,
     up-list :: <list> /* of <non-local-exit> */) => ();

// Marks
//
define method descend-re
    (re :: <mark>, target :: <substring>, case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();

     // The up-proc makes a mark of where it is and calls the next up
  local method up-proc (current-index :: <integer>, 
			current-backtrack :: <non-local-exit>, 
			current-up-list :: <list>)
	  marks[1 + 2 * re.group-number] := current-index;
	  head(current-up-list)(current-index, current-backtrack, 
				tail(current-up-list));
	end method up-proc;

  let old-start-mark = marks[2 * re.group-number];
  let old-end-mark = marks[1 + 2 * re.group-number];  
           // Save this in case this path doesn't work out

  marks[2 * re.group-number] := start-index;

  block (backtrack-to-me)
    descend-re(re.child, target, case-sensitive?, start-index,
	       marks, backtrack-to-me, pair(up-proc, up-list));
  end block;

    // Since he backtracked, clean up the marks and backtrack to
    // someone who cares.
  marks[2 * re.group-number]     := old-start-mark;
  marks[1 + 2 * re.group-number] := old-end-mark;
  backtrack-past-me();
end method descend-re;
  
// Union: Try one path.  If you get a backtrack, try the other.
//
define method descend-re
    (re :: <union>, target :: <substring>, case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();

  block (backtrack-to-me)
    descend-re(re.left, target, case-sensitive?, start-index,
	       marks, backtrack-to-me, up-list);
  end block;

  // If we've gotten this far, it means that the user backtracked.
  // Try the right, with the provision that we can do no more.

  descend-re(re.right, target, case-sensitive?, start-index,
	     marks, backtrack-past-me, up-list);
end method descend-re;

// At present the only way this should be called is if a "union" has
// only one branch.  (This happens when union is used to mark a group
// or override precedence rather than actually indicating multiple
// paths)  So, just backtrack.
//
define method descend-re
    (re == #f, target :: <substring>, case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();
  backtrack-past-me();
end method descend-re;

// Concatenation
//
define method descend-re
    (re :: <alternative>, target :: <substring>, case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();
    // up-proc is "Ok, we've matched on the left, now match on the
    // right".  If it matches, we don't ever hear about it because we
    // put nothing on the up-list.
  local method up-proc (current-index :: <integer>, 
			current-backtrack :: <non-local-exit>,
			current-up-list :: <list>)
	  descend-re(re.right, target, case-sensitive?, current-index, marks, 
		     current-backtrack, current-up-list);
	end method up-proc;

  descend-re(re.left, target, case-sensitive?, start-index, marks,
	     backtrack-past-me, pair(up-proc, up-list));
end method descend-re;
  
// Assertions
//
define method descend-re
    (re :: <parsed-assertion>, target :: <substring>,
     case-sensitive? :: <boolean>, start-index :: <integer>,
     marks :: <mutable-sequence>, backtrack-past-me :: <non-local-exit>, 
     up-list :: <list>)
 => (); 
  if (assertion-true?(re.asserts, target, start-index))
    head(up-list)(start-index, backtrack-past-me, tail(up-list));
  else
    backtrack-past-me();
  end if;
end method descend-re;

// Quantified atoms
//
define method descend-re
    (re :: <quantified-atom>, target :: <substring>, 
     case-sensitive? :: <boolean>, start-index :: <integer>, 
     marks :: <mutable-sequence>, backtrack-past-me :: <non-local-exit>,
     up-list :: <list>) => ();
  local method descend-and-quantify (min :: <integer>, max, 
				     re :: <parsed-regexp>, index :: <integer>,
				     backtrack-past-me :: <non-local-exit>,
				     up-list)

	  local method keep-quantifying (new-index :: <integer>, 
					 backtrack :: <non-local-exit>, 
					 up-list :: <list>)
		  if (new-index = index  &  min <= 1)
		    backtrack();
		         // The longest thing matched was length 0.
		         // If we don't quit now, we could get
		         // stuck in an infinite loop.

		         // This will give a false negative in some
		         // cases where the atom being quantified can
		         // match the empty string (like (a*|b)*), but
		         // Perl rejects it at parse time, so this
		         // solution is not really any worse than
		         // anyone else's.
		  else
		    descend-and-quantify(min - 1, max & (max - 1), re,
					 new-index, backtrack, up-list);
		  end if;
		end method keep-quantifying;

	  if (max = 0)    // Go up
	    head(up-list)(index, backtrack-past-me, tail(up-list));

	  elseif (min > 0) // Mandatory match
	    descend-re(re, target, case-sensitive?, index, marks, backtrack-past-me, 
		       pair(keep-quantifying, up-list));

	  else   // We've made enough matches, but we'd like to make more
	    block (backtrack-to-me)
	      descend-re(re, target, case-sensitive?, index, marks, backtrack-to-me, 
			 pair(keep-quantifying, up-list));
	    end block;
	       // If we reach here, there was a backtrack. Give up on
	       // the idea of trying to match more.
	    head(up-list)(index, backtrack-past-me, tail(up-list));
	  end if;
	end method descend-and-quantify;

  descend-and-quantify(re.min-matches, re.max-matches, re.atom,
		       start-index, backtrack-past-me, up-list);
end method descend-re;

// Characters
//
define method descend-re
    (re :: <parsed-character>, target :: <substring>, 
     case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();
  if (start-index < target.end-index
	& char-equal?(case-sensitive?, re.character,
		      target.entire-string[start-index]))
    head(up-list)(start-index + 1, backtrack-past-me, tail(up-list));
  else
    backtrack-past-me();
  end if;
end method descend-re;

// Strings
//
define method descend-re
    (re :: <parsed-string>, target :: <substring>,
     case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();
  let string = re.string;
  let final-index = start-index + string.size;
  if (final-index > target.end-index)
    backtrack-past-me();
  else
    let target-string = target.entire-string;
    for (string-index from 0, target-index from start-index below final-index)
      if (~char-equal?(case-sensitive?, string[string-index],
		       target-string[target-index]))
	backtrack-past-me();
      end if;
    end for;
    head(up-list)(final-index, backtrack-past-me, tail(up-list));
  end if;
end method descend-re;

// Character set
//
define method descend-re
    (re :: <parsed-set>, target :: <substring>, case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();
  if (start-index < target.end-index
	& member?(target.entire-string[start-index], re.char-set))
    head(up-list)(start-index + 1, backtrack-past-me, tail(up-list));
  else
    backtrack-past-me();
  end if;
end method descend-re;

// Backreferences
//
define method descend-re
    (re :: <parsed-backreference>, target :: <substring>,
     case-sensitive? :: <boolean>,
     start-index :: <integer>, marks :: <mutable-sequence>,
     backtrack-past-me :: <non-local-exit>, up-list :: <list>) => ();
  let backref-start = marks[2 * re.group-number];
  let backref-end = marks[1 + 2 * re.group-number];
  let substring-2-end-pos = start-index + (backref-end - backref-start);

  if (substring-2-end-pos <= target.end-index
	& substrings-equal?(case-sensitive?, 
			    target.entire-string, backref-start, backref-end,
			    target.entire-string,
			    start-index, substring-2-end-pos))
    head(up-list)(substring-2-end-pos, backtrack-past-me, tail(up-list));
  else
    backtrack-past-me();
  end if;
end method descend-re;
	  
// ---------------------------------------------------------------
// Supporting routines
// ---------------------------------------------------------------

define method char-equal?
    (case-sensitive? :: <boolean>, char1 :: <character>, char2 :: <character>)
 => (result :: <boolean>);
  if (case-sensitive?)
    char1 == char2;
  else
    case-insensitive-equal(char1, char2);
  end if;
end method char-equal?;

// Efficiently compare two substrings, using a provided character by
// character equal? predicate.
//
define method substrings-equal?
    (case-sensitive? :: <boolean>, string1 :: <string>, start1 :: <integer>, 
     end1 :: <integer>, string2 :: <string>, start2 :: <integer>, 
     end2 :: <integer>)
 => answer :: <boolean>;
  if (end1 - start1 ~== end2 - start2)
    #f;
  else
    block (return)
      for (index1 from start1 to end1, index2 from start2)
	if (~ char-equal?(case-sensitive?, string1[index1], string2[index2]))
	  return(#f);
	end if;
      end for;
      #t;
    end block;
  end if;
end method substrings-equal?;


define method assertion-true? (assertion :: <symbol>, target :: <substring>, 
			       index :: <integer>)
 => answer :: <boolean>;
  select (assertion)
    #"final-state"         => #t;
    #"beginning-of-string" => index == target.start-index;
    #"end-of-string"       => index >= target.end-index;
    #"word-boundary"       =>
      index = 0 | index >= target.end-index
	| (member?(target.entire-string[index], word-chars) 
	     ~== member?(target.entire-string[index - 1], word-chars));

    #"not-word-boundary"   =>
      index ~== 0 & index < target.end-index
	& (member?(target.entire-string[index], word-chars)
	     == member?(target.entire-string[index - 1], word-chars));

    otherwise              => 
      error("Unknown assertion %=", assertion);
  end select;
end method assertion-true?;
