module:   %do-replacement
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: This implements search and replace facilities, which is 
          given a wrapper and called from both regular-expressions
          and substring-search.
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

// The local method expand-replace-sequence probably generates
// excessive garbage for replace-with's that involve backslashes.  One
// might try to allocate the largest newest-piece that'll fit between
// backslashes, rather than turn each string into a character every
// time.
//
define inline function do-replacement 
    (positioner :: <function>, new-substring :: <string>,
     input :: <string>, start :: <integer>, input-end :: false-or(<integer>), 
     count :: false-or(<integer>), expand-backreferences :: <boolean>)
 => new-string :: <string>;
  local method expand-replace-sequence (marks :: <sequence>)
	  if (expand-backreferences & member?('\\', new-substring))
	    let return-string = "";
	    let index = 0;
	    while (index < size(new-substring))
	      let newest-piece 
		= if (new-substring[index] ~= '\\')
		    as(<string>, new-substring[index]);
		  else
		    index := index + 1;
		    if (~digit?(new-substring[index]))
		      as(<string>, new-substring[index]);
		    else
		      let ref-number 
			= digit-to-integer(new-substring[index]);
		      if (marks[2 * ref-number] = #f)
			"";
		      else
			copy-sequence(input, start: marks[2 * ref-number],
				      end: marks[1 + 2 * ref-number]);
		      end if;
		    end if;
		  end if;
	      return-string := concatenate(return-string, newest-piece);
	      index := index + 1;
	    end while;
	    return-string;
	  else
	    new-substring;
	  end if;
	end method expand-replace-sequence;

  let result-string = copy-sequence(input, end: start);
  let index = start;
  let num-matches = 0;
  block (done)
    while (~count | num-matches < count)
      let (#rest marks) = positioner(input, start: index, end: input-end);
      if (marks[0] = #f) done() end;
      result-string 
	:= concatenate(result-string, 
		       copy-sequence(input, start: index, end: marks[0]), 
		       expand-replace-sequence(marks));
      index := marks[1];
      num-matches := num-matches + 1;
    end while;
  end block;
  concatenate(result-string, copy-sequence(input, start: index));
end function do-replacement;
