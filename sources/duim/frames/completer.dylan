Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Chunkwise completer

// Complete STRING chunk-wise against the completion possibilities in the
// COMPLETIONS, using DELIMITERS to break the strings into chunks.  ACTION
// should be #"complete", #"complete-limited", #"complete-maximal", or
// #"completions" (see below).  LABEL-KEY and VALUE-KEY are used to extract
// the completion string and object from the entries in COMPLETIONS, and
// PREDICATE (if supplied) is applied to filter out unwanted objects.
// Returns five values, the completed string, whether or not the completion
// successfully matched, the object associated with the completion, the
// number of things that matches, and (if ACTION is #"completions") a sequence
// of possible completions.
//
// When ACTION is #"complete", this completes the input as much as possible,
// except that if the user's input exactly matches one of the possibilities,
// the shorter possibility is returned as the result, even if it is a left
// substring of another possibility.
// When ACTION is #"complete-limited", this completes the input up to the next
// partial delimiter.
// When ACTION is #"complete-maximal", this completes the input as much as possible.
// When ACTION is #"completions" or #"apropos-completions", this returns a sequence
// of the possible completions.
//
// For example,
//   complete-from-sequence("s w mc",
//                          #["one fish two fish", "red fish blue fish",
//                            "single white male", "scott wesley mckay",
//                            "on beyond zebra"], #[' ', '-'],
//                          label-key: identity, value-key: identity)
define method complete-from-sequence
    (string :: <string>, possibilities :: <sequence>, delimiters :: <sequence>,
     #key action = #"complete", predicate :: false-or(<function>) = #f,
          label-key :: <function> = first, value-key :: <function> = second)
 => (string :: false-or(<string>), success? :: <boolean>, object,
     n-matches :: <integer>, completions :: <sequence>)
  block (return)
    if (~member?(action, #[#"completions", #"apropos-completions"])
	& size(string) = 0)
      return(#f, #f, #f, 0, #[])
    else
      let best-completion = #f;
      let best-length     = #f;
      let best-object     = $unfound;
      let n-matches       = 0;
      let completions :: <stretchy-object-vector> = make(<stretchy-vector>);
      local method complete-1 (possibility) => ()
	      let completion = label-key(possibility);
	      let object     = value-key(possibility);
	      when (~predicate | predicate(object))
		// If we are doing simple completion and the user-supplied string is
		// exactly equal to this completion, then claim success (even if there
		// are other completions that have this one as a left substring!).
		when (action == #"complete"
		      & string-equal?(string, completion))
		  return(completion, #t, object, 1, #[])
		end;
		let (bc, bl, bo, nm, p)
		  = chunkwise-complete-string(string, completion, object, action, delimiters,
					      best-completion, best-length, best-object,
					      n-matches, completions);
		best-completion := bc;
		best-length     := bl;
		best-object     := bo;
		n-matches       := nm;
		completions     := p
	      end
	    end method;
      //--- If the completions were guaranteed to be sorted, this could be a bit faster
      do(complete-1, possibilities);
      return(if (best-completion) copy-sequence(best-completion, end: best-length) else string end,
	     ~(best-object == $unfound),
	     ~(best-object == $unfound) & best-object,
	     n-matches,
	     completions)
    end
  end
end method complete-from-sequence;

// Just like 'complete-from-sequence', except that the possibilities are
// gotten by funcalling a generator rather than from a completion alist.
// GENERATOR is a function of two arguments: the string to be completed and
// a continuation coroutine to call to do the completion.  It should call the
// continuation with two arguments, the completion string and an object.
// For example,
//   complete-from-generator
//     ("th", method (string, completer)
//	        for (b in #["one", "two", "three", "four"])
//		  completer(b, b)
//	        end
//	      end method, #[' ', '-'])
define method complete-from-generator
    (string :: <string>, generator :: <function>, delimiters :: <sequence>,
     #key action = #"complete", predicate :: false-or(<function>) = #f)
 => (string :: false-or(<string>), success? :: <boolean>, object,
     n-matches :: <integer>, completions :: <sequence>)
  block (return)
    if (~member?(action, #[#"completions", #"apropos-completions"])
	& size(string) = 0)
      return(#f, #f, #f, 0, #[])
    else
      let best-completion = #f;
      let best-length     = #f;
      let best-object     = $unfound;
      let n-matches       = 0;
      let completions :: <stretchy-object-vector> = make(<stretchy-vector>);
      local method complete-1 (completion, object) => ()
	      when (~predicate | predicate(object))
		when (action == #"complete"
		      & string-equal?(string, completion))
		  return(completion, #t, object, 1, #[])
		end;
		let (bc, bl, bo, nm, c)
		  = chunkwise-complete-string(string, completion, object, action, delimiters,
					      best-completion, best-length, best-object,
					      n-matches, completions);
		best-completion := bc;
		best-length     := bl;
		best-object     := bo;
		n-matches       := nm;
		completions     := c
	      end
	    end method;
      generator(string, complete-1);
      return(if (best-completion) copy-sequence(best-completion, end: best-length) else string end,
	     ~(best-object == $unfound),
	     ~(best-object == $unfound) & best-object,
	     n-matches,
	     completions)
    end
  end
end method complete-from-generator;

// The common subroutine used to do chunkwise completion
define method chunkwise-complete-string
    (string :: <string>, completion :: <string>, object, action, delimiters :: <sequence>,
     best-completion :: false-or(<string>), best-length :: false-or(<integer>), best-object,
     n-matches :: <integer>, completions :: <stretchy-object-vector>)
 => (best-completion :: false-or(<string>), best-length :: false-or(<integer>), best-object,
     n-matches :: <integer>, completions :: <stretchy-object-vector>)
  let length :: <integer>
    = size(string);
  let matches :: <integer>
    = if (action == #"apropos-completions")
	if (subsequence-position(string, completion, test: char-equal?)) length else 0 end
      else
	chunkwise-string-compare(string, completion, delimiters)
      end;
  when (matches = length)
    n-matches := n-matches + 1;
    select (action)
      #"completions", #"apropos-completions" =>
	add!(completions, list(completion, object));
      #"complete", #"complete-maximal" =>
	#f;
      #"complete-limited" =>
	// Match up only as many chunks as the user has typed
	local method count-delimiters
		  (string :: <string>) => (n :: <integer>)
		let n :: <integer> = 0;
		for (char in string)
		  when (member?(char, delimiters))
		    n := n + 1
		  end
		end;
		n
	      end method;
	local method find-delimiter
		  (string :: <string>, start :: <integer>) => (i :: false-or(<integer>))
		block (return)
		  without-bounds-checks
		    for (i from start below size(string))
		      when (member?(string[i], delimiters))
			return(i)
		      end
		    end
		  end;
		  #f
		end
	      end method;
	let nchunks :: <integer>
	  = count-delimiters(string) + 1;
	let cutoff :: false-or(<integer>)
	  = block (return)
	      let start = 0;
	      let cutoff = #f;
	      for (i from 0 below nchunks)
		let new = find-delimiter(completion, start);
		unless (new) return(#f) end;
		cutoff := new;
		start  := new + 1
	      end;
	      cutoff
	    end;
	when (cutoff)
	  completion := copy-sequence(completion, end: cutoff + 1);
	  // Increment this once more to make the calling function think
	  // that the completion is ambiguous
	  n-matches := n-matches + 1
	end;
    end;
    if (best-completion)
      let new-length :: <integer>
	= chunkwise-string-compare(best-completion, completion, delimiters,
				   merge?: #t, end1: best-length);
      if (~best-length | new-length > best-length)
	best-length := new-length;
	best-object := object
      else
	best-length := new-length;
	best-object := $unfound
      end
    else
      best-completion := copy-sequence(completion);
      best-length := size(best-completion);
      best-object := object
    end
  end;
  values(best-completion, best-length, best-object, n-matches, completions)
end method chunkwise-complete-string;

// Compare STRING1 against STRING2 in "chunks", using DELIMITERS to break
// the strings into chunks.  Returns two values, the index of the first place
// where the strings mismatch and the index of the last character that was
// unambiguous.  When MERGE? is #t, STRING1 gets side-effected.
define method chunkwise-string-compare
    (string1 :: <string>, string2 :: <string>, delimiters :: <sequence>,
     #key merge?, end1, end2)
 => (matched :: <integer>, ambiguous :: false-or(<integer>))
  let len1 :: <integer> = end1 | size(string1);
  let len2 :: <integer> = end2 | size(string2);
  let matched :: <integer> = 0;
  let ambiguous  :: false-or(<integer>) = #f;
  let i1 :: <integer> = 0;
  let i2 :: <integer> = 0;
  block (break)
    without-bounds-checks
      while (#t)
	when (i1 >= len1 | i2 >= len2)
	  break()
	end;
	let char1 :: <character> = string1[i1];
	let char2 :: <character> = string2[i2];
	if (char1 == char2 | char-equal?(char1, char2))
	  when (merge?)
	    string1[matched] := char1
	  end;
	  matched := matched + 1;
	  i1 := i1 + 1;
	  i2 := i2 + 1
	else
	  unless (ambiguous)
	    ambiguous := matched
	  end;
	  case
	    member?(char1, delimiters) =>
	      when ((~merge? & i1 > matched)
		    | member?(char2, delimiters))
		break()
	      end;
	    member?(char2, delimiters) =>
	      when ((~merge? & i2 > matched))
		break()
	      end;
	    otherwise =>
	      unless (merge?)
		break()
	      end;
	  end;
	  until (member?(string1[i1], delimiters)
		 | (i1 := i1 + 1) >= len1)
	  end;
	  until (member?(string2[i2], delimiters)
		 | (i2 := i2 + 1) >= len2)
	  end;
	end
      end
    end
  end;
  values(matched, ambiguous | matched)
end method chunkwise-string-compare;
