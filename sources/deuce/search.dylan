Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Searching through intervals

define constant <integer-vector> = limited(<vector>, of: <integer>);

// Find the string in the interval starting at the given BP, returning either
// a new BP or #f.  If 'syntax-table' is supplied, it is a syntax table used
// to compute delimiters for whole-word searching.
//--- Note that we don't presently support '\n' in the pattern string
define method search
    (bp :: <basic-bp>, string :: <byte-string>,
     #key test = char-equal?, reverse? = #f, syntax-table = #f,
	  skip-table         :: false-or(<integer-vector>) = #f,
	  reoccurrence-table :: false-or(<integer-vector>) = #f)
 => (bp :: false-or(<basic-bp>))
  let length :: <integer> = size(string);
  case
    skip-table & reoccurrence-table & ~reverse? =>
      // The fast case -- searching forward and Boyer-Moore tables supplied
      let skip-table         :: <integer-vector> = skip-table;
      let reoccurrence-table :: <integer-vector> = reoccurrence-table;
      block (return)
	local method do-search
		  (line :: <line>, si :: <integer>, ei :: <integer>, last?)
		ignore(last?);
		when (text-line?(line))
		  let index = cached-boyer-search(string, line-contents(line),
						  skip-table, reoccurrence-table,
						  test: test, start: si, end: ei);
		  when (index)
		    return(make(<simple-bp>, line: line, index: index))
		  end
		end
	      end method,
	      method do-word-search
		  (line :: <line>, si :: <integer>, ei :: <integer>, last?)
		ignore(last?);
		block (break)
		  while (#t)
		    when (text-line?(line))
		      let index = cached-boyer-search(string, line-contents(line),
						      skip-table, reoccurrence-table,
						      test: test, start: si, end: ei);
		      if (index)
			let ch1 = if (index = 0) '\n'
				  else line-contents(line)[index - 1] end;
			let ch2 = if (index + length >= ei) '\n'
				  else line-contents(line)[index + length] end;
			if (  character-syntax(ch1, syntax-table) ~= $word-alphabetic
			    & character-syntax(ch2, syntax-table) ~= $word-alphabetic)
			  return(make(<simple-bp>, line: line, index: index))
			else
			  si := index + length
			end
		      else
			break()
		      end
		    end
		  end
		end
	      end method;
	let interval = make-interval(bp, interval-end-bp(bp-buffer(bp)));
	do-lines(if (syntax-table) do-word-search else do-search end, interval);
	#f
      end;
    reverse? =>
      // Going backwards, use the slow string search
      block (return)
	local method do-reverse-search
		  (line :: <line>, si :: <integer>, ei :: <integer>, last?)
		ignore(last?);
		when (text-line?(line))
		  let index = string-reverse-search(string, line-contents(line),
						    test: test, start: si, end: ei);
		  when (index)
		    return(make(<simple-bp>, line: line, index: index))
		  end
		end
	      end method,
	      method do-reverse-word-search
		  (line :: <line>, si :: <integer>, ei :: <integer>, last?)
		ignore(last?);
		block (break)
		  while (#t)
		    when (text-line?(line))
		      let index = string-reverse-search(string, line-contents(line),
							test: test, start: si, end: ei);
		      if (index)
			let ch1 = if (index = 0) '\n'
				  else line-contents(line)[index - 1] end;
			let ch2 = if (index + length >= ei) '\n'
				  else line-contents(line)[index + length] end;
			if (  character-syntax(ch1, syntax-table) ~= $word-alphabetic
			    & character-syntax(ch2, syntax-table) ~= $word-alphabetic)
			  return(make(<simple-bp>, line: line, index: index))
			else
			  ei := index
			end
		      else
			break()
		      end
		    end
		  end
		end
	      end method;
	let interval = make-interval(interval-start-bp(bp-buffer(bp)), bp);
	do-lines(if (syntax-table) do-reverse-word-search else do-reverse-search end, interval,
		 from-end?: #t);
	#f
      end;
    otherwise =>
      // Going forward, but no Boyer-Moore tables -- use the slow string search
      block (return)
	local method do-search
		  (line :: <line>, si :: <integer>, ei :: <integer>, last?)
		ignore(last?);
		when (text-line?(line))
		  let index = string-search(string, line-contents(line),
					    test: test, start: si, end: ei);
		  when (index)
		    return(make(<simple-bp>, line: line, index: index))
		  end
		end
	      end method,
	      method do-word-search
		  (line :: <line>, si :: <integer>, ei :: <integer>, last?)
		ignore(last?);
		block (break)
		  while (#t)
		    when (text-line?(line))
		      let index = string-search(string, line-contents(line),
						test: test, start: si, end: ei);
		      if (index)
			let ch1 = if (index = 0) '\n'
				  else line-contents(line)[index - 1] end;
			let ch2 = if (index + length >= ei) '\n'
				  else line-contents(line)[index + length] end;
			if (  character-syntax(ch1, syntax-table) ~= $word-alphabetic
			    & character-syntax(ch2, syntax-table) ~= $word-alphabetic)
			  return(make(<simple-bp>, line: line, index: index))
			else
			  si := index + length
			end
		      else
			break()
		      end
		    end
		  end
		end
	      end method;
	let interval = make-interval(bp, interval-end-bp(bp-buffer(bp)));
	do-lines(if (syntax-table) do-word-search else do-search end, interval);
	#f
      end;
  end
end method search;


/// Faster Boyer-Moore searching functions

// Note the default test function is _not_ case-sensitive!
define method boyer-search 
    (pattern :: <byte-string>, source :: <byte-string>,
     #key test = char-equal?,
	  start: _start :: <integer> = 0, end: _end :: <integer> = size(source))
 => (position :: false-or(<integer>))
  let (skip-table, reoccurrence-table)
    = compute-boyer-tables(pattern, test: test);
  cached-boyer-search(pattern, source, skip-table, reoccurrence-table,
		      test: test, start: _start, end: _end)
end method boyer-search;

define sealed method compute-boyer-tables
    (pattern :: <byte-string>,
     #key skip-table, reoccurrence-table, test = char-equal?)
 => (skip-table :: <integer-vector>, reoccurrence-table :: <integer-vector>)
  let skip-table :: <integer-vector>
    = skip-table | make(<integer-vector>, size: $largest-byte-character-code, fill: 0);
  let reoccurrence-table :: <integer-vector>
    = reoccurrence-table | make(<integer-vector>, size: size(pattern), fill: 0);
  fill-skip-table!(pattern, skip-table, test: test);
  fill-reoccurrence-table!(pattern, reoccurrence-table, test: test);
  values(skip-table, reoccurrence-table)
end method compute-boyer-tables;

define sealed method cached-boyer-search 
    (pattern :: <byte-string>, source :: <byte-string>,
     skip-table :: <integer-vector>, reoccurrence-table :: <integer-vector>,
     #key test = char-equal?,
	  start: _start :: <integer> = 0, end: _end :: <integer> = size(source))
 => (position :: false-or(<integer>))
  range-check(source, size(source), _start, _end);
  let length-1 :: <integer> = size(pattern) - 1;
  without-bounds-checks
    block (return)
      // Start searching at the right-hand end of the pattern, offset by the start index
      let i :: <integer> = _start + length-1;
      while (#t)
	let j :: <integer> = length-1;
	when (i >= _end)
	  // Fail if we've gone to far ever to match
	  return(#f)
	end;
	block (break)
	  // If the last character of the pattern matches against the
	  // source string, then try to match up the entire pattern
	  while (#t)
	    when (j < 0)
	      return(i + 1)
	    end;
	    if (test(source[i], pattern[j]))
	      dec!(i);
	      dec!(j)
	    else
	      break()
	    end
	  end
	end;
	// If unsuccessful, slide the pattern over as far as possible
	inc!(i, max(skip-table[as(<integer>, source[i])],
		    reoccurrence-table[j]))
      end
    end
  end
end method cached-boyer-search;

// The skip table tells the searcher how far it can slide the pattern over the
// source string when there is a mismatch.  It is as long as our "alphabet",
// and is constructed as follows: for all the characters CH in the alphabet,
// if CH occurs in that pattern, then skip(CH) is set to (pattern-length - J)
// where J is the position of the rightmost occurrence of CH in the pattern,
// otherwise skip(CH) is set to pattern-length.  If the table is passed in
// as an argument, don't allocate it, just side-effect it.
define sealed method fill-skip-table!
    (pattern :: <byte-string>, skip-table :: <integer-vector>,
     #key test = char-equal?)
 => (skip-table :: <integer-vector>)
  let length   :: <integer> = size(pattern);
  let length-1 :: <integer> = length - 1;
  fill!(skip-table, length);
  local method rightmost-occurrence
	    (char :: <byte-character>) => (i :: false-or(<integer>))
	  // Find the rightmost occurrence of the character in the pattern
	  block (return)
	    without-bounds-checks
	      for (j :: <integer> from length-1 to 0 by -1)
		when (test(char, pattern[j]))
		  return(j)
		end
	      end
	    end;
	    #f
	  end
	end method;
  // We set the initial value above, so only look at characters in the
  // pattern (and only do them once!)
  without-bounds-checks
    for (char :: <byte-character> in pattern)
      let code = as(<integer>, char);
      when (skip-table[code] = length)
	skip-table[code] := length-1 - rightmost-occurrence(char)
      end;
      when (test = char-equal? & alpha-char?(char))
	// If the search is not case-sensitive, account for the uppercase
	// character as well as the lowercase character
	skip-table[logxor(code, #o40)] := skip-table[code]
      end
    end
  end;
  skip-table
end method fill-skip-table!;

// The reoccurrence table tells the search engine how far it can slide the pattern
// over the source string so as to align a discovered occurrence in the source
// string of some terminal substring of the pattern.  The table is just a
// convenient representation of the rightmost plausible reoccurrence of the
// terminal substring of the pattern.
define sealed method fill-reoccurrence-table!
    (pattern :: <byte-string>, reoccurrence-table :: <integer-vector>,
     #key test = char-equal?)
 => (reoccurrence-table :: <integer-vector>)
  let length   :: <integer> = size(pattern);
  let length-1 :: <integer> = length - 1;
  fill!(reoccurrence-table, length);
  local method unify
	    (str1 :: <byte-string>, from1 :: <integer>, to1 :: <integer>,
	     str2 :: <byte-string>, from2 :: <integer>, to2 :: <integer>)
	 => (match? :: <boolean>)
	  // Like 'string-equal?', except that when the index is negative,
	  // it matches.  NB: I1 will never be out of bounds, by definition
  	  // of 'rightmost-plausible-reoccurrence'.  An initial bounds check
	  // for STR2 is used to indicate an immediate failure.
	  block (return)
	    when (to2 > length-1)
	      return(#f)
	    end;
	    without-bounds-checks
	      for (i1 :: <integer> from from1 to to1,
		   i2 :: <integer> from from2 to to2)
		when (i2 >= 0 & ~test(str1[i1], str2[i2]))
		  return(#f)
		end
	      end
	    end;
	    #t
	  end
	end method,
	method rightmost-plausible-reoccurrence
	    (j :: <integer>) => (r :: <integer>)
	  // Find rightmost plausible reoccurrence, with the constraint
	  // that the sub-pattern is not preceded by the same character
	  // as the final character in the sub-pattern unless it is the
	  // first character.  NB: this can go negative!
	  block (return)
	    without-bounds-checks
	      for (k :: <integer> from length-1 by -1)
		when ((k <= 0 | ~test(pattern[k - 1], pattern[j]))
		      & unify(pattern, j + 1, length-1,
			      pattern, k, k + length-1 - j - 1))
		  return(k)
		end
	      end
	    end
	  end
	end method;
  without-bounds-checks
    for (j :: <integer> from 0 below length)
      reoccurrence-table[j] := length - rightmost-plausible-reoccurrence(j)
    end
  end;
  reoccurrence-table
end method fill-reoccurrence-table!;


/// Slow searching functions

// Note the default test function is _not_ case-sensitive!
define sealed method string-search
    (pattern :: <byte-string>, source :: <byte-string>,
     #key test = char-equal?,
	  start: _start :: <integer> = 0, end: _end :: <integer> = size(source))
 => (index :: false-or(<integer>))
  range-check(source, size(source), _start, _end);
  block (return)
    let length :: <integer> = size(pattern);
    case
      length = 0 =>
	return(_start);
      length > _end - _start =>
	return(#f);
    end;
    without-bounds-checks
      let ch1 = pattern[0];
      for (i :: <integer> from _start below _end)
	let ch2 = source[i];
	when (test(ch1, ch2))
	  when (block (break)
		  for (i1 :: <integer> from 1 below length,
		       i2 :: <integer> from i + 1,
		       until: i2 = _end & return(#f))
		    let ch1 = pattern[i1];
		    let ch2 = source[i2];
		    unless (test(ch1, ch2))
		      break(#f)
		    end;
		  finally break(#t);
		  end
		end)
	    return(i)
	  end
	end
      end
    end
  end
end method string-search;

// Note the default test function is _not_ case-sensitive!
define sealed method string-reverse-search
    (pattern :: <byte-string>, source :: <byte-string>,
     #key test = char-equal?,
	  start: _start :: <integer> = 0, end: _end :: <integer> = size(source))
 => (index :: false-or(<integer>))
  range-check(source, size(source), _start, _end);
  block (return)
    let length :: <integer> = size(pattern);
    case
      length = 0 =>
	return(_end);
      length > _end - _start =>
	return(#f);
    end;
    without-bounds-checks
      let ch1 = pattern[0];
      for (i :: <integer> from _end - length to _start by -1)
	let ch2 = source[i];
	when (test(ch1, ch2))
	  when (block (break)
		  for (i1 :: <integer> from 1 below length,
		       i2 :: <integer> from i + 1)
		    let ch1 = pattern[i1];
		    let ch2 = source[i2];
		    unless (test(ch1, ch2))
		      break(#f)
		    end;
		  finally break(#t);
		  end
		end)
	    return(i)
	  end
	end
      end
    end
  end
end method string-reverse-search;
