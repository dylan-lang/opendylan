Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motion primitives

/// Note that since these functions cons up simple BPs to hold their
/// intermediate results, there is an implicit assumption that *buffer*
/// is bound to the appropriate buffer.  Beware!

define inline function start-of-line?
    (bp :: <basic-bp>) => (start? :: <boolean>)
  bp-index(bp) = 0
end function start-of-line?;

// Make a new BP that points to the start of the line
define inline function line-start
    (line :: <basic-line>) => (bp :: <basic-bp>)
  make(<simple-bp>, line: line, index: 0)
end function line-start;


define inline function end-of-line?
    (bp :: <basic-bp>) => (end? :: <boolean>)
  bp-index(bp) = line-length(bp-line(bp))
end function end-of-line?;

// Make a new BP that points to the start of the line
define inline function line-end
    (line :: <basic-line>) => (bp :: <basic-bp>)
  make(<simple-bp>, line: line, index: line-length(line))
end function line-end;


// When 'fixup?' is #t, then this (and all the functions herein) will return
// the bounding BP when we get out of range; otherwise this will return #f.
// The bounding interval is usually the buffer, but it can be changed by
// supplying the 'interval' argument. 
// NB: this doesn't detect the case when BP argument is not in range.  That
// should be done at a higher level.
define sealed method increment-bp!
    (bp :: <basic-bp>, #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  let limit :: <basic-bp> = interval-end-bp(interval);
  case
    bp = limit =>
      fixup? & limit;
    end-of-line?(bp) =>
      // NB: always skips structural diagram lines!
      let next = line-next-in-buffer(bp-line(bp), bp-buffer(bp));
      if (next)
        move-bp!(bp, next, 0)
      else
        when (fixup?) move-bp!(bp, bp-line(limit), bp-index(limit)) end
      end;
    otherwise =>
      move-bp!(bp, bp-line(bp), bp-index(bp) + 1);
  end
end method increment-bp!;

define sealed method decrement-bp!
    (bp :: <basic-bp>, #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  let limit :: <basic-bp> = interval-start-bp(interval);
  case
    bp = limit =>
      fixup? & limit;
    start-of-line?(bp) =>
      // NB: always skips structural diagram lines!
      let prev = line-previous-in-buffer(bp-line(bp), bp-buffer(bp));
      if (prev)
        move-bp!(bp, prev, line-length(prev))
      else
        when (fixup?) move-bp!(bp, bp-line(limit), bp-index(limit)) end
      end;
    otherwise =>
      move-bp!(bp, bp-line(bp), bp-index(bp) - 1);
  end
end method decrement-bp!;


/// Higher level motion utilities

// Move the BP forward or backward in the buffer until the predicate
// returns #t.  The predicate gets called with one argument, the character
// at the BP.  The predicate actually returns a second value: #t means
// that the matching character should not be included in the motion.  For
// example, 'rubout-list' uses a predicate that succeeds for whitespace
// if no parens have been seen and the result does not want to include the
// space, but which succeeds on a balancing open-paren and the result does
// want to include the open-paren.
// NB: modifies the BP argument!
//--- This should be sped up by separating the forward and reverse cases,
//--- and inlining the calls to 'bp-character' and 'increment/decrement-bp!'
define sealed method move-forward-or-backward!
    (bp :: <basic-bp>, predicate :: <function>, reverse? :: <boolean>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  let limit :: <basic-bp>
    = if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
  if (bp = limit)
    fixup? & limit
  else
    block (return)
      let bp :: false-or(<basic-bp>) = bp;
      when (reverse?)
	bp := decrement-bp!(bp, fixup?: fixup?, interval: interval)
      end;
      while (bp)
	let char = bp-character(bp);
	let (match?, dont-include?) = predicate(char);
	when (match?)
	  return(if (dont-include? = reverse?)
		   increment-bp!(bp, fixup?: fixup?, interval: interval)
		 else
		   bp
		 end)
	end;
	unless (reverse?)
	  bp := increment-bp!(bp, fixup?: fixup?, interval: interval)
	end;
	when (bp = limit)
	  return(fixup? & limit)
	end;
	when (reverse?)
	  bp := decrement-bp!(bp, fixup?: fixup?, interval: interval)
	end
      end
    end
  end
end method move-forward-or-backward!;


// This, and all the other 'move-over-...' functions return a new BP
define sealed method move-over-characters
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  block (return)
    let bp :: <basic-bp> = copy-bp(bp);
    unless (n = 0)
      let reverse? = (n < 0);
      let limit :: <basic-bp>
	= if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
      local method true (ch)
	      ignore(ch);
	      values(#t, #f)
	  end method;
      for (i :: <integer> from 0 below abs(n))
	when (bp = limit)
	  return(fixup? & limit)
	end;
	move-forward-or-backward!(bp, true, reverse?, interval: interval)
      end
    end;
    bp
  end
end method move-over-characters;


// The idea is to first move over break characters, then move over all
// word constituents until a break character is hit
define sealed method move-over-words
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  block (return)
    let bp :: <basic-bp> = copy-bp(bp);
    unless (n = 0)
      let reverse? = (n < 0);
      let limit :: <basic-bp>
	= if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
      local method word-break-char? (ch :: <byte-character>)
	      values(word-syntax(ch) ~== $word-alphabetic, #t)
	    end method,
	    method word-char? (ch :: <byte-character>)
	      values(word-syntax(ch)  == $word-alphabetic, #t)
	    end method;
      for (i :: <integer> from 0 below abs(n))
	when (bp = limit)
          return(fixup? & limit)
        end;
	move-forward-or-backward!(bp, word-char?, reverse?, interval: interval);
	unless (bp = limit)
	  move-forward-or-backward!(bp, word-break-char?, reverse?, interval: interval)
	end
      end
    end;
    bp
  end
end method move-over-words;


define sealed method move-over-atoms
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  block (return)
    let bp :: <basic-bp> = copy-bp(bp);
    unless (n = 0)
      let reverse? = (n < 0);
      let limit :: <basic-bp>
	= if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
      local method atom-break-char? (ch :: <byte-character>)
	      values(atom-syntax(ch) ~== $atom-alphabetic, #t)
	    end method,
	    method atom-char? (ch :: <byte-character>)
	      values(atom-syntax(ch)  == $atom-alphabetic, #t)
	    end method;
      for (i :: <integer> from 0 below abs(n))
	when (bp = limit)
          return(fixup? & limit)
        end;
	move-forward-or-backward!(bp, atom-char?, reverse?, interval: interval);
	unless (bp = limit)
	  move-forward-or-backward!(bp, atom-break-char?, reverse?, interval: interval)
	end
      end
    end;
    bp
  end
end method move-over-atoms;


define sealed method move-over-lists
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  block (return)
    let bp :: <basic-bp> = copy-bp(bp);
    unless (n = 0)
      let reverse? = (n < 0);
      let limit :: <basic-bp>
	= if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
      local method true (ch)
	      ignore(ch);
	      values(#t, #f)
	    end method,
	    method non-whitespace? (ch :: <byte-character>)
	      if (any-whitespace-char?(ch)) values(#f, #f)
	      else values(#t, #t) end
	    end method;
      for (i :: <integer> from 0 below abs(n))
	when (bp = limit)
          return(fixup? & limit)
        end;
	move-forward-or-backward!(bp, non-whitespace?, reverse?, interval: interval);
	unless (bp = limit)
	  let start-char
	    = if (reverse?) bp-character-before(bp) else bp-character(bp) end;
	  when (~reverse? & list-syntax(start-char) == $list-single-quote)
	    move-forward-or-backward!(bp, true, #t, interval: interval);
	    move-forward-or-backward!(bp, true, #t, interval: interval);
	    start-char := bp-character(bp)
	  end;
	  let nbp
	    = case
		list-syntax(start-char) == $list-double-quote =>
		  move-over-matching-thing!(bp, start-char, reverse?, interval: interval);
		list-syntax(start-char) == (if (reverse?) $list-close else $list-open end) =>
		  move-over-balanced-thing!(bp, start-char, reverse?, interval: interval);
		otherwise =>
		  move-over-atom!(bp, reverse?, interval: interval);
	      end;
	  nbp | return(fixup? & limit)
	end
      end
    end;
    bp      
  end
end method move-over-lists;

// Helper function for 'move-over-lists', clobbers the BP
//--- This needs to handle $list-escape
define sealed method move-over-matching-thing!
    (bp :: <basic-bp>, char :: <byte-character>, reverse? :: <boolean>,
     #key interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  let limit :: <basic-bp>
    = if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
  local method true (ch)
	  ignore(ch);
	  values(#t, #f)
	end method,
        method matches-char? (ch :: <byte-character>)
	  if (ch = char) values(#t, #f)
	  else values(#f, #f) end
	end method;
  move-forward-or-backward!(bp, true, reverse?, interval: interval);
  unless (bp = limit)
    move-forward-or-backward!(bp, matches-char?, reverse?, interval: interval)
  end;
  let matched?
    = ((if (reverse?) bp-character(bp) else bp-character-before(bp) end) = char);
  matched? & bp
end method move-over-matching-thing!;

// Helper function for 'move-over-lists', clobbers the BP
//--- This needs to handle $list-escape
define sealed method move-over-balanced-thing!
    (bp :: <basic-bp>, start-char :: <byte-character>, reverse? :: <boolean>,
     #key interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  let end-char :: <byte-character>
    = select (start-char)
	'(' => ')'; ')' => '(';
	'[' => ']'; ']' => '[';
	'{' => '}'; '}' => '{';
      end;
  let count :: <integer> = 0;
  local method matches-char? (ch :: <byte-character>)
	  case
	    ch = start-char => inc!(count);
	    ch = end-char   => dec!(count);
	  end;
	  values(count = 0, #f)
	end method;
  move-forward-or-backward!(bp, matches-char?, reverse?, interval: interval);
  let matched?
    = ((if (reverse?) bp-character(bp) else bp-character-before(bp) end) = end-char);
  matched? & bp
end method move-over-balanced-thing!;

// Helper function for 'move-over-lists', clobbers the BP
define sealed method move-over-atom!
    (bp :: <basic-bp>, reverse? :: <boolean>,
     #key interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  let limit :: <basic-bp>
    = if (reverse?) interval-start-bp(interval) else interval-end-bp(interval) end;
  local method atom-break-char? (ch :: <byte-character>)
	  values(atom-syntax(ch) ~== $atom-alphabetic, #t)
	end method,
        method atom-char? (ch :: <byte-character>)
	  values(atom-syntax(ch)  == $atom-alphabetic, #t)
	end method;
  move-forward-or-backward!(bp, atom-char?, reverse?, interval: interval);
  unless (bp = limit)
    move-forward-or-backward!(bp, atom-break-char?, reverse?, interval: interval)
  end;
  bp
end method move-over-atom!;


define sealed method move-over-expressions
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  if (n = 0)
    copy-bp(bp)
  else
    do-move-over-expressions(buffer-major-mode(bp-buffer(bp)),
			     bp, n, fixup?: fixup?, interval: interval)
  end
end method move-over-expressions;


// Note that, by default, we limit the motion to the node we're in
// n > 0 means move down (forward), n < 0 means move up (backward)
//--- This loses (slowly!) when there's no place further "up" or "down"
define sealed method move-up-or-down-lists
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-node(bp) | bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  block (return)
    let lbp :: <basic-bp> = copy-bp(bp);
    unless (n = 0)
      let limit :: <basic-bp>
	= if (n < 0) interval-start-bp(interval) else interval-end-bp(interval) end;
      let (open, close)
	= if (n < 0) values($list-open, $list-close)
	  else values($list-close, $list-open) end;
      local method open-or-close? (ch :: <byte-character>)
	      let syntax = list-syntax(ch);
	      values(syntax == open | syntax == close, #f)
	    end method;
      for (i :: <integer> from 0 below abs(n))
	block (break)
	  while (#t)
	    when (lbp = limit)
	      return(fixup? & limit)
	    end;
	    move-forward-or-backward!(lbp, open-or-close?, n < 0, interval: interval);
	    let char = bp-character(lbp);
	    if (list-syntax(char) == close)
              when (n < 0) increment-bp!(lbp, interval: interval) end;
	      move-over-balanced-thing!(lbp, char, n < 0, interval: interval);
              if (n < 0) decrement-bp!(lbp, interval: interval)
              else increment-bp!(lbp, interval: interval) end;
	    else
	      break()
	    end
	  end
	end block;
      end
    end;
    lbp
  end
end method move-up-or-down-lists;

define sealed method move-up-or-down-expressions
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-node(bp) | bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  if (n = 0)
    copy-bp(bp)
  else
    do-move-up-or-down-expressions(buffer-major-mode(bp-buffer(bp)),
				   bp, n, fixup?: fixup?, interval: interval)
  end
end method move-up-or-down-expressions;


define sealed method move-over-lines
    (bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp),
	  skip-test = line-for-display-only?)
 => (bp :: false-or(<basic-bp>))
  let buffer = bp-buffer(bp);
  case
    n > 0 =>
      let last :: <basic-line> = bp-line(interval-end-bp(interval));
      block (return)
	for (i :: <integer> from 0 below n,
	     line = bp-line(bp)
	       then line-next-in-buffer(line, buffer, skip-test: skip-test))
	  when (~line | line == last)
	    return(fixup? & line-start(last))
	  end
	finally
	  if (line)
	    return(line-start(line))
	  else
	    return(fixup? & line-start(last))
	  end
	end
      end;
    n < 0 =>
      let first :: <basic-line> = bp-line(interval-start-bp(interval));
      block (return)
	for (i :: <integer> from 0 below -n,
	     line = bp-line(bp)
	       then line-previous-in-buffer(line, buffer, skip-test: skip-test))
	  when (~line | line == first)
	    return(fixup? & line-start(first))
	  end
	finally
	  if (line)
	    return(line-start(line))
	  else
	    return(fixup? & line-start(first))
	  end
	end
      end;
    otherwise =>
      copy-bp(bp);
  end
end method move-over-lines;


/// "Skipping" over things

define inline function forward-over
    (bp :: <basic-bp>, chars :: <sequence>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  forward-over!(copy-bp(bp), chars, fixup?: fixup?, interval: interval)
end function forward-over;

define sealed method forward-over!
    (bp :: <basic-bp>, chars :: <sequence>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  local method non-match? (char :: <byte-character>)
	  if (member?(char, chars)) values(#f, #f)
	  else values(#t, #t) end
	end method;
  move-forward-or-backward!(bp, non-match?, #f,
			    fixup?: fixup?, interval: interval)
end method forward-over!;

define sealed method forward-over!
    (bp :: <basic-bp>, chars :: <simple-object-vector>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  local method non-match? (char :: <byte-character>)
	  if (member?(char, chars)) values(#f, #f)
	  else values(#t, #t) end
	end method;
  move-forward-or-backward!(bp, non-match?, #f,
			    fixup?: fixup?, interval: interval)
end method forward-over!;


// If none of the chars are found, then
//  - if 'fixup?' is #t, the return value is the passed-in BP
//  - if 'fixup?' is #f, the return value is #f
define sealed method forward-until
    (bp :: <basic-bp>, chars :: <sequence>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  local method match? (char :: <byte-character>)
	  if (member?(char, chars)) values(#t, #t)
	  else values(#f, #f) end
	end method;
  let nbp = move-forward-or-backward!(copy-bp(bp), match?, #f,
				      fixup?: #f, interval: interval);
  nbp | (fixup? & bp)
end method forward-until;


define inline function backward-over
    (bp :: <basic-bp>, chars :: <sequence>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  backward-over!(copy-bp(bp), chars, fixup?: fixup?, interval: interval)
end function backward-over;

define sealed method backward-over!
    (bp :: <basic-bp>, chars :: <sequence>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  local method non-match? (ch :: <byte-character>)
	  if (member?(ch, chars)) values(#f, #f)
	  else values(#t, #t) end
	end method;
  move-forward-or-backward!(bp, non-match?, #t,
			    fixup?: fixup?, interval: interval)
end method backward-over!;

define sealed method backward-over!
    (bp :: <basic-bp>, chars :: <simple-object-vector>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  local method non-match? (ch :: <byte-character>)
	  if (member?(ch, chars)) values(#f, #f)
	  else values(#t, #t) end
	end method;
  move-forward-or-backward!(bp, non-match?, #t,
			    fixup?: fixup?, interval: interval)
end method backward-over!;


// If none of the chars are found, then
//  - if 'fixup?' is #t, the return value is the passed-in BP
//  - if 'fixup?' is #f, the return value is #f
define sealed method backward-until
    (bp :: <basic-bp>, chars :: <sequence>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  local method match? (ch :: <byte-character>)
	  if (member?(ch, chars)) values(#t, #t)
	  else values(#f, #f) end
	end method;
  let nbp = move-forward-or-backward!(copy-bp(bp), match?, #t,
				      fixup?: #f, interval: interval);
  nbp | (fixup? & bp)
end method backward-until;


/// Some other useful region-oriented stuff

define method definition-interval
    (bp :: <basic-bp>) => (interval :: false-or(<basic-interval>))
  do-definition-interval(buffer-major-mode(bp-buffer(bp)), bp)
end method definition-interval;

define method relevant-function-interval
    (bp :: <basic-bp>) => (interval :: false-or(<basic-interval>))
  do-relevant-function-interval(buffer-major-mode(bp-buffer(bp)), bp)
end method relevant-function-interval;


// Returns a pair of new BPs that delimit the atom
define method atom-under-bp
    (bp :: <basic-bp>) => (sbp :: <basic-bp>, ebp :: <basic-bp>)
  do-atom-under-bp(buffer-major-mode(bp-buffer(bp)), bp)
end method atom-under-bp;

define method do-atom-under-bp
    (mode :: <major-mode>, bp :: <basic-bp>)
 => (sbp :: <basic-bp>, ebp :: <basic-bp>)
  let node = bp-node(bp) | bp-buffer(bp);
  let sbp  = if (atom-syntax(bp-character-before(bp)) == $atom-delimiter)
	       forward-over(bp, #[' ', '\t', '\f'], interval: node)
	     else
	       move-over-atoms(bp, -1, interval: node)
	     end;
  let ebp  = move-over-atoms(sbp, 1, interval: node);
  values(sbp, ebp)
end method do-atom-under-bp;

define method select-atom-under-bp
    (window :: <basic-window>, bp :: <basic-bp>) => ()
  when (text-line?(bp-line(bp)))
    let (sbp, ebp) = atom-under-bp(bp);
    move-mark!(sbp, window: window);
    move-point!(ebp, window: window);
    queue-redisplay(window, $display-point)
  end
end method select-atom-under-bp;


/// Indices to BP's and vice-versa

define method char-index->bp
    (buffer :: <basic-buffer>, index :: <integer>,
     #key skip-test = line-for-display-only?)
 => (bp :: false-or(<basic-bp>))
  assert(index >= 0,
	 "The character index must be non-negative");
  block (return)
    do-lines(method (line, si, ei, last?)
	       ignore(si, ei, last?);
	       if (line-length(line) + 1 > index)
		 return(make(<simple-bp>, line: line, index: index))
	       else
		 // '+ 1' to include the '\n' character...
		 dec!(index, line-length(line) + 1)
	       end
	     end method, buffer, skip-test: skip-test);
    #f
  end
end method char-index->bp;

define method bp->char-index
    (bp :: <basic-bp>,
     #key skip-test = line-for-display-only?)
 => (index :: <integer>)
  let buffer   = bp-buffer(bp);
  let interval = make-interval(interval-start-bp(buffer), bp, in-order?: #t);
  count-characters(interval, skip-test: skip-test)
end method bp->char-index;

// Note that this can hack both buffers and sections
define method line-index->line
    (buffer :: type-union(<basic-buffer>, <basic-section>), index :: <integer>,
     #key skip-test = line-for-display-only?)
 => (line :: false-or(<basic-line>))
  assert(index >= 0,
	 "The line index must be non-negative");
  block (return)
    do-lines(method (line, si, ei, last?)
	       ignore(si, ei, last?);
	       when (index = 0)
		 return(line)
	       end;
	       dec!(index)
	     end method, buffer, skip-test: skip-test);
    #f
  end
end method line-index->line;

define method line->line-index
    (buffer :: <basic-buffer>, line :: <basic-line>,
     #key skip-test = line-for-display-only?)
 => (index :: <integer>)
  let interval = make-interval(interval-start-bp(buffer), line-start(line),
			       in-order?: #t);
  count-lines(interval, skip-test: skip-test) - 1
end method line->line-index;

define method line->line-index
    (section :: <basic-section>, line :: <basic-line>,
     #key skip-test = line-for-display-only?)
 => (index :: <integer>)
  let interval = make-interval(line-start(section-start-line(section)), line-start(line),
			       in-order?: #t);
  count-lines(interval, skip-test: skip-test) - 1
end method line->line-index;

// Note that this can hack both buffers and sections
define method line-index->bp
    (buffer :: type-union(<basic-buffer>, <basic-section>), index :: <integer>,
     #key skip-test = line-for-display-only?)
 => (bp :: false-or(<basic-bp>))
  let line = line-index->line(buffer, index, skip-test: skip-test);
  line & line-start(line)
end method line-index->bp;

define method bp->line-index
    (bp :: <basic-bp>,
     #key skip-test = line-for-display-only?)
 => (index :: <integer>)
  let buffer   = bp-buffer(bp);
  let interval = make-interval(interval-start-bp(buffer), bp,
			       in-order?: #t);
  count-lines(interval, skip-test: skip-test) - 1
end method bp->line-index;
