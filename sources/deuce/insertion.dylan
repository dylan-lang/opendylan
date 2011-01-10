Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Insertion primitives

/// Note that the insertion primitives don't check for read-only-ness.
/// That is assumed to have been done at a higher level.

/// Note that only 'insert!' does anything with the change history,
/// and the more primitive helper functions do not

// Inserts the object after BP, and returns the BP that points after
// the end of the inserted object
define generic insert!
    (bp :: <bp>, object, #key) => (new-bp :: <bp>);

define sealed method insert!
    (bp :: <basic-bp>, char :: <byte-character>, #key)
 => (new-bp :: <basic-bp>)
  let bp = insert-into-line(bp-line(bp), bp-index(bp), char);
  // Finalize the change record after we do the insertion
  when (*change-record*)
    extend-insertion-record(*change-record*, end-bp: bp)
  end;
  bp
end method insert!;

define sealed method insert!
    (bp :: <basic-bp>, string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (new-bp :: <basic-bp>)
  let bp = insert-into-line(bp-line(bp), bp-index(bp), string,
			    start: _start, end: _end);
  when (*change-record*)
    extend-insertion-record(*change-record*, end-bp: bp)
  end;
  bp
end method insert!;

// Insert a whole interval at the given BP.  This comes up when we
// yank stuff back from the kill ring.
define sealed method insert!
    (bp :: <basic-bp>, interval :: <basic-interval>, #key)
 => (new-bp :: <basic-bp>)
  let first :: <basic-bp> = interval-start-bp(interval);
  let last  :: <basic-bp> = interval-end-bp(interval);
  if (bp-line(first) == bp-line(last))
    // Single-line interval, just insert it as a string
    insert!(bp, as(<byte-string>, interval))
  else
    // Otherwise split up the line we're inserting into
    let line  = bp-line(bp);
    let index = bp-index(bp);
    let new   = split-for-insertion(line, index);
    // Copy the text of the first and last lines of the interval...
    let first-line = bp-line(first);
    let last-line  = bp-line(last);
    insert!(line-end(line), as(<byte-string>, first-line));
    let ebp = insert!(line-start(new), as(<byte-string>, last-line));
    // Then copy the rest of the interval in as lines
    unless (line-next(first-line) == last-line)
      let section = line-section(line);
      let prev = line;
      for (line = line-next(first-line) then line-next(line),
	   until: line == last-line)
	let line = copy-line(line);
	add-line!(section, line, after: prev);
	prev := line
      end
    end;
    when (*change-record*)
      extend-insertion-record(*change-record*, end-bp: ebp)
    end;
    ebp
  end
end method insert!;

// Split the line at the given index, and return the new line.
// The old line contains the line contents up to the index, and
// the new line contains the old line contents after the index.
define sealed method split-for-insertion 
    (line :: <text-line>, index :: <integer>) => (new-line :: <text-line>)
  let length   = line-length(line);
  let contents = line-contents(line);
  let section  = line-section(line);
  assert(index >= 0 & index <= length,
	 "Index %d is out of range for line %=", index, line);
  // Make a new line with the contents of the old line _after_ the index
  let new-length = length - index;
  let new-line   = make(object-class(line),
			length:   new-length,
			contents: copy-sequence(contents, start: index, end: length));
  // Truncate the new line to contain the contents up to the new '\n'
  line-length(line) := index;
  // Add the new line
  add-line!(section, new-line, after: line);
  // Relocate moving BPs.  Note that 'move-bp!' can bash the contents
  // of 'line-bps', so we need to copy it first
  for (bp :: <basic-bp> in copy-sequence(line-bps(line)))
    let i = bp-index(bp);
    when (i >= index)
      move-bp!(bp, new-line, min(i - index, new-length))
    end
  end;
  new-line
end method split-for-insertion;


define sealed method insert-into-line
    (line :: <text-line>, index :: <integer>, char :: <byte-character>, #key)
 => (bp :: <basic-bp>)
  if (char = '\n')
    let new = split-for-insertion(line, index);
    line-start(new);
  else
    let length = line-length(line);
    assert(index >= 0 & index <= length,
	   "Index %d is out of range for line %=", index, line);
    // Grow the line first, since it might clobber the contents
    line-length(line) := length + 1;
    let contents = line-contents(line);
    insert-at!(contents, char, index);
    // Relocate moving BPs
    for (bp :: <basic-bp> in line-bps(line))
      let i = bp-index(bp);
      when (i >= index)
	bp-index(bp) := i + 1
      end
    end;
    make(<simple-bp>, line: line, index: index + 1)
  end
end method insert-into-line;

define sealed method insert-into-line
    (line :: <text-line>, index :: <integer>, string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (bp :: <basic-bp>)
  let nl = position(string, '\n', start: _start, end: _end);
  if (~nl)
    // The insertion string has no newline character in it
    let string-length = _end - _start;
    if (string-length = 0)
      // Nothing to insert, do nothing
      make(<simple-bp>, line: line, index: index)
    else
      // Make room for the insertion string within the line
      let length     :: <integer> = line-length(line);
      let new-length :: <integer> = length + string-length;
      assert(index >= 0 & index <= length,
	     "Index %d is out of range for line %=", index, line);
      line-length(line) := new-length;
      let contents = line-contents(line);
      without-bounds-checks
	for (i :: <integer> from length - 1 by -1,
	     j :: <integer> from new-length - 1 by -1,
	     until: i < index)
	  contents[j] := contents[i]
	end;
	// Copy in the new contents
	for (i :: <integer> from _start below _end,
	     j :: <integer> from index)
	  contents[j] := string[i]
	end
      end;
      // Relocate moving BPs
      for (bp :: <basic-bp> in line-bps(line))
	let i = bp-index(bp);
	when (i >= index)
	  bp-index(bp) := i + string-length
	end
      end;
      make(<simple-bp>, line: line, index: index + string-length)
    end
  else
    // The insertion string has at least one newline character in it
    //--- This could do with a more efficient implementation
    until (~nl)
      let nbp = insert-into-line(line, index, string,
				 start: _start, end: nl);
      when (nl < size(string) & string[nl] = '\n')
        insert-moving!(nbp, '\n')
      end;
      line  := bp-line(nbp);
      index := bp-index(nbp);
      _start := nl + 1;
      nl := (_start < _end)
            & (position(string, '\n', start: _start, end: _end) | _end)
    end;
    make(<simple-bp>, line: line, index: index)
  end
end method insert-into-line;


// By default, inserting into a diagram creates a new line and inserts there
define method insert-into-line
    (line :: <diagram-line>, index :: <integer>, char :: <byte-character>, #key)
 => (bp :: <basic-bp>)
  assert(index = 0,
	 "Diagram lines can only be inserted into at index 0");
  let section = line-section(line);
  let new = make(<text-line>, contents: "");
  add-line!(section, new, after: line-previous(line));
  let bp = line-start(new);
  if (char = '\n') bp
  else insert!(bp, char) end
end method insert-into-line;

define method insert-into-line
    (line :: <diagram-line>, index :: <integer>, string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (bp :: <basic-bp>)
  assert(index = 0,
	 "Diagram lines can only be inserted into at index 0");
  let section = line-section(line);
  let new = make(<text-line>, contents: "");
  add-line!(section, new, after: line-previous(line));
  let bp = line-start(new);
  insert!(bp, string, start: _start, end: _end)
end method insert-into-line;


define function insert-moving!
    (bp :: <basic-bp>, object,
     #rest keys, #key start: _start, end: _end)
 => (bp :: <basic-bp>)
  ignore(_start, _end);
  let new-bp = apply(insert!, bp, object, keys);
  move-bp!(bp, bp-line(new-bp), bp-index(new-bp))
end function insert-moving!;
