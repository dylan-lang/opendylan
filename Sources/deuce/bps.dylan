Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BPs, that is, buffer pointers 

define protocol <<bp>> ()
  getter bp-line
    (bp :: <bp>) => (line :: <line>);
  setter bp-line-setter
    (line :: <line>, bp :: <bp>) => (line :: <line>);
  getter bp-index
    (bp :: <bp>) => (index :: <integer>);
  setter bp-index-setter
    (index :: <integer>, bp :: <bp>) => (index :: <integer>);
  getter bp-buffer
    (bp :: <bp>) => (buffer :: <buffer>);
  setter bp-buffer-setter
    (buffer :: <buffer>, bp :: <bp>) => (buffer :: <buffer>);
  getter bp-character
    (bp :: <bp>) => (char :: <character>);
  setter bp-character-setter
    (char :: <character>, bp :: <bp>) => (char :: <character>);
  function simple-bp?
    (bp :: <bp>) => (simple? :: <boolean>);
  function moving-bp?
    (bp :: <bp>) => (moving? :: <boolean>);
  function bp-less?
    (bp1 :: <bp>, bp2 :: <bp>) => (less? :: <boolean>);
  function bp-greater?
    (bp1 :: <bp>, bp2 :: <bp>) => (greater? :: <boolean>);
  function copy-bp
     (bp :: <bp>) => (new-bp :: <bp>);
  function move-bp!
    (bp :: <bp>, line :: <line>, index :: <integer>) => (bp :: <bp>);
  function kill-bp!
    (bp :: <bp>) => ();
end protocol <<bp>>;

// A BP ("buffer pointer") points to a line and an index in the line.
// "Moving" BPs will get this index updated as insertions and deletions
// take place within the same line.
define abstract primary class <basic-bp> (<bp>)		// _not_ open!
  sealed slot bp-line :: <basic-line>,
    required-init-keyword: line:;
  sealed slot bp-index :: <integer>,
    required-init-keyword: index:;
end class <basic-bp>;

define sealed inline method make
    (class == <bp>, #rest initargs, #key buffer = #f, moving? = #f, #all-keys)
 => (bp :: <basic-bp>)
  let class = if (buffer)
		if (moving?) <permanent-moving-bp> else <permanent-bp> end
	      else
		if (moving?) <simple-moving-bp> else <simple-bp> end
	      end;
  apply(make, class, initargs)
end method make;

// Simple, error-checking interface to create a <simple-bp>
define inline function make-bp
    (line :: <basic-line>, index :: <integer>) => (bp :: <simple-bp>)
  assert(index >= 0 & index <= line-length(line),
	 "Index %d is out of range for line %= in 'make-bp'", index, line);
  make(<simple-bp>, line: line, index: index)
end function make-bp;

define sealed method \=
    (bp1 :: <basic-bp>, bp2 :: <basic-bp>) => (equal? :: <boolean>)
  bp-line(bp1) == bp-line(bp2)
  & bp-index(bp1) = bp-index(bp2)
  & bp-buffer(bp1) == bp-buffer(bp2)
end method \=;

define sealed method bp-less?
    (bp1 :: <basic-bp>, bp2 :: <basic-bp>) => (less? :: <boolean>)
  assert(bp-buffer(bp1) == bp-buffer(bp2),
	 "'bp-less?' can't compare BPs from two different buffers: %= and %=", bp1, bp2);
  let line1 = bp-line(bp1);
  let line2 = bp-line(bp2);
  if (line1 == line2)
    bp-index(bp1) < bp-index(bp2)
  else
    line-less?(bp-buffer(bp1), line1, line2)
  end
end method bp-less?;

define sealed method bp-greater?
    (bp1 :: <basic-bp>, bp2 :: <basic-bp>) => (greater? :: <boolean>)
  assert(bp-buffer(bp1) == bp-buffer(bp2),
	 "'bp-greater?' can't compare BPs from two different buffers: %= and %=", bp1, bp2);
  let line1 = bp-line(bp1);
  let line2 = bp-line(bp2);
  if (line1 == line2)
    bp-index(bp1) > bp-index(bp2)
  else
    ~line-less?(bp-buffer(bp1), line1, line2)
  end
end method bp-greater?;

define inline function order-bps
    (bp1 :: <basic-bp>, bp2 :: <basic-bp>) => (sbp :: <basic-bp>, ebp :: <basic-bp>)
  if (bp1 = bp2 | bp-less?(bp1, bp2)) values(bp1, bp2)
  else values(bp2, bp1) end
end function order-bps;


// Moving BPs get their line and index updated as insertions and
// deletions occur
define open abstract class <moving-bp-mixin> (<bp>)
end class <moving-bp-mixin>;

define method initialize
    (bp :: <moving-bp-mixin>, #key) => ()
  next-method();
  // This line needs to keep track of the new BP
  push!(line-bps(bp-line(bp)), bp)
end method initialize;

define sealed inline method moving-bp?
    (bp :: <basic-bp>) => (moving? :: singleton(#f))
  #f
end method moving-bp?;

define sealed inline method moving-bp?
    (bp :: <moving-bp-mixin>) => (moving? :: singleton(#t))
  #t
end method moving-bp?;


// A simple BP is associated with the "current" buffer, that is, when
// you call 'bp-buffer' on a simple BP, you get the value of *buffer*
define sealed class <simple-bp> (<basic-bp>)
end class <simple-bp>;

define sealed class <simple-moving-bp> (<moving-bp-mixin>, <simple-bp>)
end class <simple-moving-bp>;

define sealed inline method bp-buffer
    (bp :: <simple-bp>) => (buffer :: <basic-buffer>)
  *buffer*
end method bp-buffer;

define sealed method copy-bp
    (bp :: <simple-bp>) => (new-bp :: <simple-bp>)
  make(<simple-bp>,
       line: bp-line(bp), index: bp-index(bp),
       moving?: moving-bp?(bp))
end method copy-bp;

define sealed inline method simple-bp?
    (bp :: <simple-bp>) => (simple? :: singleton(#t))
  #t
end method simple-bp?;


// A permanent BP is associated with a given buffer
define sealed class <permanent-bp> (<basic-bp>)
  sealed slot bp-buffer :: <basic-buffer>,
    required-init-keyword: buffer:;
end class <permanent-bp>;

define sealed class <permanent-moving-bp> (<moving-bp-mixin>, <permanent-bp>)
end class <permanent-moving-bp>;

define sealed method copy-bp
    (bp :: <permanent-bp>) => (new-bp :: <permanent-bp>)
  make(<permanent-bp>,
       line: bp-line(bp), index: bp-index(bp), buffer: bp-buffer(bp),
       moving?: moving-bp?(bp))
end method copy-bp;

define sealed inline method simple-bp?
    (bp :: <permanent-bp>) => (simple? :: singleton(#f))
  #f
end method simple-bp?;

define sealed method as
    (class :: subclass(<permanent-bp>), bp :: <simple-bp>)
 => (bp :: <permanent-bp>)
  make(<permanent-bp>,
       line: bp-line(bp), index: bp-index(bp), buffer: bp-buffer(bp),
       moving?: moving-bp?(bp))
end method as;

define sealed method as
    (class :: subclass(<permanent-bp>), bp :: <permanent-bp>)
 => (bp :: <permanent-bp>)
  bp
end method as;


/// Seal the domains on BPs

define sealed domain make (singleton(<simple-bp>));
define sealed domain initialize (<simple-bp>);

define sealed domain make (singleton(<simple-moving-bp>));
define sealed domain initialize (<simple-moving-bp>);

define sealed domain make (singleton(<permanent-bp>));
define sealed domain initialize (<permanent-bp>);

define sealed domain make (singleton(<permanent-moving-bp>));
define sealed domain initialize (<permanent-moving-bp>);


/// BP motion

define sealed method move-bp!
    (bp :: <basic-bp>, line :: <basic-line>, index :: <integer>)
 => (bp :: <basic-bp>)
  let old-line = bp-line(bp);
  assert(index >= 0 & index <= line-length(line),
	 "Index %d is out of range for line %= in 'move-bp!'", index, line);
  if (line == old-line)
    bp-index(bp) := index
  else
    when (moving-bp?(bp))
      // It's a moving BP, need to update BP relocation list
      line-bps(old-line) := remove!(line-bps(old-line), bp);
      push!(line-bps(line), bp)
    end;
    bp-line(bp)  := line;
    bp-index(bp) := index
  end;
  bp
end method move-bp!;

// Ensure no line is hanging onto this BP
define sealed method kill-bp!
    (bp :: <basic-bp>) => ()
  when (moving-bp?(bp))
    let line = bp-line(bp);
    line-bps(line) := remove!(line-bps(line), bp)
  end
end method kill-bp!;


// This "works" on diagram lines due to the default method for 'line-contents'
define sealed method bp-character
    (bp :: <basic-bp>) => (char :: <byte-character>)
  let line  = bp-line(bp);
  let index = bp-index(bp);
  if (index = line-length(line))
    '\n'
  else
    line-contents(line)[index]
  end
end method bp-character;

// NB: You can't insert '\n' this way!
// Also note that the caller is responsible for calling 'note-line-changed'
define sealed method bp-character-setter
    (char :: <byte-character>, bp :: <basic-bp>)
 => (char :: <byte-character>)
  assert(char ~== '\n',
	 "'bp-character-setter' can't insert newline characters");
  let line  = bp-line(bp);
  let index = bp-index(bp);
  line-contents(line)[index] := char
end method bp-character-setter;


define sealed method bp-character-before
    (bp :: <basic-bp>) => (char :: <byte-character>)
  let line  = bp-line(bp);
  let index = bp-index(bp);
  if (index = 0)
    '\n'
  else
    line-contents(line)[index - 1]
  end
end method bp-character-before;

define sealed method bp-character-after
    (bp :: <basic-bp>) => (char :: <byte-character>)
  let line  = bp-line(bp);
  let index = bp-index(bp);
  if (index = line-length(line) - 1)
    '\n'
  else
    line-contents(line)[index + 1]
  end
end method bp-character-after;


// Intended only to compare against strings within a single line.
// So don't bother comparing against strings with '\n' in them...
define sealed method bp-looking-at?
    (bp :: <basic-bp>, string :: <byte-string>) => (match? :: <boolean>)
  let line  = bp-line(bp);
  let index = bp-index(bp);
  text-line?(line)
  & string-equal?(line-contents(line), string,
		  start1: index, end1: min(index + size(string), line-length(line)))
end method bp-looking-at?;

define sealed method bp-looking-at-word?
    (bp :: <basic-bp>, string :: <byte-string>) => (match? :: <boolean>)
  let line  = bp-line(bp);
  let index = bp-index(bp);
  when (text-line?(line))
    let contents = line-contents(line);
    let _end     = min(index + size(string), line-length(line));
    string-equal?(contents, string, start1: index, end1: _end)
    & (_end >= line-length(line)
       | word-syntax(contents[_end]) ~= $word-alphabetic)
  end
end method bp-looking-at-word?;

define sealed method bp-looking-at-atom?
    (bp :: <basic-bp>, string :: <byte-string>) => (match? :: <boolean>)
  let line  = bp-line(bp);
  let index = bp-index(bp);
  when (text-line?(line))
    let contents = line-contents(line);
    let _end     = min(index + size(string), line-length(line));
    string-equal?(contents, string, start1: index, end1: _end)
    & (_end >= line-length(line)
       | atom-syntax(contents[_end]) ~= $atom-alphabetic)
  end
end method bp-looking-at-atom?;
