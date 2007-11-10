Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Intervals

// An interval is just some range within a buffer.  Note that the start BP
// is inclusive, but the end BP is _exclusive_.
define protocol <<interval>> ()
  getter interval-start-bp
    (interval :: <interval>) => (bp :: false-or(<bp>));
  setter interval-start-bp-setter
    (bp :: false-or(<bp>), interval :: <interval>) => (bp :: false-or(<bp>));
  getter interval-end-bp
    (interval :: <interval>) => (bp :: false-or(<bp>));
  setter interval-end-bp-setter
    (bp :: false-or(<bp>), interval :: <interval>) => (bp :: false-or(<bp>));
  getter interval-buffer
    (interval :: <interval>) => (buffer :: <buffer>);
  function copy-interval
    (interval :: <interval>, #key skip-test) => (new-interval :: <interval>);
  getter interval-read-only?
    (interval :: <interval>) => (read-only? :: <boolean>);
  setter interval-read-only?-setter
    (read-only? :: <boolean>, interval :: <interval>) => (read-only? :: <boolean>);
  // Applies the function to all the lines in the buffer/node/section/interval
  function do-lines
    (function :: <function>, interval :: type-union(<interval>, <section>),
     #key from-end?, skip-test) => ();
  // Applies the function to all the characters in the line/interval
  function do-characters
    (function :: <function>, interval :: type-union(<line>, <interval>, <section>),
     #key start: _start, end: _end, from-end?, skip-test) => ();
  function count-lines
    (interval :: type-union(<string>, <interval>, <section>), #key skip-test)
 => (nlines :: <integer>);
  function count-characters
    (interval :: type-union(<string>, <interval>, <section>), #key skip-test)
 => (nchars :: <integer>);
end protocol <<interval>>;


// An interval ranges from its start BP to its end BP, inclusive
define open abstract class <basic-interval> (<interval>)
  sealed slot interval-start-bp :: <basic-bp>,
    required-init-keyword: start-bp:;
  sealed slot interval-end-bp :: <basic-bp>,
    required-init-keyword: end-bp:;
end class <basic-interval>;

define sealed class <simple-interval> (<basic-interval>)
end class <simple-interval>;

define sealed domain make (singleton(<simple-interval>));
define sealed domain initialize (<simple-interval>);

define sealed method make
    (class == <interval>, #rest initargs,
     #key start-bp, end-bp, in-order? = #f)
 => (bp :: <simple-interval>)
  if (in-order?)
    // If he said the BPs are already in order, believe him
    make(<simple-interval>,
         start-bp: copy-bp(start-bp), end-bp: copy-bp(end-bp))
  else
    let (sbp, ebp) = order-bps(start-bp, end-bp);
    make(<simple-interval>,
	 start-bp: copy-bp(sbp), end-bp: copy-bp(ebp))
  end
end method make;

// Simple interface to create a <simple-interval>
define inline function make-interval
    (start-bp :: <basic-bp>, end-bp :: <basic-bp>, #key in-order? = #f)
 => (interval :: <simple-interval>)
  assert(bp-buffer(start-bp) == bp-buffer(end-bp),
	 "The BPs %= and %= passed to 'make-interval' are from different buffers",
	 start-bp, end-bp);
  make(<interval>, start-bp: start-bp, end-bp: end-bp, in-order?: in-order?)
end function make-interval;

define sealed inline method interval-buffer
    (interval :: <basic-interval>) => (buffer :: <basic-buffer>)
  bp-buffer(interval-start-bp(interval))
end method interval-buffer;

// Copies the interval and all of the lines in the interval
//--- This presently "flattens" the interval as it copies it, but we
//--- also need a version that preserves node and section structure when
//--- the interval exactly covers a (possibly nested) set of nodes
define method copy-interval
    (interval :: <basic-interval>,
     #key skip-test = structural-diagram-line?)
 => (new-interval :: <basic-interval>)
  let first-line :: false-or(<basic-line>) = #f;
  let last-line  :: false-or(<basic-line>) = #f;
  local method copy (line :: <line>, si :: <integer>, ei :: <integer>, last?);
	  ignore(last?);
	  let line :: <basic-line> = copy-line(line, start: si, end: ei);
	  unless (first-line)
	    first-line := line
	  end;
	  line-previous(line) := last-line;
	  when (last-line)
	    line-next(last-line) := line
	  end;
	  last-line := line;
	  line
	end method;
  do-lines(copy, interval, skip-test: skip-test);
  make-interval(line-start(first-line), line-end(last-line), in-order?: #t)
end method copy-interval;


// Calls the function with four arguments: the line, the initial index
// and final index in the line, and a boolean value that is #t iff it's
// the last line in the interval
define method do-lines
    (function :: <function>, interval :: <basic-interval>,
     #key from-end? = #f, skip-test = line-for-display-only?) => ()
  let start-bp :: <basic-bp> = interval-start-bp(interval);
  let end-bp   :: <basic-bp> = interval-end-bp(interval);
  let (start-line, start-index, end-line, end-index, step :: <function>)
    = if (from-end?)
	values(bp-line(end-bp),   bp-index(end-bp),
	       bp-line(start-bp), bp-index(start-bp),
	       line-previous-in-buffer)
      else
	values(bp-line(start-bp), bp-index(start-bp),
	       bp-line(end-bp),   bp-index(end-bp),
	       line-next-in-buffer)
      end;
  let buffer = bp-buffer(start-bp);
  assert(buffer == bp-buffer(end-bp),
	 "The interval from %= to %= does not start and end in the same buffer",
	 start-bp, end-bp);
  block (break)
    for (line = start-line
	   then step(line, buffer, skip-test: #f))
      when (line & (~skip-test | ~skip-test(line)))
        let (si, ei)
	  = if (from-end?)
	      values(if (line == end-line)   end-index   else 0 end,
		     if (line == start-line) start-index else line-length(line) end)
            else
	      values(if (line == start-line) start-index else 0 end,
		     if (line == end-line)   end-index   else line-length(line) end)
	    end;
	function(line, si, ei, line == end-line)
      end;
      when (~line | line == end-line)
	break()
      end
    end
  end
end method do-lines;

define method count-lines
    (interval :: <basic-interval>, #key skip-test = line-for-display-only?)
 => (nlines :: <integer>)
  let n :: <integer> = 0;
  do-lines(method (line :: <line>, si, ei, last?)
	     ignore(line, si, ei, last?);
	     inc!(n)
	   end method, interval, skip-test: skip-test);
  n
end method count-lines;

define method count-lines
    (string :: <byte-string>, #key skip-test = line-for-display-only?)
 => (nlines :: <integer>)
  ignore(skip-test);
  count(string, '\n') + 1
end method count-lines;


define method do-characters
    (function :: <function>, interval :: <interval>,
     #key start: _start, end: _end, from-end? = #f,
	  skip-test = line-for-display-only?) => ()
  ignore(_start, _end);
  let start-line = bp-line(interval-start-bp(interval));
  let end-line   = bp-line(interval-end-bp(interval));
  local method do-chars (line :: <line>, si :: <integer>, ei :: <integer>, last?) => ()
	  ignore(last?);
	  local method maybe-do-newline () => ()
		  // Call function on '\n' if really in interval
		  let length = line-length(line);
		  when (select (line)
			  start-line => (si <= length) & (line ~== end-line | length < ei);
			  end-line   => (length < ei);
			  otherwise  => #t;
			end)
		    function('\n', line, length)
		  end
		end method;
	  when (from-end?) maybe-do-newline() end;
	  do-characters(function, line,
			start: si, end: ei, from-end?: from-end?);
	  unless (from-end?) maybe-do-newline() end;
	end method;
  do-lines(do-chars, interval, from-end?: from-end?, skip-test: skip-test)
end method do-characters;

define method count-characters
    (interval :: <interval>, #key skip-test = line-for-display-only?)
 => (nchars :: <integer>)
  let last-index = bp-index(interval-end-bp(interval));
  let n :: <integer> = 0;
  do-lines(method (line :: <line>, si :: <integer>, ei :: <integer>, last?)
	     if (~last? | last-index > line-length(line))
	       // '+ 1' accounts for the newline character...
	       inc!(n, ei - si + 1)
	     else
	       inc!(n, ei - si)
	     end
	   end method, interval, skip-test: skip-test);
  n
end method count-characters;

define method count-characters
    (string :: <byte-string>, #key skip-test = line-for-display-only?)
 => (nchars :: <integer>)
  ignore(skip-test);
  size(string)
end method count-characters;


// Note that this _does_ include a '\n' character at the end of each line
define method as
    (class :: subclass(<string>), interval :: <interval>)
 => (string :: <byte-string>)
  let string = make(<byte-string>, size: count-characters(interval));
  let i :: <integer> = 0;
  do-lines(method (line, si, ei, last?)
	     let n :: <integer> = ei - si;
	     // Use the fastest method available to copy the line contents
	     copy-bytes(string, i, line-contents(line), si, n);
	     inc!(i, n);
	     if (~last? | ei > line-length(line))
	       string[i] := '\n';
	       inc!(i)
	     end
	   end method, interval, skip-test: diagram-line?);
  string
end method as;


// If there are any read-only lines in the interval, just claim that
// the whole interval is read-only.  Subclasses of <interval> are, of
// course, free to change this rather blunt approximation
define method interval-read-only?
    (interval :: <basic-interval>) => (read-only? :: <boolean>)
  let buffer = interval-buffer(interval);
  buffer-read-only?(buffer)
  | block (return)
      local method read-only? (line :: <line>, si, ei, last?)
	      ignore(si, ei, last?);
	      when (line-read-only?(line))
		return(#t)
	      end
	    end method;
      do-lines(read-only?, interval);
      #f
    end
end method interval-read-only?;

define method interval-read-only?-setter
    (read-only? :: <boolean>, interval :: <basic-interval>) => (read-only? :: <boolean>)
  local method set-read-only (line :: <line>, si, ei, last?)
	  ignore(si, ei, last?);
	  line-read-only?(line) := read-only?
	end method;
  do-lines(set-read-only, interval);
  read-only?
end method interval-read-only?-setter;


define method bp-within-interval?
    (bp :: <basic-bp>, interval :: <interval>) => (within? :: <boolean>)
  block (return)
    let start-bp :: <basic-bp> = interval-start-bp(interval);
    let end-bp   :: <basic-bp> = interval-end-bp(interval);
    let _line  = bp-line(bp);
    let _index = bp-index(bp);
    let first-line = bp-line(start-bp);
    let last-line  = bp-line(end-bp);
    if (first-line == last-line)
      // Speed up the case of a single-line interval, since
      // if crops up a lot during drag-selection
      when (first-line == _line)
	let first-index = bp-index(start-bp);
	let last-index  = bp-index(end-bp);
	first-index <= _index & _index <= last-index
      end
    else
      local method within-interval? (line :: <line>, si, ei, last?)
	      ignore(last?);
	      when (_line == line)
		return(case
			 line == first-line => _index >= si;
			 line == last-line  => _index <= ei;
			 otherwise => #t
		       end)
	      end
	    end method;
      do-lines(within-interval?, interval, skip-test: #f);
      #f
    end
  end
end method bp-within-interval?;


/// Nodes

define protocol <<node>> (<<interval>>)
  getter node-next
    (node :: <node>) => (next :: false-or(<node>));
  setter node-next-setter
    (next :: false-or(<node>), node :: <node>) => (next :: false-or(<node>));
  getter node-previous
    (node :: <node>) => (previous :: false-or(<node>));
  setter node-previous-setter
    (previous :: false-or(<node>), node :: <node>) => (previous :: false-or(<node>));
  getter node-parent
    (node :: <node>) => (parent :: false-or(<node>));
  setter node-parent-setter
    (parent :: false-or(<node>), node :: <node>) => (parent :: false-or(<node>));
  getter node-children
    (node :: <node>) => (children :: <sequence>);
  setter node-children-setter
    (children :: <sequence>, node :: <node>) => (children :: <sequence>);
  getter node-buffer
    (node :: <node>) => (buffer :: false-or(<buffer>));
  setter node-buffer-setter
    (buffer :: false-or(<buffer>), node :: <node>) => (buffer :: false-or(<buffer>));
  getter node-lock
    (node :: <node>) => (lock :: false-or(<exclusive-lock>));
  function note-node-changed
    (node :: <node>) => ();
  // For section nodes
  getter node-section
    (node :: <node>) => (section :: false-or(<section>));
  setter node-section-setter
    (section :: <section>, node :: <node>) => (section :: <section>);
  getter node-definition-signature
    (node :: <node>) => (signature);
  getter node-definition-name
    (node :: <node>) => (name :: false-or(<string>));
  getter node-definition-type
    (node :: <node>) => (type :: false-or(<symbol>));
end protocol <<node>>;

// Buffers are composed of a linked list of nodes.  Some of the nodes
// are likely to be <section-nodes> which contain <section> data.
define open abstract primary class <basic-node> (<basic-interval>, <node>)
  sealed slot node-next :: false-or(<basic-node>) = #f,
    init-keyword: next:;
  sealed slot node-previous :: false-or(<basic-node>) = #f,
    init-keyword: previous:;
  // Some buffers have their nodes in a tree structure
  //--- Finish implementing the the parent/children stuff (e.g., 'do-lines')
  sealed slot node-parent :: false-or(<basic-node>) = #f,
    init-keyword: parent:;
  sealed slot node-children :: <list> = #(),
    init-keyword: children:;
  sealed slot node-buffer :: false-or(<basic-buffer>) = #f,
    init-keyword: buffer:;
  sealed constant slot node-lock :: false-or(<exclusive-lock>) = #f,
    init-keyword: lock:;
end class <basic-node>;

define method note-node-changed
    (node :: <basic-node>) => ()
  let buffer = node-buffer(node);
  when (buffer)
    note-buffer-changed(buffer)
  end
end method note-node-changed;

define method do-lines
    (function :: <function>, node :: <basic-node>,
     #key from-end? = #f, skip-test) => ()
  ignore(from-end?, skip-test);
  error("There is no 'do-lines' method for the node %=", node)
end method do-lines;

define method count-lines
    (node :: <basic-node>, #key skip-test) => (nlines :: <integer>)
  ignore(skip-test);
  error("There is no 'count-lines' method for the node %=", node)
end method count-lines;


/// Section nodes

// A section node is a node that contains a single section.  For example,
// file buffers consist of a sequence of section nodes, and each section
// node contains one section of the file.
define open abstract class <section-node> (<basic-node>)
  sealed slot node-section :: <basic-section>,
    required-init-keyword: section:;
end class <section-node>;

define sealed inline method make
    (class == <section-node>, #rest initargs, #key, #all-keys)
 => (node :: <simple-section-node>)
  apply(make, <simple-section-node>, initargs)
end method make;

// Non-section nodes just return #f for this...
define method node-section
    (node :: <basic-node>) => (section :: singleton(#f))
  #f
end method node-section;


define sealed class <simple-section-node> (<section-node>)
end class <simple-section-node>;

define sealed domain make (singleton(<simple-section-node>));
define sealed domain initialize (<simple-section-node>);


define method make-section-node
    (buffer :: <buffer>, section :: <section>,
     #key node-class = <section-node>)
 => (node :: <node>)
  let start-bp
    = make(<bp>,
	   line: section-start-line(section), index: 0,
	   buffer: buffer);
  let end-bp
    = make(<bp>,
	   line: section-end-line(section), index: line-length(section-end-line(section)),
	   buffer: buffer,
	   moving?: #t);
  // Note that we don't associate the node with the buffer yet,
  // since it will probably get added with 'add-node!' later
  let node = make(node-class,
		  start-bp: start-bp, end-bp: end-bp,
		  section:  section);
  push!(section-nodes(section), node);
  node
end method make-section-node;

define method make-empty-section-node
    (buffer :: <buffer>,
     #key section-class = <section>,
	  node-class    = <section-node>)
 => (node :: <node>)
  let section = make-empty-section(section-class: section-class);
  make-section-node(buffer, section, node-class: node-class)
end method make-empty-section-node;


// A header node is a special node reserved for the header of a source
// file in some language.  Language-specific major modes are intended
// to provide a concrete class for this.
define open abstract class <header-node> (<section-node>)
end class <header-node>;

// Section nodes use the lock for the section itself
define sealed inline method node-lock
    (node :: <section-node>) => (lock :: false-or(<exclusive-lock>))
  section-lock(node-section(node))
end method node-lock;

define method do-lines
    (function :: <function>, node :: <section-node>,
     #key from-end? = #f, skip-test = line-for-display-only?) => ()
  do-lines(function, node-section(node), from-end?: from-end?, skip-test: skip-test)
end method do-lines;

define method count-lines
    (node :: <section-node>, #key skip-test = line-for-display-only?)
 => (nlines :: <integer>)
  count-lines(node-section(node), skip-test: skip-test)
end method count-lines;


/// Definition nodes

// A definition node is a special kind of section node that has some
// language-specific information associated with it.  Language-specific
// major modes are intended to provide a concrete class for this.
define open abstract class <definition-node> (<section-node>)
end class <definition-node>;

define method node-definition-signature
    (node :: <node>) => (signature)
  #f
end method node-definition-signature;

// The default method asks the section what its signature is
define method node-definition-signature
    (node :: <definition-node>) => (signature)
  section-definition-signature(node-section(node))
end method node-definition-signature;


define method node-definition-name
    (node :: <node>) => (name :: false-or(<string>))
  #f
end method node-definition-name;

// The default method asks the section what its name is
define method node-definition-name
    (node :: <definition-node>) => (name :: false-or(<string>))
  section-definition-name(node-section(node))
end method node-definition-name;


define method node-definition-type
    (node :: <node>) => (type :: false-or(<symbol>))
  #f
end method node-definition-type;

define method node-definition-type
    (node :: <definition-node>) => (type :: false-or(<symbol>))
  section-definition-type(node-section(node))
end method node-definition-type;


define sealed inline method make
    (class == <definition-node>, #rest initargs, #key, #all-keys)
 => (node :: <simple-definition-node>)
  apply(make, <simple-definition-node>, initargs)
end method make;

define sealed class <simple-definition-node> (<definition-node>)
end class <simple-definition-node>;

define sealed domain make (singleton(<simple-definition-node>));
define sealed domain initialize (<simple-definition-node>);
