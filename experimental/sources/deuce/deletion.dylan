Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Deletion primitives

/// Note that the deletion primitives don't check for read-only-ness.
/// That is assumed to have been done at a higher level.

/// Note that only 'delete!' does anything with the change history,
/// and the more primitive helper functions do not

// Push the interval onto the kill ring, then delete it
define method kill!
    (interval :: <basic-interval>,
     #key merge? = #f, reverse? = #f, clipboard? = $unsupplied)
 => (bp :: false-or(<basic-bp>))
  when (*editor-frame*)
    let editor    = frame-editor(*editor-frame*);
    let kill-ring = editor-kill-history(editor);
    let new-elt   = add-to-kill-ring(kill-ring, interval,
				     merge?: merge?, reverse?: reverse?);
    when (clipboard? == #t
	  | (unsupplied?(clipboard?) & clipboard-policy(editor-policy(editor))))
      let window = frame-window(*editor-frame*);
      add-to-clipboard(window, new-elt)
    end
  end;
  delete!(interval)
end method kill!;


// Deletes the interval, returning the BP that points to the end of the interval
define generic delete!
    (interval :: <interval>) => (bp :: false-or(<bp>));

// This returns #f when it fails to delete across hard sections
// Note that this has an implicit assumption that no section appears
// more than once within a buffer
define sealed method delete!
    (interval :: <basic-interval>)
 => (bp :: false-or(<basic-bp>))
  let start-bp :: <basic-bp> = interval-start-bp(interval);
  let end-bp   :: <basic-bp> = interval-end-bp(interval);
  let start-node  = bp-node(start-bp);
  let end-node    = bp-node(end-bp);
  let start-line  = bp-line(start-bp);
  let end-line    = bp-line(end-bp);
  let start-index = bp-index(start-bp);
  let end-index   = bp-index(end-bp);
  assert(start-index >= 0 & start-index <= line-length(start-line),
	 "Start index %d is out of range for line %=", start-index, start-line);
  assert(end-index >= 0 & end-index <= line-length(end-line),
	 "End index %d is out of range for line %=", end-index, end-line);
  // Finalize the change record before we do the deletion
  when (*change-record*)
    extend-deletion-record(*change-record*, interval: interval)
  end;
  if (start-line == end-line)
    delete-within-line(start-line, start-index, end-index);
    start-bp
  else
    // Deleting across multiple lines, maybe even multiple sections
    let buffer        = bp-buffer(start-bp);
    let start-section = line-section(start-line);
    let end-section   = line-section(end-line);
    let resectionize? = (start-section ~== end-section);
    // Don't allow this deletion if the region spans sections
    // in a buffer that has hard sections
    unless (buffer-has-hard-sections?(buffer)
            & start-section ~== end-section)
      // First tell the major mode we're deleting multiple lines
      note-multi-line-deletion(buffer-major-mode(buffer), interval);
      //---*** Relocate moving BPs
      let following-line
	= delete-across-lines(start-line, start-index, end-line, end-index);
      when (start-section)
	let start-section :: <basic-section> = start-section;	// force tighter type...
	start-section.%n-lines := #f;
	// If we deleted into or over 'section-end-line(start-section)',
	// we need to fix it to point to the last line currently in the
        // section.  There are separate cases for "into" and "over" to
        // save having to do something like 'bp-within-interval?'.
	when (section-end-line(start-section) == end-line)
          section-end-line(start-section) := start-line
        end;
        when (end-section & start-section ~== end-section)
	  let end-section :: <basic-section> = end-section;	// force tighter type...
          end-section.%n-lines := #f;
          section-end-line(start-section) := start-line;	// note duplicate line
          section-start-line(end-section) := following-line;
          unless (following-line)
            section-end-line(end-section) := #f
          end
        end
      end;
      // Clean up nodes and sections that got emptied dy the deletion.
      // First pull all remaining lines into the first section...
      when (start-section & end-section & start-section ~== end-section)
	merge-sections(start-section, end-section)
      end;
      // ...then fix up the first node's start and end BPs
      let (first-line, last-line)
	= if (start-section) values(section-start-line(start-section),
				    section-end-line(start-section))
	  else values(start-node & bp-line(interval-start-bp(start-node)),
		      end-node   & bp-line(interval-end-bp(end-node)))
	  end;
      when (start-node)
	move-bp!(interval-start-bp(start-node), first-line, 0);
	move-bp!(interval-end-bp(start-node),   last-line, line-length(last-line));
	when (end-node)
	  // ...and get rid of the empty sections and nodes at the end
	  when (start-node ~== end-node)
	    remove-empty-nodes(start-node, end-node)
	  end
	end
      end;
      when (resectionize?)
	resectionize-section(start-section)
      end;
      update-buffer-line-count(buffer, start-node);
      start-bp
    end
  end
end method delete!;


// Deletion within a single line
define method delete-within-line
    (line :: <text-line>, start-index :: <integer>, end-index :: <integer>) => ()
  // First relocate moving BPs
  for (bp :: <basic-bp> in line-bps(line))
    let i = bp-index(bp);
    when (i > start-index)
      if (i <= end-index) bp-index(bp) := start-index
      else bp-index(bp) := i - (end-index - start-index) end
    end
  end;
  // Then slide the contents of the line down and adjust its length
  let length   = line-length(line);
  let contents = line-contents(line);
  without-bounds-checks
    for (i :: <integer> from start-index,
	 j :: <integer> from end-index below length)
      contents[i] := contents[j]
    end
  end;
  line-length(line) := length - (end-index - start-index)
end method delete-within-line;

// Default method on diagram lines is a no-op
define method delete-within-line
    (line :: <diagram-line>, start-index :: <integer>, end-index :: <integer>) => ()
  #f
end method delete-within-line;


// Deletion across multiple lines
define method delete-across-lines
    (start-line :: <text-line>,  start-index :: <integer>,
     end-line   :: <basic-line>, end-index   :: <integer>)
 => (following-line :: false-or(<basic-line>))
  // Delete the chaff from the end of the start line
  let new-length = start-index;
  line-length(start-line) := new-length;
  // Copy the remainder of the end line into the start line
  insert-into-line(start-line, new-length, as(<byte-string>, end-line),
		   start: end-index);
  // Unlink the intervening lines, including the original end-line
  let following-line = line-next(end-line);
  line-next(start-line) := following-line;
  when (following-line)
    line-previous(following-line) := start-line
  end;
  following-line
end method delete-across-lines;

define method delete-across-lines
    (start-line :: <diagram-line>, start-index :: <integer>,
     end-line   :: <text-line>,    end-index   :: <integer>)
 => (following-line :: false-or(<basic-line>))
  // Things are different for diagram lines, in that we can't pull
  // the "left over" contents of the end line into the start line.
  // Just delete the initial contents of the last line, but leave the
  // first line intact, unlinking the intervening lines
  delete-within-line(end-line, 0, end-index);
  let following-line = end-line;
  line-next(start-line) := following-line;
  line-previous(following-line) := start-line;
  following-line
end method delete-across-lines;

define method delete-across-lines
    (start-line :: <diagram-line>, start-index :: <integer>,
     end-line   :: <diagram-line>, end-index   :: <integer>)
 => (following-line :: false-or(<basic-line>))
  // If there's a diagram line at both the start and the end,
  // just unlink the intervening lines
  let following-line = line-next(end-line);
  line-next(start-line) := following-line;
  when (following-line)
    line-previous(following-line) := start-line
  end;
  following-line
end method delete-across-lines;


define open generic note-multi-line-deletion
    (mode :: <mode>, interval :: <interval>) => ();

define method note-multi-line-deletion
    (mode :: <mode>, interval :: <interval>) => ()
  #f
end method note-multi-line-deletion;


/// Fixing up sections and nodes

// This simply pulls all the lines from the last section into the first
// section.  Any sections between the first and the last (exclusive) have
// already been emptied, so don't worry about them.
define method merge-sections
    (start-section :: <basic-section>, end-section :: <basic-section>) => ()
  assert(start-section ~== end-section,
	 "The start and end sections must be different in 'merge-sections'");
  let start2 = section-start-line(end-section);
  let end2   = section-end-line(end-section);
  // Do nothing if the end section is already empty
  when (start2 & end2)
    for (line = start2 then line-next(line),
	 until: ~line)
      line-section(line) := start-section
    end;
    section-end-line(start-section) := end2
  end
end method merge-sections;

// Remove all of the empty sections and nodes.  We take all sections
// from the start-section (exclusive) to the end-section (inclusive)
// to be empty now.
define method remove-empty-nodes
    (start-node :: <basic-node>, end-node :: <basic-node>) => ()
  assert(start-node ~== end-node,
	 "The start and end nodes must be different in 'cleanup-empty-nodes'");
  block (break)
    let next = node-next(start-node);
    while (next)
      let node :: <basic-node> = next;
      next := node-next(node);		// before we call 'remove-node!'...
      let section = node-section(node);
      when (section)
	let container = section-container(section);
	when (container)
	  remove-section!(container, section)
	end
      end;
      when (section | interval-start-bp(node) = interval-end-bp(node))
	let buffer = node-buffer(node);
	when (buffer)
	  remove-node!(buffer, node)
	end
      end;
      when (node == end-node)
        break()
      end
    end
  end      
end method remove-empty-nodes;
