Module:    print-internals
Author:    wlott@cs.cmu.edu
Synopsis:  Most of Dick Water's pretty printer.
Copyright: See below.

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
//

/// This file contains a more or less straight conversion of CMU Common Lisp's
/// rewrite of the Dick Water's pretty printer code.
///


//// User tunable parameters.

define variable *print-miser-width* :: false-or(<integer>) = #f;

define variable *default-line-length* :: <integer> = 80;


//// Random internal constants. 

define constant $initial-buffer-size = 128;

define constant $newline = as(<integer>, '\n');


//// Tame byte copying utility.

define method copy-maybe-overlapping-bytes
    (src :: <byte-string>, src-start :: <integer>,
     dst :: <byte-string>, dst-start :: <integer>, n :: <integer>)
 => ()
  case
    src ~== dst
      => // Use the standard "any which way" version, which is likely to be
         // faster.
         copy-bytes(dst, dst-start, src, src-start, n);
    src-start < 0 | src-start + n > size(src)
      => error("Source parameters for copy-maybe-overlapping-bytes from %= "
               "are out of range - start at %d, copy %d bytes.",
               src, src-start, n);
    dst-start < 0 | dst-start + n > size(dst)
      => error("Destination parameters for copy-maybe-overlapping-bytes "
               "to %= are out of range - start at %d, copy %d bytes.",
               dst, dst-start, n);
    src-start < dst-start
      => // Right shift, so iterate right to left to preserve sanity.
         for (src-i :: <integer> from src-start + n - 1 to src-start by -1,
              dst-i :: <integer> from dst-start + n - 1 to dst-start by -1)
           dst[dst-i] := src[src-i];
         end;
    src-start > dst-start
      => // Left shift, so iterate left to right.
         for (src-i :: <integer> from src-start to src-start + n - 1, 
              dst-i :: <integer> from dst-start to dst-start + n - 1)
           dst[dst-i] := src[src-i];
         end;
    otherwise
      => // Copying to the same place, so nothing to do.
  end;
end method;


//// Types.

// There are three different units for measuring character positions:
//  <index> - index into the output buffer.
//  <column> - offset (in characters) from the start of the current line.
//  <position> - some position in the stream of characters cycling through
//               the output buffer.
// 
define constant <index> = limited(<integer>, min: 0);
define constant <column> = limited(<integer>, min: 0);
define constant <position> = <integer>;

// <pretty-stream> -- exported.
//
// Stream used for pretty printing.
// 
define class <pretty-stream> (<sequence-stream>)
  inherited slot stream-direction, init-value: #"output";

  //
  // The stream where the output is finally going to go.
  constant slot pretty-stream-target :: <stream>,
    required-init-keyword: target:;
  //
  // The line length for this stream.
  constant slot pretty-stream-line-length :: <column> = *default-line-length*,
    init-keyword: line-length:;

  /*---*** andrewa: this isn't used...
  //
  // Buffer handed off to the user as part of the stream extension protocol.
  constant slot pretty-stream-user-buffer :: <buffer>
    = make(<buffer>, size: 1024);
  */

  //
  // Buffer holding pending output.
  slot pretty-stream-buffer :: <byte-string>
    = make(<byte-string>, size: $initial-buffer-size);
  //
  // The index into the buffer where more text should be put.
  slot pretty-stream-buffer-fill-pointer :: <index> = 0;
  //
  // Whenever we output stuff from the buffer, we shift the remaining noise
  // over.  This makes it difficult to keep references to locations in
  // the buffer.  Therefore, we have to keep track of the total amount of
  // stuff that has been shifted out of the buffer.  This is the delta between
  // the <position> and <index> types.
  slot pretty-stream-buffer-offset :: <position> = 0;
  //
  // The column the first character in the buffer will appear in.  Normally
  // zero, but if we end up with a very long line with no breaks in it we
  // might have to output part of it.  Then this will no longer be zero.
  slot pretty-stream-buffer-start-column :: <column> = 0,
    init-keyword: column:;
  //
  // The line number we are currently on.  Used for *print-lines* abrevs and
  // to tell when sections have been split across multiple lines.
  slot pretty-stream-line-number :: <integer> = 0;
  //
  // Stack of logical blocks in effect at the buffer start.
  slot pretty-stream-blocks :: <list>
    = list(make(<logical-block>,
		start-column: 0,
		section-column: 0,
		per-line-prefix-end: 0,
		prefix-length: 0,
		suffix-length: 0,
		section-start-line: 0));
  //
  // Buffer holding the per-line prefix active at the buffer start.
  // Indentation is included in this.  The amount of this currently in use
  // is stored in the logical block stack.
  slot pretty-stream-prefix :: <byte-string>
    = make(<byte-string>, size: $initial-buffer-size);
  //
  // Buffer holding the total remaining suffix active at the buffer start.
  // The characters are right-justified in the buffer to make it easier
  // to output the buffer.  The length is stored in the logical block
  // stack.
  slot pretty-stream-suffix :: <byte-string>
    = make(<byte-string>, size: $initial-buffer-size);
  //
  // Deque of pending operations (indents, newlines, tabs, etc.).  Entries
  // are push-last'ed onto the end, and pop'ed from the front.
  constant slot pretty-stream-queue :: <deque> = make(<deque>);
  //
  // Stack of block-start queue entries in effect at the queue head.
  slot pretty-stream-pending-blocks :: <list> = #();
  //
  // Set to #t when the stream is closed.  Can't do anything with it after
  // that.
  slot pretty-stream-closed? :: <boolean> = #f;
end class <pretty-stream>;

/*
define sealed domain make (singleton(<pretty-stream>));
define sealed domain initialize (<pretty-stream>);
*/


//// position/column/index conversion routines

// index-posn -- internal
//
// Convert from a buffer-index to a position.  Just add the buffer-offset.
//
define method index-posn
    (index :: <index>, stream :: <pretty-stream>)
 => (posn :: <position>)
  index + stream.pretty-stream-buffer-offset;
end;

// posn-index -- internal.
//
// Convert from a position to a buffer-index.  Just subtract the buffer-offset.

define method posn-index
    (posn :: <position>, stream :: <pretty-stream>)
 => (index :: <index>)
  posn - stream.pretty-stream-buffer-offset;
end;

// posn-column -- internal.
//
// Convert a position to a column.  First, convert the position to an index
// and then convert that index to a column.  (Index-column is defined with
// the tab related functions, because it has to take tabs into account.)
// 
define method posn-column
    (posn :: <position>, stream :: <pretty-stream>)
 => (column :: <column>)
  index-column(posn-index(posn, stream), stream);
end;


//// Stream extension routines.

define method write-element
    (stream :: <pretty-stream>, char :: <character>) => ();
  let string = make(<string>, size: 1);
  string[0] := char;
  append-output(stream, string, 0, 1)
end method write-element;

define method write
    (stream :: <pretty-stream>, string :: <string>, #key start, end: stop) => ();
  append-output(stream, string, start | 0, stop | string.size)
end method write;

define method close
    (stream :: <pretty-stream>, #rest keys, #key) => ()
  unless (stream.pretty-stream-closed?)
    maybe-output(stream, #f);
    expand-tabs(stream, #f);
    write(stream.pretty-stream-target, stream.pretty-stream-buffer,
	  start: 0, end: stream.pretty-stream-buffer-fill-pointer);
    stream.pretty-stream-closed? := #t;
  end;
end;


//// Stuff to append output.

// append-output -- internal.
//
// Copy a bunch of output into the buffer.  If there are any newlines, they
// get enqueued as ``literal'' conditional newlines.  Everything else is
// just handed to append-raw-output.
//
// NOTE: with the advent of the Streams Library providing a new-line function,
// we probably should not be turning \n characters into real newlines.  They
// should just be dumped as is, and if that doesn't work on the native
// platform, oh well, the user wasn't coding portably.  Having said that, I'm
// not modifying this code, and we'll see if anyone notices :-).
// 
define method append-output
    (stream :: <pretty-stream>, buffer :: <buffer>, start :: <buffer-index>,
     stop :: <buffer-index>) => ()
  local
    method repeat (chunk-start, index)
      if (index == stop)
	unless (chunk-start == index)
	  append-raw-output(stream, buffer, chunk-start, index);
	end;
      elseif (buffer[index] == $newline)
	unless (chunk-start == index)
	  append-raw-output(stream, buffer, chunk-start, index);
	end;
	enqueue-newline(stream, #"literal");
	repeat(index + 1, index + 1);
      else
	repeat(chunk-start, index + 1);
      end;
    end;
  repeat(start, start);
end;

define method append-output
    (stream :: <pretty-stream>, buffer :: <byte-string>, start :: <buffer-index>,
     stop :: <buffer-index>) => ()
  local
    method repeat (chunk-start, index)
      if (index == stop)
	unless (chunk-start == index)
	  append-raw-output(stream, buffer, chunk-start, index);
	end;
      elseif (buffer[index] == '\n')
	unless (chunk-start == index)
	  append-raw-output(stream, buffer, chunk-start, index);
	end;
	enqueue-newline(stream, #"literal");
	repeat(index + 1, index + 1);
      else
	repeat(chunk-start, index + 1);
      end;
    end;
  repeat(start, start);
end;

// append-raw-output -- internal.
//
// Actually copy the stuff into the buffer.  Bad things will happen if there
// are any newlines in stuff.
//
// Assure-space-in-buffer is not guarenteed to return all the space we want
// so we might have to iterate.
// 
define method append-raw-output
    (stream :: <pretty-stream>, stuff :: type-union(<buffer>, <byte-string>),
     start :: <buffer-index>, stop :: <buffer-index>) => ()
  let chars = stop - start;
  let available = assure-space-in-buffer(stream, chars);
  let count = min(chars, available);
  let fill-pointer = stream.pretty-stream-buffer-fill-pointer;
  let new-fill-ptr = fill-pointer + count;
  copy-bytes(stream.pretty-stream-buffer, fill-pointer, stuff, start, count);
  stream.pretty-stream-buffer-fill-pointer := new-fill-ptr;
  unless (count == chars)
    append-raw-output(stream, stuff, start + count, stop);
  end;
end;


//// Logical blocks.

// <logical-block> -- internal.
//
// Object representing logical blocks.  Hence the name.  Okay, so this isn't
// a very useful comment, but what else is there to say?
//
define class <logical-block> (<object>)
  //
  // The column this logical block started in.
  constant slot logical-block-start-column :: <column>,
    required-init-keyword: start-column:;
  //
  // The column the current section started in.
  slot logical-block-section-column :: <column>,
    required-init-keyword: section-column:;
  //
  // The length of the per-line prefix.  We can't move the indentation
  // left of this.
  slot logical-block-per-line-prefix-end :: <index>,
    required-init-keyword: per-line-prefix-end:;
  //
  // The overall length of the prefix, including any indentation.
  slot logical-block-prefix-length :: <index>,
    required-init-keyword: prefix-length:;
  //
  // The overall length of the suffix.
  slot logical-block-suffix-length :: <index>,
    required-init-keyword: suffix-length:;
  // 
  // The line number the current section started on.
  slot logical-block-section-start-line :: <integer>,
    required-init-keyword: section-start-line:;
end class <logical-block>;

/*
define sealed domain make (singleton(<logical-block>));
define sealed domain initialize (<logical-block>);
*/

// really-start-logical-block -- internal.
//
// Called by maybe-output when a logical-block will not fit entirly on one
// line.  We set the indentation to whatever column we are currently at, and
// add the prefix (which is a per-line-prefix) and suffix to the total
// per-line-prefix and suffix stored in the stream.
// 
define method really-start-logical-block
    (stream :: <pretty-stream>, column :: <column>,
     prefix :: false-or(<string>), suffix :: false-or(<string>)) => ()
  let blocks = stream.pretty-stream-blocks;
  let prev-block = blocks.head;
  let per-line-end = prev-block.logical-block-per-line-prefix-end;
  let prefix-length = prev-block.logical-block-prefix-length;
  let suffix-length = prev-block.logical-block-suffix-length;
  let new-block = make(<logical-block>,
		       start-column: column,
		       section-column: column,
		       per-line-prefix-end: per-line-end,
		       prefix-length: prefix-length,
		       suffix-length: suffix-length,
		       section-start-line: stream.pretty-stream-line-number);
  stream.pretty-stream-blocks := pair(new-block, blocks);
  set-indentation(stream, column);
  if (prefix)
    // We know that we don't have to grow the prefix because set-indentation
    // did it for us.  This is because the prefix has already been output once,
    // resulting in the current column being at the end of the prefix.
    // Therefore, set-indentation grew the prefix enough to put spaces in where
    // we are about to put the per-line-prefix.
    new-block.logical-block-per-line-prefix-end := column;
    copy-bytes(stream.pretty-stream-prefix, column - prefix.size,
	       prefix, 0,
	       prefix.size);
  end;
  if (suffix)
    let total-suffix = stream.pretty-stream-suffix;
    let total-suffix-len = total-suffix.size;
    let additional = suffix.size;
    let new-suffix-len = suffix-length + additional;
    if (new-suffix-len > total-suffix-len)
      let new-total-suffix-len
	= max(total-suffix-len * 2,
	      suffix-length + floor/(additional * 5, 4));
      let new-total-suffix = make(<byte-string>, size: new-total-suffix-len);
      copy-bytes(new-total-suffix, new-total-suffix-len - suffix-length,
		 total-suffix, total-suffix-len - suffix-length,
     suffix-length);
      total-suffix := new-total-suffix;
      total-suffix-len := new-total-suffix-len;
      stream.pretty-stream-suffix := total-suffix;
    end;
    copy-bytes(total-suffix, total-suffix-len - new-suffix-len,
	       suffix, 0,
	       additional);
    new-block.logical-block-suffix-length := new-suffix-len;
  end;
end method;

// set-indentation -- internal.
//
// Set the indentation to the given column.  Basically, we just grow the
// per-line-prefix if necessary, and fill the new part with spaces.
//
define method set-indentation
    (stream :: <pretty-stream>, column :: <column>) => ()
  let prefix = stream.pretty-stream-prefix;
  let prefix-len = prefix.size;
  let this-block = stream.pretty-stream-blocks.head;
  let current = this-block.logical-block-prefix-length;
  let minimum = this-block.logical-block-per-line-prefix-end;
  let column = max(minimum, column);
  if (column > prefix-len)
    let new-prefix-len
      = max(prefix-len * 2, prefix-len + floor/((column - prefix-len) * 5, 4));
    let new-prefix = make(<byte-string>, size: new-prefix-len);
    copy-bytes(new-prefix, 0, prefix, 0, current);
    prefix := stream.pretty-stream-prefix := new-prefix;
  end;
  if (column > current)
    fill!(prefix, ' ', start: current, end: column);
  end;
  this-block.logical-block-prefix-length := column;
end;

// really-end-logical-block -- internal.
//
// Called by maybe-output at the end of a logical block that didn't fit on
// one line.  We just finish off the block, and reset the indentation.
// 
define method really-end-logical-block
    (stream :: <pretty-stream>) => ()
  let old = stream.pretty-stream-blocks.head;
  let old-indent = old.logical-block-prefix-length;
  stream.pretty-stream-blocks := stream.pretty-stream-blocks.tail;
  let new = stream.pretty-stream-blocks.head;
  let new-indent = new.logical-block-prefix-length;
  if (new-indent > old-indent)
    fill!(stream.pretty-stream-prefix, ' ',
	  start: old-indent, end: new-indent);
  end;
end;


//// The pending operation queue.

// <queued-op> -- internal.
//
// All the different ops that we queue up inherit from this.
//
// We need to queue these things up, because we find out about them as the
// text is being generated (i.e. as stuff is being added to the buffer), but
// we don't act on them until we are actually sending the output on to the
// target (i.e. as stuff is being removed from the buffer).
//
// While stuff is in the buffer/queue, it represents stuff we've been told
// about but haven't decided what to do about yet.
// 
define abstract class <queued-op> (<object>)
  //
  // The position this op occurred at.
  slot op-posn :: <position> = 0;
end;

/*
define sealed domain initialize (<queued-op>);
*/

// enqueue -- internal.
//
// Add the op to the stream's queue after setting the ops position to
// the current position.
// 
define method enqueue
    (stream :: <pretty-stream>, op :: <queued-op>)
 => (op :: <queued-op>)
  op.op-posn := index-posn(stream.pretty-stream-buffer-fill-pointer, stream);
  push-last(stream.pretty-stream-queue, op);
  op;
end;

// <section-start> -- internal.
//
// The start of a section.
//
define abstract class <section-start> (<queued-op>)
  //
  // The depth of this section.  I.e. the number of logical blocks
  // surrounding it.
  constant slot section-start-depth :: <integer>,
    required-init-keyword: depth:;
  //
  // The thing that ends this section, or #f if we don't know yet.
  slot section-start-section-end ::
    type-union(singleton(#f), <newline>, <block-end>)
    = #f;
end;

/*
define sealed domain make (singleton(<section-start>));
*/

// <pretty-newline-kind> & <newline-kind> -- internal.
//
// The different kinds of newlines.  <pretty-newline-kind> just covers those
// that are pretty.  <newline-kind> adds literal newlines.
// 
define constant <pretty-newline-kind>
  = one-of(#"linear", #"fill", #"miser", #"mandatory");
//
define constant <newline-kind>
  = type-union(<pretty-newline-kind>, singleton(#"literal"));

// <newline> -- internal.
// 
define class <newline> (<section-start>)
  //
  // The kind of newline it is.
  constant slot newline-kind :: <newline-kind>, required-init-keyword: kind:;
end;

/*
define sealed domain make (singleton(<newline>));
define sealed domain initialize (<newline>);
*/

// enqueue-newline -- internal.
//
// Queue up a newline.  In addition to adding the new newline op to the
// queue, we need to figure out if this newline closes off some section.
// And finally, we check to see if there is any outputting we can do.
//
define method enqueue-newline
    (stream :: <pretty-stream>, kind :: <newline-kind>) => ()
  let depth = stream.pretty-stream-pending-blocks.size;
  let newline = enqueue(stream, make(<newline>, kind: kind, depth: depth));
  for (entry in stream.pretty-stream-queue)
    if (~(newline == entry)
	  & instance?(entry, <section-start>)
	  & ~entry.section-start-section-end
	  & depth <= entry.section-start-depth)
      entry.section-start-section-end := newline;
    end;
  end;
  maybe-output(stream, kind == #"literal" | kind == #"mandatory");
end;

// <indentation-kind> -- internal
//
// The different kinds of indentations.
// 
define constant <indentation-kind>
  = one-of(#"block", #"current");

// <indentation> -- internal.
//
// Represents a change in the indentation.
// 
define class <indentation> (<queued-op>)
  //
  // What the indentation is relative to.
  constant slot indentation-kind :: <indentation-kind>,
    required-init-keyword: kind:;
  //
  // The delta.
  constant slot indentation-amount :: <integer>,
    required-init-keyword: amount:;
end;

/*
define sealed domain make (singleton(<indentation>));
*/

// enqueue-indent -- internal.
//
// Queue up a change in the indentation.
// 
define method enqueue-indent
    (stream :: <pretty-stream>, kind :: <indentation-kind>, amount :: <integer>) => ()
  enqueue(stream, make(<indentation>, kind: kind, amount: amount));
end;

// <block-start> -- internal.
//
// Represents the start of some logical-block.
//
define class <block-start> (<section-start>)
  //
  // The <block-end> op that corresponds to this <block-start>.  #f until
  // this block ends.
  slot block-start-block-end :: false-or(<block-end>) = #f;
  //
  // The per-line-prefix, if there is one.
  constant slot block-start-prefix :: false-or(<byte-string>),
    required-init-keyword: prefix:;
  //
  // The suffix, if there is one.
  constant slot block-start-suffix :: false-or(<byte-string>),
    required-init-keyword: suffix:;
end;

/*
define sealed domain make (singleton(<block-start>));
*/

// start-logical-block -- internal.
//
// Qeueu up the start of a logical block.  Also, add the prefix to the end
// of the buffer.
//
define method start-logical-block
    (stream :: <pretty-stream>, prefix :: false-or(<byte-string>),
     per-line? :: <boolean>, suffix :: false-or(<byte-string>)) => ()
  if (prefix)
    append-raw-output(stream, prefix, 0, prefix.size);
  end;
  let pending-blocks = stream.pretty-stream-pending-blocks;
  let start = enqueue(stream,
		      make(<block-start>,
			   prefix: per-line? & prefix,
			   suffix: suffix,
			   depth: pending-blocks.size));
  stream.pretty-stream-pending-blocks := pair(start, pending-blocks);
end;

// <block-end> -- internal.
//
// Represents the end of a logical-block.
// 
define class <block-end> (<queued-op>)
  /*---*** andrewa: this isn't used...
  //
  // The suffix for the block this block-end is ending.
  constant slot block-end-suffix :: false-or(<byte-string>),
    required-init-keyword: suffix:;
  */
end;

/*
define sealed domain make (singleton(<block-end>));
*/

// end-logical-block -- internal
//
// Queue up the end of a logical-block.  Also, append the suffix (found in
// the corresponding block-start) to the buffer.
//
define method end-logical-block
    (stream :: <pretty-stream>, aborted? :: <boolean>) => ()
  let blocks = stream.pretty-stream-pending-blocks;
  let start = blocks.head;
  stream.pretty-stream-pending-blocks := blocks.tail;
  let suffix = ~aborted? & start.block-start-suffix;
  let stop = enqueue(stream, make(<block-end>, suffix: suffix));
  if (suffix)
    append-raw-output(stream, suffix, 0, suffix.size);
  end;
  start.block-start-block-end := stop;
end;

// <tab> -- internal.
// 
define class <tab> (<queued-op>)
  //
  // Various parameters for the tab.
  constant slot tab-section? :: <boolean>, required-init-keyword: section?:;
  constant slot tab-relative? :: <boolean>, required-init-keyword: relative?:;
  constant slot tab-colnum :: <column>, required-init-keyword: colnum:;
  constant slot tab-colinc :: <integer>, required-init-keyword: colinc:;
end;

/*
define sealed domain make (singleton(<tab>));
define sealed domain initialize (<tab>);
*/

// <tab-kind> -- internal.
//
// The different kinds of tabs.
// 
define constant <tab-kind>
  = one-of(#"line", #"line-relative", #"section", #"section-relative");

// enqueue-tab -- internal.
//
// Queue up a tab.  Not too exciting.
// 
define method enqueue-tab
    (stream :: <pretty-stream>, kind :: <tab-kind>, colnum :: <column>,
     colinc :: <integer>) => ()
  let (section?, relative?)
    = select (kind)
	#"line" => values(#f, #f);
	#"line-relative" => values(#f, #t);
	#"section" => values(#t, #f);
	#"section-relative" => values(#t, #t);
      end;
  enqueue(stream,
	  make(<tab>, section?: section?, relative?: relative?,
	       colnum: colnum, colinc: colinc));
end;


//// Tab support.

// compute-tab-size -- internal.
//
// Figure out the size (i.e. number of spaces) this tab will expand to
// if started at the given column and section-start.
// 
define method compute-tab-size
    (tab :: <tab>, section-start :: <column>, column :: <column>)
 => (size :: <integer>)
  let origin = if (tab.tab-section?) section-start else 0 end;
  let colnum = tab.tab-colnum;
  let colinc = tab.tab-colinc;
  if (tab.tab-relative?)
    unless (colinc <= 1)
      let newposn = column + colnum;
      let rem = remainder(newposn, colinc);
      unless (zero?(rem))
	colnum := colnum + colinc - rem;
      end;
    end;
    colnum;
  elseif (column <= colnum + origin)
    colnum + origin - column;
  else
    colinc - remainder(column - origin, colinc);
  end;
end;

// index-column -- internal.
//
// Figure out what column corresponds to the given index by expanding any
// tabs that get in the way.  We just scan down the queue looking for tabs
// that need to be expanded, keeping track of what column we are at and where
// the latest section started.  Actaully, column and section-start hold the
// delta between raw indexes and the real column (i.e. spaces added by tabs)
// instead of the real column directly.  So we have to add the index in
// if we want the real column.  We do this because it makes the record keeping
// a little easier.
// 
define method index-column
    (index :: <index>, stream :: <pretty-stream>)
 => (column :: <column>)
  let column = stream.pretty-stream-buffer-start-column;
  let section-start
    = stream.pretty-stream-blocks.head.logical-block-section-column;
  let end-posn = index-posn(index, stream);
  block (return)
    for (op in stream.pretty-stream-queue)
      if (op.op-posn >= end-posn)
	return();
      end;
      if (instance?(op, <tab>))
	column := column
	  + compute-tab-size(op, section-start,
			     column + posn-index(op.op-posn, stream));
      elseif (instance?(op, <section-start>))
	section-start := column + posn-index(op.op-posn, stream);
      end;
    end;
  end;
  column + index;
end;

// expand-tabs -- internal.
//
// Find and expand (i.e. replace with spaces) the tabs up though the given
// queued-op.  We do this in two passes.  First, we figure out how much
// we need to insert where.  And second, we do the actual insertions.
// 
define method expand-tabs
    (stream :: <pretty-stream>, through :: false-or(<queued-op>)) => ()
  let insertions = #();
  let additional = 0;
  let column = stream.pretty-stream-buffer-start-column;
  let section-start
    = stream.pretty-stream-blocks.head.logical-block-section-column;
  block (return)
    for (op in stream.pretty-stream-queue)
      if (instance?(op, <tab>))
	let index = posn-index(op.op-posn, stream);
	let tabsize = compute-tab-size(op, section-start, column + index);
	unless (zero?(tabsize))
	  insertions := pair(pair(index, tabsize), insertions);
	  additional := additional + tabsize;
	  column := column + tabsize;
	end;
      elseif (instance?(op, <section-start>))
	section-start := column + posn-index(op.op-posn, stream);
      end;
      if (op == through)
	return();
      end;
    end;
  end;
  unless (empty?(insertions))
    let fill-ptr = stream.pretty-stream-buffer-fill-pointer;
    let new-fill-ptr = fill-ptr + additional;
    let buffer = stream.pretty-stream-buffer;
    let new-buffer = buffer;
    let len = buffer.size;
    let stop = fill-ptr;
    if (new-fill-ptr > len)
      let new-len = max(len * 2, fill-ptr + floor/(additional * 5, 4));
      new-buffer := make(<byte-string>, size: new-len);
      stream.pretty-stream-buffer := new-buffer;
    end;
    stream.pretty-stream-buffer-fill-pointer := new-fill-ptr;
    stream.pretty-stream-buffer-offset
      := stream.pretty-stream-buffer-offset - additional;
    for (insertion in insertions)
      let srcpos = insertion.head;
      let amount = insertion.tail;
      let dstpos = srcpos + additional;
      let tabpos = dstpos - amount;
      copy-maybe-overlapping-bytes
        (buffer, srcpos, new-buffer, dstpos, stop - srcpos);
      fill!(new-buffer, ' ', start: tabpos, end: dstpos);
      additional := additional - amount;
      stop := tabpos;
    end;
    unless (new-buffer == buffer)
      copy-bytes(new-buffer, 0, buffer, 0, stop);
    end;
  end;
end;


//// Stuff to do the actual outputting.

// assure-space-in-buffer -- internal.
//
// Make sure there is some space in the buffer, and return how much that is.
// If there isn't any space in the buffer, first try to output some stuff
// in order to make space.  If that doesn't work, then grow the buffer.
// 
define method assure-space-in-buffer
    (stream :: <pretty-stream>, want :: <integer>)
 => (available :: <integer>)
  let buffer = stream.pretty-stream-buffer;
  let length = buffer.size;
  let fill-ptr = stream.pretty-stream-buffer-fill-pointer;
  let available = length - fill-ptr;
  if (available > 0)
    available;
  elseif (fill-ptr > stream.pretty-stream-line-length)
    unless (maybe-output(stream, #f))
      output-partial-line(stream);
    end;
    assure-space-in-buffer(stream, want);
  else
    let new-length = max(length * 2, length + floor/(want * 5, 4));
    let new-buffer = make(<byte-string>, size: new-length);
    stream.pretty-stream-buffer := new-buffer;
    copy-bytes(new-buffer, 0, buffer, 0, fill-ptr);
    new-length - fill-ptr;
  end;
end;

// maybe-output -- internal.
//
// See if anything can be output, and if so, do so.  
//
// We scan down the queue, checking each op to see if there is anything
// we can do.  If there isn't, then we leave that op in the queue and quit
// the loop.
//
define method maybe-output
    (stream :: <pretty-stream>, force-newlines? :: <boolean>)
 => (did-anything? :: <boolean>)
  let queue = stream.pretty-stream-queue;
  let output-anything? = #f;
  block (return)
    until (queue.empty?)
      // Don't actually pop the queue until we've actually processed this op.
      let next = queue.first;
      if (instance?(next, <newline>))
	// For newlines, check to see if we should break.  If so, output a
	// line.
	let kind = next.newline-kind;
	if (if (kind == #"literal" | kind == #"mandatory" | kind == #"linear")
	      // We always break at linear newlines, because if this block
	      // fit on a single line, everything inside it would have been
	      // deleted from the queue.
	      #t;
	    elseif (kind == #"miser")
	      stream.misering?;
	    elseif (kind == #"fill")
	      stream.misering?
		| (stream.pretty-stream-line-number
		     > (stream.pretty-stream-blocks.head
			  .logical-block-section-start-line))
		| (select (fits-on-line?(stream,
					 next.section-start-section-end,
					 force-newlines?))
		     #t => #f;
		     #f => #t;
		     #"dont-know" => return();
		   end);
	    else
	      error("Strange kind of newline: %=", kind);
	    end)
	  output-anything? := #t;
	  output-line(stream, next);
	end;
      elseif (instance?(next, <indentation>))
	// For indentations, set the indent level unless we are misering.
	unless (misering?(stream))
	  set-indentation(stream,
			  next.indentation-amount
			    + select (next.indentation-kind)
				#"block" =>
				  stream.pretty-stream-blocks.head
				    .logical-block-start-column;
				#"current" =>
				  posn-column(next.op-posn, stream);
				otherwise =>
				  error("Strange kind of indentation: %=",
					next.indentation-kind);
			      end);
	end;
      elseif (instance?(next, <block-start>))
	// For block-starts, check to see if the whole block fits on a line.
	select (fits-on-line?(stream, next.section-start-section-end,
			      force-newlines?))
	  #t =>
	    // If so, delete everything up to the block-end.  We leave the
	    // block-end in the queue so that when we get to the pop below
	    // it has something to remove.
	    let stop = next.block-start-block-end;
	    expand-tabs(stream, stop);
	    until (queue.first == stop)
	      pop(queue);
	    end;
	  #f =>
	    // If not, then really start the logical block.
	    really-start-logical-block(stream,
				       posn-column(next.op-posn, stream),
				       next.block-start-prefix,
				       next.block-start-suffix);
	  #"dont-know" =>
	    // If we can't tell, give up for now.
	    return();
	end;
      elseif (instance?(next, <block-end>))
	// Done with this block.
	really-end-logical-block(stream);
      elseif (instance?(next, <tab>))
	// Expand out the tab.
	expand-tabs(stream, next);
      else
	error("Strange thing in queue: %=", next);
      end;
      pop(queue);
    end;
  end;
  output-anything?;
end;

// misering? -- internal.
//
// Return #t if we should be misering, #f if not.
// 
define method misering?
    (stream :: <pretty-stream>) => (misering? :: <boolean>)
  if (*print-miser-width*)
    let line-len = stream.pretty-stream-line-length;
    let blocks = stream.pretty-stream-blocks;
    let start-column = blocks.head.logical-block-start-column;
    line-len - start-column <= *print-miser-width*;
  end;
end;

// fits-on-line? -- internal.
//
// Return #t if everything until until-op fits on a single line, #f if not,
// and #"dont-know" if we can't tell.
// 
define method fits-on-line?
    (stream :: <pretty-stream>, until-op :: false-or(<queued-op>),
     force-newlines? :: <boolean>)
 => (fits :: one-of(#t, #f, #"dont-know"))
  let available = stream.pretty-stream-line-length;
  //(when (and *print-lines*
  //	       (= *print-lines* (pretty-stream-line-number stream)))
  //  (decf available 3) ; for the `` ..''
  //  (decf available (logical-block-suffix-length
  //		       (car (pretty-stream-blocks stream)))))
  if (until-op)
    posn-column(until-op.op-posn, stream) <= available;
  elseif (force-newlines?)
    #f;
  elseif (index-column(stream.pretty-stream-buffer-fill-pointer, stream)
	    > available)
    #f;
  else
    #"dont-know";
  end;
end;

// output-line -- internal.
//
// Actually output a line worth of stuff.  Newline is the newline that ends
// this line.  All tabs will already have been expanded, so we don't have to
// mess with them.
// 
define method output-line (stream :: <pretty-stream>, newline :: <newline>) => ()
  let target = stream.pretty-stream-target;
  let buffer = stream.pretty-stream-buffer;
  let kind = newline.newline-kind;
  let literal? = kind == #"literal";
  let amount-to-consume = posn-index(newline.op-posn, stream);
  let amount-to-print
    = if (literal?)
	amount-to-consume;
      else
	// It it wasn't a literal newline, back up the amount we are going
	// to print to get rid of any spaces at the end.
	local method repeat (index)
		if (zero?(index))
		  0;
		else
		  let new-index = index - 1;
		  if (buffer[new-index] ~= ' ')
		    index;
		  else
		    repeat(new-index);
		  end;
		end;
	      end;
	repeat(amount-to-consume);
      end;
  write(target, buffer, start: 0, end: amount-to-print);
  let line-number = stream.pretty-stream-line-number + 1;
  //  (when (and *print-lines* (>= line-number *print-lines*))
  //	(write-string " .." target)
  //	(let ((suffix-length (logical-block-suffix-length
  //			      (car (pretty-stream-blocks stream)))))
  //	  (unless (zerop suffix-length)
  //	    (let* ((suffix (pretty-stream-suffix stream))
  //		   (len (length suffix)))
  //	      (write-string suffix target
  //			    :start (- len suffix-length)
  //			    :end len))))
  //	(throw 'line-limit-abbreviation-happened t))
  new-line(target);
  stream.pretty-stream-line-number := line-number;
  stream.pretty-stream-buffer-start-column := 0;
  // Copy the per-line-prefix into the output buffer.  This also takes care of
  // any indentation, as that has been added to the per-line-prefix buffer.
  let fill-ptr = stream.pretty-stream-buffer-fill-pointer;
  let next-block = stream.pretty-stream-blocks.head;
  let prefix-len = if (literal?)
		     next-block.logical-block-per-line-prefix-end;
		   else
		     next-block.logical-block-prefix-length;
		   end;
  let shift = amount-to-consume - prefix-len;
  let new-fill-ptr = fill-ptr - shift;
  let new-buffer = buffer;
  let buffer-length = buffer.size;
  if (new-fill-ptr > buffer-length)
    let extra = new-fill-ptr - buffer-length;
    new-buffer := make(<byte-string>,
		       size: max(buffer-length * 2,
				 buffer-length + floor/(extra * 5, 4)));
    stream.pretty-stream-buffer := new-buffer;
  end;
  copy-maybe-overlapping-bytes
    (buffer, amount-to-consume, new-buffer, prefix-len, 
       fill-ptr - amount-to-consume);
  copy-bytes(new-buffer, 0, stream.pretty-stream-prefix, 0, prefix-len);
  stream.pretty-stream-buffer-fill-pointer := new-fill-ptr;
  stream.pretty-stream-buffer-offset
    := stream.pretty-stream-buffer-offset + shift;
  unless (literal?)
    next-block.logical-block-section-column := prefix-len;
    next-block.logical-block-section-start-line := line-number;
  end;
end;

// output-partial-line -- internal.
//
// Output as much of a line as we can.  Basically, everything up until the
// first op in the queue.
// 
define method output-partial-line (stream :: <pretty-stream>) => ()
  let fill-ptr = stream.pretty-stream-buffer-fill-pointer;
  let queue = stream.pretty-stream-queue;
  let count = if (empty?(queue))
		fill-ptr;
	      else
		posn-index(queue.first.op-posn, stream);
	      end;
  let new-fill-ptr = fill-ptr - count;
  let buffer = stream.pretty-stream-buffer;
  if (zero?(count))
    error("Output-partial-line called when nothing can be output.");
  end;
  write(stream.pretty-stream-target, buffer, start: 0, end: count);
  stream.pretty-stream-buffer-start-column
    := stream.pretty-stream-buffer-start-column + count;
  copy-bytes(buffer, 0, buffer, count, new-fill-ptr);
  stream.pretty-stream-buffer-fill-pointer := new-fill-ptr;
  stream.pretty-stream-buffer-offset
    := stream.pretty-stream-buffer-offset + count;
end;


//// Interface routines.

define macro printing-logical-block
  { printing-logical-block (?stream:name, #rest ?options:expression) ?:body end }
    => { begin
	   let print-body = method (?stream) ?body end;
	   pprint-logical-block(?stream, body: print-body, ?options)
	 end }
end macro printing-logical-block;

// pprint-logical-block -- exported.
//
// Start a logical block, creating a pretty-stream if necessary.
// 
define open generic pprint-logical-block
    (stream :: <stream>,
     #key column :: <integer>,
          prefix :: false-or(<byte-string>),
          per-line-prefix :: false-or(<byte-string>),
          body :: <function>,
          suffix :: false-or(<byte-string>)) => ();

//
// When called on a regular stream, create <pretty-stream> and use it instead.
// 
define method pprint-logical-block
    (stream :: <stream>,
     #key column :: <integer> = 0,
          prefix :: false-or(<byte-string>),
          per-line-prefix :: false-or(<byte-string>),
          body :: <function>,
          suffix :: false-or(<byte-string>)) => ()
  if (prefix & per-line-prefix)
    error("Can't specify both a prefix: and a per-line-prefix:");
  end;
  case
    (*print-pretty?*) =>	//---*** argh, we shouldn't have to do this
      let stream = make(<pretty-stream>, target: stream, column: column);
      pprint-logical-block(stream,
			   prefix: prefix,
			   per-line-prefix: per-line-prefix,
			   body: body,
			   suffix: suffix);
      close(stream);
    otherwise =>
      if (prefix | per-line-prefix)
	write(stream, (prefix | per-line-prefix));
      end;
      body(stream);
      if (suffix) write(stream, suffix) end;
  end case;
end;
//
// When called on a <pretty-stream>, just use it directly.
// 
define sealed method pprint-logical-block
    (stream :: <pretty-stream>,
     #key column :: <integer> = 0,
          prefix :: false-or(<byte-string>),
          per-line-prefix :: false-or(<byte-string>),
          body :: <function>,
          suffix :: false-or(<byte-string>)) => ()
  if (prefix & per-line-prefix)
    error("Can't specify both a prefix: and a per-line-prefix:");
  end;
  if (stream.pretty-stream-closed?)
    error("%= has been closed");
  end;
  let aborted? = #t;
  block ()
    start-logical-block(stream, prefix | per-line-prefix,
			per-line-prefix ~= #f,
			suffix);
    body(stream);
    aborted? := #f;
  cleanup
    end-logical-block(stream, aborted?);
  end;
end;

// pprint-newline -- exported.
//
// Output a conditional newline of some kind.
// 
define open generic pprint-newline
    (kind :: <pretty-newline-kind>, stream :: <stream>) => ();

define sealed method pprint-newline
    (kind :: <pretty-newline-kind>, stream :: <pretty-stream>) => ()
  if (stream.pretty-stream-closed?)
    error("%= has been closed");
  end;
  enqueue-newline(stream, kind);
end;

// pprint-indent -- exported.
//
// Change the indentation.
// 
define open generic pprint-indent
    (relative-to :: <indentation-kind>, n :: <integer>,
     stream :: <stream>) => ();

define sealed method pprint-indent
    (relative-to :: <indentation-kind>, n :: <integer>,
     stream :: <pretty-stream>) => ()
  if (stream.pretty-stream-closed?)
    error("%= has been closed");
  end;
  enqueue-indent(stream, relative-to, n);
end;

// pprint-tab -- exported.
//
// Output a tab.
// 
define open generic pprint-tab
    (kind :: <tab-kind>, colnum :: <integer>, colinc :: <integer>,
     stream :: <stream>) => ();

define sealed method pprint-tab
    (kind :: <tab-kind>, colnum :: <integer>, colinc :: <integer>,
     stream :: <pretty-stream>) => ()
  if (stream.pretty-stream-closed?)
    error("%= has been closed");
  end;
  enqueue-tab(stream, kind, colnum, colinc);
end;

