Module:       streams-internals
Synopsis:     Implementation of concrete file streams
Author:       Toby Weinberg, Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// File stream accessor class

define open abstract primary class <external-file-accessor>
    (<external-stream-accessor>)
end class <external-file-accessor>;

/// File stream classes

// File streams basically to not inherit any significant methods from
// <single-buffered-stream> or <positionable-stream>, only protocol.
// This is because they use power of two sized buffers which are aligned with
// disk segments.  <single-buffered-stream> uses non-aligned buffers
// because it has to allow for non-positionable streams as well as
// positionable ones.  File streams are positionable.  Combining aligned
// buffers with non- positionable streams is painful. In particular
// non-positionable input-output streams are pathological.
// Non-positionable input and output streams don't really need specifically
// aligned buffers, as long as the buffer sizes are well chosen they will
// in fact remain aligned.  So it is simpler just to implement aligned
// positionable buffered streams for <file-stream>.
define open abstract primary class <file-stream>
    (<single-buffered-stream>,
     <external-stream>,
     <basic-positionable-stream>)
  constant slot stream-locator, 
    required-init-keyword: locator:;
  slot accessor :: false-or(<external-stream-accessor>) = #f,
    init-keyword: accessor:;  // inherited from <external-stream>
//   slot initial-position :: <position-type> = 0; // inherited from <basic-positionable-stream>
//   slot current-position :: <position-type> = 0; // inherited from <basic-positionable-stream>
//   slot final-position :: <position-type> = 0; // inherited from <basic-positionable-stream>
end class <file-stream>;

define sealed class <general-file-stream>
    (<file-stream>, <general-typed-stream>)
  inherited slot stream-element-type = <character>;
  keyword encoding:;
end class;

define open class <byte-char-file-stream>
    (<file-stream>, <byte-char-element-stream>)
  inherited slot stream-element-type = <byte-character>;
  keyword encoding:;
end class;

define open class <byte-file-stream>
    (<file-stream>, <byte-element-stream>)
  inherited slot stream-element-type = <byte>;
  keyword encoding:;
end class;

/*---*** andrewa: how can we get these to work?
define copy-down-stream <byte-char-file-stream> 
   element <byte-character> sequence <byte-string>;

define copy-down-stream <byte-file-stream> 
   element <byte> sequence <byte-string>;
*/

/// Creating file streams

define method initialize
    (stream :: <file-stream>, #rest initargs,
     #key buffer-size: requested-buffer-size = #f, locator) => ()
  next-method();
  unless (stream.accessor)
    stream.accessor := apply(new-accessor, #"file", initargs);
  end;
  if (requested-buffer-size)
    stream-shared-buffer(stream) := 
      make-<power-of-two-buffer>(known-power-of-two-size?: #f,
				 size: requested-buffer-size);
    // At this point, stream.input-buffer and stream.output-buffer are both
    // false.
  else 
    stream-shared-buffer(stream) := 
      make-<power-of-two-buffer>
        (known-power-of-two-size?: #t, 
	 size: accessor-preferred-buffer-size(stream.accessor));
  end if;
  // This call synchronizes the accessor file position with the stream
  // position.  It is necessary since #"append" mode can set the accessor
  // file position to the end of file.  For <file-stream> this also
  // has the effect of initializing the stream-input-buffer (for
  // #"input" or  #"input-output" streams, or the stream-output-buffer
  // for #"output" streams.
  stream-position(stream) := stream.accessor.accessor-position;
  values();
end method initialize;

define method make (class == <file-stream>, #rest initargs,
		    #key locator, element-type = <byte-character>, encoding)
 => (stream :: <file-stream>)
  let type
    = apply(type-for-file-stream, locator, element-type, encoding, initargs);
  if (type == class)
    next-method()
  else
    apply(make, type, initargs)
  end
end method make;

define open generic type-for-file-stream
    (locator :: <object>,
     element-type :: false-or(<type>), encoding :: <object>,
     #key, #all-keys)
 => (file-stream-type /* ---*** :: subclass(<file-stream>) */);

define method type-for-file-stream 
    (locator :: <object>,
     element-type :: false-or(<type>), encoding :: <object>,
     #key)
 => (file-stream-type /* ---*** :: subclass(<file-stream>) */)
  <general-file-stream>
end method type-for-file-stream;

define method type-for-file-stream 
    (locator :: <object>,
     element-type == <byte-character>, encoding :: <object>,
     #key)
 => (file-stream-type /* ---*** :: subclass(<file-stream>) */)
  <byte-char-file-stream>
end method type-for-file-stream;

//---*** This equates <character> with <byte-character>.  Hmm...
define method type-for-file-stream 
    (locator :: <object>,
     element-type == <character>, encoding :: <object>,
     #key)
 => (file-stream-type /* ---*** :: subclass(<file-stream>) */)
  <byte-char-file-stream>
end method type-for-file-stream;

define method type-for-file-stream 
    (locator :: <object>,
     element-type  == <byte>, encoding :: <object>,
     #key)
 => (file-stream-type :: subclass(<file-stream>))
  <byte-file-stream>
end method type-for-file-stream;


/// File stream implementation

define method stream-at-end?
    (stream :: <file-stream>) => (at-end? :: <boolean>)
  readable?(stream)
  & ~get-input-buffer(stream);
end method stream-at-end?;

define method stream-input-available?
    (stream :: <file-stream>) => (available? :: <boolean>)
  readable?(stream)
  & error("stream-input-available?") // ### FIXME
end method stream-input-available?;

// Note that there is always a stream-shared-buffer, even at the beginning
// of file when there is no input or output buffer. If the shared buffer is
// dirty then it might be larger than the size known to the accessor.  This
// is a way of telling the size without releasing the buffer.  Tally-ho.

define method stream-size 
    (the-stream :: <file-stream>) => (the-size :: false-or(<integer>))
  let the-buffer  = stream-shared-buffer(the-stream);
  if (the-buffer & the-buffer.buffer-dirty?)
    let the-buffer :: <buffer> = the-buffer;
    let the-size = the-stream.accessor.accessor-size;
    if (the-size)
      max(the-buffer.buffer-position + the-buffer.buffer-end, the-size)
  else
      the-buffer.buffer-position + the-buffer.buffer-end
    end if
  elseif (the-stream.accessor)
    the-stream.accessor.accessor-size
  else
    error(make(<stream-closed-error>, stream: the-stream,
               format-string: "Can't determine size of closed stream"))
  end if
end method;

define method close (stream :: <file-stream>, #key) => ();
  if (stream-open?(stream))
   next-method ();
    // Now zero out the buffers so that any attempt to use the stream
    // forces a call to do-get-x-buffer, where the appropriate call to
    // ensure-readable or ensure-writable will diagnose the problem.
    // and signal an appropriate error.
    stream.stream-shared-buffer := #f;
    stream.stream-input-buffer := #f;
    stream.stream-output-buffer := #f;
    stream.accessor := #f;
  end if;
end method;

/// Aligned power of two buffer methods

// Helper function for filling buffers from the accessor.
define function load-buffer
    (the-stream :: <file-stream>, the-buffer :: <power-of-two-buffer>,
     desired-file-position :: <integer>, start :: <integer>, 
     count :: <integer>) => (nread :: <integer>)
  if (desired-file-position ~= the-stream.accessor.accessor-position)
    accessor-position(the-stream.accessor) := desired-file-position;
  end if;
  let (nread, eof?)
    = accessor-read-into!
    (the-stream.accessor, the-stream, start, count, buffer: the-buffer);
  the-buffer.buffer-end := start + nread;
  nread
end function;

define function load-buffer-and-fill
    (the-stream :: <file-stream>, the-buffer :: <power-of-two-buffer>,
     desired-file-position :: <integer>, start :: <integer>, 
     count :: <integer>) => ()
  let nread =
    load-buffer(the-stream, the-buffer, desired-file-position, start, count);
  buffer-fill(the-buffer, 0, start: the-buffer.buffer-end);  
  nread
end function;

// We get here only when 'stream-input-buffer' is #f.  Since changing
// position gets a new buffer eagerly, this method is only called when
// read-element changes the mode from input to output (for input-output
// streams).  This method returns
// either the gotten input buffer or #f if no more input is available.
// We might need to "turn around" the output buffer.  If this happens it is
// possible that the buffer might be partial with buffer-end not equal to
// buffer-size.  The buffer might be partial because we are at end of file
// or because we did a get-next-output-buffer with the buffer position
// somewhere interior in the file.  Note that we guarantee that
// buffer-start is 0 for all input or input-output streams.  Non-0 starts
// only occur with output-only streams.
define method do-get-input-buffer
    (the-stream :: <file-stream>, 
     #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  ensure-readable(the-stream);
  let current-buffer :: false-or(<buffer>) = stream-shared-buffer(the-stream);
  if (stream-output-buffer(the-stream))
    // Turn around the output buffer.  The only reason to do this is
    // because we are going to request input from the same buffer which we
    // were using for output.
    stream-input-buffer(the-stream) := current-buffer;
    do-release-output-buffer(the-stream);
    if (current-buffer.buffer-next >= current-buffer.buffer-end)
      // Try to load the rest of the buffer to sync the buffer with the
      // file.  Another possibility is to force the buffer and read from
      // the beginning. 
      assert (current-buffer.buffer-end < current-buffer.buffer-size);
      let read-position = 
	current-buffer.buffer-position + current-buffer.buffer-end;
      let start = current-buffer.buffer-end;
      let count = current-buffer.buffer-end - start;
      load-buffer(the-stream, current-buffer, read-position, start, count);
      if (current-buffer.buffer-next >= current-buffer.buffer-end)
	// update the stream before the calling code signals eof
	force-buffer(current-buffer, the-stream, 
		     return-fresh-buffer?: #f);
	current-buffer := #f;   // eof
      end if;
    end if;
  else
    error("File stream has neither an input or output buffer");
  end if;
  current-buffer
end method do-get-input-buffer;

// This is only called when buffer-next is equal to buffer-end, that is
// when buffer-next has been incremented 1 past the end of buffer by
// read-element.  So the new value for buffer-next is buffer-start.  If the
// current buffer is aligned, the next buffer will also be aligned, unless
// buffer-end is less than buffer-size. This will only
// happen at the end of file, so the last read, the one which returns 0
// bytes will not be aligned -- tough.

define method do-next-input-buffer
    (the-stream :: <file-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  ignore(wait?, bytes);  
  let the-buffer :: <buffer> = stream-input-buffer(the-stream);
  let the-size :: <buffer-index> = the-buffer.buffer-size;
  let start  :: <buffer-index> = 0;
  let next-buffer-position = 
    the-buffer.buffer-position + the-buffer.buffer-end;
  // The accessor-position can be out of sync with the stream-position for
  // several reasons at this point.  The direction of the stream, input vs.
  // output, may have changed any number of times before we release this
  // buffer.  Force-output may have been called on a dirty buffer somewhere
  // part way through the use of this buffer and that will have reset the
  // accessor position.  If we have to write out a dirty buffer
  // we know where the accessor-position is after we do that.  Of course
  // the buffer can be dirty only if this is an input-output stream.
  the-buffer := force-buffer(the-buffer, the-stream, return-fresh-buffer?: #t);
  the-stream.stream-input-buffer := the-buffer;
  the-stream.stream-shared-buffer := the-buffer;  
  let nread =
    load-buffer(the-stream, the-buffer,  next-buffer-position, start, 
		the-size);
  // Do all of this initialization even if nothing was read (eof)
  // because read line actually leaves the buffer as the last empty
  // buffer rather than signaling/taking eof actions if the last
  // buffer doesn't end in a newline.  Bad stuff.
  the-buffer.buffer-position := next-buffer-position;
  the-buffer.buffer-dirty? := #f;
  the-buffer.buffer-end  := nread;
  the-buffer.buffer-start := start;
  the-buffer.buffer-next := start;
  if (nread > 0)
    the-buffer
  else
    #f		// end of file
  end
end method do-next-input-buffer;



//  We get here only when 'stream-output-buffer' is #f.  This can only
// happen for two reasons, beginning of stream or we need to turn around
// the input buffer of an input-output stream.  If we are off the end of
// stream we want to fill the buffer with nulls so that any set position
// has nulls for uninitialized locations.
// We might need to get rid of the input buffer.
define method do-get-output-buffer
    (the-stream :: <file-stream>, #key bytes = 1)
 => (the-buffer :: <buffer>)
  ensure-writable(the-stream);
  let the-buffer :: <buffer> = stream-shared-buffer(the-stream);
  if (stream-input-buffer(the-stream))
    // assume any input buffer is only partial if it is at end of file
    buffer-fill(the-buffer, 0, start: the-buffer.buffer-end);
    do-release-input-buffer(the-stream);
    stream-output-buffer(the-stream) := the-buffer;
  else
    error("File stream has neither an input or output buffer");
  end if;
  the-buffer
end method do-get-output-buffer;

// This can only be called when 'stream-output-buffer' has a buffer in
// it.  We have to worry whether the accessor-position is right.  If force
// output has been called while this buffer was held or if it was
// input-output and changed from input to output the accessor position can
// be wrong. Force-buffer adjusts the position to buffer-position +
// buffer-start.
define method do-next-output-buffer
    (the-stream :: <file-stream>, #key bytes = 1)
 => (the-buffer :: <buffer>)
  ignore(bytes);
  let the-buffer :: <buffer> = stream-output-buffer(the-stream);
  assert(the-buffer.buffer-end = the-buffer.buffer-size);
  let next-buffer-position = 
    the-buffer.buffer-position + the-buffer.buffer-end; 
  the-buffer := force-buffer(the-buffer, the-stream, return-fresh-buffer?: #t);
  the-stream.stream-output-buffer := the-buffer;
  the-stream.stream-shared-buffer := the-buffer;  
  buffer-fill(the-buffer, 0); // ensures gaps fill with zero
  the-buffer.buffer-position := next-buffer-position;
  the-buffer.buffer-dirty? := #f;
  the-buffer.buffer-start := 0;
  the-buffer.buffer-next := 0;
  the-buffer.buffer-end  := 0;
  the-buffer
end method do-next-output-buffer;


// Helper functions for forcing output buffers.  Since output buffers can
// change to input buffers without been synched it is necessary to force
// out any dirty buffer regardless of whether it is input or output.
define method force-output-buffers
    (stream :: <file-stream>) => ()
  let sb = stream-shared-buffer(stream);
  if (sb)
    do-force-output-buffers(stream)
  end;
  values()
end method force-output-buffers;

define method do-force-output-buffers
    (the-stream :: <file-stream>) => ()
  let the-buffer :: <buffer> = stream-shared-buffer(the-stream);
  force-buffer(the-buffer, the-stream, return-fresh-buffer?: #f);
  values()
end method do-force-output-buffers;

// Force out a buffer, regardless of whether the buffer is output or not.
// This is important because single-buffered input-output streams may have
// dirty buffers which are currently input buffers.
// make these function when through debugged
define function force-buffer 
    (the-buffer :: <buffer>, the-stream :: <file-stream>,
     #key return-fresh-buffer? = #f)
 => (the-buffer :: <buffer>)
  if (the-buffer.buffer-dirty?)
    let start :: <buffer-index> = the-buffer.buffer-start;
    let count = the-buffer.buffer-end - start;
    let new-file-position = the-buffer.buffer-position + start;
    if (new-file-position ~= the-stream.accessor.accessor-position)
      accessor-position(the-stream.accessor) := new-file-position;
    end if;
    if (count > 0)		// implies valid output buffer
      let (nwritten :: <integer>, new-buffer :: <buffer>)
	= accessor-write-from(the-stream.accessor, the-stream, start, count,
			      buffer: the-buffer, 
			      return-fresh-buffer?: return-fresh-buffer?);
      the-buffer := new-buffer;
      if (nwritten ~= count)
	error("Bad write count")
      end;
      if (write-only?(the-stream))
        the-buffer.buffer-start := start + nwritten;
      end;
    end;
    the-buffer.buffer-dirty? := #f;
  end if;
  the-buffer
end;

/// Positioning methods on aligned power of two buffers.
 
define method stream-position
    (stream :: <file-stream>) => (position :: <integer>)
  if (stream-input-buffer(stream) | stream-output-buffer(stream))
    stream-shared-buffer(stream).buffer-position
      + stream-shared-buffer(stream).buffer-next 
  elseif (closed?(stream))
    // It's pretty much arbitrary, but it's important that the position
    // not be the end of the stream.  Which it is likely to be in many
    // cases.  If it is the end of stream, certain operations like,
    // read-to-end, won't signal an error when invoked on a closed
    // stream.
    0
  else
    // The stream has just been opened but nothing has been done yet.  If it
    // was opened with append mode the position might not be 0.  Gotta check
    // the accessor position instead.
    stream.accessor.accessor-position
  end if;
end method stream-position;

define method stream-position-setter
    (position :: <integer>, stream :: <file-stream>)
 => (position :: <integer>)
  let size-of-stream :: false-or(<integer>) = stream-size(stream);
  if ((position >= 0) & (~size-of-stream | position <= size-of-stream))
    // Don't call next-method() it just figures out the error cases again
    stream.current-position := position;
    adjust-stream-position-from-start(position, stream, size-of-stream);
  else
    if (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
		 format-string: 
		   "Can't set position of closed stream"));
    else       
       error(make(<stream-position-error>, stream: stream, 
                  size: stream.accessor.accessor-size, position: position));
    end if;
  end;
  position
end method;

// Special for dood - avoids the overhead of adjust-stream-position, but
// allows setting the position to extend the file, unlike
// stream-position-setter.
define method writable-file-stream-position-setter
    (position :: <integer>, stream :: <file-stream>)
 => (position :: <integer>)
  let size-of-stream :: <integer> = stream-size(stream);
  if ((position >= 0))
    stream.current-position := position;
    adjust-stream-position-from-start(position, stream, size-of-stream);
  else 
    signal(make(<stream-position-error>, stream: stream, 
		size: stream.accessor.accessor-size, position: position));
  end;
  position
end method;

define method adjust-stream-position
    (stream :: <file-stream>, delta :: <integer>,
     #key from = #"current")
 => (position :: <integer>)
  let size-of-stream :: <integer> = stream-size(stream);
  let position-from-start
    = select (from)
	#"current" => stream-position(stream) + delta;
	#"start"   => delta;
	#"end"     => size-of-stream + delta;
      end;
  if ((position-from-start < 0) | 
	((write-only?(stream)) & (position-from-start > size-of-stream)))
    if (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
		 format-string: 
		   "Can't set position of closed stream"));
    else       
       error(make(<stream-position-error>, stream: stream, 
                  size: stream.accessor.accessor-size, position: position));
    end if;
  else
    // Don't call next-method() it just figures out everything above again
    stream.current-position := position;
    adjust-stream-position-from-start
      (position-from-start, stream, size-of-stream);
  end if;
  position-from-start
end method adjust-stream-position;

    
define constant $null-buffer :: <buffer> = 
  make(<buffer>, size: 1, fill: 0, buffer-start: 0, buffer-end: 1);

define method adjust-stream-position-from-start
    (position-from-start :: <integer>, the-stream :: <file-stream>,
     size-of-stream :: false-or(<integer>)) => ()

  select (the-stream.stream-direction)
    // Input streams
    #"input" => 
      let the-buffer :: <buffer> = stream-shared-buffer(the-stream);
      let new-buffer-position = 
	logand(the-buffer.buffer-off-page-bits, position-from-start);
      let new-buffer-next = 
	logand(the-buffer.buffer-on-page-bits, position-from-start);
      // Input only streams can't have partial buffers or dirty buffers.
      if (size-of-stream & position-from-start > size-of-stream)
	signal(make(<stream-position-error>, stream: the-stream, 
		    size: size-of-stream, position: position-from-start));
      elseif (stream-input-buffer(the-stream)
                & (new-buffer-position = the-buffer.buffer-position)
                & (new-buffer-next >= the-buffer.buffer-start)
                & (new-buffer-next <= the-buffer.buffer-end))
	// just set the position
	the-buffer.buffer-next := new-buffer-next;
      else
	// empty the buffer
        the-buffer.buffer-end := new-buffer-next;
	the-buffer.buffer-position := new-buffer-position;
	the-buffer.buffer-start := new-buffer-next;
	the-buffer.buffer-next := new-buffer-next;
	// Setting the input buffer turns out to be necessary. In the case
	// that setting the stream position is the first thing done after
	// the stream is opened then there is a shared buffer but no input
	// or output buffer.  Get-input-buffer doesn't like that.
	stream-input-buffer(the-stream) := the-buffer;
	// buffer-dirty? should always be #f
      end if;
    // Input-output streams
    #"input-output" =>
      unless (size-of-stream)
        error(make(<stream-position-error>, stream: the-stream, 
                   size: size-of-stream, position: position-from-start,
                   format-string: "input-output stream must be positionable"));
      end unless;
      let the-buffer :: <buffer> = stream-shared-buffer(the-stream);
      let new-buffer-position = 
	logand(the-buffer.buffer-off-page-bits, position-from-start);
      let new-buffer-next = 
	logand(the-buffer.buffer-on-page-bits, position-from-start);
      if (position-from-start > size-of-stream) // off the end of file
	// Note position-from-start = size-of-stream is fine because there
	// are no gaps and get-output-buffer will work.  Get-output-buffer
	// knows perfectly well how to deal with buffer-next = buffer-end.
	// That is the normal case for write-element.
	if (new-buffer-position = the-buffer.buffer-position)
	  // New position is in the last buffer of the stream and the last
	  // buffer is current and the new position is off the end of file.
	  // Now check for partial buffer.  Could be partial and not at end.
	  let start-to-buffer-end = 
	    the-buffer.buffer-position + the-buffer.buffer-end;
	  if (start-to-buffer-end < size-of-stream)
	    // There is stuff in the file that isn't in the buffer, fix that
	    load-buffer-and-fill(the-stream, the-buffer, 
				 start-to-buffer-end, // file position
				 the-buffer.buffer-end, // start
				 size-of-stream - start-to-buffer-end);
	  end if;
	else // new position is off end-of-file and not in the current buffer
	  the-buffer := force-buffer(the-buffer, the-stream, 
				     return-fresh-buffer?: #t);
	  the-stream.stream-shared-buffer := the-buffer;
	  // Don't set stream-input/output-buffer now it's done at the
	  // end of this else clause.
	  if (new-buffer-position < size-of-stream)
	    // We weren't in the last buffer, so move to it and load it up.
	    let start = 0;
	    load-buffer-and-fill(the-stream, the-buffer, new-buffer-position,
				 start, the-buffer.buffer-size);
	  else 
	    // new-buffer-position is off the eof, so this buffer is
	    // completely off the end of file.  All nulls.
	    buffer-fill(the-buffer, 0, start: 0);
	  end if;
	  the-buffer.buffer-start := 0;
	  the-buffer.buffer-position := new-buffer-position;
	  if (new-buffer-next = 0)
	    // We have a problem.  Just setting buffer-dirty? to #t isn't
	    // going to force growing the file because with buffer-end equal
	    // to buffer-start the force-output-buffers call in close won't
	    // actually write any nulls to the file.   My solution is to
	    // write a null to (position-from-start - 1) this is safe to do
	    // because the test on this branch was (position-from-start >
	    // size-of-file) so there must be at least one null at eof.
	    $null-buffer.buffer-position := position-from-start - 1;
	    $null-buffer.buffer-dirty? := #t;
	    force-buffer
	      ($null-buffer, the-stream, return-fresh-buffer?: #f);
	  end if;
	end if;
	the-buffer.buffer-next := new-buffer-next;
	// Setting buffer-end to be the new position includes any nulls in
	// this buffer and allows get-output-buffer to be called on the newly
	// positioned stream.
	the-buffer.buffer-end := new-buffer-next;
	// Setting position off the end of file automatically makes the buffer
	// dirty because we have appended nulls to the file. 
	the-buffer.buffer-dirty? := #t;
	// Assume we are going to do output 8).  We're off the eof so
	// doing input would be an error.  We've already done all the
	// work to turn around the buffer so why make get-output-buffer do it
	// again? 
	if (stream-input-buffer(the-stream))
	  do-release-input-buffer(the-stream);
	  stream-output-buffer(the-stream) := the-buffer;
	end if;
      elseif (new-buffer-position = the-buffer.buffer-position)
	// change position within the current buffer, but not off the eof,
	// possibly exactly at the eof but that is just fine (whew!).
	// First fill in any partial buffer.
	if (the-buffer.buffer-end < the-buffer.buffer-size)
	  let start-to-buffer-end = 
	    the-buffer.buffer-position + the-buffer.buffer-end;
	    if (start-to-buffer-end < size-of-stream)
	      // There is stuff in the file that isn't in the buffer, fix that
	      let start = the-buffer.buffer-end;
	      load-buffer(the-stream, the-buffer, start-to-buffer-end,
			  start, 
			  min(the-buffer.buffer-size,
			      size-of-stream - start-to-buffer-end));
	    end if;
	  // load-buffer sets a new value for buffer-end
	  buffer-fill(the-buffer, 0, start: the-buffer.buffer-end);
	end if; // not a partial buffer
	the-buffer.buffer-next := new-buffer-next;
      else // change position to an interior position in some other buffer
	the-buffer := force-buffer(the-buffer, the-stream, 
				   return-fresh-buffer?: #t);
	the-stream.stream-shared-buffer := the-buffer;
	// Don't set stream-input/output-buffer now it's done at the
	// end of this else clause.
	let start = 0;
	load-buffer-and-fill(the-stream, the-buffer, new-buffer-position,
			     start,  the-buffer.buffer-size);
	the-buffer.buffer-position := new-buffer-position;
	the-buffer.buffer-start := start;
	the-buffer.buffer-next := new-buffer-next;
	the-buffer.buffer-dirty? := #f;
      end if; 
      if (stream-input-buffer(the-stream)) 
	stream-input-buffer(the-stream) := the-buffer;
      else
	//  Either stream-output-buffer is set (still to the old
	//  buffer) or neither the input nor output buffer is set.  In
	//  either case this is a reasonable choice.
	stream-output-buffer(the-stream) := the-buffer;
      end if;
    // Output streams
    #"output" =>
      let the-buffer :: <buffer> = stream-shared-buffer(the-stream);
      let new-buffer-position = 
	logand(the-buffer.buffer-off-page-bits, position-from-start);
      let new-buffer-next = 
	logand(the-buffer.buffer-on-page-bits, position-from-start);
      // Output only streams have special problems with fixed alignment
      // buffers.  Basically we allow buffer-start to be non-zero whenever
      // the position is set to the middle of the buffer.  We always flush
      // the buffer when a set position occurs unless the position is into
      // the area between  buffer-start and buffer-end in the current buffer.
      // This is the case where it is important that the number of bytes
      // flushed is calculated as buffer-end - buffer-start, not buffer-size
      // - buffer-end.
      if ((new-buffer-position = the-buffer.buffer-position)
	    & (new-buffer-next >= the-buffer.buffer-start)
	    & (new-buffer-next <= the-buffer.buffer-end))
	// Just move the pointer.  It's in the dirty region already.  Note
	// that the <= test says that if you move to the current position,
	// that is new-buffer-next = the-buffer.buffer-next then you just
	// continue writing.  This is also fine if you set the position to
	// 0 before any writes have occurred.
	the-buffer.buffer-next := new-buffer-next;
      else
	// Move to a new buffer, even if it is the same
	// buffer-position.  Can't deal with holes except at the
	// beginning or end of the buffer.	
	the-buffer := force-buffer(the-buffer, the-stream, 
				   return-fresh-buffer?: #t);
	the-stream.stream-shared-buffer := the-buffer;
	// Stream-output-buffer is set at the end of the #"output" case.
	the-buffer.buffer-position := new-buffer-position;
	the-buffer.buffer-next := new-buffer-next;
	the-buffer.buffer-end := new-buffer-next;
	if (size-of-stream
              & position-from-start > size-of-stream) // off the end of file.
	  // We force a null out to guarantee that the stream
	  // size grows when position is set past the end of file.
	  if (new-buffer-next = 0)
	    // Shit, we have to force the null into the position one before
	    // the beginning of this buffer.
	    $null-buffer.buffer-position := position-from-start - 1;
	    $null-buffer.buffer-dirty? := #t;
	    force-buffer($null-buffer, the-stream,
			 return-fresh-buffer?: #f);
	    the-buffer.buffer-start := new-buffer-next;
	  else
	    the-buffer[new-buffer-next - 1] := 0;
	    the-buffer.buffer-start := new-buffer-next  - 1;
	    the-buffer.buffer-dirty? := #t;
	  end if;
	else
	  the-buffer.buffer-start := new-buffer-next;
	end if;
      end if;
      // Make sure.  Could be first operation on stream.
      stream-output-buffer(the-stream) := the-buffer;
    #"closed" =>
       error(make(<stream-closed-error>, stream: the-stream,
		 format-string: 
		   "Can't set position of closed end stream"));     
  end select;
  values()
end method;

//--- We don't bother verifying that the unread element is the
//--- same as the previously read element.  Tough.
define method unread-element
    (stream :: <file-stream>, elt :: <object>)
 => (element :: <object>)
  adjust-stream-position(stream, -1);
  elt
end method unread-element;

define method read-to-end
    (stream :: <file-stream>) => (seq :: <sequence>)
  if (stream-open?(stream))
    let the-size = stream-size(stream);
    if (the-size)
      let n = the-size - stream-position(stream);
      read(stream, n)
    else
      // The file length is unavailable, so fall back
      next-method()
    end if
  else
    error(make(<stream-closed-error>, stream: stream,
	       format-string: 
		 "Can't read from closed stream"));

  end if
end method read-to-end;

define method stream-contents
    (stream :: <file-stream>, #key clear-contents? = #f)
 => (contents :: <sequence>)
  if (~readable?(stream))
    if (write-only?(stream))
      error("Cannot use stream-contents on an output only file stream: %=",
	    stream.stream-locator);
    elseif (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
		 format-string: 
		   "Can't set call stream-contents on a closed stream")); 
    else
      error("Cannot call stream-contents. Stream isn't readable: %=",
	    stream.stream-locator);
    end if;
  elseif (clear-contents?)
    error ("Cannot use clear-contents? keyword argument to streams-contents"
           " when the stream is a subclass of <file-stream>: %=",
	   stream.stream-locator);
  else
    let original-position = stream-position(stream);
    force-output(stream);
    stream-position(stream) := 0;
    let contents = read(stream, stream-size(stream));
    stream-position(stream) := original-position;
    contents
  end
end method stream-contents;

//---*** What about 'stream-contents-as'?
