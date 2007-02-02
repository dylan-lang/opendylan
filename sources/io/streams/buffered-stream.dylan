Module:       streams-internals
Synopsis:     Abstract classes and generic definitions for buffered streams
Author:       Toby Weinberg, Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Buffered streams

define open generic stream-input-buffer 
    (stream :: <buffered-stream>)
 => (input-buffer :: false-or(<buffer>));

define open generic stream-input-buffer-setter 
    (value :: false-or(<buffer>), stream :: <buffered-stream>)
 => (input-buffer :: false-or(<buffer>));


define open generic stream-output-buffer 
    (stream :: <buffered-stream>)
 => (output-buffer :: false-or(<buffer>));

define open generic stream-output-buffer-setter 
    (value :: false-or(<buffer>), stream :: <buffered-stream>)
 => ( output-buffer :: false-or(<buffer>));


// Note that the 'buffer-size:' initarg gets handles by the concrete
// stream class that uses <buffered-stream>
define open abstract primary class <buffered-stream> (<basic-stream>)
  // The input buffer currently in use
  slot actual-stream-input-buffer  :: false-or(<buffer>) = #f;
  // The output buffer currently in use
  slot actual-stream-output-buffer :: false-or(<buffer>) = #f;
end class <buffered-stream>;

// stream-input-buffer and stream-output-buffer need to be virtual in order
// to allow multi-buffered-streams to redefine them as methods rather than
// slots.

define inline method stream-input-buffer (stream :: <buffered-stream>)
 => (input-buffer :: false-or(<buffer>))
  stream.actual-stream-input-buffer
end method;

define inline method stream-input-buffer-setter
    (value ::  false-or(<buffer>), stream :: <buffered-stream>)
 => (input-buffer :: false-or(<buffer>))
  stream.actual-stream-input-buffer := value
end method;

define inline method stream-output-buffer (stream :: <buffered-stream>)
 => (input-buffer :: false-or(<buffer>))
  stream.actual-stream-output-buffer
end method;

define inline method stream-output-buffer-setter
    (value :: false-or(<buffer>), stream :: <buffered-stream>)
 => (input-buffer :: false-or(<buffer>))
  stream.actual-stream-output-buffer := value
end method;

//--- is this hyper-paranoid?  It's only here to catch internal errors.
define method stream-sequence
    (stream :: <buffered-stream>) => ();
  error("<buffered-stream>'s do not have an underlying sequence");
end method stream-sequence;


define inline function ensure-input-buffer (stream :: <buffered-stream>) => ()
  unless (stream-input-buffer(stream))
    error("Stream %= does not have the input buffer", stream)
  end
end function ensure-input-buffer;

define inline function ensure-output-buffer (stream :: <buffered-stream>) => ()
  unless (stream-output-buffer(stream))
    error("Stream %= does not have the output buffer", stream)
  end
end function ensure-output-buffer;


/// Buffered input protocol

define inline function get-input-buffer
    (stream :: <buffered-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  let sb = stream-input-buffer(stream);
  if (sb)
    let sb :: <buffer> = sb;
    if (sb.buffer-next = sb.buffer-end)	// gone past last valid byte?
      do-next-input-buffer(stream, wait?: wait?, bytes: bytes)
	// This returns #f if number of bytes read is 0
    else
      sb
    end
  else
    do-get-input-buffer(stream, wait?: wait?, bytes: bytes)
  end
end function get-input-buffer;

// No default method for this.  Look for interesting methods under
// <console-stream> and <file-stream> and <multi-buffered-stream>
define open generic do-get-input-buffer
    (stream :: <stream>, #key wait?, bytes)
 => (buffer :: false-or(<buffer>));

define inline function next-input-buffer
    (stream :: <buffered-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  ensure-input-buffer(stream);
  do-next-input-buffer(stream, wait?: wait?, bytes: bytes)
end function next-input-buffer;

// Look for interesting methods under <double-buffered-stream> and
// <file-stream> and <multi-buffered-stream>
define open generic do-next-input-buffer
    (stream :: <stream>, #key wait?, bytes)
 => (buffer :: false-or(<buffer>));

// This can only be called when 'stream-input-buffer' has a buffer in it

define inline function release-input-buffer
    (stream :: <buffered-stream>) => ()
  ensure-input-buffer(stream);
  do-release-input-buffer(stream)
end function release-input-buffer;

define open generic do-release-input-buffer
    (stream :: <stream>) => ();

define method do-release-input-buffer
    (stream :: <buffered-stream>) => ()
  values()
end method do-release-input-buffer;


define inline function input-available-at-source?
    (stream :: <buffered-stream>)
 => (input-available? :: <boolean>)
  do-input-available-at-source?(stream)
end function input-available-at-source?;

define open generic do-input-available-at-source?
    (stream :: <stream>)
 => (input-available? :: <boolean>);

define method do-input-available-at-source?
    (stream :: <buffered-stream>) => (input-available? :: <boolean>)
  #t
end method do-input-available-at-source?;


define macro with-input-buffer
  { with-input-buffer (?buffer:variable = ?stream:expression,
		       #key ?bytes:expression = 1)
       ?body:body
     end }
  => { begin
         with-stream-locked(?stream)
           let ?buffer :: false-or(<buffer>) 
             = get-input-buffer(?stream, bytes: ?bytes);
           ?body
         end
       end }
end macro with-input-buffer;


/// Buffered output protocol

define inline function get-output-buffer
    (stream :: <buffered-stream>, #key bytes = 1)
 => (buffer :: false-or(<buffer>))
  with-stream-locked(stream)
  let sb = stream-output-buffer(stream);
  if (sb)
    let sb :: <buffer> = sb; // HACK: TYPE ONLY
    if (sb.buffer-next = sb.buffer-size) // gone past the end of the buffer?
      do-next-output-buffer(stream, bytes: bytes)
    else
      sb
    end
  else
    do-get-output-buffer(stream, bytes: bytes)
  end
  end
end function get-output-buffer;

// No default method for this
define open generic do-get-output-buffer
    (stream :: <stream>, #key bytes)
 => (buffer :: false-or(<buffer>));


define inline function next-output-buffer
    (stream :: <buffered-stream>, #key bytes = 1) => ()
  ensure-output-buffer(stream);
  do-next-output-buffer(stream, bytes: bytes)
end function next-output-buffer;

define open generic do-next-output-buffer
    (stream :: <stream>, #key bytes)
 => (buffer :: false-or(<buffer>));

define inline function release-output-buffer
    (stream :: <buffered-stream>) => ()
  ensure-output-buffer(stream);
  do-release-output-buffer(stream)
end function release-output-buffer;

define open generic do-release-output-buffer
    (stream :: <stream>) => ();

define method do-release-output-buffer
    (stream :: <buffered-stream>) => ()
  values()
end method do-release-output-buffer;

define macro with-output-buffer
  { with-output-buffer (?buffer:variable = ?stream:expression,
			#key ?bytes:expression = 1)
      ?body:body
    end }
  => { begin
         with-stream-locked(?stream)
           let ?buffer :: false-or(<buffer>)
             = get-output-buffer(?stream, bytes: ?bytes);
           ?body
         end
       end }
end macro with-output-buffer;


/// Single buffered streams

define open generic stream-shared-buffer
    (stream :: <single-buffered-stream>)
 => (buffer :: false-or(<buffer>));

define open generic stream-shared-buffer-setter
    (buffer :: false-or(<buffer>), stream :: <single-buffered-stream>)
 => (buffer :: false-or(<buffer>));

define open abstract primary class <single-buffered-stream> (<buffered-stream>)
  // This buffer gets shared for both input and output
  slot stream-shared-buffer :: false-or(<buffer>) = #f,
    init-keyword: buffer:;
end class <single-buffered-stream>;

define method do-release-input-buffer
    (stream :: <single-buffered-stream>) => ()
  stream-input-buffer(stream) := #f
end method do-release-input-buffer;

define method do-release-output-buffer
    (stream :: <single-buffered-stream>) => ()
  stream-output-buffer(stream) := #f
end method do-release-output-buffer;


/// Double buffered streams

define open abstract primary class <double-buffered-stream> (<buffered-stream>)
  inherited slot stream-input-buffer = #f,
    init-keyword: input-buffer:;
  inherited slot stream-output-buffer = #f,
    init-keyword: output-buffer:;
end class <double-buffered-stream>;

define method close (stream :: <double-buffered-stream>, #key) => ();
  if (stream-open?(stream))
    next-method ();
    // Now zero out the buffers so that any attempt to use the stream
    // forces a call to do-get-x-buffer which will diagnose the problem
    // and signal an appropriate error.
    stream.stream-input-buffer := #f;
    stream.stream-output-buffer := #f;
  end if;
end method;

define method do-get-input-buffer
    (stream :: <double-buffered-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  ignore(wait?, bytes);
  // There should be a buffer under normal circumstances, so it's an
  // error if this method is ever called (the method is only called
  // when the streams input-buffer is #f.  Diagnose the problem and
  // emit an appropriate error message.
  if (closed?(stream))
    error(make(<stream-closed-error>, stream: stream,
	       format-string: 
		 "Can't read from closed stream"));
  elseif (write-only?(stream))
    error(make(<stream-not-readable>, stream: stream,
	       format-string: 
		 "Can't read from write-only stream"));
  else  error("Internal error: input buffer missing");
  end if;
  #f
end method do-get-input-buffer;

define method do-next-input-buffer
    (stream :: <double-buffered-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  ignore(wait?, bytes);
  let sb :: <buffer> = stream-input-buffer(stream);
  let bufsiz :: <buffer-index> = sb.buffer-size;
  let start  :: <buffer-index> = 0;  // Not really necessary, but safe
  let (nread, eof?)
    = accessor-read-into!(stream.accessor, stream, start, bufsiz);
  if (nread > 0)
    sb.buffer-next := start;
    sb.buffer-end  := nread;
    sb
  else
    #f		// end of file
  end
end method do-next-input-buffer;

define method do-get-output-buffer
    (stream :: <double-buffered-stream>, #key bytes = 1)
 => (buffer :: false-or(<buffer>));
  ignore(bytes);
  // There should be a buffer under normal circumstances, so it's an
  // error if this method is ever called (the method is only called
  // when the streams input-buffer is #f.  Diagnose the problem and
  // emit an appropriate error message.
  if (closed?(stream))
    error(make(<stream-closed-error>, stream: stream,
	       format-string: 
		 "Can't write to closed stream"));
  elseif (read-only?(stream))
    error(make(<stream-not-writable>, stream: stream,
	       format-string: 
		 "Can't write to read-only stream"));
  else
    error("Internal error: output buffer missing");
  end if;a:;
  #f
end method do-get-output-buffer;

define method do-next-output-buffer
    (stream :: <double-buffered-stream>, #key bytes = 1)
 => (buffer :: <buffer>)
  ignore(bytes);
  do-force-output-buffers(stream);
  let sb :: <buffer> = stream-output-buffer(stream);
  sb.buffer-start := 0;
  sb.buffer-end := 0;
  sb.buffer-next := 0;
  sb.buffer-dirty? := #f;
  buffer-fill(sb, 0);		// ensures gaps fill with zero
  sb
end method do-next-output-buffer;

// Helper functions for forcing output buffers
define method force-output-buffers
    (stream :: <double-buffered-stream>) => ()
  let sb = stream-output-buffer(stream);
  if (sb)
    do-force-output-buffers(stream)
  end
end method force-output-buffers;

// Continue here.  Check to see that the
// standard-io/win32-sio-streams.dylan stuff isn't relying on having the
// next and  end pointers reset by this method.
define method do-force-output-buffers
    (stream :: <double-buffered-stream>) => ()
  with-stream-locked(stream)
  // This method ignores the buffer-dirty? flag.  This is backward
  // compatible with the old streams library.
  let sb :: <buffer> = stream-output-buffer(stream);
  let start :: <buffer-index> = sb.buffer-start;
  let count = sb.buffer-end - start;
  if (count > 0)		// implies valid output buffer
    let nwritten
      = accessor-write-from(stream.accessor, stream, start, count);
    if (nwritten ~= count)
      error("Bad write count")
    end
  end;
  accessor-force-output(stream.accessor, stream);
  sb.buffer-dirty? := #f;
  // Don't reset the buffer next and end pointers here.  That is out to the
  // discretion of subclasses.  Aligned buffers for instance should not
  // have their pointers reset.
  values()
  end;
end method do-force-output-buffers;


/// Readable stream protocol

// Note that we don't implement 'unread-element' for buffered streams,
// we leave that to the concrete subclasses of <positionable-stream>
define method read-element
    (stream :: <buffered-stream>,
     #key on-end-of-stream = unsupplied())
 => (element :: <object>)
  with-input-buffer (sb = stream)
    if (sb)
      let sb :: <buffer> = sb; // HACK: TYPE ONLY
      let bi :: <buffer-index> = sb.buffer-next;
      let elt = coerce-to-element(stream, sb, bi);
      sb.buffer-next := bi + 1;
      elt
    else
      end-of-stream-value(stream, on-end-of-stream)
    end
  end
end method read-element;

define method peek
    (stream :: <buffered-stream>,
     #key on-end-of-stream = unsupplied())
 => (element :: <object>)
  with-input-buffer (sb = stream)
    if (sb)
      let sb :: <buffer> = sb; // HACK: TYPE ONLY
      coerce-to-element(stream, sb, sb.buffer-next)
    else
      end-of-stream-value(stream, on-end-of-stream)
    end
  end
end method peek;

define method read
    (stream :: <buffered-stream>, n :: <integer>,
     #key on-end-of-stream = unsupplied())
 => (elements)
  let elements = make(stream-sequence-class(stream), size: n);
  read-into!(stream, n, elements, on-end-of-stream: on-end-of-stream);
  elements
end method read;

//---*** andrewa: this is a bad name, since this isn't meant to know about
//---*** multi-buffered-streams!
define variable *multi-buffer-bytes* :: <integer> = 0;

define method read-into!
    (stream :: <buffered-stream>, n :: <integer>, seq :: <mutable-sequence>,
     #key start :: <integer> = 0, on-end-of-stream = unsupplied())
 => (n-read)
  if (n > 0)
    with-input-buffer (sb = stream)
      let e :: <integer> = start + n;
      // Fill in the result sequence
      *multi-buffer-bytes* := *multi-buffer-bytes* + n;
      iterate loop (i :: <integer> = start, sb :: false-or(<buffer>) = sb)
	if (sb & (i < e))
	  let sb :: <buffer> = sb;
	  let bi :: <buffer-index> = sb.buffer-next;
	  let ei :: <buffer-index> = sb.buffer-end;
	  if (bi >= ei)
	    loop(i, do-next-input-buffer(stream))
	  else
	    let count :: <integer> = min(ei - bi, e - i);
	    coerce-to-sequence(stream, sb, bi, seq, i, count);
	    sb.buffer-next := bi + count;
	    loop(i + count, sb)
	  end
	else 
	  // Signal error if we didn't get enough data
	  if (n > i - start)
	    n := i - start;
	    unless (supplied?(on-end-of-stream))
	      signal(make(<incomplete-read-error>,
			  stream: stream,
			  count: n, sequence: seq))
	    end
	  end
	end
      end iterate
    end;
  end;
  n
end method read-into!;


define method discard-input
    (stream :: <buffered-stream>) => ()
  let sb = stream-input-buffer(stream);
  if (sb)
    let start  :: <buffer-index> = sb.buffer-start;
    sb.buffer-next := start;
    sb.buffer-end  := start
  end
end method discard-input;



define method do-force-output
    (stream :: <buffered-stream>) => ()
  if (writable?(stream))
    force-output-buffers(stream)
  end
end method do-force-output;

define open generic force-output-buffers
    (stream :: <stream>) => ();

define open generic do-force-output-buffers
    (stream :: <stream>) => ();

define method discard-output
    (stream :: <buffered-stream>) => ()
  let sb = stream-output-buffer(stream);
  if (sb)
    let start  :: <buffer-index> = sb.buffer-start;
    sb.buffer-next := start;
    sb.buffer-end  := start
  end
end method discard-output;


/// Line-oriented I/O

define method read-line
    (stream :: <buffered-stream>,
     #key on-end-of-stream = unsupplied())
 => (string-or-eof :: <object>, newline? :: <boolean>)
  // If we're at the end before we've started to read anything,
  // signal end-of-stream instead of incomplete read
  if (stream-at-end?(stream))
    if (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
		 format-string: 
		   "Can't set position of closed stream"));
    else       
      values(end-of-stream-value(stream, on-end-of-stream), #f)
    end if;
  else 
    let line = #f;
    let matched? = #f;
    // This assumes that the elements of <buffer> are <byte>s.  But
    // that assumption is shot through this code so talley-ho...
    let nl = as(<byte>, '\n');
    let rt = as(<byte>, '\r');
    with-input-buffer (sb = stream)
      local method extend-line (sb :: <buffer>, i, bi) => ()
	      if (line)
		let new = make(stream-sequence-class(stream),
			       size: line.size + i - bi);
		copy-bytes(line, 0, new, 0, line.size);
		copy-bytes(sb, bi, new, line.size, i - bi);
		line := new
	      else
		line := make(stream-sequence-class(stream),
			     size: i - bi);
		copy-bytes(sb, bi, line, 0, i - bi)
	      end
	    end method;
      iterate loop (sb :: false-or(<buffer>) = sb)
        if (sb & ~matched?)
	  let sb :: <buffer> = sb; // HACK: TYPE ONLY
	  let bi :: <buffer-index> = sb.buffer-next;
	  let ei :: <buffer-index> = sb.buffer-end;
	  if (bi >= ei)
	    loop(do-next-input-buffer(stream))
	  else
	    // Scan for a newline, then copy the whole line
	    for (i :: <buffer-index> from bi below ei, until: matched?)
	      let elt = buffer-ref(sb, i);
	      if (elt == nl | elt == rt)
		extend-line(sb, i, bi);
		matched? := #t;
		sb.buffer-next := i + 1;
		if (elt == rt)
		  // If '\r' is followed by '\n', eat the '\n'
		  // Too bad we have to do 'peek' the hard way...
		  let ni :: <buffer-index> = sb.buffer-next;
		  let nsb :: false-or(<buffer>) = sb;
		  if (ni >= ei)
		    nsb := do-next-input-buffer(stream);
		    if (nsb)
		      sb := nsb;
		      ni := nsb.buffer-next
		    end if
		  end if;
		  let peek-elt = nsb & buffer-ref(nsb, ni);
		  if (peek-elt == nl)
		    nsb.buffer-next := ni + 1
		  end if
		end if
	      end if
	    end for;
	    // Extend line and update buffer if we didn't find a newline
	    unless (matched?)
	      extend-line(sb, ei, bi);
	      sb.buffer-next := ei
	    end unless;
	    loop(sb)
	  end if;
	end if;
      end iterate;
    end with-input-buffer;
    values(line, matched?)
  end if
end method read-line;

define method read-line-into!
    (stream :: <buffered-stream>, string :: <string>,
     #key start :: <integer> = 0, on-end-of-stream = unsupplied(), grow? = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  let ssize :: <integer> = string.size;
  let index :: <integer> = start;
  let overflow :: false-or(<stretchy-vector>) = #f;
  local method add-with-overflow (elt)
	  if (grow? & index >= ssize)
	    unless (overflow)
	      overflow := make(<stretchy-vector>,
			       size: max(0, start - ssize),
			       fill: ' ');
	    end unless;
	    add!(overflow, elt);
	  else
	    string[index] := elt;
	    index := index + 1
	  end
	end method;
  // Same deal as 'read-line'
  if (stream-at-end?(stream))
    if (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
		 format-string: 
		   "Can't set position of closed stream"));
    else       
      values(end-of-stream-value(stream, on-end-of-stream), #f)
    end if;
  else 
    let matched? = #f;
    // This assumes that the elements of <buffer> are <byte>s.  But
    // that assumption is shot through this code so talley-ho...
    let nl = as(<byte>, '\n');
    let rt = as(<byte>, '\r');
    with-input-buffer (sb = stream)
      iterate loop (sb :: false-or(<buffer>) = sb)
        if (sb & ~matched?)
	  let sb :: <buffer> = sb; // HACK: TYPE ONLY
	  let bi :: <buffer-index> = sb.buffer-next;
	  let ei :: <buffer-index> = sb.buffer-end;
	  if (bi >= ei)
	    loop(do-next-input-buffer(stream))
	  else
	    let next-elt = buffer-ref(sb, bi);
	    sb.buffer-next := bi + 1;
	    if (next-elt == nl)
	      matched? := #t
	    elseif (next-elt == rt)
	      matched? := #t;
	      let ni :: <buffer-index> = sb.buffer-next;
	      let nsb :: false-or(<buffer>) = sb;
	      if (ni >= ei)
		nsb := do-next-input-buffer(stream);
		if (nsb)
		  sb := nsb;
		  ni := sb.buffer-next
		end
	      end;
	      let peek-elt = nsb & buffer-ref(nsb, ni);
	      if (peek-elt == nl)
		nsb.buffer-next := ni + 1
	      end
	    else
	      add-with-overflow(next-elt)
	    end if;
	    loop(sb)
	  end
	end if
      end iterate
    end with-input-buffer;
    if (overflow)
      string := concatenate(string, overflow);
    end;
    values(string, matched?)
  end
end method read-line-into!;


/// "High performance" functions

// Skip over the next n elements, even if the stream isn't positionable
define method read-skip
    (stream :: <buffered-stream>, n :: <integer>) => ()
  if (n > 0)
    with-input-buffer (sb = stream)
      let i :: <integer> = 0;
      let e :: <integer> = n;
      while (sb & (i < e))
	let sb :: <buffer> = sb; // HACK: TYPE ONLY
	let bi :: <buffer-index> = sb.buffer-next;
	let ei :: <buffer-index> = sb.buffer-end;
	if (bi >= ei)
	  sb := do-next-input-buffer(stream)
	else
	  let count = min(ei - bi, e - i);
	  i := i + count;
	  sb.buffer-next := bi + count
	end
      end
    end
  end
end method read-skip;



/// Writable stream protocol
// This all uses the new buffer-dirty? flag.  Although double buffered
// streams ignores this flag do-force-output-buffers.  File streams and
// multi-buffered streams do use the flag so it must be set consistently.

define method write-element
    (stream :: <buffered-stream>, elt :: <object>) => ()
  with-output-buffer (sb = stream)
    let sb :: <buffer> = sb; // HACK: TYPE ONLY
    let bi :: <buffer-index> = sb.buffer-next;
    coerce-from-element(stream, sb, bi, elt);
    sb.buffer-next := bi + 1;
    sb.buffer-end  := max(bi + 1, sb.buffer-end);
    sb.buffer-dirty? := #t;
  end
end method write-element;

define method write
    (stream :: <buffered-stream>, elements :: <sequence>,
     #key start: _start :: <integer> = 0, end: _end = #f) => ()
  with-stream-locked(stream)
  with-output-buffer (sb = stream)
    let e :: <integer> = _end | elements.size;
    iterate loop (i :: <integer> = _start, sb :: false-or(<buffer>) = sb)
      if (sb & i < e)
	let sb :: <buffer> = sb; // HACK: TYPE ONLY
	let bi :: <buffer-index> = sb.buffer-next;
	let bufsiz :: <buffer-index> = sb.buffer-size;
	if (bi >= bufsiz)
	  loop(i, do-next-output-buffer(stream))
	else
	  let count :: <buffer-index> = min(bufsiz - bi, e - i);
	  coerce-from-sequence(stream, sb, bi, elements, i, count);
	  sb.buffer-dirty? := #t;
	  sb.buffer-next := bi + count;
	  sb.buffer-end  := max(bi + count, sb.buffer-end);
	  loop(i + count, sb)
	end
      else
	if (i < e)
	  signal(make(<end-of-stream-error>, stream: stream))
	end
      end
    end iterate
  end
  end
end method write;


define method write-line
    (stream :: <buffered-stream>, elements :: <string>,
     #key start: _start :: <integer> = 0, end: _end = #f) => ()
  with-output-buffer (sb = stream)
    local method write-elts (elts :: <string>, i :: <integer>, e :: <integer>)
	    iterate loop (i :: <integer> = i, sb :: false-or(<buffer>) = sb)
	      if (sb & i < e)
                /* ---*** There used to be a line here:

                let sb :: <buffer> = sb; // HACK: TYPE ONLY

                This was obviously intended to nail down the type of sb.
                Now for some reason this led to a type estimate of "<bottom>"
                in the call to coerce-from-sequence further down. Furthermore,
                type inference should find out that sb is not #f all on itself,
                because it is used in the condition of the if.

                I suspect type inference for if statements and their condition
                variables to be broken.  Needs to be researched and fixed.  I'm
                taking out the quoted line to get rid of a serious warning that
                might scare users.

                -- Andreas Bogk, Oct 2005 */
                  
		let bi :: <buffer-index> = sb.buffer-next;
		let bufsiz :: <buffer-index> = sb.buffer-size;
		if (bi >= bufsiz)
		  loop(i, do-next-output-buffer(stream))
		else
		  let count :: <buffer-index> = min(bufsiz - bi, e - i);
                  coerce-from-sequence(stream, sb, bi, elts, i, count);
		  sb.buffer-dirty? := #t;
		  sb.buffer-next := bi + count;
		  sb.buffer-end  := max(bi + count, sb.buffer-end);
		  loop(i + count, sb)
		end
              else
                if (i < e)
                  signal(make(<end-of-stream-error>, stream: stream))
                end
              end
	    end iterate
          end method;
    write-elts(elements, _start, _end | elements.size);
    write-elts(stream.newline-sequence, 0, size(stream.newline-sequence))
  end
end method write-line;


// Write n copies of the given element

define method write-fill
    (stream :: <buffered-stream>, elt :: <object>, n :: <integer>) => ()
  with-output-buffer (sb = stream)
    let e :: <integer> = n;
    iterate loop (i :: <integer> = 0, sb :: false-or(<buffer>) = sb)
      if (sb & i < e)
	let sb :: <buffer> = sb; // HACK: TYPE ONLY
	let bi :: <buffer-index> = sb.buffer-next;
	let bufsiz :: <buffer-index> = sb.buffer-size;
	if (bi >= bufsiz)
	  loop(i, do-next-output-buffer(stream))
	else
	  let count = min(bufsiz - bi, e - i);
	  let rep = stream.from-element-mapper(elt);
	  buffer-fill(sb, rep, start: bi, end: bi + count);
	  sb.buffer-dirty? := #t;
	  sb.buffer-next := bi + count;
	  sb.buffer-end  := max(bi + count, sb.buffer-end);
	  loop(i + count, sb)
	end
      end
    end iterate
  end
end method write-fill;

