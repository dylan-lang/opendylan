Module:       streams-internals
Synopsis:     Implementation of multi-buffered streams for dood.
Author:       Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed primary class <buffer-vector> (<object>)
  slot size-for-buffers :: <integer>; // all the same size
  slot buffer-shift-amount :: <integer>;
  slot buffer-vector-size-mask :: <integer>;
  slot buffer-preemption-index :: <integer> = 0;
  slot buffers :: <simple-object-vector>; 
  // = limited(<vector>, of: <power-of-two-buffer>);
end class;

// $empty-buffer is used as fill for the buffers (vector) slot. It is
// important for the use count to be 3 so that uninitialized buffers are
// correctly preempted.
define constant $empty-buffer =
  make-<power-of-two-buffer>(size: 2, known-power-of-two-size: #t, 
                             use-count: 3);

define constant $free-stream-ids :: <object-deque> = make(<deque>);
define constant $stream-id-to-stream-map :: <stretchy-object-vector>
  = make(<stretchy-object-vector>);

define function new-stream-id(the-stream :: <multi-buffered-stream>) 
 => (new-id :: <integer>)
  let new-id =
    if ( ~ empty?($free-stream-ids))
      pop($free-stream-ids);
    else
      let new-id = size($stream-id-to-stream-map);
      size($stream-id-to-stream-map) := new-id + 1;
      new-id
    end if;
  $stream-id-to-stream-map[new-id] := the-stream;
  new-id
end function; 
 
define constant <buffer-map-entry> = <integer>;
define constant <buffer-map> = limited(<stretchy-vector>, of: <buffer-map-entry>,
                                       default-fill: 0);

define constant $buffer-map-index-size = 24; // TODO: MACHINE INDEP

define inline function buffer-map-entry-empty?
    (bme :: <buffer-map-entry>) => (res :: <boolean>)
  ~logbit?(0, bme)
end function;

define inline function buffer-map-entry-deposit-empty
    (v :: <boolean>, bme :: <buffer-map-entry>)
  logbit-deposit(~v, 0, bme)
end function;

define inline function buffer-map-entry-dirty?
    (bme :: <buffer-map-entry>) => (res :: <boolean>)
  logbit?(1, bme)
end function;

define inline function buffer-map-entry-deposit-dirty
    (v :: <boolean>, bme :: <buffer-map-entry>)
  logbit-deposit(v, 1, bme)
end function;

define inline function buffer-map-entry-index
    (bme :: <buffer-map-entry>) => (res :: <integer>)
  bit-field-extract(2, $buffer-map-index-size, bme)
end function;

define inline function buffer-map-entry-deposit-index
    (i :: <integer>, bme :: <buffer-map-entry>)
  buffer-map-entry-deposit-dirty
    (#t, buffer-map-entry-deposit-empty
           (#f, bit-field-deposit(i, 2, $buffer-map-index-size, bme)))
end function;

define sealed primary class <multi-buffered-stream> (<file-stream>)
  constant slot buffer-map :: <buffer-map> = make(<buffer-map>);
  slot buffer-vector :: <buffer-vector>;
  slot stream-id :: false-or(<integer>);
end class;

define sealed primary class <general-multi-buffered-stream>
    (<multi-buffered-stream>, <general-typed-stream>)
  inherited slot stream-element-type = <character>;
  keyword encoding:;
end class;

define sealed primary class <byte-char-multi-buffered-stream>
    (<multi-buffered-stream>, <byte-char-element-stream>)
  inherited slot stream-element-type = <byte-character>;
  keyword encoding:;
end class;

define sealed primary class <byte-multi-buffered-stream>
    (<multi-buffered-stream>, <byte-element-stream>)
  inherited slot stream-element-type = <byte>;
  keyword encoding:;
end class;


/// Creating file streams

define sealed method initialize
    (the-vector :: <buffer-vector>, #rest initargs,
     #key buffer-size: requested-buffer-size :: false-or(<integer>) = #f,
     number-of-buffers: requested-number-of-buffers :: <integer> = 32,
     stream :: false-or(<external-stream>) = #f) => ()
  next-method();
  let (buffer-size, shift-amount) = 
    round-to-power-of-two( if (requested-buffer-size) 
                             round-to-power-of-two(requested-buffer-size)
                           elseif (stream)
                             accessor-preferred-buffer-size(stream.accessor)
                           else 1024 end);
  the-vector.size-for-buffers := buffer-size;
  the-vector.buffer-shift-amount := - shift-amount;
  let number-of-buffers =
    round-to-power-of-two(requested-number-of-buffers);
  the-vector.buffers := make(<simple-object-vector>,
                             // limited(<vector>, of: <power-of-two-buffer>);
                             size: number-of-buffers,
                             fill: $empty-buffer);
  the-vector.buffer-vector-size-mask := number-of-buffers - 1;
end method initialize;

define method multi-buffered-stream-direction 
    (direction :: false-or(<symbol>)) => (res :: <symbol>)
  if (direction == #"input")
    #"input"
  else
    #"input-output"
  end if
end method;

define sealed method initialize
    (the-stream :: <multi-buffered-stream>,
     #rest initargs,
     #key direction = #"input-output",
          buffer-vector: input-buffer-vector :: false-or(<buffer-vector>) = #f,
          buffer-size: requested-buffer-size = #f,
          number-of-buffers: requested-number-of-buffers = 32) => ()
  let direction = multi-buffered-stream-direction(direction);
  apply(next-method, the-stream, direction: direction, initargs);
  // This is wrong, shouldn't really be subclass of file streams sigh.  The
  // initialize method creates the shared buffer which we don't want.
  the-stream.stream-shared-buffer := #f; // Get rid of the bum
  // Have to calculate the size again to get the shift amount--yuck
  the-stream.buffer-vector :=   
    if (input-buffer-vector) 
      input-buffer-vector
    else 
      make(<buffer-vector>,
           stream: the-stream, 
           buffer-size: requested-buffer-size, 
           number-of-buffers: requested-number-of-buffers);
    end if;
  the-stream.stream-id := new-stream-id(the-stream);
end method initialize;

define sealed method make 
    (class == <multi-buffered-stream>, #rest initargs,
     #key locator, direction, element-type = <byte-character>, encoding)
 => (stream :: <multi-buffered-stream>)
  let type
    = apply(type-for-multi-buffered-stream, locator, element-type, 
            encoding, initargs);
  let direction = multi-buffered-stream-direction(direction);
  if (type == class)
    apply(next-method, class, direction: direction, initargs)
  else
    apply(make, type, direction: direction, initargs)
  end
end method make;

define sealed generic type-for-multi-buffered-stream
    (locator :: <object>,
     element-type :: false-or(<type>), encoding :: <object>,
     #key, #all-keys)
 => (multi-buffered-stream-type /* ---*** :: subclass(<multi-buffered-stream>) */);

define sealed method type-for-multi-buffered-stream 
    (locator :: <object>,
     element-type :: false-or(<type>), encoding :: <object>,
     #key)
 => (multi-buffered-stream-type /* ---*** :: subclass(<multi-buffered-stream>) */)
  <general-multi-buffered-stream>
end method type-for-multi-buffered-stream;

define sealed method type-for-multi-buffered-stream 
    (locator :: <object>,
     element-type == <byte-character>, encoding :: <object>,
     #key)
 => (multi-buffered-stream-type /* ---*** :: subclass(<multi-buffered-stream>) */)
  <byte-char-multi-buffered-stream>
end method type-for-multi-buffered-stream;

//---*** This equates <character> with <byte-character>.  Hmm...
define sealed method type-for-multi-buffered-stream 
    (locator :: <object>,
     element-type == <character>, encoding :: <object>,
     #key)
 => (multi-buffered-stream-type /* ---*** :: subclass(<multi-buffered-stream>) */)
  <byte-char-multi-buffered-stream>
end method type-for-multi-buffered-stream;

define sealed method type-for-multi-buffered-stream 
    (locator :: <object>,
     element-type  == <byte>, encoding :: <object>,
     #key)
 => (multi-buffered-stream-type /* ---*** :: subclass(<multi-buffered-stream>) */)
  <byte-multi-buffered-stream>
end method type-for-multi-buffered-stream;


/// File stream implementation

define sealed method stream-at-end?
    (stream :: <multi-buffered-stream>) => (at-end? :: <boolean>)
  stream-position(stream) >= stream.stream-size
end method stream-at-end?;

define sealed method stream-input-available?
    (stream :: <multi-buffered-stream>) => (available? :: <boolean>)
  if (stream.stream-open?)
    stream-position(stream) < stream.stream-size
  else
    #f
  end
end method stream-input-available?;

define sealed method stream-size 
    (the-stream :: <multi-buffered-stream>) => (the-size :: <integer>);
  if (the-stream.stream-open?)
  // If the last buffer for this stream is paged in and modified then
  // use the maximum of buffer-end for that last buffer and the
  // accessor file size, otherwise use the accessor file size.
  let last-buffer :: false-or(<power-of-two-buffer>) = 
    if ((the-stream.buffer-map.size > 0) 
          & ~buffer-map-entry-empty?(the-stream.buffer-map.last)) 
      let index = buffer-map-entry-index(the-stream.buffer-map.last);
      the-stream.buffer-vector.buffers[index] 
    end if;
  if (last-buffer & last-buffer.buffer-dirty?)
    let last-buffer :: <buffer> = last-buffer; // HACK: TYPE ONLY
    max(last-buffer.buffer-position + last-buffer.buffer-end,
          the-stream.accessor.accessor-size)
  else
      the-stream.accessor.accessor-size
  end if
  else
    error(make(<stream-closed-error>, stream: the-stream,
               format-string: "Cant get the size of a closed stream"));
  end;
end method;


/// Aligned power of two buffer methods

// Helper function for filling buffers from the accessor.
// still good load-buffer, load-buffer-and-fill
// Note that the setter methods are never used.

define sealed method stream-shared-buffer-setter
    (value ::  false-or(<buffer>), stream :: <multi-buffered-stream>)
 => (input-buffer :: false-or(<buffer>))
  next-method();
  stream.actual-stream-input-buffer := value;
  stream.actual-stream-output-buffer := value;
end method;

define sealed method stream-input-buffer-setter
    (value ::  false-or(<buffer>), stream :: <multi-buffered-stream>)
 => (input-buffer :: false-or(<buffer>))
  stream.stream-shared-buffer := value;
end method;

define sealed method stream-output-buffer-setter
    (value :: false-or(<buffer>), stream :: <multi-buffered-stream>)
 => (input-buffer :: false-or(<buffer>))
  stream.stream-shared-buffer := value;
end method;

// Only calls if stream-input-buffer is false, (stream-input-buffer is eq
// to stream-shared-buffer)
define sealed method do-get-input-buffer
    (the-stream :: <multi-buffered-stream>, 
     #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  let(the-buffer, eof?) = 
    do-get-buffer(the-stream, wait?: wait?, bytes: bytes);
  stream-shared-buffer(the-stream) := the-buffer;
  the-buffer.buffer-use-count := 0;
  if (eof?) #f else the-buffer end
end method do-get-input-buffer;

define sealed method do-get-buffer (the-stream :: <multi-buffered-stream>,
                             #key wait? = #t, bytes = 1)
 => (the-buffer :: <power-of-two-buffer>, eof? :: <boolean>)
  let the-position :: <integer> = the-stream.stream-position; // current-position?
  let eof? = #f;
  let buffer-vector :: <buffer-vector> = the-stream.buffer-vector;
  let buffer-map-index :: <buffer-index>
    = ash(the-position, buffer-vector.buffer-shift-amount);
  let buffer-map :: <buffer-map> = the-stream.buffer-map;
  // Make sure we have a map entry
  unless (buffer-map-index < buffer-map.size)
    grow-buffer-map(the-stream, buffer-map-index);
  end unless;
  // Get a valid buffer index
  let map-entry = buffer-map[buffer-map-index];
  let buffer-index :: <buffer-index> 
    = if (buffer-map-entry-empty?(map-entry))
        let buffer-index = 
          preempt-buffer(the-stream, the-position, wait?: wait?, bytes: bytes);
        unless (buffer-map-entry-dirty?(map-entry))
          *multi-buffer-working-set* := *multi-buffer-working-set* + 1;
        end unless;
        buffer-map[buffer-map-index]
          := buffer-map-entry-deposit-index(buffer-index, map-entry);
        buffer-index
      else 
        buffer-map-entry-index(map-entry)
      end;
  let the-buffer :: <power-of-two-buffer>
    = buffer-vector.buffers[buffer-index];
  // set buffer-next
  let new-buffer-next :: <integer> = 
    logand(the-buffer.buffer-on-page-bits, the-position);
  the-buffer.buffer-next := new-buffer-next;
  if (new-buffer-next > the-buffer.buffer-end)
    // we are creating a new end of file by setting the position off the
    // end of file.
    the-buffer.buffer-dirty? := #t;
    the-buffer.buffer-end := new-buffer-next;
    eof? := #t;
  elseif (new-buffer-next = the-buffer.buffer-end)
    eof? := #t;
  end if;
  values(the-buffer, eof?)
end method;

define function grow-buffer-map 
    (the-stream :: <multi-buffered-stream>, new-index :: <integer>)
 => ()
  // This would be a whole lot simpler if I could just set the buffer-map size
  // and be sure that the fill would be #f.
  iterate loop (map-index :: <integer> = the-stream.buffer-map.size)
    unless (map-index > new-index)
      add!(the-stream.buffer-map, 
           buffer-map-entry-deposit-empty
             (#t, buffer-map-entry-deposit-dirty(#f, 0)));
      loop(map-index + 1)
    end unless;
  end iterate;
end function;

define function multi-buffer-total-bytes () => (res :: <integer>)
  *multi-buffer-bytes*
end function;

define variable *multi-buffer-reads* :: <integer> = 0;

define function multi-buffer-total-reads () => (res :: <integer>)
  *multi-buffer-reads*
end function;

define variable *multi-buffer-working-set* :: <integer> = 0;

define function multi-buffer-total-working-set () => (res :: <integer>)
  *multi-buffer-working-set*
end function;

define method multi-buffer-reads (stream :: <multi-buffered-stream>) => (res :: <integer>)
  // TODO
  multi-buffer-total-reads()
end method;

define method multi-buffer-bytes (stream :: <multi-buffered-stream>) => (res :: <integer>)
  // TODO
  multi-buffer-total-bytes()
end method;

define method multi-buffer-working-set (stream :: <multi-buffered-stream>) => (res :: <integer>)
  let count :: <integer> = 0;
  for (map-entry in stream.buffer-map)
    when (buffer-map-entry-dirty?(map-entry))
      count := count + 1;
    end when;
  end for;
  count
end method;

// Find an empty buffer slot or else preempt an existing buffer, load the
// buffer from the underlying file and return the index of buffer in the
// buffer-vector vector. 
define function preempt-buffer
    (the-stream :: <multi-buffered-stream>, the-position :: <integer>, 
     #key  wait?,  bytes)
 => (buffer-index :: <integer>);
  if (closed?(the-stream))
    error(make(<stream-closed-error>, stream: the-stream,
               format-string: 
                   "Stream closed: Can't read, write or set-position"));
  end if;
  let the-buffer-vector :: <buffer-vector> = the-stream.buffer-vector;
  let buffer-index :: <integer> = 
    logand(the-buffer-vector.buffer-preemption-index + 1, 
           the-buffer-vector.buffer-vector-size-mask);
  let the-buffer :: <power-of-two-buffer> = 
    the-buffer-vector.buffers[buffer-index];
  if (the-buffer.buffer-use-count < 3) 
    the-buffer.buffer-use-count := the-buffer.buffer-use-count + 1;
  end if;
  let (buffer-index :: <integer>, the-buffer :: <power-of-two-buffer>)
    = iterate loop (index :: <integer> = buffer-index,
                    buffer :: <power-of-two-buffer> = the-buffer)
        if (buffer.buffer-use-count = 3)
          values(index, buffer)
        else
          let buffer-index :: <buffer-index> =
            logand(index + 1, 
                   the-stream.buffer-vector.buffer-vector-size-mask);
          let the-buffer :: <power-of-two-buffer> =
            the-buffer-vector.buffers[buffer-index];
          if (the-buffer.buffer-use-count < 3)
            the-buffer.buffer-use-count := the-buffer.buffer-use-count + 1;
          end if;
          loop(buffer-index, the-buffer)
        end if
      end iterate;
  the-stream.buffer-vector.buffer-preemption-index := buffer-index;
  let new-buffer-position :: <integer> = 37; // doesn't matter
  if (the-buffer == $empty-buffer)
    the-buffer :=
      make-<power-of-two-buffer>
        (stream-id: the-stream.stream-id,
         size: the-stream.buffer-vector.size-for-buffers, 
         known-power-of-two-size?: #t, fill: 0);
    the-buffer-vector.buffers[buffer-index] := the-buffer;
    new-buffer-position := logand(the-buffer.buffer-off-page-bits, the-position); 
    let start = 0; let the-size = the-buffer.buffer-size;
    load-buffer(the-stream, the-buffer, new-buffer-position, start, the-size);
  else
    // Zero out the buffer-map entry in the stream which currently
    // owns the we are preempting. Of course maybe nobody owns the
    // buffer any more if the owning stream has been closed so we must
    // check first.
    if (the-buffer.buffer-owning-stream)
      let the-owning-stream =
        $stream-id-to-stream-map[the-buffer.buffer-owning-stream];
      let map-index
        = ash(the-buffer.buffer-position,
              the-stream.buffer-vector.buffer-shift-amount);
      let buffer-map = the-owning-stream.buffer-map;
      buffer-map[map-index] 
        := buffer-map-entry-deposit-empty(#t, buffer-map[map-index]);
      // Zero out the stream-shared-buffer in the stream which currently
      // owns the buffer we are preempting if the stream-shared-buffer is
      // the preempted buffer.  This can't happen unless the
      // buffer-vector is being shared by more than one stream.
      if (the-owning-stream.stream-shared-buffer == the-buffer)
        // First save the position
        the-owning-stream.current-position 
          := stream-position(the-owning-stream);
        the-owning-stream.stream-shared-buffer := #f;
      end if;
      if (the-buffer.buffer-dirty? & read-only?(the-owning-stream))
        error(make(<stream-not-writable>, stream: the-owning-stream,
                   format-string: 
                     "Internal error: buffer for read-only" 
                     "<multi-buffered-stream> (dood stream) was"
                     "modified, can't write the modified buffer."
                     ));
      end if;
      force-buffer(the-buffer, the-owning-stream);
    end if;
    // Claim the buffer for the current stream.
    the-buffer.buffer-owning-stream := the-stream.stream-id;
    new-buffer-position := logand(the-buffer.buffer-off-page-bits, the-position); 
    let start = 0; let the-size = the-buffer.buffer-size;
    load-buffer-and-fill(the-stream, the-buffer, new-buffer-position, start,
                         the-size);
  end if;
  *multi-buffer-reads* := *multi-buffer-reads* + 1;
  the-buffer.buffer-position := new-buffer-position;
  the-buffer.buffer-start := 0;
  the-buffer.buffer-dirty? := #f;
  the-buffer.buffer-use-count := 0;
  buffer-index
end function;

// This is only called when buffer-next is equal to buffer-end, that is
// when buffer-next has been incremented 1 past the end of buffer by
// read-element.  
define sealed method do-next-input-buffer
    (the-stream :: <multi-buffered-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  ignore(wait?, bytes);
  let last-buffer :: <buffer> = stream-input-buffer(the-stream);
  the-stream.stream-position := 
    last-buffer.buffer-position + last-buffer.buffer-end;
  let(new-buffer, eof?) = 
    do-get-buffer(the-stream, wait?: wait?, bytes: bytes);
  stream-shared-buffer(the-stream) := new-buffer;
  new-buffer.buffer-use-count := 0;
  if (eof?) #f else new-buffer end
end method;



//  We get here only when 'stream-output-buffer' is #f.
define sealed method do-get-output-buffer
    (the-stream :: <multi-buffered-stream>, #key bytes = 1)
 => (the-buffer :: <buffer>)
  let (the-buffer, eof?) = do-get-buffer(the-stream, bytes: bytes);
  ignore(eof?);
  stream-shared-buffer(the-stream) := the-buffer;
  the-buffer.buffer-use-count := 0;
  the-buffer
end method do-get-output-buffer;

// This can only be called when 'stream-output-buffer' has a buffer in
// it.  
define sealed method do-next-output-buffer
    (the-stream :: <multi-buffered-stream>, #key bytes = 1)
 => (the-buffer :: <buffer>)
  ignore(bytes);
  let last-buffer :: <buffer> = stream-input-buffer(the-stream);
  the-stream.stream-position := 
    last-buffer.buffer-position + last-buffer.buffer-end;
  let(new-buffer, eof?) = 
    do-get-buffer(the-stream, bytes: bytes);
  ignore(eof?);
  stream-shared-buffer(the-stream) := new-buffer;
  new-buffer.buffer-use-count := 0;
  new-buffer
end method;

// Helper functions for forcing output buffers.  Since output buffers can
// change to input buffers without been synched it is necessary to force
// out any dirty buffer regardless of whether it is input or output.
define sealed method force-output-buffers
    (stream :: <multi-buffered-stream>) => ()
  unless (empty?(stream.buffer-map)) 
    let map-entry       = stream.buffer-map.last;
    let map-entry-index = buffer-map-entry-index(map-entry);
    when (~buffer-map-entry-empty?(map-entry)
            & stream.buffer-vector.buffers[map-entry-index].buffer-dirty?
            & stream.buffer-vector.buffers[map-entry-index].buffer-end = 0)
      // special case set beginning position to last buffer but didn't write
      // anything into that buffer.  Make sure that the previous buffer is
      // marked dirty
      stream-position(stream) := 
        stream.buffer-vector.buffers[map-entry-index].buffer-position - 1;
      let previous-buffer = do-get-buffer(stream);
      previous-buffer.buffer-dirty? := #t;
    end when;
  end unless;
  // Sort the dirty buffers by increasingIbuffer position to minimize
  // disk head movement.
  let sordid-buffers :: <stretchy-vector> = make(<stretchy-vector>);
  for (buffer in stream.buffer-vector.buffers)
    if ((buffer.buffer-owning-stream == stream.stream-id) & buffer.buffer-dirty?)
      sordid-buffers := add!(sordid-buffers, buffer);
    end;
  end for;
  if (read-only?(stream) & ~empty? (sordid-buffers))
    error(make(<stream-not-writable>, stream: stream,
               format-string: 
                 "Internal error closing stream: buffer for read-only" 
                 "<multi-buffered-stream> (dood stream) was"
                 "modified. Can't write the modified buffer."
                 ));
  else 
    sordid-buffers := 
      sort!(sordid-buffers, 
            test:
              method (buffer-1 :: <buffer>, buffer-2 :: <buffer>)
                buffer-1.buffer-position < buffer-2.buffer-position
              end method);
    for (buffer in sordid-buffers)
      force-buffer(buffer, stream);
    end for;
  end if;

  values()
end method force-output-buffers;

define method close
    (stream :: <multi-buffered-stream>,
     #rest keys, #key abort? = #f, wait?, synchronize?) => ()
  ignore(wait?, synchronize?);
  if (stream-open?(stream))
    if (abort?)
      for (buffer in stream.buffer-vector.buffers)
        if (buffer.buffer-owning-stream == stream.stream-id)
          buffer.buffer-dirty? := #f;
        end if;
      end for;
    end if;
    next-method();
    // Now force all the stream-ids for buffers in the streams buffer
    // vector to be #f (and remember to check for false buffers everywhere)
    for (buffer in stream.buffer-vector.buffers)
      if (buffer.buffer-owning-stream == stream.stream-id)
        buffer.buffer-owning-stream := #f;
      end;
    end for;
    $stream-id-to-stream-map[stream.stream-id] := #f;
    push($free-stream-ids, stream.stream-id);
    stream.stream-id := #f;
    // now zero out the buffer map
    for (map-index from 0 below stream.buffer-map.size)
      stream.buffer-map[map-index]
        := buffer-map-entry-deposit-empty(#t, stream.buffer-map[map-index]);
    end for;
    // Now zero out the stream shared buffer buffers so that any attempt
    // to use the stream forces a call to do-get-buffer which in turn
    // will call preempt-buffer because we just zeroed out all of the
    // buffer-map entries.  Preempt buffer will figure out what is
    // wrong if anybody tries to read or write to the stream.
   stream.stream-shared-buffer := #f;   
  end if;
end method close;

define sealed method do-force-output-buffers
    (the-stream :: <multi-buffered-stream>) => ()
  force-output-buffers(the-stream);
  values()
end method do-force-output-buffers;

/// Positioning methods
define sealed method stream-position
    (stream :: <multi-buffered-stream>) => (position :: <integer>)
  if(stream.stream-open?)
  if (stream-shared-buffer(stream))
    stream-shared-buffer(stream).buffer-position
      + stream-shared-buffer(stream).buffer-next 
  else
    stream.current-position
  end
  else
    error(make(<stream-closed-error>, stream: stream,
               format-string: "Cant get the position of a closed stream"));
  end
end method stream-position;

// Optimize this for setting position to current buffer or to current position
define sealed method stream-position-setter
    (the-position :: <integer>, stream :: <multi-buffered-stream>)
 => (the-position :: <integer>)
  let size-of-stream :: <integer> = stream-size(stream);
  if ((the-position >= 0) & (the-position <= size-of-stream))
    // Don't call next-method() it just figures out the error cases again
    stream.current-position := the-position;
    stream-shared-buffer(stream) := #f;
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
  the-position
end method;

// Special for dood - avoids the overhead of adjust-stream-position, but
// allows setting the position to extend the file, unlike
// stream-position-setter.
define function multi-buffered-stream-position-setter
    (the-position :: <integer>, stream :: <multi-buffered-stream>)
 => (the-position :: <integer>)
  if (stream-shared-buffer(stream))
    let the-buffer :: <buffer> = stream-shared-buffer(stream);
    if((the-buffer.buffer-position + the-buffer.buffer-next) == the-position)
      /* do nothing, already there*/
    elseif (logand(the-buffer.buffer-off-page-bits, the-position)
              == the-buffer.buffer-position)
        /* set position to same page */
        the-buffer.buffer-next 
          := logand(the-buffer.buffer-on-page-bits, the-position);
    elseif (the-position >= 0)
      stream.current-position := the-position;
      stream-shared-buffer(stream) := #f;
    else 
      if (closed?(stream))
        error(make(<stream-closed-error>, stream: stream,
                   format-string: 
                     "Can't set position of closed stream"));
      else       
        error(make(<stream-position-error>, stream: stream, 
                   size: stream.accessor.accessor-size, position: position));
      end if;
    end if;
  elseif (the-position >= 0)
    stream.current-position := the-position;
  else 
    if (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
                 format-string: 
                   "Can't set position of closed stream"));
    else       
      error(make(<stream-position-error>, stream: stream, 
                 size: stream.accessor.accessor-size, position: position));
    end if; 
  end if;
  the-position
end function;

define sealed method adjust-stream-position
    (stream :: <multi-buffered-stream>, delta :: <integer>,
     #key from = #"current")
 => (position :: <integer>)
  let size-of-stream :: <integer> = stream-size(stream);
  let position-from-start
    = select (from)
        #"current" => stream-position(stream) + delta;
        #"start"   => delta;
        #"end"     => size-of-stream + delta;
      end;
  if (position-from-start < 0)
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
    stream.current-position := position-from-start;
    stream-shared-buffer(stream) := #f;
  end if;
  position-from-start
end method adjust-stream-position;

// These methods really belong to multi-buffered-streams however they
// use low level primitives which differ in the native and emulator
// versions so putting them here is convenient, if a bit of a hack.

// Special read and write methods for dood

// These functions assume that since the buffer is a power of 2 greater
// than 8 and since the write is aligned on a full word boundary, that
// if there is space for the first byte in the buffer than there is
// space for the entire four or eight  bytes.

define inline function write-4-aligned-bytes-from-word
    (stream :: <multi-buffered-stream>, word :: <machine-word>) => ()
  with-output-buffer (sb = stream)
    let sb :: <buffer> = sb; // HACK: TYPE ONLY
    let bi :: <buffer-index> = sb.buffer-next;
    primitive-element
        (primitive-repeated-slot-as-raw(sb, primitive-repeated-slot-offset(sb)),
         integer-as-raw(0), integer-as-raw(bi))
      := primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(word));
    sb.buffer-next := bi + 4;
    sb.buffer-end  := max(bi + 4, sb.buffer-end);
    sb.buffer-dirty? := #t;
  end;
end function write-4-aligned-bytes-from-word;

define inline function read-4-aligned-bytes-as-word
    (stream :: <multi-buffered-stream>,
     #key on-end-of-stream = unsupplied())
 => (word)
  with-input-buffer (sb = stream)
    if (sb)
      let sb :: <buffer> = sb; // HACK: TYPE ONLY
      let bi :: <buffer-index> = sb.buffer-next;
      sb.buffer-next := bi + 4;
      primitive-wrap-machine-word
        (primitive-cast-pointer-as-raw
           (primitive-element
              (primitive-repeated-slot-as-raw(sb, primitive-repeated-slot-offset(sb)),
               integer-as-raw(0), integer-as-raw(bi))))
    else
      error("End of stream in read-4-aligned-bytes");
      as(<machine-word>, 0) // HACK: AVOID BOXING
    end if
  end with-input-buffer
end function read-4-aligned-bytes-as-word;

define function write-4-aligned-bytes
    (stream :: <multi-buffered-stream>, byte-1 :: <integer>, 
     byte-2 :: <integer>, byte-3 :: <integer>, byte-4 :: <integer>) => ()
  with-output-buffer (sb = stream)
    let sb :: <buffer> = sb; // HACK: TYPE ONLY
    let bi :: <buffer-index> = sb.buffer-next;
    without-bounds-checks
      sb[bi] := byte-1; sb[bi + 1] := byte-2; sb[bi + 2] := byte-3;
      sb[bi + 3] := byte-4;
    end without-bounds-checks;
    sb.buffer-next := bi + 4;
    sb.buffer-end  := max(bi + 4, sb.buffer-end);
    sb.buffer-dirty? := #t;
  end;
end function write-4-aligned-bytes;

define function write-8-aligned-bytes
    (stream :: <multi-buffered-stream>, byte-1 :: <integer>, 
     byte-2 :: <integer>, byte-3 :: <integer>, byte-4 :: <integer>,
     byte-5 :: <integer>, byte-6 :: <integer>, byte-7 :: <integer>,
     byte-8 :: <integer>) => ()
  with-output-buffer (sb = stream)
    let sb :: <buffer> = sb; // HACK: TYPE ONLY
    let bi :: <buffer-index> = sb.buffer-next;
    without-bounds-checks
      sb[bi] := byte-1; sb[bi + 1] := byte-2; sb[bi + 2] := byte-3;
      sb[bi + 3] := byte-4; sb[bi + 4] := byte-5; sb[bi + 5] := byte-6;
      sb[bi + 6] := byte-7; sb[bi + 7] := byte-8;
    end without-bounds-checks;
    sb.buffer-next := bi + 8;
    sb.buffer-end  := max(bi + 8, sb.buffer-end);
    sb.buffer-dirty? := #t;
  end
end function write-8-aligned-bytes;

define function read-4-aligned-bytes
    (stream :: <multi-buffered-stream>,
     #key on-end-of-stream = unsupplied())
 => (byte-1 :: <integer>, byte-2 :: <integer>, byte-3 :: <integer>, 
     byte-4 :: <integer>)
  with-input-buffer (sb = stream)
    if (sb)
      *multi-buffer-bytes* := *multi-buffer-bytes* + 4;
      let sb :: <buffer> = sb; // HACK: TYPE ONLY
      let bi :: <buffer-index> = sb.buffer-next;
      sb.buffer-next := bi + 4;
      without-bounds-checks
       values(sb[bi], sb[bi + 1], sb[bi + 2], sb[bi + 3])
      end without-bounds-checks;
    else
      error("End of stream in read-4-aligned-bytes");
    end
  end
end function read-4-aligned-bytes;

define function read-8-aligned-bytes
    (stream :: <multi-buffered-stream>,
     #key on-end-of-stream = unsupplied())
 => (byte-1 :: <integer>, byte-2 :: <integer>, byte-3 :: <integer>, 
     byte-4 :: <integer>, byte-5 :: <integer>, byte-6 :: <integer>,
     byte-7 :: <integer>, byte-8 :: <integer>)
  with-input-buffer (sb = stream)
    if (sb)
      *multi-buffer-bytes* := *multi-buffer-bytes* + 8;
      let sb :: <buffer> = sb; // HACK: TYPE ONLY
      let bi :: <buffer-index> = sb.buffer-next;
      sb.buffer-next := bi + 8;
      without-bounds-checks
        values(sb[bi], sb[bi + 1], sb[bi + 2], sb[bi + 3], sb[bi + 4],
               sb[bi + 5], sb[bi + 6], sb[bi + 7])
      end without-bounds-checks;
    else
      error("End of stream in read-8-aligned-bytes");
    end
  end
end function read-8-aligned-bytes;

