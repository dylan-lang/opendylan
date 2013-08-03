Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Interval streams

define sealed class <interval-stream>
    (<positionable-stream>,
     <basic-stream>)                // needed for ensure-readable, etc.
  sealed constant slot %start-bp :: <basic-bp>,
    required-init-keyword: start-bp:;
  sealed constant slot %end-bp :: <basic-bp>,
    required-init-keyword: end-bp:;
  sealed constant slot %buffer :: <basic-buffer>,
    required-init-keyword: buffer:;
  sealed slot %current-position :: <basic-bp>;
end class <interval-stream>;

define sealed class <repainting-interval-stream> (<interval-stream>)
  sealed constant slot %window :: <window>, required-init-keyword: window:;
end;

define sealed domain make (singleton(<interval-stream>));
define sealed domain initialize (<interval-stream>);

define sealed domain make (singleton(<repainting-interval-stream>));
define sealed domain initialize (<repainting-interval-stream>);

define sealed method make
    (class == <interval-stream>, #rest initargs,
     #key buffer, interval, direction, #all-keys)
 => (stream :: <interval-stream>)
  ignore(direction);
  let (start-bp, end-bp)
    = values(interval-start-bp(buffer | interval), interval-end-bp(buffer | interval));
  let buffer
    = buffer
      | select (interval by instance?)
          <buffer>  => interval;
          otherwise => bp-buffer(start-bp);
        end;
  with-keywords-removed (initargs = initargs, #[interval:])
    apply(next-method, class,
          start-bp: start-bp, end-bp: end-bp,
          buffer: buffer, initargs)
  end
end method make;

define sealed method make
    (class == <repainting-interval-stream>, #rest initargs,
     #key buffer, interval, direction, window, #all-keys)
 => (stream :: <interval-stream>)
  ignore(direction);
  let (start-bp, end-bp)
    = values(interval-start-bp(buffer | interval), interval-end-bp(buffer | interval));
  let buffer
    = buffer
      | select (interval by instance?)
          <buffer>  => interval;
          otherwise => bp-buffer(start-bp);
        end;
  with-keywords-removed (initargs = initargs, #[interval:])
    apply(next-method, class,
          start-bp: start-bp, end-bp: end-bp,
          buffer: buffer, window: window, initargs)
  end
end method make;

define sealed method initialize
    (stream :: <interval-stream>, #key interval, direction) => ()
  ignore(interval);
  next-method();
  select (direction)
    #"input" =>
      stream.%current-position := copy-bp(stream.%start-bp);
    #"output", #"input-output" =>
      stream.%current-position := copy-bp(stream.%end-bp);
  end
end method initialize;


/// Readable stream protocol

define sealed method read-element
    (stream :: <interval-stream>,
     #key on-end-of-stream = $unsupplied)
 => (char :: <object>)
  ensure-readable(stream);
  let bp :: <basic-bp> = stream.%current-position;
  if (bp = stream.%end-bp)
    end-of-stream-value(stream, on-end-of-stream)
  else
    let char = bp-character(bp);
    increment-bp!(bp);
    char
  end
end method read-element;

define sealed method unread-element
    (stream :: <interval-stream>, char :: <object>)
 => (char :: <object>)
  ensure-readable(stream);
  let bp :: <basic-bp> = stream.%current-position;
  unless (bp = stream.%start-bp)
    decrement-bp!(bp)
  end;
  char
end method unread-element;

define sealed method peek
    (stream :: <interval-stream>,
     #key on-end-of-stream = $unsupplied)
 => (char :: <object>)
  ensure-readable(stream);
  let bp :: <basic-bp> = stream.%current-position;
  if (bp = stream.%end-bp)
    end-of-stream-value(stream, on-end-of-stream)
  else
    let char = bp-character(bp);
    char
  end
end method peek;

define sealed method read
    (stream :: <interval-stream>, n :: <integer>,
     #key on-end-of-stream = $unsupplied)
 => (string-or-eof :: <object>)
  ensure-readable(stream);
  let bp1 :: <basic-bp> = stream.%current-position;
  let bp2 :: <basic-bp> = move-over-characters(bp1, n, fixup?: #f);
  let interval = bp2 & make-interval(bp1, bp2, in-order?: #t);
  let n-read   = bp2 & count-characters(interval);
  when (n-read)
    move-bp!(bp1, bp-line(bp2), bp-index(bp2))
  end;
  if (n-read & n-read >= n)
    as(<byte-string>, interval)
  else
    if (unsupplied?(on-end-of-stream))
      signal(make(<incomplete-read-error>,
                  stream: stream,
                  sequence: if (interval) as(<byte-string>, interval) else "" end,
                  count: n-read | 0))
    else
      on-end-of-stream
    end
  end
end method read;

define sealed method read-into!
    (stream :: <interval-stream>, n :: <integer>, dst :: <mutable-sequence>,
     #key start = 0, on-end-of-stream = $unsupplied)
 => (n-read :: false-or(<integer>))
  ensure-readable(stream);
  let bp1 :: <basic-bp> = stream.%current-position;
  let bp2 :: <basic-bp> = move-over-characters(bp1, n, fixup?: #f);
  let limit :: <integer> = size(dst);
  let interval = bp2 & make-interval(bp1, bp2, in-order?: #t);
  let n-read   = bp2 & count-characters(interval);
  when (n-read)
    let i :: <integer> = start;
    block (break)
      do-lines(method (line, si, ei, last?)
                 ignore(last?);
                 let n :: <integer> = ei - si;
                 copy-bytes(dst, i, line-contents(line), si, min(n, limit));
                 inc!(i, n);
                 dec!(limit, n);
                 when (limit <= 0)
                   break()
                 end
               end method, interval)
    end block;
    move-bp!(bp1, bp-line(bp2), bp-index(bp2));
  end;
  if ((n-read & n-read >= limit) | supplied?(on-end-of-stream))
    n-read
  else
    signal(make(<incomplete-read-error>,
                stream: stream,
                sequence: if (interval) as(<byte-string>, interval) else "" end,
                count: n-read | 0))
  end
end method read-into!;

define sealed method read-line
    (stream :: <interval-stream>,
     #key on-end-of-stream = $unsupplied)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  if (stream-at-end?(stream))
    values(end-of-stream-value(stream, on-end-of-stream), #f)
  else
    let bp :: <basic-bp> = stream.%current-position;
    let line  = bp-line(bp);
    let index = bp-index(bp);
    let length   = line-length(line);
    let contents = line-contents(line);
    let string = copy-sequence(contents, start: index, end: length);
    let next = line-next-in-buffer(line, stream.%buffer);
    if (next)
      move-bp!(bp, next, 0)
    else
      move-bp!(bp, bp-line(stream.%end-bp), bp-index(stream.%end-bp))
    end;
    string
  end
end method read-line;

define sealed method read-line-into!
    (stream :: <interval-stream>, string :: <byte-string>,
     #key start = 0, on-end-of-stream = $unsupplied, grow? = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  if (stream-at-end?(stream))
    values(end-of-stream-value(stream, on-end-of-stream), #f)
  else
    let bp :: <basic-bp> = stream.%current-position;
    let line  = bp-line(bp);
    let index = bp-index(bp);
    let length   = line-length(line);
    let contents = line-contents(line);
    case
      length - index <= size(string) - start =>
        copy-bytes(string, start, contents, index, length - index);
      grow? =>
        string := make(type-for-copy(string), size: length - index);
        copy-bytes(string, start, contents, index, length - index);
      otherwise =>
        copy-bytes(string, start, contents, index, size(string) - start);
    end;
    let next = line-next-in-buffer(line, stream.%buffer);
    if (next)
      move-bp!(bp, next, 0)
    else
      move-bp!(bp, bp-line(stream.%end-bp), bp-index(stream.%end-bp))
    end;
    string
  end
end method read-line-into!;

define sealed method stream-input-available?
    (stream :: <interval-stream>) => (available? :: <boolean>)
  stream-direction(stream) ~== #"output"
  & ~stream-at-end?(stream)
end method stream-input-available?;


/// Writable stream protocol

define sealed method write-element
    (stream :: <interval-stream>, char :: <object>) => ()
  ensure-writable(stream);
  let bp :: <basic-bp> = stream.%current-position;
  insert-moving!(bp, char)
end method write-element;

define sealed method write-element
    (stream :: <repainting-interval-stream>, char :: <object>) => ()
  next-method();
  // ---*** The centering argument is a hack, and doesn't belong here
  queue-redisplay(stream.%window, $display-text, centering: 1);
  redisplay-window(stream.%window);
end;

define sealed method write
    (stream :: <interval-stream>, string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string)) => ()
  ensure-writable(stream);
  let bp :: <basic-bp> = stream.%current-position;
  insert-moving!(bp, string, start: _start, end: _end)
end method write;

define sealed method write
    (stream :: <repainting-interval-stream>, string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string)) => ()
  next-method();
  // ---*** The centering argument is a hack, and doesn't belong here
  queue-redisplay(stream.%window, $display-text, centering: 1);
  redisplay-window(stream.%window);
end;


/// Positionable stream protocol

define sealed method stream-position
    (stream :: <interval-stream>) => (position :: <basic-bp>)
  stream.%current-position
end method stream-position;

define sealed method stream-position-setter
    (position :: <basic-bp>, stream :: <interval-stream>)
 => (position :: <basic-bp>)
  move-bp!(stream.%current-position, bp-line(position), bp-index(position));
  position
end method stream-position-setter;

define sealed method stream-position-setter
    (position == #"start", stream :: <interval-stream>)
 => (position :: <basic-bp>)
  stream-position(stream) := stream.%start-bp
end method stream-position-setter;

define sealed method stream-position-setter
    (position == #"end", stream :: <interval-stream>)
 => (position :: <basic-bp>)
  stream-position(stream) := stream.%end-bp
end method stream-position-setter;

define sealed method adjust-stream-position
    (stream :: <interval-stream>, delta :: <integer>,
     #key from = #"current")
 => (position :: <basic-bp>)
  stream-position(stream)
    := select (from)
         #"current" => move-over-characters(stream.%current-position, delta);
         #"start"   => move-over-characters(stream.%start-bp, delta);
         #"end"     => move-over-characters(stream.%end-bp, delta);
       end
end method adjust-stream-position;


/// Stream query and contents accessing

define sealed method stream-at-end?
    (stream :: <interval-stream>) => (at-end? :: <boolean>)
  if (stream-direction(stream) == #"output")
    #f
  else
    stream.%current-position = stream.%end-bp
  end
end method stream-at-end?;

define sealed method stream-size
    (stream :: <interval-stream>) => (size :: <integer>)
  let bp1 = stream.%start-bp;
  let bp2 = stream.%end-bp;
  let interval = make-interval(bp1, bp2, in-order?: #t);
  count-characters(interval)
end method stream-size;

define sealed method stream-contents
    (stream :: <interval-stream>, #key clear-contents? = #t)
 => (contents :: <byte-string>)
  ignore(clear-contents?);
  let bp1 = stream.%start-bp;
  let bp2 = stream.%end-bp;
  let interval = make-interval(bp1, bp2, in-order?: #t);
  as(<byte-string>, interval)
end method stream-contents;
