Module:       streams-internals
Synopsis:     Implementation of abstract wrapper stream classes and defaults
Author:       Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Wrapper streams

define open abstract primary class <wrapper-stream> (<basic-stream>)
  slot %inner-stream :: <stream>,
    required-init-keyword: inner-stream:;
end class <wrapper-stream>;

define sealed class <simple-wrapper-stream> (<wrapper-stream>)
end class <simple-wrapper-stream>;

define sealed inline method make
    (class == <wrapper-stream>, #rest initargs, #key)
 => (stream :: <simple-wrapper-stream>)
  apply(make, <simple-wrapper-stream>, initargs)
end method make;

define sealed domain make (singleton(<simple-wrapper-stream>));
define sealed domain initialize (<simple-wrapper-stream>);


define open generic inner-stream
    (wrapper-stream :: <wrapper-stream>) => (stream :: <stream>);

define method inner-stream
    (wrapper-stream :: <wrapper-stream>) => (stream :: <stream>)
  wrapper-stream.%inner-stream;
end method inner-stream;

define open generic inner-stream-setter 
    (stream :: <stream>, wrapper-stream :: <wrapper-stream>)
 => (stream :: <stream>);

define method inner-stream-setter
    (stream :: <stream>, wrapper-stream :: <wrapper-stream>)
 => (stream :: <stream>)
  wrapper-stream.%inner-stream := stream;
  stream.outer-stream := wrapper-stream;
  stream
end method inner-stream-setter;

define method initialize
    (wrapper-stream :: <wrapper-stream>, #key inner-stream: stream) => ()
  next-method();
  stream.outer-stream := wrapper-stream;
  unless (slot-initialized?(wrapper-stream, outer-stream))
    wrapper-stream.outer-stream := wrapper-stream
  end
end method initialize;


/// The following methods define the core protocols that defer to their
/// underlying stream.  The following methods on non-wrapper-streams do
/// _not_ defer to their outer-stream.

/// Readable stream protocol

define method read-element
    (stream :: <wrapper-stream>, #rest keys, #key on-end-of-stream)
 => (element :: <object>)
  ignore(on-end-of-stream);
  apply(read-element, stream.inner-stream, keys)
end method read-element;

define method unread-element
    (stream :: <wrapper-stream>, elt)
 => (element :: <object>)
  unread-element(stream.inner-stream, elt)
end method unread-element;

define method peek
    (stream :: <wrapper-stream>, #rest keys, #key on-end-of-stream)
 => (element :: <object>)
  ignore(on-end-of-stream);
  apply(peek, stream.inner-stream, keys)
end method peek;

define method read
    (stream :: <wrapper-stream>, n :: <integer>, #rest keys, #key on-end-of-stream)
 => (elements-or-eof :: <object>)
  ignore(on-end-of-stream);
  apply(read, stream.inner-stream, n, keys)
end method read;

define method read-into!
    (stream :: <wrapper-stream>, n :: <integer>, seq :: <mutable-sequence>,
     #rest keys, #key start, on-end-of-stream)
 => (n-read-or-eof :: <object>)
  ignore(start, on-end-of-stream);
  apply(read-into!, stream.inner-stream, n, seq, keys)
end method read-into!;

define method stream-input-available? 
    (stream :: <wrapper-stream>) => (available? :: <boolean>)
  stream-input-available?(stream.inner-stream)
end method stream-input-available?;

define method discard-input
    (stream :: <wrapper-stream>) => ()
  discard-input(stream.inner-stream)
end method discard-input;


/// Writable stream protocol

define method write-element
    (stream :: <wrapper-stream>, elt :: <object>) => ()
  write-element(stream.inner-stream, elt)
end method write-element;

define method write
    (stream :: <wrapper-stream>, elements :: <sequence>,
     #rest keys, #key start: _start, end: _end) => ()
  ignore(_start, _end);
  apply(write, stream.inner-stream, elements, keys)
end method write;

define method force-output
    (stream :: <wrapper-stream>, #key synchronize?) => ()
  force-output(stream.inner-stream, syncronize?: #t);
end method force-output;

define method synchronize-output
    (stream :: <wrapper-stream>) => ()
  synchronize-output(stream.inner-stream)
end method synchronize-output;

define method discard-output
    (stream :: <wrapper-stream>) => ()
  discard-output(stream.inner-stream)
end method discard-output;


/// Convenience functions

define sealed method read-to
    (stream :: <wrapper-stream>, elt, #rest keys, #key on-end-of-stream, test)
 => (sequence-or-eof :: <object>, found? :: <boolean>)
  ignore(on-end-of-stream, test);
  apply(read-to, stream.inner-stream, elt, keys)
end method read-to;

define sealed method read-through
    (stream :: <wrapper-stream>, elt, #rest keys, #key on-end-of-stream, test)
 => (sequence-or-eof :: <object>, found? :: <boolean>)
  ignore(on-end-of-stream, test);
  apply(read-through, stream.inner-stream, elt, keys)
end method read-through;

define method read-to-end
    (stream :: <wrapper-stream>)
 => (elements :: <sequence>)
  read-to-end(stream.inner-stream)
end method read-to-end;

define method skip-through
    (stream :: <wrapper-stream>, elt, #rest keys, #key test)
 => (found? :: <boolean>)
  ignore(test);
  apply(skip-through, stream.inner-stream, elt, keys)
end method skip-through;


/// Line-oriented functions

define method read-line
    (stream :: <wrapper-stream>, #rest keys, #key on-end-of-stream)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  ignore(on-end-of-stream);
  apply(read-line, stream.inner-stream, keys)
end method read-line;

define method read-line-into!
    (stream :: <wrapper-stream>, string :: <string>,
     #rest keys, #key start, on-end-of-stream, grow?)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  ignore(start, on-end-of-stream, grow?);
  apply(read-line-into!, stream.inner-stream, string, keys)
end method read-line-into!;

define method write-line
    (stream :: <wrapper-stream>, string :: <string>,
     #rest keys, #key start: _start, end: _end) => ()
  ignore(_start, _end);
  apply(write-line, stream.inner-stream, string, keys)
end method write-line;

define method new-line (stream :: <wrapper-stream>) => ()
  new-line(stream.inner-stream)
end method new-line;


/// Querying streams

define method close
    (stream :: <wrapper-stream>, #rest keys, #key wait?, synchronize?, abort?) => ()
  ignore(wait?, synchronize?, abort?);
  apply(close, stream.inner-stream, keys);
end method close;

define method stream-open?
    (stream :: <wrapper-stream>) => (open? :: <boolean>)
  stream-open?(stream.inner-stream)
end method stream-open?;

define method stream-element-type
    (stream :: <wrapper-stream>) => (element-type :: <type>)
  stream-element-type(stream.inner-stream)
end method stream-element-type;

define method stream-at-end?
    (stream :: <wrapper-stream>) => (at-end? :: <boolean>)
  stream-at-end?(stream.inner-stream)
end method stream-at-end?;

define method stream-direction
    (stream :: <wrapper-stream>) => (at-end? :: <boolean>)
  stream-direction(stream.inner-stream)
end method stream-direction;


/// Positionable stream protocol

define method stream-position
    (stream :: <wrapper-stream>) => (position :: <position-type>)
  stream-position(stream.inner-stream)
end method stream-position;

define method stream-position-setter 
    (position :: <position-type>, stream :: <wrapper-stream>)
 => (position :: <position-type>)
  stream-position-setter(position, stream.inner-stream)
end method stream-position-setter;

define method adjust-stream-position
    (stream :: <wrapper-stream>, delta :: <integer>, #rest keys, #key from)
 => (position :: <position-type>)
  ignore(from);
  apply(adjust-stream-position, stream.inner-stream, delta, keys)
end method adjust-stream-position;

define method stream-size
    (stream :: <wrapper-stream>) => (size :: false-or(<integer>))
  stream-size(stream.inner-stream)
end method stream-size;

define method stream-contents
    (stream :: <wrapper-stream>, #rest keys, #key clear-contents?)
 => (sequence :: <sequence>)
  ignore(clear-contents?);
  apply(stream-contents, stream.inner-stream, keys)
end method stream-contents;

define method stream-contents-as
    (type :: <type>, stream :: <wrapper-stream>, #rest keys, #key clear-contents?)
 => (sequence :: <sequence>)
  ignore(clear-contents?);
  apply(stream-contents-as, type, stream.inner-stream, keys)
end method stream-contents-as;


/// Locking

define method stream-locked?
    (stream :: <wrapper-stream>) => (locked? :: <boolean>)
  stream-locked?(stream.inner-stream)
end method stream-locked?;

define method lock-stream
    (stream :: <wrapper-stream>) => ()
  lock-stream(stream.inner-stream)
end method lock-stream;

define method unlock-stream
    (stream :: <wrapper-stream>) => ()
  unlock-stream(stream.inner-stream)
end method unlock-stream;

/// Sequence streams

define method stream-sequence-class
    (stream :: <wrapper-stream>) => (class :: subclass(<sequence>))
  stream-sequence-class(stream.inner-stream)
end method stream-sequence-class;


/// File streams

define method stream-limit
    (stream :: <wrapper-stream>) => (size :: false-or(<position-type>))
  stream-limit(stream.inner-stream)
end method stream-limit;
