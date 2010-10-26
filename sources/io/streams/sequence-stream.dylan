Module:       streams-internals
Synopsis:     Implementation of streams for streaming over sequences
Author:       Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sequence stream classes

define open primary class <sequence-stream> (<basic-positionable-stream>)
  slot stream-sequence :: <sequence> = make(<vector>),
    init-keyword: contents:;
  slot stream-limit :: false-or(<position-type>) = #f;
//   slot initial-position :: <position-type> = 0; // inherited from <basic-positionable-stream>
//   slot current-position :: <position-type> = 0; // inherited from <basic-positionable-stream>
//   slot final-position :: <position-type> = 0;   // inherited from <basic-positionable-stream>
end class <sequence-stream>;

define open primary class <string-stream> (<sequence-stream>)
  inherited slot stream-sequence = make(<string>);
end class <string-stream>;

define sealed class <byte-string-stream> (<string-stream>)
  inherited slot stream-sequence = make(<byte-string>);
end class <byte-string-stream>;

define sealed class <unicode-string-stream> (<string-stream>)
  inherited slot stream-sequence = make(<unicode-string>);
end class <unicode-string-stream>;


/// Macros

define macro with-output-to-string
  { with-output-to-string (?stream:name, #rest ?options:*)
      ?body:body
    end }
 => { let ?stream :: <string-stream>
        = make(<string-stream>, direction: #"output", ?options);
      ?body;
      stream-contents(?stream, clear-contents?: #f) }
  { with-output-to-string (?stream:name :: ?class:name, #rest ?options:*)
      ?body:body
    end }
 => { let ?stream :: ?class = make(?class, direction: #"output", ?options);
      ?body;
      stream-contents(?stream, clear-contents?: #f) }
end macro with-output-to-string;

define macro with-input-from-string
  { with-input-from-string (?stream:name = ?string:expression, #rest ?options:*)
      ?body:body
    end }
 => { let ?stream :: <string-stream>
        = make(<string-stream>, contents: ?string, direction: #"input", ?options);
      ?body;
    }
  { with-input-from-string (?stream:name :: ?class:name = ?string:expression, #rest ?options:*)
      ?body:body
    end }
 => { let ?stream :: ?class = make(?class, contents: ?string, direction: #"input", ?options);
      ?body;
    }
end macro with-input-from-string;


/// Creating sequence streams

define method make
    (class == <sequence-stream>, #rest initargs, #key contents, element-type)
 => (stream /* ---*** :: subclass(<sequence-stream>) */)
  let type = type-for-sequence-stream(contents);
  if (type == class)
    next-method()
  else
    apply(make, type, initargs)
  end
end method make;

define method initialize
    (stream :: <sequence-stream>,
     #key direction, start: _start = 0, end: _end) => ()
  next-method();
  stream.initial-position := _start;
  stream.current-position := _start;
  stream.stream-limit := _end;
  if (direction == #"output")
    stream.final-position := _start
  else
    stream.final-position := size(stream-sequence(stream))
  end
end method initialize;

define open generic type-for-sequence-stream
    (contents :: false-or(<sequence>))
 => (sequence-stream-type :: <type>);

define method type-for-sequence-stream
    (contents == #f) => (type :: singleton(<sequence-stream>))
  <sequence-stream>
end method type-for-sequence-stream;

define method type-for-sequence-stream
    (contents :: <sequence>) => (type :: singleton(<sequence-stream>))
  <sequence-stream>
end method type-for-sequence-stream;

define method type-for-sequence-stream
    (contents :: <string>) => (type :: singleton(<string-stream>))
  <string-stream>
end method type-for-sequence-stream;

define method type-for-sequence-stream
    (contents :: <byte-string>) => (type :: singleton(<byte-string-stream>))
  <byte-string-stream>
end method type-for-sequence-stream;

define method type-for-sequence-stream
    (contents :: <unicode-string>) => (type :: singleton(<unicode-string-stream>))
  <unicode-string-stream>
end method type-for-sequence-stream;


/// Readable stream protocol

define method read-element
    (stream :: <sequence-stream>,
     #key on-end-of-stream = unsupplied())
 => (element :: <object>)
  ensure-readable(stream);
  let seq   :: <sequence> = stream-sequence(stream);
  let pos   :: <integer>  = stream.current-position;
  let limit :: <integer>  = stream-limit(stream) | stream.final-position;
  if (pos < limit)
    let elt = seq[pos];
    stream.current-position := pos + 1;
    elt
  else
    end-of-stream-value(stream, on-end-of-stream)
  end
end method read-element;

define method unread-element
    (stream :: <sequence-stream>, elt :: <object>)
 => (element :: <object>)
  ensure-readable(stream);
  let pos   :: <integer>  = stream.current-position;
  if (pos > 0)
    stream.current-position := pos - 1;
    let sequence = stream-sequence(stream);
    // Try not to upset immutable sequences
    if (sequence[pos - 1] ~== elt)
      sequence[pos - 1] := elt
    end
  end;
  elt
end method unread-element;

define method peek
    (stream :: <sequence-stream>,
     #key on-end-of-stream = unsupplied())
 => (element :: <object>)
  ensure-readable(stream);
  let seq   :: <sequence> = stream-sequence(stream);
  let pos   :: <integer>  = stream.current-position;
  let limit :: <integer>  = stream-limit(stream) | stream.final-position;
  if (pos < limit)
    seq[pos]
  else
    end-of-stream-value(stream, on-end-of-stream)
  end
end method peek;

define method read 
    (stream :: <sequence-stream>, n :: <integer>,
     #key on-end-of-stream = unsupplied())
 => (elements)
  ensure-readable(stream);
  let seq   :: <sequence> = stream-sequence(stream);
  let pos   :: <integer>  = stream.current-position;
  let src-n :: <integer>  = (stream-limit(stream) | stream.final-position) - pos;
  if (n > src-n)
    if (unsupplied?(on-end-of-stream))
      signal(make(<incomplete-read-error>,
		  stream: stream,
		  count: src-n,
		  sequence: copy-sequence(seq, start: pos, end: pos + src-n)));
    end;
    n := src-n
  end;
  let elements = copy-sequence(seq, start: pos, end: pos + n);
  stream.current-position := pos + n;
  elements
end method read;

define method read-into!
    (stream :: <sequence-stream>, n :: <integer>, dst :: <mutable-sequence>,
     #key start = 0, on-end-of-stream = unsupplied())
 => (n-read)
  ensure-readable(stream);
  let seq   :: <sequence> = stream-sequence(stream);
  let pos   :: <integer>  = stream.current-position;
  let src-n :: <integer>  = (stream-limit(stream) | stream.final-position) - pos;
  let dst-n :: <integer>  = dst.size - start;
  let n-read :: <integer> = min(n, src-n, dst-n);
  copy-bytes(dst, start, seq, pos, n-read);
  stream.current-position := pos + n-read;
  if (n > src-n & dst-n > src-n
      & unsupplied?(on-end-of-stream))
    signal(make(<incomplete-read-error>,
		stream: stream, 
		count: n-read,
		sequence: copy-sequence(dst, start: start, end: start + n-read)))
  end;
  n-read
end method read-into!;

define method stream-input-available?
    (stream :: <sequence-stream>) => (available? :: <boolean>)
  readable?(stream)
  & ~stream-at-end?(stream)
end method stream-input-available?;


/// Writable stream protocol

define method write-element
    (stream :: <sequence-stream>, elt :: <object>) => ()
  ensure-writable(stream);
  let seq :: <sequence> = stream-sequence(stream);
  let pos :: <integer>  = stream.current-position;
  // Grow the sequence if necessary
  if (pos >= seq.size)
    while (pos >= seq.size)
      seq := grow-for-stream(seq, pos + 1);
      stream-sequence(stream) := seq
    end
  end;
  // Insert the new element
  seq[pos] := elt;
  stream.current-position := pos + 1;
  if (pos >= stream.final-position)
    stream.final-position := pos + 1
  end
end method write-element;

define method write
    (stream :: <sequence-stream>, src :: <sequence>,
     #key start: start-index = 0, end: _end = #f) => ()
  ensure-writable(stream);
  let dst   :: <sequence> = stream-sequence(stream);
  let pos   :: <integer>  = stream.current-position;
  let count :: <integer>  = (_end | src.size) - start-index;
  let required-space = pos + count;
  while (dst.size < required-space)
    dst := grow-for-stream(dst, required-space);
    stream-sequence(stream) := dst
  end;
  copy-bytes(dst, pos, src, start-index, count);
  let new-pos = pos + count;
  stream.current-position := new-pos;
  if (new-pos > stream.final-position)
    stream.final-position := new-pos
  end
end method write;

define method grow-for-stream
    (seq :: <mutable-sequence>, min-size :: <integer>)
 => (new-seq :: <sequence>)
  let n :: <integer> = seq.size;
  let new-seq = make(object-class(seq), size: max(min-size, 2 * n));
  copy-bytes(new-seq, 0, seq, 0, n);
  new-seq
end method grow-for-stream;

define method grow-for-stream
    (vec :: <stretchy-vector>, min-size :: <integer>)
 => (vec :: <stretchy-vector>)
  vec.size := min-size;
  vec
end method grow-for-stream;


/// Stream query and contents accessing

define method stream-at-end?
    (stream :: <sequence-stream>) => (at-end? :: <boolean>)
  if (write-only?(stream))
    #f
  else
    let limit = (read-only?(stream) & stream-limit(stream));
    stream.current-position >= (limit | stream.final-position)
  end
end method stream-at-end?;

define method stream-size
    (stream :: <sequence-stream>) => (size :: <integer>)
  (stream-limit(stream) | stream.final-position)
  - stream.initial-position
end method stream-size;

define method clear-contents (stream :: <sequence-stream>) => ()
  stream.current-position := 0;
  stream.final-position := stream-limit(stream) | 0
end method clear-contents;

define method stream-contents
    (stream :: <sequence-stream>, #key clear-contents? = #t)
 => (contents :: <sequence>)
  let type = type-for-copy(stream-sequence(stream));
  stream-contents-as(type, stream, clear-contents?: clear-contents?)
end method stream-contents;

define method stream-contents-as
    (type :: <type>, stream :: <sequence-stream>, #key clear-contents? = #t)
 => (contents :: <sequence>)
  let _start = stream.initial-position;
  let _end = stream-limit(stream) | stream.final-position;
  let n = _end - _start;
  let result = make(type, size: n);
  copy-bytes(result, 0, stream-sequence(stream), _start, n);
  if (clear-contents?)
    clear-contents(stream)
  end;
  result
end method stream-contents-as;

define method stream-sequence-class
    (stream :: <sequence-stream>) => (class /* ---*** subclass(<sequence>) */)
  type-for-copy(stream-sequence(stream))
end method stream-sequence-class;


/// Seal some domains

define sealed domain make (singleton(<byte-string-stream>));
define sealed domain initialize (<byte-string-stream>);
define sealed domain read-element (<byte-string-stream>);
define sealed domain unread-element (<byte-string-stream>, <object>);
define sealed domain peek (<byte-string-stream>);
define sealed domain read (<byte-string-stream>, <integer>);
define sealed domain read-into! (<byte-string-stream>, <integer>, <mutable-sequence>);
define sealed domain stream-input-available? (<byte-string-stream>);
define sealed domain write-element (<byte-string-stream>, <object>);
define sealed domain write (<byte-string-stream>, <sequence>);
define sealed domain stream-at-end? (<byte-string-stream>);
define sealed domain stream-size (<byte-string-stream>);
define sealed domain clear-contents (<byte-string-stream>);
define sealed domain stream-contents (<byte-string-stream>);
define sealed domain stream-contents-as (<type>, <byte-string-stream>);

define sealed domain make (singleton(<unicode-string-stream>));
define sealed domain initialize (<unicode-string-stream>);
define sealed domain read-element (<unicode-string-stream>);
define sealed domain unread-element (<unicode-string-stream>, <object>);
define sealed domain peek (<unicode-string-stream>);
define sealed domain read (<unicode-string-stream>, <integer>);
define sealed domain read-into! (<unicode-string-stream>, <integer>, <mutable-sequence>);
define sealed domain stream-input-available? (<unicode-string-stream>);
define sealed domain write-element (<unicode-string-stream>, <object>);
define sealed domain write (<unicode-string-stream>, <sequence>);
define sealed domain stream-at-end? (<unicode-string-stream>);
define sealed domain stream-size (<unicode-string-stream>);
define sealed domain clear-contents (<unicode-string-stream>);
define sealed domain stream-contents (<unicode-string-stream>);
define sealed domain stream-contents-as (<type>, <unicode-string-stream>);
