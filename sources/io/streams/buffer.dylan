Module:       streams-internals
Synopsis:     Definition of buffer class and methods for buffered streams
Author:       Toby Weinberg, Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Buffers

define constant <buffer-index> = <integer>;

define method element-type (buffer :: <buffer>) => (type :: <type>)
  <byte>
end;

// 'buffer-start' is the first valid index in a buffer, usually 0.
// 'buffer-size' is the size of the buffer's underlying vector, i.e., the
//   maximum number of bytes it can ever hold.
// 'buffer-end' is one more than last valid index in a buffer (that is, it's
//   the number of bytes read in an input buffer, or the "high water mark"
//   for an output buffer).
// 'buffer-next' is where the next input byte comes from, or where the next
//   output byte goes to.
// 'buffer-position' is the position in the underlying stream where the
//   first byte of the buffer goes.  In single buffered streams this will
//   be the same as the file-position (fp) for output streams and the same
//   as the file-position - buffer-size for input streams.
// 'buffer-dirty?' is #f if the buffer is unmodified (no different from the
//   similar bits in the underlying file), #t if the buffer is modified.

define sealed generic buffer-start
    (buffer :: <buffer>) => (start :: <buffer-index>);

/* Folded into <buffer>
define sealed primary class <power-of-two-buffer> (<buffer>)
  slot buffer-start :: <buffer-index> = 0, init-keyword: buffer-start:;
  slot on-page-bits :: <integer>;
  slot off-page-bits :: <integer>;
end class;
*/

define constant <power-of-two-buffer> :: <class> = <buffer>;

/*
define method initialize 
    (the-buffer :: <power-of-two-buffer>, 
     #rest initialization-arguments, 
     #key 
       known-power-of-two-size? :: <boolean> = #f,
       size: requested-buffer-size) => ();
  if (known-power-of-two-size?)
    next-method();
  else
    apply(next-method, the-buffer, 
	  size: round-to-power-of-two(requested-buffer-size),
	  initialization-arguments);
  end if;
  the-buffer.buffer-on-page-bits := the-buffer.buffer-size - 1;
  the-buffer.buffer-off-page-bits := lognot(the-buffer.on-page-bits);
end method initialize;
*/

// Hack
define method make-<power-of-two-buffer>
    (#rest initialization-arguments, 
     #key 
       known-power-of-two-size? :: <boolean> = #f,
       size: requested-buffer-size,
       fill = '\0', 
     #all-keys) => (the-buffer :: <power-of-two-buffer>);
  let the-buffer =
    if (known-power-of-two-size?)
      apply(make, <power-of-two-buffer>, initialization-arguments);
    else
      apply(make, <power-of-two-buffer>, 
	    size: round-to-power-of-two(requested-buffer-size),
	    initialization-arguments);
    end if;
  the-buffer.buffer-on-page-bits := the-buffer.buffer-size - 1;
  the-buffer.buffer-off-page-bits := lognot(the-buffer.buffer-on-page-bits);
  the-buffer
end method;

define function round-to-power-of-two
    (requested-size :: <integer>) => 
    (rounded-size :: <integer>, log-two-size :: <integer>)
  unless (requested-size > 0)
    error("requested-size, %d is <= 0", requested-size);
  end unless;
  let rounded-size = 2;
  let number-of-shifts = 1; // log 2 of the buffer size
  if (requested-size > 2) // 
    requested-size := requested-size - 1;
    rounded-size := 1;
    number-of-shifts := 0;
    until (requested-size = 0)
      // right shift requested-size
      requested-size := ash(requested-size, -1);
      // left shift rounded-size
      rounded-size := ash(rounded-size, 1);
      number-of-shifts := number-of-shifts + 1;
    end until;
  end if;
  values (rounded-size, number-of-shifts)
end function;

/*
define method print-object (buffer :: <buffer>, stream) => ()
  debug-format(stream, "<buffer>[");
  for (index from 0 below buffer.size)
    if (index = 0)
      debug-format(stream, " ")
    end;
    print-object(buffer[index], stream);
    debug-format(stream, " ")
  end;
  debug-format(stream, "]")
end method print-object;
*/


/// Internal buffering protocols

define open generic buffer-subsequence
    (buffer :: <buffer>, class :: subclass (<mutable-sequence>),
     start-index :: <buffer-index>, end-index :: <buffer-index>)
 => (result :: <mutable-sequence>);

define sealed method buffer-subsequence
    (buffer :: <buffer>, class == <byte-string>,
     start-index :: <buffer-index>, end-index :: <buffer-index>)
 => (result :: <byte-string>)
  let count = end-index - start-index;
  let seq = make(class, size: count);
  copy-bytes(seq, 0, buffer, start-index, count);
  seq
end method buffer-subsequence;

define sealed method buffer-subsequence
    (buffer :: <buffer>, class == <byte-vector>,
     start-index :: <buffer-index>, end-index :: <buffer-index>)
 => (result :: <byte-vector>)
  let count = end-index - start-index;
  let seq = make(class, size: count);
  copy-bytes(seq, 0, buffer, start-index, count);
  seq
end method buffer-subsequence;

/*
define sealed method buffer-subsequence
    (buffer :: <buffer>, class == <unicode-string>,
     start-index :: <buffer-index>, end-index :: <buffer-index>)
 => (result :: <byte-string>)
  let count = end-index - start-index;
  assert(count >= 0 & even?(count));
  let seq = make(class, size: end-index - start-index);
  for (i from 0 below count,
       j from 0 by 2)
    seq[j] := bytes-to-unicode-character(byte-vector-ref(buffer, start-index + i + 0),
                                         byte-vector-ref(buffer, start-index + i + 1))
  end;
  seq
end method buffer-subsequence;
*/


define open generic copy-into-buffer!
    (buffer :: <buffer>,
     buffer-start-index :: <buffer-index>,
     sequence :: <mutable-sequence>,
     #key start: start-index, end: end-index) => ();

define sealed method copy-into-buffer!
    (buffer :: <buffer>, buffer-start-index :: <buffer-index>,
     sequence :: <byte-string>,
     #key start: start-index = 0, end: end-index = sequence.size) => ()
  copy-bytes(buffer, buffer-start-index, sequence, start-index, end-index - start-index)
end method copy-into-buffer!;

define sealed method copy-into-buffer!
    (buffer :: <buffer>, buffer-start-index :: <buffer-index>,
     sequence :: <byte-vector>,
     #key start: start-index = 0, end: end-index = sequence.size) => ()
  copy-bytes(buffer, buffer-start-index, sequence, start-index, end-index - start-index)
end method copy-into-buffer!;

define sealed method copy-into-buffer!
    (buffer :: <buffer>, buffer-start-index :: <buffer-index>,
     sequence :: <buffer>,
     #key start: start-index = 0, end: end-index = sequence.size) => ()
  copy-bytes(buffer, buffer-start-index, sequence, start-index, end-index - start-index)
end method copy-into-buffer!;


define open generic copy-from-buffer!
    (buffer :: <buffer>,
     buffer-start-index :: <buffer-index>,
     sequence :: <mutable-sequence>,
     #key start: start-index, end: end-index) => ();

define sealed method copy-from-buffer!
    (buffer :: <buffer>, buffer-start-index :: <buffer-index>,
     sequence :: <byte-string>,
     #key start: start-index = 0, end: end-index = sequence.size) => ()
  copy-bytes(sequence, start-index, buffer, buffer-start-index, end-index - start-index)
end method copy-from-buffer!;

define sealed method copy-from-buffer!
    (buffer :: <buffer>, buffer-start-index :: <buffer-index>,
     sequence :: <byte-vector>,
     #key start: start-index = 0, end: end-index = sequence.size) => ()
  copy-bytes(sequence, start-index, buffer, buffer-start-index, end-index - start-index)
end method copy-from-buffer!;

//
// TYPE-FOR-COPY
//

define sealed /*inline*/ method type-for-copy (object :: <buffer>)
 => (c :: <class>)
  <buffer>
end method type-for-copy;

// eof
