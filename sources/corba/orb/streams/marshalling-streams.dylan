Module: orb-streams
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <marshalling-buffer> = limited(<stretchy-vector>, of: <byte>);

define constant $default-marshalling-buffer-capacity :: <integer> = 500;

define class <marshalling-stream> (<stream>)
  slot marshalling-stream-buffer :: <marshalling-buffer>
    = make(<marshalling-buffer>, capacity: $default-marshalling-buffer-capacity), init-keyword: buffer:;
  slot marshalling-stream-buffer-capacity :: <integer>
    = $default-marshalling-buffer-capacity, init-keyword: buffer-capacity:;
  slot marshalling-stream-input-index
    = 0;
  slot marshalling-stream-little-endian? :: <boolean>
    = architecture-little-endian?();
end class;

define sealed domain make (subclass(<marshalling-stream>));
define sealed domain initialize (<marshalling-stream>);

define class <marshalling-wrapper-stream> (<marshalling-stream>, <wrapper-stream>)
end class;

define method initialize (marshalling-stream :: <marshalling-wrapper-stream>, #key)
 => ()
  next-method();
  let connection-stream = inner-stream(marshalling-stream);
  let lock = stream-lock(connection-stream);
  unless (lock)
    stream-lock(connection-stream) := make(<lock>);
  end unless;
end method;

define method marshalling-stream-output-index (stream :: <marshalling-stream>)
  size(marshalling-stream-buffer(stream));
end method;

define method marshalling-stream-output-index-setter (new-value :: <integer>, stream :: <marshalling-stream>)
  size(marshalling-stream-buffer(stream)) := new-value;
end method;

define method write-bytes (stream :: <marshalling-stream>, value :: <integer>, n :: dylan/<integer>)
 => ()
  if (marshalling-stream-little-endian?(stream))
    for (i :: dylan/<integer> from 0 below n)
      write-element(stream, logand(value, #xff));
      value := ash(value, -8);
    end for;
  else
    let shift :: dylan/<integer> = - (8 * (n - 1));
    for (i :: dylan/<integer> from 0 below n)
      write-element(stream, logand(ash(value, shift), #xff));
      shift := dylan/+(shift, 8);
    end for;
  end if;
end method;

define method read-bytes
    (stream :: <marshalling-stream>, n :: dylan/<integer>,
     #key on-end-of-stream = unsupplied(),
     signed? :: <boolean> = #f)
 => (result)
  if (signed?)
    read-signed-bytes(stream, n, on-end-of-stream: on-end-of-stream)
  else
    read-unsigned-bytes(stream, n, on-end-of-stream: on-end-of-stream)
  end if;
end method;

define method read-unsigned-bytes
    (stream :: <marshalling-stream>, n :: dylan/<integer>, #key on-end-of-stream = unsupplied())
 => (result)
  let value :: <integer> = 0;
  if (marshalling-stream-little-endian?(stream))
    let shift :: dylan/<integer> = 0;
    for (i :: dylan/<integer> from 0 below n)
      value := value + ash(read-element(stream, on-end-of-stream: on-end-of-stream), shift);
      shift := dylan/+(shift, 8);
    end for;
  else
    let shift :: dylan/<integer> = 8 * (n - 1);
    for (i :: dylan/<integer> from 0 below n)
      value := value + ash(read-element(stream, on-end-of-stream: on-end-of-stream), shift);
      shift := dylan/-(shift, 8);
    end for;
  end if;
  value
end method;

define method read-signed-bytes
    (stream :: <marshalling-stream>, n :: dylan/<integer>, #key on-end-of-stream = unsupplied())
 => (result)
  let (high-bit, mask) =
    select (n)
      2 => values(#x8000, #x7fff);
      4 => values(#x80000000, #x7fffffff);
    end select;
  let bytes = read-unsigned-bytes(stream, n, on-end-of-stream: on-end-of-stream);
  if (logand(bytes, high-bit) = 0)
    bytes
  else
    (- (high-bit - logand(bytes, mask)))
  end if;
end method;

define constant $giop-header-size = 12;

/// ---*** hmmm this does hand unmarshalling of the header fields
/// perhaps this should be coded more abstractly using the IDL structures?

define method force-input (marshalling-stream :: <marshalling-stream>, #key reset-index? = #t)
 => ()
  let connection-stream :: <stream> = inner-stream(marshalling-stream);
  let buffer :: <marshalling-buffer> = marshalling-stream-buffer(marshalling-stream);
  size(buffer) := 0;
  for (i from 0 below $giop-header-size)
    buffer := add!(buffer, read-element(connection-stream));
  end for;
  check-giop-magic-header(marshalling-stream);
  let message-size = marshalling-buffer-size(buffer);
  if (marshalling-stream-buffer-capacity(marshalling-stream) < message-size)
    let new-capacity = message-size + $giop-header-size;
    let new-buffer :: <marshalling-buffer>
      = make(<marshalling-buffer>, size: $giop-header-size, capacity: new-capacity, fill: 0);
    new-buffer := replace-subsequence!(new-buffer, buffer, end: $giop-header-size);
    marshalling-stream-buffer(marshalling-stream) := new-buffer;
    marshalling-stream-buffer-capacity(marshalling-stream) := new-capacity;
    buffer := new-buffer;
  end if;
  for (i from 0 below message-size)
    buffer := add!(buffer, read-element(connection-stream));
  end for;
  marshalling-stream-buffer(marshalling-stream) := buffer;
  when (reset-index?)
    marshalling-stream-input-index(marshalling-stream) := 0;
  end when;
  check-giop-version(marshalling-stream);
end method;

define constant $giop-magic :: <vector> = map(curry(as, <integer>), #['G','I','O','P']);
define constant $giop-version :: <vector> = #[1,0];

// NB discard any extra input because it could be anything
define method check-giop-magic-header (stream :: <marshalling-stream>)
 => ()
  let buffer = marshalling-stream-buffer(stream);
  unless ((buffer[0] = $giop-magic[0])
	    & (buffer[1] = $giop-magic[1])
	    & (buffer[2] = $giop-magic[2])
	    & (buffer[3] = $giop-magic[3]))
    discard-input(stream);
    error(make(<giop-message-error>, stream: stream));
  end unless;
end method;

// NB Don't discard any input because already read exactly the
// right amount
define method check-giop-version (stream :: <marshalling-stream>)
 => ()
  let buffer = marshalling-stream-buffer(stream);
  unless ((buffer[4] = $giop-version[0])
	    & (buffer[5] = $giop-version[1]))
    error(make(<giop-message-error>, stream: stream));
  end unless;
end method;

define method marshalling-buffer-little-endian? (buffer :: <marshalling-buffer>)
 => (little-endian? :: <boolean>)
  buffer[6] ~= 0;
end method;

define method marshalling-buffer-size (buffer :: <marshalling-buffer>)
 => (length)
  if (marshalling-buffer-little-endian?(buffer))
    ash(buffer[11], 24)
      + ash(buffer[10], 16)
      + ash(buffer[9], 8)
      + buffer[8]
  else
    ash(buffer[8], 24)
      + ash(buffer[9], 16)
      + ash(buffer[10], 8)
      + buffer[11]
  end if;
end method;

define method marshalling-buffer-as-string (buffer :: <marshalling-buffer>, #key reversed? :: <boolean> = #f)
  with-output-to-string (string-stream)
    if (reversed?)
      for (byte in buffer)
	write-element(string-stream, integer-as-hex-character(logand(byte, #xf)));
	write-element(string-stream, integer-as-hex-character(ash(byte, -4)));
      end for;
    else
      for (byte in buffer)
	write-element(string-stream, integer-as-hex-character(ash(byte, -4)));
	write-element(string-stream, integer-as-hex-character(logand(byte, #xf)));
      end for;
    end if;
  end;
end method;

define method string-as-marshalling-buffer (string :: <string>)
  let buffer = make(<marshalling-buffer>);
  for (i from 0 below size(string) by 2)
    buffer := add!(buffer, 
		   ash(hex-character-as-integer(string[i]), 4) +
		     hex-character-as-integer(string[1 + i]));
  end for;
  buffer;
end method;

define method integer-as-hex-character (x :: <integer>)
 => (hex-char :: <character>)
  let chars = "0123456789abcdef";
  chars[x];
end method;

define constant $hex-char-values :: <table> = make(<table>);
$hex-char-values['0'] := 0;
$hex-char-values['1'] := 1;
$hex-char-values['2'] := 2;
$hex-char-values['3'] := 3;
$hex-char-values['4'] := 4;
$hex-char-values['5'] := 5;
$hex-char-values['6'] := 6;
$hex-char-values['7'] := 7;
$hex-char-values['8'] := 8;
$hex-char-values['9'] := 9;
$hex-char-values['a'] := 10;
$hex-char-values['b'] := 11;
$hex-char-values['c'] := 12;
$hex-char-values['d'] := 13;
$hex-char-values['e'] := 14;
$hex-char-values['f'] := 15;
$hex-char-values['A'] := 10;
$hex-char-values['B'] := 11;
$hex-char-values['C'] := 12;
$hex-char-values['D'] := 13;
$hex-char-values['E'] := 14;
$hex-char-values['F'] := 15;

define method hex-character-as-integer (hex-char :: <character>)
  $hex-char-values[hex-char];
end method;

define method write-element (stream :: <marshalling-stream>, byte)
    => ()
  marshalling-stream-buffer(stream) := add!(marshalling-stream-buffer(stream), byte);
end method;

define method read-element (stream :: <marshalling-stream>, #key on-end-of-stream = unsupplied())
 => (byte)
  let buffer :: <marshalling-buffer> = marshalling-stream-buffer(stream);
  let length :: <integer> = size(buffer);
  let index :: <integer> = marshalling-stream-input-index(stream);
  if (index >= length)
    if (unsupplied?(on-end-of-stream))
      error(make(<end-of-stream-error>, stream: stream));
    else
      on-end-of-stream
    end if;
  else
    marshalling-stream-input-index(stream) := index + 1;
    buffer[index];
  end if;
end method;

define method align-output-stream (stream :: <marshalling-stream>, alignment :: dylan/<integer>)
  let buffer :: <marshalling-buffer> = marshalling-stream-buffer(stream);
  let padding :: <integer> = modulo(alignment - modulo(size(buffer) - output-marshalling-alignment-constraint(), alignment), alignment);
  for (i from 0 below padding)
    buffer := add!(buffer, 0);
  end for;
  marshalling-stream-buffer(stream) := buffer;
end method;

/// ---*** I think it would be nicer to do this using the marshalling engine
define method set-buffer-size (stream :: <marshalling-stream>, start-of-giop :: <integer>, message-length :: <integer>)
  let buffer = marshalling-stream-buffer(stream);
  let little-endian? = marshalling-stream-little-endian?(stream);
  let byte0 = logand(message-length, #xff);
  let byte1 = logand(ash(message-length, -8), #xff);
  let byte2 = logand(ash(message-length, -16), #xff);
  let byte3 = logand(ash(message-length, -24), #xff);
  if (little-endian?)
    buffer[start-of-giop] := byte0;
    buffer[start-of-giop + 1] := byte1;
    buffer[start-of-giop + 2] := byte2;
    buffer[start-of-giop + 3] := byte3;
  else
    buffer[start-of-giop + 3] := byte0;
    buffer[start-of-giop + 2] := byte1;
    buffer[start-of-giop + 1] := byte2;
    buffer[start-of-giop] := byte3;
  end if;
end method;

define method force-output (marshalling-stream :: <marshalling-stream>, #key)
 => ()
  let connection-stream = inner-stream(marshalling-stream);
  let buffer = marshalling-stream-buffer(marshalling-stream);
  with-lock (stream-lock(connection-stream))
    for (byte in buffer)
      write-element(connection-stream, byte);
    end for;
    force-output(connection-stream); // This should be equivalent to next-method()
  end with-lock;
end method;

define method align-input-stream (stream :: <marshalling-stream>, alignment :: <integer>)
  let index :: <integer> = marshalling-stream-input-index(stream);
  let pseudo-index :: <integer> = index - input-marshalling-alignment-constraint();
  let alignment-diff :: <integer> = modulo(pseudo-index, alignment);
  unless (alignment-diff = 0)
    let length = size(marshalling-stream-buffer(stream));
    let padding = alignment - alignment-diff;
    let new-index = index + padding;
    if (new-index >= length)
      error(make(<end-of-stream-error>, stream: stream));
    end if;
    marshalling-stream-input-index(stream) := new-index;
  end unless;
end method;

define method close (stream :: <marshalling-stream>, #key)
 => ()
  /// NB do nothing, especially not closing inner connection stream!
end method;
