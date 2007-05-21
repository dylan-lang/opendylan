Module:       streams-internals
Synopsis:     Abstract classes and generic definitions for typed streams
Author:       Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Type coercion functions

define function byte-to-byte-char
    (byte :: <byte>) => (char :: <byte-character>)
  as(<byte-character>, byte)
end function byte-to-byte-char;

define function byte-char-to-byte
    (character :: <byte-character>) => (byte :: <byte>)
  as(<integer>, character)
end function byte-char-to-byte;

define function byte-to-byte (byte :: <byte>) => (byte :: <byte>)
  byte
end function byte-to-byte;


define open abstract class <typed-stream> (<basic-stream>)
  // Assume that 
  slot sequence-type /* ---*** :: subclass(<sequence>) */ = <byte-string>;
  slot to-element-mapper    :: <function> = byte-to-byte-char;
  slot from-element-mapper  :: <function> = byte-char-to-byte;
  constant slot to-sequence-mapper   :: <function> = 
      method (s, ss, d, ds, n) => () copy-bytes(d, ds, s, ss, n) end;
  constant slot from-sequence-mapper :: <function> = 
      method (s, ss, d, ds, n) => () copy-bytes(d, ds, s, ss, n) end;
end class <typed-stream>;

define open abstract class <general-typed-stream> (<typed-stream>)
end class <general-typed-stream>;

define method initialize
    (stream :: <general-typed-stream>, #rest initargs, #key, #all-keys) => ()
  next-method();
  //---*** What about Unicode streams?  Need to convert to/from
  //---*** 2-byte representation to Unicode characters.
  if (subtype?(stream-element-type(stream), <byte>))
    // Remember to convert buffers etc.
    stream.sequence-type        := <byte-vector>;
    stream.to-element-mapper    := byte-to-byte;
    stream.from-element-mapper  := byte-to-byte
  end
end method initialize;

define open abstract class <byte-char-element-stream> (<typed-stream>)
end class <byte-char-element-stream>;

define open abstract class <byte-element-stream> (<typed-stream>)
  inherited slot sequence-type        = <byte-vector>;
  inherited slot to-element-mapper    = byte-to-byte;
  inherited slot from-element-mapper  = byte-to-byte;
end class <byte-element-stream>;


define method stream-sequence-class
    (stream :: <typed-stream>) => (class /* ---*** :: subclass(<sequence>) */)
  stream.sequence-type
end method stream-sequence-class;

define generic coerce-to-element
    (stream :: <stream>, buffer :: <buffer>, index :: <integer>)
 => (element);

define method coerce-to-element
    (stream :: <general-typed-stream>, buffer :: <buffer>, index :: <integer>)
 => (element)
  stream.to-element-mapper(buffer-ref(buffer, index))
end method coerce-to-element;

define sealed inline method coerce-to-element
    (stream :: <byte-char-element-stream>, buffer :: <buffer>, index :: <integer>)
 => (element)
  byte-to-byte-char(buffer-ref(buffer, index))
end method coerce-to-element;

define sealed inline method coerce-to-element
    (stream :: <byte-element-stream>, buffer :: <buffer>, index :: <integer>)
 => (element)
  buffer-ref(buffer, index)
end method coerce-to-element;

define method coerce-to-element
    (stream :: <stream>, buffer :: <buffer>, index :: <integer>)
 => (element)
  byte-to-byte-char(buffer-ref(buffer, index))
end method coerce-to-element;


define generic coerce-from-element
    (stream :: <stream>, buffer :: <buffer>, index :: <integer>, elt)
 => (element);

define method coerce-from-element
    (stream :: <general-typed-stream>, buffer :: <buffer>, index :: <integer>, elt)
 => (element)
  buffer-ref(buffer, index) := stream.from-element-mapper(elt)
end method coerce-from-element;

define sealed inline method coerce-from-element
    (stream :: <byte-char-element-stream>, buffer :: <buffer>, index :: <integer>, elt)
 => (element)
  buffer-ref(buffer, index) := byte-char-to-byte(elt)
end method coerce-from-element;

define sealed inline method coerce-from-element
    (stream :: <byte-element-stream>, buffer :: <buffer>, index :: <integer>, elt)
 => (element)
  buffer-ref(buffer, index) := elt
end method coerce-from-element;

define method coerce-from-element
    (stream :: <stream>, buffer :: <buffer>, index :: <integer>, elt)
 => (element)
  buffer-ref(buffer, index) := byte-char-to-byte(elt)
end method coerce-from-element;


define generic coerce-to-sequence
    (stream :: <stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ();

define method coerce-to-sequence
    (stream :: <general-typed-stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  stream.to-sequence-mapper(buffer, buf-start, sequence, seq-start, count)
end method coerce-to-sequence;

define sealed inline method coerce-to-sequence
    (stream :: <byte-char-element-stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  copy-bytes(sequence, seq-start, buffer, buf-start, count)
end method coerce-to-sequence;

define sealed inline method coerce-to-sequence
    (stream :: <byte-element-stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  copy-bytes(sequence, seq-start, buffer, buf-start, count)
end method coerce-to-sequence;

define method coerce-to-sequence
    (stream :: <stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  copy-bytes(sequence, seq-start, buffer, buf-start, count)
end method coerce-to-sequence;


define generic coerce-from-sequence
    (stream :: <stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ();

define method coerce-from-sequence
    (stream :: <general-typed-stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  stream.from-sequence-mapper(sequence, seq-start, buffer, buf-start, count)
end method coerce-from-sequence;

define sealed inline method coerce-from-sequence
    (stream :: <byte-char-element-stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  copy-bytes(buffer, buf-start, sequence, seq-start, count)
end method coerce-from-sequence;

define sealed inline method coerce-from-sequence
    (stream :: <byte-element-stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  copy-bytes(buffer, buf-start, sequence, seq-start, count)
end method coerce-from-sequence;

define method coerce-from-sequence
    (stream :: <stream>,
     buffer :: <buffer>, buf-start :: <integer>,
     sequence :: <sequence>, seq-start :: <integer>,
     count :: <integer>) => ()
  copy-bytes(buffer, buf-start, sequence, seq-start, count)
end method coerce-from-sequence;
