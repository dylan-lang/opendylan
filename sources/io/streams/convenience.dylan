Module:       streams-internals
Synopsis:     Implementation of convenience and line-oriented stream methods
Author:       Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Convenience functions

define open generic read-to
    (stream :: <stream>, elt,
     #key on-end-of-stream = unsupplied(), test = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);

define open generic read-through
    (stream :: <stream>, elt,
     #key on-end-of-stream = unsupplied(), test = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);

define open generic read-to-end
    (stream :: <stream>)
 => (elements :: <sequence>);

define open generic skip-through
    (stream :: <stream>, elt, #key test = \==)
 => (found? :: <boolean>);

define method read-to
    (stream :: <stream>, elt,
     #key on-end-of-stream = unsupplied(), test = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>)
  // If we're at the end before we've started to read anything,
  // signal end-of-stream instead of incomplete read
  if (stream-at-end?(stream))
    values(end-of-stream-value(stream, on-end-of-stream), #f)
  else 
    let seq = make(<stretchy-vector>);
    let matched? = #f;
    while (~stream-at-end?(stream) & ~matched?)
      let next-elt = read-element(stream);
      if (test(next-elt, elt))
        matched? := #t
      else
        add!(seq, next-elt)
      end
    end;
    values(as(stream-sequence-class(stream), seq), matched?)
  end
end method read-to;

define method read-through
    (stream :: <stream>, elt,
     #key on-end-of-stream = unsupplied(), test = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>)
  // If we're at the end before we've started to read anything,
  // signal end-of-stream instead of incomplete read
  if (stream-at-end?(stream))
    values(end-of-stream-value(stream, on-end-of-stream), #f)
  else 
    let seq = make(<stretchy-vector>);
    let matched? = #f;
    while (~stream-at-end?(stream) & ~matched?)
      let next-elt = read-element(stream);
      add!(seq, next-elt);
      if (test(next-elt, elt))
        matched? := #t
      end
    end;
    values(as(stream-sequence-class(stream), seq), matched?)
  end
end method read-through;

define method read-to-end
    (stream :: <stream>)
 => (elements :: <sequence>)
  let seq = make(<stretchy-vector>);
  while (~stream-at-end?(stream))
    add!(seq, read-element(stream))
  end;
  as(stream-sequence-class(stream), seq)
end method read-to-end;

define method skip-through
    (stream :: <stream>, elt, #key test = \==)
 => (found? :: <boolean>)
  let found? = #f;
  while (~stream-at-end?(stream) & ~found?)
    found? := test(read-element(stream), elt)
  end;
  found? & #t
end method skip-through;


///  Line-oriented input and output

define open generic read-line
    (stream :: <stream>, #key on-end-of-stream)
 => (string-or-eof :: <object>, newline? :: <boolean>);

define open generic read-line-into!
    (stream :: <stream>, string :: <string>,
     #key start, on-end-of-stream, grow?)
 => (string-or-eof :: <object>, newline? :: <boolean>);

define open generic write-line
    (stream :: <stream>, elements :: <string>,
     #key start, end: end-index) => ();

define open generic new-line (stream :: <stream>) => ();

define method new-line (stream :: <stream>) => ()
  write(stream, stream.newline-sequence);
end method new-line;

// NB newline-sequence is _not_ explicitly defined in the doc.
define generic newline-sequence
    (stream :: <stream>) => (elements :: <sequence>);

define method newline-sequence
    (stream :: <stream>) => (elements :: <string>)
  "\n"
end method newline-sequence;

// 'write-line' always writes 'newline-sequence', but 'read-line' tries
// to be more robust, accepting any of "\n", "\r", or "\r\n"
define method read-line
    (stream :: <stream>,
     #key on-end-of-stream = unsupplied())
 => (string-or-eof :: <object>, newline? :: <boolean>)
  // If we're at the end before we've started to read anything,
  // signal end-of-stream instead of incomplete read
  if (stream-at-end?(stream))
    values(end-of-stream-value(stream, on-end-of-stream), #f)
  else 
    let seq = make(<stretchy-vector>);
    let matched? = #f;
    while (~stream-at-end?(stream) & ~matched?)
      let next-elt = read-element(stream);
      if (next-elt == '\n')
        matched? := #t
      elseif (next-elt == '\r')
        matched? := #t;
        // If '\r' is followed by '\n', eat the '\n'
        if (peek(stream) == '\n')
          read-element(stream)
        end
      else
        add!(seq, next-elt)
      end
    end;
    values(as(stream-sequence-class(stream), seq), matched?)
  end
end method read-line;

define method read-line-into!
    (stream :: <stream>, string :: <string>,
     #key start = 0, on-end-of-stream = unsupplied(), grow? = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  let ssize :: <integer> = string.size;
  let index :: <integer> = start;
  let overflow = #f;
  local method add-with-overflow (elt)
	  if (grow? & index >= ssize)
	    unless (overflow)
	      overflow := make(<stretchy-vector>,
			       size: max(0, start - ssize),
			       fill: ' ')
	    end;
	    add!(overflow, elt)
	  else
	    string[index] := elt;
	    index := index + 1
	  end
	end method;
  // Same deal as 'read-line'
  if (stream-at-end?(stream))
    values(end-of-stream-value(stream, on-end-of-stream), #f)
  else
    let matched? = #f;
    while (~stream-at-end?(stream) & ~matched?)
      let next-elt = read-element(stream);
      if (next-elt == '\n')
        matched? := #t
      elseif (next-elt == '\r')
        matched? := #t;
        // If '\r' is followed by '\n', eat the '\n'
        if (peek(stream) == '\n')
          read-element(stream)
        end
      else
        add-with-overflow(next-elt)
      end
    end;
    if (overflow)
      string := concatenate(string, overflow);
    end;
    values(string, matched?)
  end
end method read-line-into!;

define method write-line
    (stream :: <stream>, elements :: <string>,
     #key start: start-index = 0, end: _end = #f) => ()
  with-stream-locked (stream)
    write(stream, elements, start: start-index, end: _end | elements.size);
    new-line(stream)
  end;
  #f
end method write-line;


/// Handle chunks of text obeying new-line conventions

define open generic read-character
    (stream :: <stream>, #key on-end-of-stream)
 => (character-or-eof);

define open generic read-text
    (stream :: <stream>, n :: <integer>, #key on-end-of-stream)
 => (string-or-eof);

define open generic read-text-into!
    (stream :: <stream>, n :: <integer>, text :: <string>,
     #key start :: <integer>, on-end-of-stream)
 => (count-or-eof);

define open generic write-text
    (stream :: <stream>, text :: <string>,
     #key start :: <integer>,
          end: end-index :: <integer>)
 => ();

define method read-character
    (stream :: <buffered-stream>,
     #key on-end-of-stream = unsupplied())
 => (element :: <object>)
  if (stream-at-end?(stream))
    end-of-stream-value(stream, on-end-of-stream)
  else
    let character = read-element(stream);
    select (character)
      '\r' =>
	if (peek(stream) == '\n')
	  read-element(stream)
	end;
	'\n';
      otherwise =>
	character;
    end
  end
end method read-character;

// 'write-line' always writes 'newline-sequence', but 'read-text' tries
// to be more robust, accepting any of "\n", "\r", or "\r\n"
define method read-text
    (stream :: <stream>, n :: <integer>,
     #key on-end-of-stream = unsupplied())
 => (string-or-eof)
  let text = make(<byte-string>, size: n);
  let value = read-text-into!(stream, n, text, on-end-of-stream: on-end-of-stream);
  if (value == on-end-of-stream)
    value
  else
    text
  end
end method read-text;

define method read-text-into!
    (stream :: <stream>, n :: <integer>, text :: <string>,
     #key start :: <integer> = 0,
          on-end-of-stream = unsupplied())
 => (count-or-eof)
  block (return)
    for (count :: <integer> from 0 below n,
	 index :: <integer> from start)
      if (stream-at-end?(stream))
	return(end-of-stream-value(stream, on-end-of-stream))
      end;
      let character = read-element(stream);
      let character :: <character>
	= select (character)
	    '\r' =>
	      if (peek(stream) == '\n')
		read-element(stream)
	      end;
	      '\n';
	    otherwise =>
	      character;
	  end;
      text[index] := character
    end;
    n
  end
end method read-text-into!;

define inline method read-text-into!
    (stream :: <string-stream>, n :: <integer>, text :: <string>,
     #key start = 0, on-end-of-stream = unsupplied())
 => (count-or-eof)
  read-into!(stream, n, text, start: start, on-end-of-stream: on-end-of-stream)
end method read-text-into!;

define method write-text
    (stream :: <stream>, text :: <string>,
     #key start: start-index :: <integer> = 0,
          end: end-index :: <integer> = text.size)
 => ()
  with-stream-locked (stream)
    let old-index :: <integer> = start-index;
    for (index :: <integer> from start-index below end-index)
      if (text[index] == '\n')
	write(stream, text, start: old-index, end: index);
	new-line(stream);
	old-index := index + 1;
      end
    end;
    write(stream, text, start: old-index, end: end-index)
  end;
  #f
end method write-text;

define inline method write-text
    (stream :: <string-stream>, text :: <string>,
     #key start: start-index :: <integer> = 0, 
          end: end-index :: <integer> = text.size)
 => ()
  write(stream, text, start: start-index, end: end-index)
end method write-text;
