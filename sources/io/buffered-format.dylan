Module:       format-internals
Author:       Scott McKay
Synopsis:     This file implements 'format' to buffered output streams
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Faster 'format' to buffered output streams

// Assumes, but does not verify, that the output buffer is held
define method buffered-write-element
    (stream :: <buffered-stream>, sb :: <buffer>, elt :: <object>) => ()
  if (sb.buffer-next >= sb.buffer-size)
    sb := do-next-output-buffer(stream, bytes: 1)
  end;
  let bi :: <buffer-index> = sb.buffer-next;
  coerce-from-element(stream, sb, bi, elt);
  sb.buffer-dirty? := #t;
  sb.buffer-next := bi + 1;
  sb.buffer-end  := max(bi + 1, sb.buffer-end)
end method buffered-write-element;

// Assumes, but does not verify, that the output buffer is held
define method buffered-write
    (stream :: <buffered-stream>, sb :: <buffer>, elements :: <sequence>,
     #key start: _start = 0, end: _end = unsupplied()) => ()
  let i :: <integer> = _start;
  let e :: <integer> = if (supplied?(_end)) _end else elements.size end;
  while (sb & (i < e))
    let bi :: <buffer-index> = sb.buffer-next;
    let bufsiz :: <buffer-index> = sb.buffer-size;
    if (bi >= bufsiz)
      sb := do-next-output-buffer(stream)
    else
      let count = min(bufsiz - bi, e - i);
      coerce-from-sequence(stream, sb, bi, elements, i, count);
      sb.buffer-dirty? := #t;
      i := i + count;
      sb.buffer-next := bi + count;
      sb.buffer-end  := max(bi + count, sb.buffer-end)
    end
  end;
  if (i < e)
    signal(make(<end-of-stream-error>, stream: stream))
  end
end method buffered-write;

define method format
    (stream :: <buffered-stream>, control-string :: <byte-string>, #rest args) => ()
  let control-len :: <integer> = control-string.size;
  block (exit)
    let start :: <integer> = 0;
    let arg-i :: <integer> = 0;
    // Ensure all output is contiguous at stream's destination.
    lock-stream(stream);
    // Grab the output buffer, and don't let go for a while
    let sb = get-output-buffer(stream);
    // The rest is just like the non-buffered case of 'format',
    // except that we use the two functions above...
    while (start < control-len)
      // Skip to dispatch char.
      for (i = start then (i + 1),
	   until: ((i == control-len)
		   | (control-string[i] == $dispatch-char)
		   | (control-string[i] == '\n')))
      finally
        if (i ~== start)
          buffered-write(stream, sb, control-string, start: start, end: i);
        end;
	if (i == control-len)
	  exit()
	else
	  start := i + 1
	end
      end;
      if (control-string[start - 1] == '\n')
	new-line(stream)
      else
	// Parse for field within which to pad output.
	let (field, field-spec-end)
	  = if (char-classes[as(<byte>, control-string[start])] == #"digit")
	      parse-integer(control-string, start)
	    end;
	if (field)
	  // Capture output in string and compute padding.
	  // Assume the output is very small in length.
	  let s = make(<byte-string-stream>,
		       contents: make(<byte-string>, size: 80),
		       direction: #"output");
	  if (do-dispatch(control-string[field-spec-end], s,
			  element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1
	  end;
	  let output = s.stream-contents;
	  let output-len :: <integer> = output.size;
	  let padding :: <integer> = (abs(field) - output-len);
	  case
	    (padding < 0) =>
	      buffered-write(stream, sb, output);
	    (field > 0) =>
	      buffered-write(stream, sb, make(<byte-string>, size: padding, fill: ' '));
	      buffered-write(stream, sb, output);
	    otherwise =>
	      buffered-write(stream, sb, output);
	      buffered-write(stream, sb, make(<byte-string>, size: padding, fill: ' '));
	  end;
	  start := field-spec-end + 1	// Add one to skip dispatch char.
	else
	  if (buffered-do-dispatch(control-string[start], stream, sb,
				   element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1
	  end;
	  start := start + 1		// Add one to skip dispatch char.
	end
      end
    end while;
  cleanup
    unlock-stream(stream)
  end
end method format;

define method buffered-do-dispatch
    (char :: <byte-character>, stream :: <buffered-stream>, sb :: <buffer>, arg)
 => (consumed-arg? :: <boolean>)
  select (char by \==)
    ('s'), ('S') =>
      if (instance?(arg, <byte-string>))
	// Simulate "write-message" upon the argument.  This code must be
	// changed if the semantics of "write-message" changes.
	buffered-write(stream, sb, arg)
      else
	print-message(arg, stream)
      end;
      #t;
    ('c'), ('C') =>
      select (arg by instance?)
	<byte-character> =>
	  buffered-write-element(stream, sb, arg);
	<character> =>
	  print-message(arg, stream);
	otherwise =>
	  error("The %%C format directive only works for characters: %=", arg);
      end;
      #t;
    ('=') =>
      dynamic-bind (*print-escape?* = #t)
        print-object(arg, stream)
      end;
      #t;
    ('d'), ('D') =>
      buffered-format-integer(arg, 10, stream, sb);
      #t;
    ('b'), ('B') =>
      buffered-format-integer(arg, 2, stream, sb);
      #t;
    ('o'), ('O') =>
      buffered-format-integer(arg, 8, stream, sb);
      #t;
    ('x'), ('X') =>
      buffered-format-integer(arg, 16, stream, sb);
      #t;
    ('m'), ('M') =>
      apply(arg, list(stream));
      #t;
    ('%') =>
      buffered-write-element(stream, sb, '%');
      #f;
    otherwise =>
      error("Unknown format dispatch character, %c", char);
  end
end method buffered-do-dispatch;

///---*** KLUDGE: Temporary method until division is implemented for <double-integer>
///---*** Once division is available, remove this method!
define method buffered-format-integer
    (arg :: <double-integer>, radix :: limited(<integer>, min: 2, max: 36),
     stream :: <buffered-stream>, sb :: <buffer>) => ()
  print(arg, stream)
end method buffered-format-integer;

///---*** KLUDGE: Temporary method until we have better numerics.
define method buffered-format-integer
    (arg :: <machine-word>, radix :: limited(<integer>, min: 2, max: 36),
     stream :: <buffered-stream>, sb :: <buffer>) => ()
  print(arg, stream)
end method buffered-format-integer;

define method buffered-format-integer
    (arg :: <integer>, radix :: limited(<integer>, min: 2, max: 36),
     stream :: <buffered-stream>, sb :: <buffer>) => ()
  local method repeat (arg :: <integer>, digits :: <list>)
	  let (quotient :: <integer>, remainder :: <integer>)
	    = floor/(arg, radix);
	  let digits = pair($digits[remainder], digits);
	  if (zero?(quotient))
	    for (digit in digits)
	      buffered-write-element(stream, sb, digit)
	    end
	  else
	    repeat(quotient, digits)
	  end
	end;
  // Set up for the iteration.
  if (negative?(arg))
    buffered-write-element(stream, sb, '-');
    // Pick off one digit before beginning the iteration to ensure that we
    // don't need Generic-arithmetic.  If arg were the minimum signed
    // machine word, and we simply negated it and called repeat, then it
    // would turn into an integer that was one larger than the maximum
    // signed integer.
    let (quotient :: <integer>, remainder :: <integer>)
      = truncate/(arg, radix);
    if (~ zero?(quotient))
      repeat(- quotient, list($digits[- remainder]))
    else
      buffered-write-element(stream, sb, $digits[- remainder])
    end
  else
    repeat(arg, #())
  end
end method buffered-format-integer;

define method buffered-format-integer
    (arg :: <float>, radix :: limited(<integer>, min: 2, max: 36),
     stream :: <buffered-stream>, buffer :: <buffer>) => ()
  //--- Should we really be this compulsive?
  assert(radix = 10, "Can only print floats in base 10");
  print(arg, stream)
end method buffered-format-integer;
