Module:       common-dylan-internals
Author:       Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <stream-error> (<error>, <format-string-condition>)
  constant slot stream-error-stream :: <stream>,
    required-init-keyword: stream:;
end;

// TODO: yuck.  Would be nicer to just have a condition-to-string method..
// andrewa: note that then you need to update the runtime manager to
// know about the new class too, it is simpler to rely on subclassing
// <format-string-condition>.
define method make
    (class :: subclass(<stream-error>), 
     #rest args, 
     #key stream :: <stream>, 
          format-string, 
          format-arguments = format-string & #[],
     #all-keys)
 => (error :: <stream-error>)
  apply(next-method,
	class,
	stream: stream,
	format-string:    format-string    | "Condition %s for stream %s",
	format-arguments: format-arguments | vector(class, stream),
	args)
end method make;


define class <end-of-stream-error> (<stream-error>)
end class <end-of-stream-error>;

define method make
    (class == <end-of-stream-error>, #key stream :: <stream>)
 => (error :: <end-of-stream-error>)
  next-method(class,
	      stream: stream,
	      format-string:    "End of stream %s",
	      format-arguments: vector(stream))
end method make;

define class <incomplete-read-error> (<end-of-stream-error>)
  constant slot stream-error-sequence, 
    required-init-keyword: sequence:;
  constant slot stream-error-count,
    required-init-keyword: count:;
end class <incomplete-read-error>;

define class <incomplete-write-error> (<end-of-stream-error>)
  constant slot stream-error-count, 
    required-init-keyword: count:;
end class <incomplete-write-error>;



define open generic stream-element-type 
    (stream :: <stream>) => (type :: <type>);

define open generic open-file-stream
    (filename :: <object>, #key, #all-keys) => (stream :: <stream>);

define open generic stream-open? 
    (stream :: <stream>) => (open? :: <boolean>);

define method stream-open? (stream :: <stream>) => (open? :: <boolean>);
  #t
end method stream-open?;

define open generic stream-at-end? 
    (stream :: <stream>) => (at-end? :: <boolean>);

define open generic stream-size 
    (stream :: <stream>) => (size :: false-or(<abstract-integer>));

define method stream-size (stream :: <stream>) => (size :: singleton(#f))
  #f
end method stream-size;


define open abstract class <positionable-stream> (<stream>) end;

define open generic stream-position 
    (stream :: <stream>) => (position);

define open generic stream-position-setter 
    (position, stream :: <stream>) => (position :: <object>);

define open generic adjust-stream-position 
    (stream :: <stream>, delta, #key from) => (position);


define open generic read-element 
    (stream :: <stream>, #key on-end-of-stream) => (element);

define open generic unread-element
    (stream :: <stream>, element) => (element);

define open generic peek
    (stream :: <stream>, #key on-end-of-stream) => (element);

define open generic read
    (stream :: <stream>, n :: <integer>, #key on-end-of-stream)
 => (sequence-or-eof);

define method read (stream :: <stream>, n :: <integer>,
		    #key on-end-of-stream = unsupplied())
 => (elements)
  let elements = make(<vector>, size: n);
  if (supplied?(on-end-of-stream))
    read-into!(stream, n, elements, on-end-of-stream: on-end-of-stream);
  else
    read-into!(stream, n, elements);
  end;
  elements
end method read;


define open generic read-into! 
    (stream :: <stream>, n :: <integer>, sequence :: <mutable-sequence>,
     #key start, on-end-of-stream)
 => (count);


define method read-into!
    (stream :: <stream>, n :: <integer>, sequence :: <mutable-sequence>,
     #key start = 0, on-end-of-stream = unsupplied())
 => (count)
  let limit = min(n + start, sequence.size);
  iterate loop (i = start)
    if (i < limit)
      let elt = read-element(stream, on-end-of-stream: unfound());
      if (found?(elt))
	sequence[i] := elt;
	loop(i + 1);
      elseif (supplied?(on-end-of-stream))
	i - start
      else
	signal(make(<incomplete-read-error>,
		    stream: stream,
		    count: i - start, // seems kinda redundant...
		    sequence: copy-sequence(sequence, start: start, end: i)))
      end
    else
      i - start
    end if;
  end;
end method read-into!;


define open generic discard-input (stream :: <stream>) => ();

define method discard-input (stream :: <stream>) => ()
  #f
end method discard-input;

define open generic stream-input-available? 
    (stream :: <stream>) => (available? :: <boolean>);

define open generic stream-contents 
    (stream :: <stream>, #key clear-contents?) => (sequence :: <sequence>);

define open generic stream-contents-as 
    (type :: <type>, stream :: <stream>, #key clear-contents?)
 => (sequence :: <sequence>);


define open generic write-element 
    (stream :: <stream>, element) => ();

define open generic write 
    (stream :: <stream>, elements :: <sequence>, #key start, \end) => ();

define method write
    (stream :: <stream>, elements :: <sequence>,
     #key start: start-index = 0, end: end-index) => ()
  for (i from start-index below end-index | elements.size)
    write-element(stream, elements[i])
  end;
  #f
end method write;

define open generic force-output 
    (stream :: <stream>,  #key /* synchronize? :: <boolean> */) => ();

define method force-output
    (stream :: <stream>, 
     #key synchronize? :: <boolean>) => ()
  ignore(stream, synchronize?);
end method force-output;

define open generic wait-for-io-completion (stream :: <stream>) => ();
define method wait-for-io-completion (stream :: <stream>) => ()
  ignore(stream);
end method wait-for-io-completion;

define open generic synchronize-output (stream :: <stream>) => ();
define method synchronize-output (stream :: <stream>) => ()
  #f
end method synchronize-output;

define open generic discard-output (stream :: <stream>) => ();
define method discard-output (stream :: <stream>) => ()
  #f
end method discard-output;


