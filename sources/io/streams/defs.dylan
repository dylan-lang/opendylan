Module:       streams-internals
Synopsis:     Basic definitions for streams
Author:       Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stream position type

//--- This would be a subclass of <magnitude>, if it existed
define open abstract class <stream-position> (<object>)
end class <stream-position>;

//---*** Should be 'type-union(<stream-position>, <integer>)'
define constant <position-type> = <object>;

/// Conditions

define sealed class <stream-position-error> (<stream-error>)
  constant slot stream-error-requested-position, 
    required-init-keyword: position:;
  constant slot stream-error-size-of-stream, 
    required-init-keyword: size:;
end class <stream-position-error>;

define class <stream-closed-error> (<stream-error>)
end class;

define class <stream-not-writable> (<stream-error>)
end class;

define class <stream-not-readable> (<stream-error>)
end class;

/// End-of-stream handling utilities

define function end-of-stream-value (stream :: <stream>, value)
  if (supplied?(value))
    value
  else
    signal(make(<end-of-stream-error>, stream: stream))
  end
end function end-of-stream-value;

define function stream-limit-or-error
    (stream :: <stream>) => (limit :: <position-type>)
  stream-limit(stream)
  | error("Stream %= has no well-defined limit", stream)
end function stream-limit-or-error;


/// Direction checking utilities

define function ensure-readable (stream :: <stream>) => ()
   unless (readable?(stream))
    // Not readable, so the error is either that it's closed or write-only
    if (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
		 format-string: 
		   "Can't read from closed stream"));
    elseif (write-only?(stream))
      error(make(<stream-not-readable>, stream: stream,
		 format-string: 
		   "Can't read from write-only stream"));
    else
      error("Stream %= not readable, don't know why", stream);
    end if;
  end unless;
end function ensure-readable;

define function ensure-writable (stream :: <stream>) => ()
   unless (writable?(stream))
    // Not writable, so the error is either that it's closed or write-only
    if (closed?(stream))
      error(make(<stream-closed-error>, stream: stream,
		 format-string: 
		   "Can't write to closed stream"));
    elseif (read-only?(stream))
      error(make(<stream-not-writable>, stream: stream,
		 format-string: 
		   "Can't write to read-only stream"));
    else
      error("Stream %= not writable, don't know why", stream);
    end if;
  end unless;  
end function ensure-writable;
