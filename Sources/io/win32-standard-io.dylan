Module:       io-internals
Synopsis:     *standard-input*, *standard-output*, *standard-error*
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// From WINBASE.H
define constant $STD_INPUT_HANDLE  = -10;
define constant $STD_OUTPUT_HANDLE = -11;
define constant $STD_ERROR_HANDLE  = -12;

// FormatMessage
define constant FORMAT_MESSAGE_FLAGS    = #x00001100;
define constant FORMAT_MESSAGE_LANGUAGE = #x00000400;

// NOTE -- Should probably have one of these per thread, possibly per stream ...
define variable actual-count-ptr =
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
           (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
        => (pointer :: <raw-c-pointer>)
         (integer-as-raw(0), integer-as-raw(4))
        end));

/*
define function force-object-black (buffer :: <buffer>) => ()
  // To force an object to turn black, read it's wrapper and write it back.
  let zero = integer-as-raw(0);
  primitive-element(buffer, zero, zero)
    := primitive-element(buffer, zero, zero)
end function;

define function win32-read
    (handle :: <machine-word>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nread :: false-or(<integer>))
  // If the OS call fails, it might be because of a GC read/write barrier.
  // Try the operation again a couple of times after triggering the GC
  // to blacken the object, just in case.
  // THIS IS A TEMPORARY HACK!
  force-object-black(data); 
  win32-read-internal(handle, data, offset, count)
end function;
*/

define function win32-read  //  -internal
    (handle :: <machine-word>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nread :: false-or(<integer>))
  let success? = primitive-raw-as-boolean
                   (%call-c-function ("ReadFile", c-modifiers: "__stdcall")
                        (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
                         count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
                         lpOverlapped :: <raw-c-pointer>)
                     => (success? :: <raw-c-signed-int>)
                      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
                       primitive-cast-raw-as-pointer
                         (primitive-machine-word-add
			    (primitive-cast-pointer-as-raw
			       (primitive-repeated-slot-as-raw(data, primitive-repeated-slot-offset(data))), 
                             primitive-cast-pointer-as-raw(integer-as-raw(offset)))),
                       integer-as-raw(count),
                       primitive-cast-raw-as-pointer
                         (primitive-unwrap-machine-word(actual-count-ptr)),
                       primitive-cast-raw-as-pointer(integer-as-raw(0)))
                    end);
  success? & raw-as-integer
               (primitive-c-unsigned-long-at
                  (primitive-unwrap-machine-word(actual-count-ptr),
                   integer-as-raw(0),
                   integer-as-raw(0)))
end function win32-read;  //  -internal;

/*
define function win32-write
    (handle :: <machine-word>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nwritten :: false-or(<integer>))
  // If the OS call fails, it might be because of a GC read/write barrier.
  // Try the operation again a couple of times after triggering the GC
  // to blacken the object, just in case.
  // THIS IS A TEMPORARY HACK!
  force-object-black(data);
  win32-write-internal(handle, data, offset, count);
end function;
*/

define function win32-write  //  -internal
    (handle :: <machine-word>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nwritten :: false-or(<integer>))
  let success? = primitive-raw-as-boolean
                   (%call-c-function ("WriteFile", c-modifiers: "__stdcall")
                        (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
                         count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
                         lpOverlapped :: <raw-c-pointer>)
                     => (success? :: <raw-c-signed-int>)
                      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
                       primitive-cast-raw-as-pointer
                         (primitive-machine-word-add
			    (primitive-cast-pointer-as-raw
			       (primitive-repeated-slot-as-raw(data, primitive-repeated-slot-offset(data))), 
                             primitive-cast-pointer-as-raw(integer-as-raw(offset)))),
                       integer-as-raw(count),
                       primitive-cast-raw-as-pointer
                         (primitive-unwrap-machine-word(actual-count-ptr)),
                       primitive-cast-raw-as-pointer(integer-as-raw(0)))
                    end);
  success? & raw-as-integer
               (primitive-c-unsigned-long-at
                  (primitive-unwrap-machine-word(actual-count-ptr),
                   integer-as-raw(0),
                   integer-as-raw(0)))
end function win32-write; //  -internal;

define function win32-force-output (handle :: <machine-word>) => (success? :: <boolean>)
  primitive-raw-as-boolean
    (%call-c-function ("FlushFileBuffers", c-modifiers: "__stdcall")
         (handle :: <raw-c-pointer>) => (success? :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)))
      end)
end function win32-force-output;

// NOTE -- Should probably have one of these per thread ...
define variable message-buffer-ptr =
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
           (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
        => (pointer :: <raw-c-pointer>)
         (integer-as-raw(0), integer-as-raw(4))
        end));

define function win32-last-error-message () => (message :: <string>)
  let status = primitive-wrap-machine-word
		 (%call-c-function ("GetLastError", c-modifiers: "__stdcall")
		      () => (status :: <raw-c-unsigned-long>)
		    ()
		  end);
  %call-c-function ("FormatMessageA", c-modifiers: "__stdcall")
      (flags :: <raw-c-unsigned-long>, lpSource :: <raw-c-pointer>,
       message-id :: <raw-c-unsigned-long>, language-id :: <raw-c-unsigned-long>,
       lpBuffer :: <raw-c-pointer>, bytes :: <raw-c-unsigned-long>,
       lpArguments :: <raw-c-pointer>)
   => (count :: <raw-c-unsigned-long>)
    (integer-as-raw(FORMAT_MESSAGE_FLAGS),
     primitive-cast-raw-as-pointer(integer-as-raw(0)),
     primitive-unwrap-machine-word(status),
     integer-as-raw(FORMAT_MESSAGE_LANGUAGE),
     primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(message-buffer-ptr)),
     integer-as-raw(0),
     primitive-cast-raw-as-pointer(integer-as-raw(0)))
  end;
  let message = primitive-raw-as-string
                  (primitive-c-pointer-at
                     (primitive-unwrap-machine-word(message-buffer-ptr),
                      integer-as-raw(0),
                      integer-as-raw(0)));
  %call-c-function ("LocalFree", c-modifiers: "__stdcall")
     (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
    (primitive-c-pointer-at(primitive-unwrap-machine-word(message-buffer-ptr),
                            integer-as-raw(0),
                            integer-as-raw(0)))
  end;
  message
end function win32-last-error-message;

// Should really signal a distinct error class ...
define function win32-console-error (operation :: <string>)
  let status-message = win32-last-error-message();
  error("%s: Can't %s", status-message, operation);
  #f
end function win32-console-error;


/// Here's the actual console implementation ...

define class <console-stream-accessor> (<external-stream-accessor>)
end class <console-stream-accessor>;

define class <console-stream> (<double-buffered-stream>,
                               <byte-char-element-stream>,
                               <external-stream>)
  inherited slot stream-element-type = <byte-character>;
  constant slot handle :: <integer>, required-init-keyword: handle:;
  slot console :: false-or(<machine-word>) = #f;
  slot accessor :: false-or(<console-stream-accessor>) = #f,
    init-keyword: accessor:;  // inherited from <external-stream>
end class <console-stream>;

define method initialize (stream :: <console-stream>, #rest initargs, #key) => ()
  next-method();
  stream.accessor := make(<console-stream-accessor>);
  if (stream-direction(stream) = #"input")
    stream-input-buffer(stream) := make(<buffer>, size: 128)
  else
    stream-output-buffer(stream) := make(<buffer>, size: 1024);
    // Force first attempt to get an output buffer to find the console
    stream-output-buffer(stream).buffer-next := stream-output-buffer(stream).buffer-size
  end
end method initialize;

define variable *standard-input* 
  = make(<console-stream>, handle: $STD_INPUT_HANDLE, direction: #"input");

define variable *standard-output*
  = make(<console-stream>, handle: $STD_OUTPUT_HANDLE, direction: #"output");

define variable *standard-error*
  = make(<console-stream>, handle: $STD_ERROR_HANDLE, direction: #"output");

define function ensure-console (stream :: <console-stream>) => ()
  local method call-succeeded? (result :: <machine-word>) => (success :: <boolean>)
          primitive-machine-word-not-equal?
	    (primitive-unwrap-machine-word(result),
	     integer-as-raw(-1))
          & primitive-machine-word-not-equal?
	      (primitive-unwrap-machine-word(result),
	       integer-as-raw(0))
        end method;
  local method get-handle () => (handle :: <machine-word>)
          primitive-wrap-machine-word
            (primitive-cast-pointer-as-raw
               (%call-c-function ("GetStdHandle", c-modifiers: "__stdcall")
                    (nStdHandle :: <raw-c-unsigned-long>)
                 => (handle :: <raw-c-pointer>)
                  (integer-as-raw(stream.handle))
                end))
        end method;
  unless (stream.console)
    let handle = get-handle();
    if (call-succeeded?(handle))
      stream.console := handle
    elseif (primitive-raw-as-boolean
              (%call-c-function ("AllocConsole", c-modifiers: "__stdcall")
                   () => (success? :: <raw-c-signed-int>)
                 ()
               end))
      let handle = get-handle();
      stream.console := call-succeeded?(handle) & handle
    else
      win32-console-error("create a console window")
    end;
    unless (stream.console)
      win32-console-error("locate the console window")
    end
  end
end function ensure-console;

define function flush-stdout () => ()
  let ob = stream-output-buffer(*standard-output*);
  if (ob & ~(ob.buffer-start = ob.buffer-end))
    force-output(*standard-output*)
  end
end function flush-stdout;

// Doesn't really close the stream cause you can't really close
// console streams for Windows.  Just flushes standard output.  This
// is only expected to be called on application exit.
define method close
    (stream :: <console-stream>,
     #key abort? = #f) => ()
  unless (abort?)
    if (stream.stream-direction == #"output") 
      let ob = stream-output-buffer(stream);
      if (ob & ~(ob.buffer-start = ob.buffer-end))
	force-output(stream);
      end if;
    end if;
  end;
  //  DON'T CALL NEXT-METHOD()!! Lower level close methods shouldn't
  //  be called on console streams.
end method close;

/// Input protocol ...

define method do-next-input-buffer
    (stream :: <console-stream>, #key wait? = #t, bytes = 1)
 => (buffer :: false-or(<buffer>))
  ignore(wait?, bytes);
  ensure-console(stream);
  next-method()
end method do-next-input-buffer;

define method stream-input-available?
    (stream :: <console-stream>) => (input-available? :: <boolean>)
  stream-direction(stream) = #"input"
  & (stream-input-buffer(stream).buffer-next < stream-input-buffer(stream).buffer-end
     | do-input-available-at-source?(stream))
end method stream-input-available?;

define method do-input-available-at-source?
    (stream :: <console-stream>) => (input-available? :: <boolean>)
  // Need something here (PeekConsole)?
  #t
end method do-input-available-at-source?;

define method read-element
    (stream :: <console-stream>, #key on-end-of-stream)
 => (element :: <object>)
  flush-stdout();
  next-method()
end method read-element;

define method peek
    (stream :: <console-stream>, #key on-end-of-stream)
 => (element :: <object>)
  flush-stdout();
  next-method()
end method peek;

define method read
    (stream :: <console-stream>, n :: <integer>, #key on-end-of-stream)
 => (elements :: <object>)
  flush-stdout();
  next-method()
end method read;

define method read-into!
    (stream :: <console-stream>, 
       n :: <integer>, sequence :: <mutable-sequence>,
     #key start = 0, on-end-of-stream)
 => (count-or-eof :: <object>)
  flush-stdout();
  next-method()
end method read-into!;

define method read-line
    (stream :: <console-stream>, #key on-end-of-stream)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  flush-stdout();
  next-method()
end method read-line;

define method accessor-read-into!
    (accessor :: <console-stream-accessor>, stream :: <console-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  ignore(accessor);
  let bufv = as(<vector>, buffer | stream-input-buffer(stream));
  // N.B. No checking for sufficient length, e.g.
  // if (offset + count > buffer.size) error "Argh!!" end;
  let nread = win32-read(stream.console, bufv, offset, count);
  unless (nread)
    win32-console-error("read from console window")
  end;
  nread
end method accessor-read-into!;


/// Output protocol ...

define method do-next-output-buffer
    (stream :: <console-stream>, #key bytes = 1)
 => (buffer :: false-or(<buffer>))
  ignore(bytes);
  ensure-console(stream);
  next-method()
end method do-next-output-buffer;

define method write-element
    (stream :: <console-stream>, element :: <character>) => ()
  next-method();
  if (element == '\n' | element == '\r')
    force-output(stream)
  end
end method write-element;

define method write-line
    (stream :: <console-stream>, elements :: <string>,
     #key start: _start = 0, end: _end = #f) => ()
  next-method();
  force-output(stream);
end method write-line;

define method new-line
    (stream :: <console-stream>) => ()
  next-method();
  force-output(stream)
end method new-line;

define method accessor-write-from
    (accessor :: <console-stream-accessor>, stream :: <console-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer,
     return-fresh-buffer? = #f)
 => (number-of-bytes-written :: <integer>, new-buffer :: <buffer>)
  ignore(accessor);
  let buffer = buffer | stream-output-buffer(stream);
  let bufv = as(<vector>, buffer);
  // N.B. No checking for sufficient length, e.g.
  // if (offset + count > buffer.size) error "Argh!!" end;
  let nwritten = win32-write(stream.console, bufv, offset, count);
  if (~nwritten)
    win32-console-error("write to console window")
  elseif (nwritten = count)
    nwritten
  else
    // Should use win32-console-error or variant here ...
    error("write: didn't write sufficient characters (%d instead of %d)",
          nwritten, count)
  end;
  values(nwritten, buffer)
end method accessor-write-from;

define method accessor-force-output
    (accessor :: <console-stream-accessor>,
     stream :: <console-stream>)
 => ()
  ignore(accessor);
  win32-force-output(stream.console);
end method accessor-force-output;

define method do-force-output-buffers
    (stream :: <console-stream>) => ()
  next-method();
  let sb :: <buffer> = stream-output-buffer(stream);
  sb.buffer-next := 0;
  sb.buffer-end := 0;
  values()
end method do-force-output-buffers;

define method accessor-newline-sequence
    (accessor :: <console-stream-accessor>)
 => (newline-sequence :: <sequence>);
  "\r\n"
end method accessor-newline-sequence;

define method accessor-close
    (accessor :: <console-stream-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  #f
end method;

