Module:       io-internals
Synopsis:     Win32 stream accessors
Author:       Eliot Miranda, Scott McKay, Marc Ferguson, Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $preferred-buffer-size = 1024 * 16;

define sealed class <native-file-accessor> (<external-file-accessor>)
  slot %file-handle :: false-or(<machine-word>) = #f;
  slot file-position :: <integer> = -1,
    init-keyword: file-position:;
  slot actual-file-position :: <integer> = -1;	// The physical file position 
  						// for async access.
  constant slot asynchronous? :: <boolean> = #f, 
    init-keyword: asynchronous?:;
  slot access-lock :: <simple-lock>;  // Lock for accesses - async stream only.
  sealed slot accessor-positionable? :: <boolean> = #f;
  sealed slot accessor-at-end? :: <boolean> = #f;
end class <native-file-accessor>;

define method file-handle
    (the-accessor :: <native-file-accessor>)
 => (handle :: <machine-word>)
  the-accessor.%file-handle
    | error("cannot operate on a closed file")
end method;

// An attempt at a portable flexible interface to OS read/write/seek
// functionality.  Legal values for TYPE might include #"file", #"pipe",
// #"tcp", #"udp".  Legal values for LOCATOR depend on TYPE.  
define sideways method platform-accessor-class
    (type == #"file", locator)
 => (class :: singleton(<native-file-accessor>))
  <native-file-accessor>
end method platform-accessor-class;

define method accessor-fd
    (the-accessor :: <native-file-accessor>)
 => (the-fd :: false-or(<machine-word>))
  the-accessor.file-handle
end method;

// Should really signal a subclass of <file-error> ...
define function win32-file-error
    (accessor :: <native-file-accessor>, operation :: <string>,
     additional-information, #rest additional-information-args)
  let status-message = win32-last-error-message();
  if (additional-information)
    apply(error,
          concatenate("%s: Can't %s ", additional-information),
          status-message, operation, additional-information-args)
  else
    error("%s: Can't %s", status-message, operation)
  end;
  #f
end function win32-file-error;

// Legal values for direction are #"input", #"output", #"input-output"
// Legal values for if-exists are #"new-version", #"overwrite", #"replace",
//                                #"truncate", #"signal", #"append"
// NB #"append" does _not_ imply unix open(2) append semantics, _only_
// that writing is likely to continue from the end.  So its merely a hint
// as to where to go first.
// Legal values for if-does-not-exist are #"signal", #"create"
define method accessor-open
    (accessor :: <native-file-accessor>, locator :: <machine-word>,
     #key direction = #"input", if-exists, if-does-not-exist,
       file-descriptor: initial-file-handle = #f, // :: false-or(<machine-word>)
       file-position: initial-file-position = #f, // :: false-or(<integer>)?
       overlapped? :: <boolean> = #f,
     #all-keys) => ();
  accessor.%file-handle := initial-file-handle;
  accessor.accessor-positionable?
    := win32-file-type(initial-file-handle) = $FILE_TYPE_DISK;
  accessor.file-position
    := if (initial-file-position)
         as(<integer>, initial-file-position)
       else
         win32-set-file-position(initial-file-handle, 0, $FILE_CURRENT) | 0
       end if;
  if (accessor.asynchronous?)
    accessor.access-lock := make(<simple-lock>);
  end if;
end method accessor-open;

define method accessor-close (accessor :: <native-file-accessor>,
			      #key abort? = #f, wait? = #t)
			  => (closed? :: <boolean>)
  if (accessor.asynchronous?)
    enqueue-operation(make(<pending-operation>, operation: accessor-close-async,
			   accessor: accessor));
    if (wait?)
      async-wait-for-completion(accessor);
      async-check-for-errors(accessor);
    end if;
  else
    accessor-close-internal(accessor);
  end if;
  #t
end method accessor-close;

define function accessor-close-async (op :: <pending-operation>) => ()
  accessor-close-internal(op.pending-accessor);
end function accessor-close-async;

define function accessor-close-internal (accessor :: <native-file-accessor>) 
				     => ()
  let handle = accessor.%file-handle;
  if (handle)
    if (~win32-close(handle) /* & ~abort? */)
      win32-file-error(accessor, "close", "handle %=", handle)
    else
      accessor.%file-handle := #f;
    end
  end;
end function accessor-close-internal;

define method accessor-wait-for-completion (accessor :: <native-file-accessor>)
					=> ()
  async-wait-for-completion(accessor);
  async-check-for-errors(accessor);
end method accessor-wait-for-completion;

define method accessor-open?
    (accessor :: <native-file-accessor>) => (open? :: <boolean>)
  accessor.file-handle & #t
end method accessor-open?;

define method accessor-preferred-buffer-size
    (accessor :: <native-file-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

define method accessor-size
    (accessor :: <native-file-accessor>)
 => (size :: false-or(<integer>))
  accessor.accessor-positionable?
    & win32-file-size(accessor.file-handle)
end method accessor-size;

define method accessor-position
    (accessor :: <native-file-accessor>)
 => (position :: <integer>)
  accessor.file-position
end;

define method accessor-position-setter
    (position :: <integer>, accessor :: <native-file-accessor>)
 => (position :: <integer>)
  let handle = accessor.file-handle;

  if (position ~= accessor.file-position)
    if (accessor.asynchronous?)
      accessor.accessor-at-end? := #f;
      accessor.file-position := position;
      position
    else
      let success = win32-set-file-position(handle, position, $FILE_BEGIN);
      if (success)
        accessor.accessor-at-end? := #f;
	accessor.file-position := position;
        position
      else
	win32-file-error(accessor, "set position", "to %d", position);
      end
    end if
  else
    position
  end
end method accessor-position-setter;

define method accessor-read-into!
    (accessor :: <native-file-accessor>, stream :: <file-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  if (accessor.accessor-at-end?)
    0
  else
    let handle = accessor.file-handle;
    let buffer :: <buffer> = buffer | stream-input-buffer(stream);
    let file-position-before-read = accessor.file-position;
    let nread :: false-or(<integer>) = #f;

    if (accessor.asynchronous?)
      async-wait-for-overlapping-write-completion(accessor, 
                                                  file-position-before-read, 
                                                  count);

      with-lock (accessor.access-lock)
        if (accessor.actual-file-position ~= file-position-before-read)
          if (~ win32-set-file-position(handle,
                                        file-position-before-read, $FILE_BEGIN))
            win32-file-error(accessor, "set position", 
                             "to %d", file-position-before-read);
          end if;
        end if;
	
        nread := win32-read(handle, buffer, offset, count);
        if (nread)
          accessor.actual-file-position := file-position-before-read + nread;
        end if;
      end with-lock;
    else
      nread := win32-read(handle, buffer, offset, count);
    end if;

    if (~nread)
      win32-file-error(accessor, "read", #f)
    elseif (nread = 0)
      accessor.accessor-at-end? := #t;
    else
      accessor.file-position := file-position-before-read + nread;
    end;
    nread
  end if
end method accessor-read-into!;

define method accessor-write-from
	(accessor :: <native-file-accessor>, stream :: <file-stream>,
	 offset :: <buffer-index>, count :: <buffer-index>, #key buffer, 
	 return-fresh-buffer? = #f)
     => (number-of-bytes-written :: <integer>, new-buffer :: <buffer>)
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let file-position-before-write :: <integer> = accessor.file-position;

  let number-of-bytes-written :: <integer> =
    if (accessor.asynchronous?)
      async-check-for-errors(accessor);
      buffer := enqueue-write(
	make(<pending-write>, operation: accessor-write-from-async,
	     stream: stream, accessor: accessor, 
	     file-offset: file-position-before-write, count: count, 
	     buffer: buffer, buffer-offset: offset),
	return-fresh-buffer?
      );
      count
    else
      accessor-write-from-internal(buffer, accessor, stream, 
      				   file-position-before-write, offset, count)
    end if;

  if (number-of-bytes-written > 0)
    // Windows should always perform complete write.
    let file-position-after-write :: <integer> = 
      file-position-before-write + number-of-bytes-written;
    accessor.file-position := file-position-after-write;
  end if;

  values(number-of-bytes-written, buffer)
end method accessor-write-from;

define function accessor-write-from-async (op :: <pending-write>) => ()
  accessor-write-from-internal(
	op.pending-buffer, op.pending-accessor, op.pending-stream, 
	op.pending-file-offset, op.pending-buffer-offset, op.pending-count
  );
end function accessor-write-from-async;

define function accessor-write-from-internal
	(buffer :: <buffer>, accessor :: <native-file-accessor>, 
	 stream :: <file-stream>, file-offset :: <integer>,
	 buffer-offset :: <buffer-index>, count :: <buffer-index>)
     => (number-of-bytes-written :: <integer>)

  let handle = accessor.file-handle;
  let number-of-bytes-written :: false-or(<integer>) = #f;

  if (accessor.asynchronous?)
    with-lock (accessor.access-lock)
      if (accessor.actual-file-position ~= file-offset)
	if (~ win32-set-file-position(handle, file-offset, $FILE_BEGIN))
	  win32-file-error(accessor, "set position", 
			   "to %d", file-offset);
	end if;
      end if;

      number-of-bytes-written := 
        win32-write(handle, buffer, buffer-offset, count);

      if (number-of-bytes-written)
	accessor.actual-file-position := 
		file-offset + number-of-bytes-written;
      end if;
    end with-lock;
  else
    number-of-bytes-written := 
      win32-write(handle, buffer, buffer-offset, count);
  end if;


  if (number-of-bytes-written)
    if (number-of-bytes-written ~= count)
      // Should use win32-file-error or variant here ...
      error("win32-write: wrote fewer characters than asked (%d instead of %d)",
	    number-of-bytes-written, count);
    end if;
  else
    win32-file-error(accessor, "write", #f);
  end;
  number-of-bytes-written
end function accessor-write-from-internal;

define method accessor-synchronize
    (accessor :: <native-file-accessor>,
     stream :: <file-stream>)
 => ()
  let handle = accessor.file-handle;
  let success :: <boolean> = win32-synchronize(handle);
  if (~success)
    win32-file-error(accessor, "force output", #f)
  end
end method accessor-synchronize;

define method accessor-newline-sequence
    (accessor :: <native-file-accessor>)
 => (string :: <string>)
  "\r\n"
end method accessor-newline-sequence;
