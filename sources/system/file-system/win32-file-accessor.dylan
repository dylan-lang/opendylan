Module:       system-internals
Synopsis:     Win32 stream accessors
Author:       Eliot Miranda, Scott McKay, Marc Ferguson, Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $preferred-buffer-size = 1024 * 16;

define sealed class <win32-file-accessor> (<external-file-accessor>)
  slot file-handle :: false-or(<machine-word>) = #f;
  slot file-position :: <integer> = -1;
  slot actual-file-position :: <integer> = -1;	// The physical file position 
  						// for async access.
  constant slot locator, 
    required-init-keyword: locator:;
  constant slot asynchronous? :: <boolean> = #f, 
    init-keyword: asynchronous?:;
  slot access-lock :: <simple-lock>;  // Lock for accesses - async stream only.
end class <win32-file-accessor>;

// An attempt at a portable flexible interface to OS read/write/seek
// functionality.  Legal values for TYPE might include #"file", #"pipe",
// #"tcp", #"udp".  Legal values for LOCATOR depend on TYPE.  
define sideways method platform-accessor-class
    (type == #"file", locator)
 => (class :: singleton(<win32-file-accessor>))
  <win32-file-accessor>
end method platform-accessor-class;

define method accessor-fd
  ( the-accessor :: <win32-file-accessor> ) 
 => (the-fd :: false-or(<machine-word>))
  if (the-accessor.file-handle)
    the-accessor.file-handle
  end if
end method;

// Should really signal a subclass of <file-error> ...
define function win32-file-error
    (accessor :: <win32-file-accessor>, operation :: <string>,
     additional-information, #rest additional-information-args)
  let reference = as(<string>, accessor.locator);
  let status-message = win32-last-error-message();
  if (additional-information)
    apply(error,
          concatenate("%s: Can't %s %s ", additional-information),
          status-message, operation, reference, additional-information-args)
  else
    error("%s: Can't %s %s", status-message, operation, reference)
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
    (accessor :: <win32-file-accessor>,
     #key direction = #"input", if-exists, if-does-not-exist,
       fd: initial-file-handle = #f,  // :: false-or(<machine-word>)
       file-position: initial-file-position = #f, // :: false-or(<integer>)?
       file-size: initial-file-size = #f, // :: false-or(<integer>)?
       overlapped? :: <boolean> = #f,
       share? :: <boolean> = #t, // only shared access allowed in the past
       share-mode :: one-of(#"default", #"exclusive", #"share-read",
			    #"share-write", #"share-read-write") = #"default",
     #all-keys) => ();
  block (return)
    if (initial-file-handle)
      accessor.file-handle := as(<machine-word>, initial-file-handle);
      accessor.file-size :=
	if (initial-file-size) as(<integer>, initial-file-size) else #f end if;
      accessor.file-position :=
	if (initial-file-position) as(<integer>, initial-file-position)
	else -1 end if;
      return()
    elseif (initial-file-position | initial-file-size)
      error("Cannot create a file accessor which specifies either"
	      "file-position: or file-size: keywords but does not specify"
	      "file-handle:");
    end if;
    select (direction)
      #"input" =>
	if-exists := #"overwrite";
	if-does-not-exist := if-does-not-exist | #"signal";
      #"output" =>
	if-exists := if-exists | #"new-version";
	if-does-not-exist := if-does-not-exist | #"create";
      #"input-output" =>
	if-exists := if-exists | #"overwrite";
	if-does-not-exist := if-does-not-exist | #"create";
    end;
    let fdwAccess
      = select (direction)
	  #"input"        => $GENERIC_READ;
	  #"output"       => $GENERIC_WRITE;
	  #"input-output" => logior($GENERIC_READ, $GENERIC_WRITE);
	end;
    // Actually the #"default" share-mode doesn't really make a sense
    // at all it's here for backward compatibility only.  The default
    // translates as: 
    // 
    // If it's input, allow others to read and nobody else to write.
    // That isn't senseless but but isn't consistent with the behavior
    // of input-output.
    // If it's output, allow others to write but nobody to read.  Why
    // on earth allow others to write?
    // If it's input-output allow others to read or write.  That
    // makes no sense.  The logic of the others should have the access
    // be exclusive for this case.
    // I expect the default was intended to be share read and write,
    // but somebody thought  you couldn't have the access different
    // from the direction somehow?
    if (share-mode = #"default" & (~share?))
      share-mode := #"exclusive";
    end if;
    let fdwShareMode
      =  select (share-mode by \==)
	   #"default" =>
	     select (direction)
	       #"input"        => $FILE_SHARE_READ;
	       #"output"       => $FILE_SHARE_WRITE;
	       #"input-output" => 
		 logior($FILE_SHARE_READ, $FILE_SHARE_WRITE);
	     end select;
	   #"exclusive" => 0;
	   #"share-read" => $FILE_SHARE_READ;
	   #"share-write" => $FILE_SHARE_WRITE;
	   #"share-read-write" => 
	     logior($FILE_SHARE_READ, $FILE_SHARE_WRITE);
	 end select;
    let path = as(<string>, accessor.locator);
    let exists :: <boolean> = win32-file-exists?(path);
    let fdwCreate = 0;
    if (exists)
      select (if-exists)
	#"signal" =>
	  return(signal(make(<file-exists-error>, locator: accessor.locator)));
	#"new-version", #"replace" =>
	  fdwCreate := $CREATE_ALWAYS;
	#"overwrite", #"append" =>
	  fdwCreate := $OPEN_EXISTING;
	#"truncate" =>
	  fdwCreate := $TRUNCATE_EXISTING;
      end
    else
      select (if-does-not-exist)
	#"signal" =>
	  return(signal(make(<file-does-not-exist-error>, locator: accessor.locator)));
	#"create" =>
	  fdwCreate := $CREATE_NEW;
      end
    end;
    let handle = 
      win32-open/create(path, fdwAccess, fdwShareMode, fdwCreate,
			overlapped?: overlapped?);
    if (handle)
      accessor.file-handle := handle;
      *open-accessors*[accessor] := #t;
      let fsize = win32-file-size(handle);
      let fpos  = if (if-exists == #"append") fsize else 0 end;
      if (fsize)
	let success :: <boolean> = win32-set-file-position(handle, fpos);
	if (success)
	  accessor.file-size     := fsize;
	  accessor.file-position := fpos;
	else
	  return(win32-file-error(accessor, "set position of", "to %d", 0))
	end
      else
	return(win32-file-error(accessor, "find size of", #f))
      end
    else
      if (win32-access-error?())
	return(signal(make(<invalid-file-permissions-error>, locator: accessor.locator)))
      else
	return(win32-file-error(accessor, "open/create", #f))
      end if
    end if;
    if (accessor.asynchronous?)
      accessor.access-lock := make(<simple-lock>);
    end if;
  end block;
  accessor.actual-file-position := accessor.file-position;
end method accessor-open;

define method accessor-close (accessor :: <win32-file-accessor>,
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

define function accessor-close-internal (accessor :: <win32-file-accessor>) 
				     => ()
  let handle = accessor.file-handle;
  if (handle)
    if (~win32-close(handle) /* & ~abort? */)
      win32-file-error(accessor, "close", #f)
    else
      accessor.file-handle := #f;
      remove-key!(*open-accessors*, accessor);
    end
  end;
end function accessor-close-internal;

define method accessor-wait-for-completion (accessor :: <win32-file-accessor>)
					=> ()
  async-wait-for-completion(accessor);
  async-check-for-errors(accessor);
end method accessor-wait-for-completion;

define method accessor-open?
    (accessor :: <win32-file-accessor>) => (open? :: <boolean>)
  accessor.file-handle & #t
end method accessor-open?;

define method accessor-preferred-buffer-size
    (accessor :: <win32-file-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

define method accessor-file-position
    (accessor :: <win32-file-accessor>)
 => (position :: <integer>)
  accessor.file-position
end;

define method accessor-set-file-position
    (accessor :: <win32-file-accessor>, requested-position :: <integer>)
 => (ok? :: <boolean>)
  let handle = accessor.file-handle;

  // Oddly enough we don't want to flag the error here.  Since getting
  // new input and output buffers does a file position set before
  // anything else happens if we flag the false file-handle here all of
  // the errors look like position errors when what the user was doing
  // was reading or writing.  So just skip the set position if it's false.

  if (handle & (requested-position ~= accessor.file-position))
    if (accessor.asynchronous?)
      accessor.file-position := requested-position;
      #t
    else
      let success :: <boolean> = 
	win32-set-file-position(handle, requested-position);
      if (success)
	accessor.file-position := requested-position;
	#t
      else
	win32-file-error(accessor, "set position of", 
			 "to %d", requested-position);
	#f
      end
    end if
  else
    #t
  end
end method accessor-set-file-position;

define method accessor-read-into!
    (accessor :: <win32-file-accessor>, stream :: <file-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  let handle = accessor.file-handle;
  unless (handle)
    error("Can't read from closed stream over file %s",
	  as(<string>, accessor.locator));
  end unless; 
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
				      file-position-before-read))
	  win32-file-error(accessor, "set position of", 
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
    win32-file-error(accessor, "read from", #f)
  else
    accessor.file-position := file-position-before-read + nread;
  end;
  nread
end method accessor-read-into!;

define method accessor-write-from
	(accessor :: <win32-file-accessor>, stream :: <file-stream>,
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
    if (file-position-after-write > accessor.file-size)
      accessor.file-size := file-position-after-write;
    end;
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
	(buffer :: <buffer>, accessor :: <win32-file-accessor>, 
	 stream :: <file-stream>, file-offset :: <integer>,
	 buffer-offset :: <buffer-index>, count :: <buffer-index>)
     => (number-of-bytes-written :: <integer>)

  let handle = accessor.file-handle;
  unless (handle)
    error("Can't write to closed stream over file %s",
	  as(<string>, accessor.locator));
  end unless; 
  let number-of-bytes-written :: false-or(<integer>) = #f;

  if (accessor.asynchronous?)
    with-lock (accessor.access-lock)
      if (accessor.actual-file-position ~= file-offset)
	if (~ win32-set-file-position(handle,
				      file-offset))
	  win32-file-error(accessor, "set position of", 
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
    win32-file-error(accessor, "write to", #f);
  end;
  number-of-bytes-written
end function accessor-write-from-internal;

define method accessor-force-output
    (accessor :: <win32-file-accessor>,
     stream :: <file-stream>)
 => ()

  let handle = accessor.file-handle;
  unless (handle)
    error("accessor-force-output called on closed stream over file %s",
	  as(<string>, accessor.locator));
  end unless; 

  let success :: <boolean> = win32-force-output(handle);
  if (~success)
    win32-file-error(accessor, "force output to", #f)
  end
end method accessor-force-output;

define method accessor-newline-sequence
    (accessor :: <win32-file-accessor>)
 => (string :: <string>)
  "\r\n"
end method accessor-newline-sequence;
