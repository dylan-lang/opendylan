Module:       system-internals
Synopsis:     Unix stream accessors (assuming ~ System V release 5.3 semantics)
Author:       Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// For now...
// This number has two be a power of two or all Hell will break loose.
define constant $preferred-buffer-size = 8192; // size for alphas

define sealed class <unix-file-accessor> (<external-file-accessor>)
  slot file-descriptor :: false-or(<integer>) = #f;
  slot file-position :: <integer> = -1;
  constant slot locator,
    required-init-keyword: locator:;
  constant slot asynchronous? :: <boolean> = #f, 
    init-keyword: asynchronous?:;
end class <unix-file-accessor>;

ignore(asynchronous?);

// An attempt at a portable flexible interface to OS read/write/seek
// functionality.  Legal values for TYPE might include #"file", #"pipe",
// #"tcp", #"udp".  Legal values for LOCATOR depend on TYPE.  
define sideways method platform-accessor-class
    (type == #"file", locator)
 => (class :: singleton(<unix-file-accessor>))
  <unix-file-accessor>
end method platform-accessor-class;

define method accessor-fd
  ( the-accessor :: <unix-file-accessor> ) 
 => (the-fd :: false-or(<machine-word>))
  if (the-accessor.file-descriptor)
    as(<machine-word>, the-accessor.file-descriptor)
  end if
end method;

define method file-handle
    (the-accessor :: <unix-file-accessor>) => (the-file-handle :: false-or(<machine-word>))
  the-accessor.accessor-fd
end method file-handle;

// standard rw-r--r-- permissions; constant for now.
define constant $file_create_permissions = 420;

// Legal values for direction are #"input", #"output", #"input-output"
// Legal values for if-exists are #"new-version", #"overwrite", #"replace",
//                                #"truncate", #"signal", #"append"
// NB #"append" does _not_ imply unix open(2) append semantics, _only_
// that writing is likely to continue from the end.  So its merely a hint
// as to where to go first.
// Legal values for if-does-not-exist are #"signal", #"create"
define method accessor-open
    (accessor :: <unix-file-accessor>,
     #key direction = #"input", if-exists, if-does-not-exist,
       fd: initial-file-descriptor = #f,  // :: false-or(type-union(<integer>, <machine-word>))
       file-position: initial-file-position = #f, // :: false-or(<integer>)?
       file-size: initial-file-size = #f, // :: false-or(<integer>)?
     #all-keys) => ()
  block (return)
    if (initial-file-descriptor)
      accessor.file-descriptor := as(<integer>, initial-file-descriptor);
      accessor.file-size :=
	if (initial-file-size) as(<integer>, initial-file-size) else 0 end if;
      accessor.file-position :=
	if (initial-file-position) as(<integer>, initial-file-position)
	else 0 end if;
      return()
    elseif (initial-file-position | initial-file-size)
      error("Cannot create a file accessor which specifies either "
	      "file-position: or file-size: keywords but does not specify "
	      "file-handle:");
    end if;
    // This is a hack which should be replaced by using the file-handle: and
    // file-position: and file-size: keywords
    if (instance?(accessor.locator, <integer>))
      accessor.file-descriptor := accessor.locator;
      accessor.file-size := 0;
      accessor.file-position := 0;
      return()
    end;
    let pathstring = as(<string>, accessor.locator);
    let exists = unix-file-exists?(pathstring);
    let (mode-code, if-exists, if-does-not-exist)
      = select (direction)
	  #"input" =>
	    values($o_rdonly, 
		   #"overwrite",
		   (if-does-not-exist | #"signal"));
	  #"output" =>
	    values(logior($o_wronly, $o_sync),
		   (if-exists | #"new-version"),
		   (if-does-not-exist | #"create"));
	  #"input-output" =>
	    values(logior($o_rdwr, $o_sync),
		   (if-exists | #"overwrite"),
		   (if-does-not-exist | #"create"));
	end;
    let mode-code 
      = if (exists)
	  select (if-exists)
	    #"signal" =>
	      return(signal(make(<file-exists-error>,
				 locator: accessor.locator)));
	    #"new-version", #"replace" =>
	      if (unix-delete-file(pathstring))
		logior(mode-code, $o_creat);
	      else
		let errno = unix-errno-value();
		if (errno = $e_access)
		  return(signal(make(<invalid-file-permissions-error>,
				     locator: accessor.locator)));
		else
		  unix-error("unlink", errno: errno);
		end;
	      end;
	    #"overwrite", #"append" => 
	      mode-code;
	    #"truncate" =>
	      logior(mode-code, $o_trunc);
	  end
	else
	  select (if-does-not-exist)
	    #"signal" =>
	      return(signal(make(<file-does-not-exist-error>,
				 locator: accessor.locator)));
	    #"create" =>
	      logior(mode-code, $o_creat);
	  end
	end;
    let fd = unix-open(pathstring, mode-code, $file_create_permissions);
    if (fd < 0)
      let errno = unix-errno-value();
      if (errno = $e_access)
	return(signal(make(<invalid-file-permissions-error>,
			   locator: accessor.locator)));
      else
	return(unix-error(concatenate("open(\"", pathstring, "\")"),
			  errno: errno));
      end
    else
      accessor.file-descriptor := fd;
      *open-accessors*[accessor] := #t;
      let fsize = unix-lseek(fd, 0, $seek_end);
      let fpos  = if (if-exists == #"append") fsize else 0 end;
      unix-lseek(fd, fpos, $seek_set);
      if (fsize < 0)
	unix-error("lseek")
      else
        accessor.file-size     := fsize;
        accessor.file-position := fpos
      end;
      // IMPORTANT!!
      // Once the file has been created the required reopen behaviour is
      // overwrite.  E.g., if an if-exists: #"truncate" file-stream is
      // reopened after close we don't want it truncated again.
      // accessor.exists-behaviour = #"overwrite";
      // By the same token, if the underlying file has been removed by the
      // time a reopen occurs a signal is appropriate.
      // accessor.not-exists-behaviour = #"signal";
    end
  end
end method accessor-open;

define method accessor-close
    (accessor :: <unix-file-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  let fd = accessor.file-descriptor;
  if (fd)
    if (unix-close(fd) < 0 & ~abort?)
      unix-error("close")
    else
      accessor.file-descriptor := #f;
      remove-key!(*open-accessors*, accessor)
    end
  end;
  #t
end method accessor-close;

define method accessor-open?
    (accessor :: <unix-file-accessor>) => (open? :: <boolean>)
  accessor.file-descriptor & #t
end method accessor-open?;

define method accessor-preferred-buffer-size
    (accessor :: <unix-file-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

// Make a version which doesn't test first
define method accessor-set-file-position
    (accessor :: <unix-file-accessor>, requested-position :: <integer>)
 => (ok? :: <boolean>)
  let old-position = accessor.file-position;
  if (requested-position ~= old-position)
    let new-position = 
      unix-lseek(accessor.file-descriptor, requested-position, $seek_set);
    if (requested-position ~= new-position)
      if (new-position < 0)
	unix-error("lseek");
      else
	error("lseek seeked to wrong postion")
      end;
      #f
    else
      accessor.file-position := new-position;
      #t
    end
  else
    #t
  end
end method accessor-set-file-position;

define method accessor-read-into!
    (accessor :: <unix-file-accessor>, stream :: <file-stream>,
     offset :: <integer>, count :: <integer>, #key buffer)
 => (nread :: <integer>)
  let buffer :: <buffer> = buffer | stream-input-buffer(stream);
  let file-position-before-read = accessor.file-position;
  let nread :: <integer>
    = unix-read(accessor.file-descriptor, buffer, offset, count);
  if (nread < 0)
    unix-error("read");
  elseif (nread > 0)
    accessor.file-position := file-position-before-read + nread;
  end;
  nread
end method accessor-read-into!;

define method accessor-write-from
    (accessor :: <unix-file-accessor>, stream :: <file-stream>,
     offset :: <integer>, count :: <integer>, #key buffer,
     return-fresh-buffer? = #f)
 => (nwritten :: <integer>, new-buffer :: <buffer>)
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let file-position-before-write = accessor.file-position;
  let file-position-after-write = file-position-before-write;
  let nwritten :: <integer>
    = unix-write(accessor.file-descriptor, buffer, offset, count);
  if (nwritten > 0)
    // NB Can loop until empty, too lazy at the moment
    file-position-after-write := file-position-before-write + nwritten;
    if (file-position-after-write > accessor.file-size)
      accessor.file-size := file-position-after-write;
    end;
    accessor.file-position := file-position-after-write;
  end;
  if (nwritten ~= count)
    if (nwritten < 0)
      unix-error("write")
    else
      error("write: didn't write sufficient characters (%d instead of %d)",
            nwritten, count)
    end
  end;
  values(nwritten, buffer)
end method accessor-write-from;

define method accessor-newline-sequence
    (accessor :: <unix-file-accessor>)
 => (string :: <string>)
  "\n"
end method accessor-newline-sequence;
