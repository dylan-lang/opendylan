Module:       system-internals
Synopsis:     Macintosh stream accessors for Mac OS Carbon
Author:       Gary Palter, Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// For now...
define constant $preferred-buffer-size = 1024 * 16;

define sealed class <carbon-file-accessor> (<external-file-accessor>)
  slot file-refnum :: false-or(<integer>) = #f;
  slot file-position :: <integer> = -1;
  slot file-mode :: <integer> = -1;
  constant slot file-count-buffer :: <byte-string>
    = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
  constant slot file-ioparam-block :: <byte-string>
    = make(<byte-string>, size: $IOPARAM-SIZE, fill: '\0');
  slot locator, init-keyword: locator:;
end class <carbon-file-accessor>;

/// Ensure that the accessor's locator is a <file-locator>
define method initialize (accessor :: <carbon-file-accessor>, #rest initargs, #key) => ()
  next-method();
  accessor.locator := as(<file-locator>, accessor.locator)
end method initialize;

// An attempt at a portable flexible interface to OS read/write/seek
// functionality.  Legal values for TYPE might include #"file", #"pipe",
// #"tcp", #"udp".  Legal values for LOCATOR depend on TYPE.  
define sideways method platform-accessor-class
    (type == #"file", locator) => (class == <carbon-file-accessor>)
  <carbon-file-accessor>
end method platform-accessor-class;

define method file-handle (accessor :: <carbon-file-accessor>)
 => (handle :: false-or(<machine-word>))
  accessor.file-refnum & as(<machine-word>, accessor.file-refnum)
end method file-handle;

define method accessor-fd (accessor :: <carbon-file-accessor>)
 => (fd :: false-or(<machine-word>))
  accessor.file-handle
end method;

define sealed class <carbon-file-io-error> (<file-error>)
  constant slot cfioe-operation :: <string>, required-init-keyword: operation:;
  constant slot cfioe-status :: <integer>, required-init-keyword: status:;
  constant slot cfioe-additional-information :: false-or(<string>) = #f,
    init-keyword: additional-information:;
  constant slot cfioe-additional-information-args :: false-or(<sequence>) = #f,
    init-keyword: additional-information-args:;
end class <carbon-file-io-error>;

define method condition-to-string
    (error :: <carbon-file-io-error>) => (string :: <string>)
  let reference = file-error-locator(error);
  let operation = cfioe-operation(error);
  let status = cfioe-status(error);
  let additional-information = cfioe-additional-information(error);
  let additional-information-args = cfioe-additional-information-args(error);
  if (zero?(status))
    if (additional-information)
      apply(format-to-string,
	    concatenate("Can't %s %s ", additional-information),
	    operation, reference, additional-information-args)
    else
      format-to-string("Can't %s %s", operation, reference)
    end
  else
    if (additional-information)
      apply(format-to-string,
	    concatenate("MacOS error #%d: Can't %s %s ", additional-information),
	    status, operation, reference, additional-information-args)
    else
      format-to-string("MacOS error #%d: Can't %s %s", status, operation, reference)
    end
  end
end method condition-to-string;

define function carbon-file-error
    (accessor :: <carbon-file-accessor>, operation :: <string>, status :: <integer>,
     additional-information, #rest additional-information-args)
  let reference = as(<string>, accessor.locator);
  error(make(<carbon-file-io-error>,
	     locator: accessor.locator,
	     operation: operation,
	     status: status,
	     additional-information: additional-information,
	     additional-information-args: additional-information-args))
end function carbon-file-error;

define function access-error? (status :: <integer>) => (error? :: <boolean>)
  status = $permErr | status = $afpAccessDenied
end function access-error?;

// Legal values for direction are #"input", #"output", #"input-output"
// Legal values for if-exists are #"new-version", #"overwrite", #"replace",
//                                #"truncate", #"signal", #"append"
// NB #"append" does _not_ imply unix open(2) append semantics, _only_
// that writing is likely to continue from the end.  So its merely a hint
// as to where to go first.
// Legal values for if-does-not-exist are #"signal", #"create"
define method accessor-open
    (accessor :: <carbon-file-accessor>,
     #key direction = #"input",
          if-exists,
          if-does-not-exist,
          fd: initial-file-refnum :: false-or(<machine-word>) = #f,
          file-position: initial-file-position :: false-or(<integer>) = #f,
          file-size: initial-file-size :: false-or(<integer>) = #f,
          asynchronous? :: <boolean> = #f,
          overlapped? :: <boolean> = #f,
	  share? :: <boolean> = #t,
          share-mode :: one-of(#"default", #"exclusive", #"share-read",
			       #"share-write", #"share-read-write") = #"default",
      #all-keys)
 => ()
  ignore(asynchronous?, overlapped?, share?, share-mode);
  block (return)
    if (initial-file-refnum)
      accessor.file-refnum := as(<integer>, initial-file-refnum);
      accessor.file-size :=
	if (initial-file-size) as(<integer>, initial-file-size) else #f end if;
      accessor.file-position :=
	if (initial-file-position) as(<integer>, initial-file-position) else -1 end if;
      return()
    elseif (initial-file-position | initial-file-size)
      error("Cannot create a file accessor which specifies either"
	      "file-position: or file-size: keywords but does not specify fd:");
    end if;
    let path = accessor.locator;
    let exists :: <boolean> = carbon-file-exists?(path);
    let truncate? :: <boolean> = #f;
    select (direction)
      #"input" =>
	accessor.file-mode := $fsRdPerm;
	if-exists := #"overwrite";
	if-does-not-exist := if-does-not-exist | #"signal";
      #"output" =>
	accessor.file-mode := $fsWrPerm;
	if-exists := if-exists | #"new-version";
	if-does-not-exist := if-does-not-exist | #"create";
      #"input-output" =>
	accessor.file-mode := $fsRdWrPerm;
	if-exists := if-exists | #"overwrite";
	if-does-not-exist := if-does-not-exist | #"create";
    end;
    if (exists)
      select (if-exists)
	#"signal" =>
	  return(signal(make(<file-exists-error>, locator: path)));
	#"new-version", #"replace" =>
          let status = carbon-delete-file(path);
          if (status = 0)
            status := carbon-create-file(path);
          end;
	  if (access-error?(status))
	    return(signal(make(<invalid-file-permissions-error>, locator: path)));
	  elseif (status ~= 0)
	    return(carbon-file-error(accessor, "replace", status, #f));
	  end;
	#"overwrite", #"append" => #f;
	#"truncate" =>
	  truncate? := #t;
      end
    else
      select (if-does-not-exist)
	#"signal" =>
	  return(signal(make(<file-does-not-exist-error>, locator: path)));
	#"create" =>
          let status = carbon-create-file(path);
          if (access-error?(status))
	    return(signal(make(<invalid-file-permissions-error>, locator: path)))
	  elseif (status ~= 0)
	    return(carbon-file-error(accessor, "create", status, #f))
	  end;
      end
    end;
    let (refnum, status) = carbon-open(path, accessor.file-mode);
    if (status ~= 0)
      if (access-error?(status))
	return(signal(make(<invalid-file-permissions-error>, locator: path)))
      else
	return(carbon-file-error(accessor, "open", status, #f))
      end
    else
      accessor.file-refnum := refnum;
      *open-accessors*[accessor] := #t;
      if (truncate?)
        let status = carbon-truncate(refnum);
        if (status ~= 0)
          return(carbon-file-error(accessor, "truncate", status, #f))
        end
      end;
      let (fsize, status) = carbon-file-size(refnum);
      if (status ~= 0)
	return(carbon-file-error(accessor, "find size of", status, #f))
      else
        let fpos = if (if-exists == #"append") fsize else 0 end;
        let status
	  = carbon-set-file-position(refnum, fpos, accessor.file-mode);
        if (status ~= 0)
          return(carbon-file-error(accessor, "set position of", status, "to %d", fsize))
        end;
        accessor.file-size     := fsize;
        accessor.file-position := fpos
      end
    end
  end
end method accessor-open;

define method accessor-close 
    (accessor :: <carbon-file-accessor>, #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  let refnum = accessor.file-refnum;
  if (refnum)
    let status = carbon-close(refnum, accessor.locator);
    if (status ~= 0 & ~abort?)
      carbon-file-error(accessor, "close", status, #f)
    else
      accessor.file-refnum := #f;
      remove-key!(*open-accessors*, accessor)
    end
  end;
  #t
end method accessor-close;

define method accessor-open? (accessor :: <carbon-file-accessor>) => (open? :: <boolean>)
  accessor.file-refnum & #t
end method accessor-open?;

define method accessor-preferred-buffer-size (accessor :: <carbon-file-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

define method accessor-file-position (accessor :: <carbon-file-accessor>)
 => (position :: <integer>)
  accessor.file-position
end method accessor-file-position;

define method accessor-set-file-position 
    (accessor :: <carbon-file-accessor>, requested-position :: <integer>)
 => (success? :: <boolean>)
  // Don't complain if the stream is no longer open (i.e., if there's no file refNum)
  // otherwise all errors will appear to be positioning errors.  (Getting new input or
  // output buffers always tries to set the file position before reading/writing.)
  if (accessor.file-refnum)
    let current-position = accessor.file-position;
    if (requested-position ~= current-position)
      let status = carbon-set-file-position(accessor.file-refnum, requested-position,
					    accessor.file-mode);
      if (status ~= 0)
	carbon-file-error(accessor, "set position of", status, "to %d", requested-position);
	#f
      else
	accessor.file-position := requested-position;
	#t
      end
    else
      #t
    end
  end
end method accessor-set-file-position;

define inline function check-for-closed-stream 
    (accessor :: <carbon-file-accessor>, operation :: <string>) => ()
  unless (accessor.file-refnum)
    carbon-file-error(accessor, operation, 0, "using a closed stream")
  end
end function check-for-closed-stream;

define method accessor-read-into!
    (accessor :: <carbon-file-accessor>, stream :: <file-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  check-for-closed-stream(accessor, "read from");
  let buffer :: <buffer> = buffer | stream-input-buffer(stream);
  let position-before-read = accessor.file-position;
  let (nread, status)
    = carbon-read(accessor.file-refnum, accessor.file-count-buffer, buffer, offset, count);
  if (nread > 0)
    accessor.file-position := position-before-read + nread;
    nread
  elseif (status = $eofErr | (status = $noErr & nread = 0))
    nread
  else
    carbon-file-error(accessor, "read from", status, #f)
  end
end method accessor-read-into!;

define method accessor-write-from
    (accessor :: <carbon-file-accessor>, stream :: <file-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer, 
     return-fresh-buffer? = #f)
 => (nwritten :: <integer>, new-buffer :: <buffer>)
  check-for-closed-stream(accessor, "write to");
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let position-before-write = accessor.file-position;
  let (nwritten, status)
    = carbon-write(accessor.file-refnum, accessor.file-count-buffer, buffer, offset, count);
  if (nwritten > 0)
    // NB Can loop until empty, too lazy at the moment
    let position-after-write = position-before-write + nwritten;
    if (position-after-write > accessor.file-size)
      accessor.file-size := position-after-write
    end;
    accessor.file-position := position-after-write
  end;
  if (nwritten ~= count)
    if (status ~= 0)
      carbon-file-error(accessor, "write to", status, #f)
    else
      carbon-file-error(accessor, "write all data to", 0, "(%d requested; %d written)",
			count, nwritten)
    end
  end;
  values(nwritten, buffer)
end method accessor-write-from;

define method accessor-force-output 
    (accessor :: <carbon-file-accessor>, stream :: <file-stream>) => ()
  check-for-closed-stream(accessor, "force output to");
  let status = carbon-force-output(accessor.file-refnum, accessor.file-ioparam-block,
				   accessor.locator);
  if (status ~= 0)
    carbon-file-error(accessor, "force output to", status, #f)
  end
end method accessor-force-output;

define method accessor-newline-sequence (accessor :: <carbon-file-accessor>)
 => (string :: <string>)
  "\r"
end method accessor-newline-sequence;
