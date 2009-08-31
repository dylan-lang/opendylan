Module:       io-internals
Synopsis:     Unix stream accessors (assuming ~ System V release 5.3 semantics)
Author:       Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1994-2001 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Native file accessor class for POSIX platforms

define sealed class <native-file-accessor> (<external-file-accessor>)
  slot file-descriptor :: false-or(<integer>) = #f,
    init-keyword: file-descriptor:;
  slot file-position :: <integer> = 0,
    init-keyword: file-position:;
  constant slot asynchronous? :: <boolean> = #f, 
    init-keyword: asynchronous?:;
  sealed slot accessor-positionable? :: <boolean> = #f;
  sealed slot accessor-preferred-buffer-size :: <integer> = 0;
  sealed slot accessor-at-end? :: <boolean> = #f;
end class <native-file-accessor>;

ignore(asynchronous?);

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
  if (the-accessor.file-descriptor)
    as(<machine-word>, the-accessor.file-descriptor)
  end if
end method;

// Legal values for direction are #"input", #"output", #"input-output"
// Legal values for if-exists are #"new-version", #"overwrite", #"replace",
//                                #"truncate", #"signal", #"append"
// NB #"append" does _not_ imply unix open(2) append semantics, _only_
// that writing is likely to continue from the end.  So its merely a hint
// as to where to go first.
// Legal values for if-does-not-exist are #"signal", #"create"
define method accessor-open
    (accessor :: <native-file-accessor>, locator :: <integer>,
     #key direction = #"input", if-exists, if-does-not-exist,
       file-descriptor: initial-file-descriptor,
       file-position: initial-file-position = #f, // :: false-or(<integer>)?
       file-size: initial-file-size = #f, // :: false-or(<integer>)?
     #all-keys) => ()
  accessor.file-descriptor := initial-file-descriptor;
  let (preferred-size, positionable?) = unix-fd-info(initial-file-descriptor);
  accessor.accessor-preferred-buffer-size := preferred-size;
  accessor.accessor-positionable? := positionable?;
  if (positionable? & ~initial-file-position)
    accessor.file-position := unix-lseek(initial-file-descriptor, 0, $seek_cur);
  end if;
  *open-accessors*[accessor] := #t;
end method accessor-open;

define method accessor-close
    (accessor :: <native-file-accessor>,
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
    (accessor :: <native-file-accessor>) => (open? :: <boolean>)
  accessor.file-descriptor & #t
end method accessor-open?;

define method accessor-size
    (accessor :: <native-file-accessor>)
 => (size :: false-or(<integer>))
  accessor.accessor-positionable?
  & begin
      let fd = accessor.file-descriptor;
      let old-position = accessor.file-position;
      let new-position = unix-lseek(fd, 0, $seek_end);
      new-position >= 0
        & unix-lseek(fd, old-position, $seek_set) = old-position
        & new-position
    end
end method accessor-size;

define inline method accessor-position
    (accessor :: <native-file-accessor>)
 => (position :: <integer>)
  accessor.file-position
end method accessor-position;

define method accessor-position-setter
    (position :: <integer>, accessor :: <native-file-accessor>)
 => (position :: <integer>)
  let old-position = accessor.file-position;
  if (position ~= old-position)
    let new-position = 
      unix-lseek(accessor.file-descriptor, position, $seek_set);
    if (position ~= new-position)
      if (new-position < 0)
	unix-error("lseek");
      else
	error("lseek seeked to wrong postion")
      end;
    else
      accessor.accessor-at-end? := #f;
      accessor.file-position := new-position
    end
  else
    position
  end
end method accessor-position-setter;

define method accessor-read-into!
    (accessor :: <native-file-accessor>, stream :: <file-stream>,
     offset :: <integer>, count :: <integer>, #key buffer)
 => (nread :: <integer>)
  if (accessor.accessor-at-end?)
    0
  else
    let buffer :: <buffer> = buffer | stream-input-buffer(stream);
    let file-position-before-read = accessor.file-position;
    let nread :: <integer>
      = unix-read(accessor.file-descriptor, buffer, offset, count);
    if (nread < 0)
      unix-error("read");
    elseif (nread = 0)
      accessor.accessor-at-end? := #t;
    else
      accessor.file-position := file-position-before-read + nread;
    end;
    nread
  end;
end method accessor-read-into!;

define method accessor-write-from
    (accessor :: <native-file-accessor>, stream :: <file-stream>,
     offset :: <integer>, count :: <integer>, #key buffer,
     return-fresh-buffer? = #f)
 => (nwritten :: <integer>, new-buffer :: <buffer>)
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let file-position-before-write = accessor.file-position;
  let nwritten :: <integer>
    = unix-write(accessor.file-descriptor, buffer, offset, count);
  if (nwritten > 0)
    // NB Can loop until empty, too lazy at the moment
    accessor.file-position := file-position-before-write + nwritten;
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

define method accessor-force-output
    (accessor :: <native-file-accessor>,
     stream :: <file-stream>)
 => ()
  unix-fsync(accessor.file-descriptor);
end method accessor-force-output;

define method accessor-newline-sequence
    (accessor :: <native-file-accessor>)
 => (string :: <string>)
  "\n"
end method accessor-newline-sequence;
