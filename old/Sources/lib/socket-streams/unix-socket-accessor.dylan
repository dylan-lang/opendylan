Module:     socket-streams-internals
Author:     James Casey
Synopsis:   An object-oriented streams library for berkeley sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <unix-socket-accessor> (<socket-accessor>)
end class <unix-socket-accessor>;

define method platform-accessor-class
    (type == #"socket", locator)
 => (class :: singleton(<unix-socket-accessor>))
  <unix-socket-accessor>
end method platform-accessor-class;

define method accessor-open
    (accessor :: <unix-socket-accessor>,
     #key direction, if-exists, if-does-not-exist,
     #all-keys) => ()
  ignore(if-exists, if-does-not-exist);

  if (accessor.socket-descriptor)
    *open-accessors*[accessor] := #t;
    accessor.socket-connection-eof? := #f;
  end;
end method accessor-open;

define method accessor-close
    (accessor :: <unix-socket-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  let fd = accessor.socket-descriptor;
  if (fd)
    if ((unix-close(fd) < 0) & ~ abort?)
      unix-error("close");
    else
      accessor.socket-descriptor := #f;
      remove-key!(*open-accessors*, accessor);
    end
  end if;
  #t
end method accessor-close;

define method accessor-open?
    (accessor :: <unix-socket-accessor>) => (open? :: <boolean>)
  ~ accessor.socket-descriptor
end method accessor-open?;

define constant $preferred-buffer-size = 1024;

define method accessor-preferred-buffer-size
    (accessor :: <unix-socket-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

define method accessor-read-into!
    (accessor :: <unix-socket-accessor>, stream :: <socket-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>, end-of-file? :: <boolean>)
  let buffer :: <buffer> = buffer | stream-input-buffer(stream);
  if (accessor.socket-connection-eof?)
    error("Stream closed")
  else
    let nread = unix-read(accessor.socket-descriptor,
			  as(<vector>, buffer), offset, count);
    if (nread < 0)
      accessor.socket-connection-eof? := #t;
      unix-error("read")
    else
      //---*** Is this EOF condition correct?  I bet not
      accessor.socket-connection-eof? := (nread <= 0);
      values(nread, accessor.socket-connection-eof?)
    end
  end
end method read-into!;

define method accessor-write-from
    (accessor :: <unix-socket-accessor>, stream :: <socket-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nwritten :: <integer>, end-of-file? :: <boolean>)
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let remaining = count;
  while (remaining > 0)
    let nwritten = unix-write (accessor.socket-descriptor,
			       as(<vector>, buffer), count - remaining, remaining);
    if (nwritten < 0)
      unix-error("write")
    end;
    remaining := remaining - nwritten
  end;
  values(count, #f)
end method accessor-write-from;

define method accessor-newline-sequence
    (accessor :: <unix-socket-accessor>)
 => (string :: <string>)
  "\n"
end method accessor-newline-sequence;