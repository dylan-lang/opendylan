Module:     tcp-streams
Author:     Nosa Omo
Synopsis:   An object-oriented streams library for tcp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sealed class <unix-tcp-accessor> (<external-stream-accessor>)
  slot tcp-connection = -1;
  slot tcp-connection-eof? = #f;
  slot port, required-init-keyword: port:;
  slot host = #f, init-keyword: host:;
end class <unix-tcp-accessor>;

define method platform-accessor-class
    (type == #"tcp", locator)
 => (class :: singleton(<unix-tcp-accessor>))
  <unix-tcp-accessor>
end method platform-accessor-class;

define method accessor-open
    (accessor :: <unix-tcp-accessor>,
     #key direction, if-exists, if-does-not-exist,
     #all-keys) => ()
  ignore(if-exists, if-does-not-exist);
  accessor.tcp-connection := unix-open(accessor.host, accessor.port);
  if (accessor.tcp-connection < 0)
    unix-error("connect-to-named-server")
  else
    *open-accessors*[accessor] := #t
  end;
  accessor.tcp-connection-eof? := #f
end method accessor-open;

define method accessor-close
    (accessor :: <unix-tcp-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  let fd = accessor.tcp-connection;
  if (fd)
    if ((unix-close(fd) < 0) & ~ abort?)
      unix-error("close")
    else
      accessor.tcp-connection := -1;
      remove-key!(*open-accessors*, accessor)
    end
  end;
  #t
end method accessor-close;

define method accessor-open?
    (accessor :: <unix-tcp-accessor>) => (open? :: <boolean>)
  ~(accessor.tcp-connection < 0)
end method accessor-open?;

define constant $preferred-buffer-size = 1024;

define method accessor-preferred-buffer-size
    (accessor :: <unix-tcp-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

define method accessor-read-into!
    (accessor :: <unix-tcp-accessor>, stream :: <tcp-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>, end-of-file? :: <boolean>)
  let buffer :: <buffer> = buffer | stream-input-buffer(stream);
  if (accessor.tcp-connection-eof?)
    error("Stream closed")
  else
    let nread = unix-read(accessor.tcp-connection,
			  as(<vector>, buffer), offset, count);
    if (nread < 0)
      accessor.tcp-connection-eof? := #t;
      unix-error("read")
    else
      //---*** Is this EOF condition correct?  I bet not
      accessor.tcp-connection-eof? := (nread <= 0);
      values(nread, accessor.tcp-connection-eof?)
    end
  end
end method read-into!;

define method accessor-write-from
    (accessor :: <unix-tcp-accessor>, stream :: <tcp-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nwritten :: <integer>, end-of-file? :: <boolean>)
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let remaining = count;
  while (remaining > 0)
    let nwritten = unix-write (accessor.tcp-connection,
			       as(<vector>, buffer), count - remaining, remaining);
    if (nwritten < 0)
      unix-error("write")
    end;
    remaining := remaining - nwritten
  end;
  values(count, #f)
end method accessor-write-from;

define method accessor-newline-sequence
    (accessor :: <unix-tcp-accessor>)
 => (string :: <string>)
  "\n"
end method accessor-newline-sequence;
