Module:     tcp-streams
Author:     Nosa Omo
Synopsis:   An object-oriented streams library for tcp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <win32-tcp-accessor> (<external-stream-accessor>)
  slot tcp-connection-eof? = #f;
end class <win32-tcp-accessor>;

define method platform-accessor-class
    (type == #"tcp", locator)
 => (class :: singleton(<win32-tcp-accessor>))
  <win32-tcp-accessor>
end method platform-accessor-class;

define method accessor-open
    (accessor :: <win32-tcp-accessor>,
     #key direction, if-exists, if-does-not-exist,
     #all-keys) => ()
  ignore(if-exists, if-does-not-exist);
  //---*** What magic opens the TCP connection?
end method accessor-open;

define method accessor-close
    (accessor :: <win32-tcp-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  #t
end method accessor-close;

define method accessor-open?
    (accessor :: <win32-tcp-accessor>) => (open? :: <boolean>)
  #t
end method accessor-open?;

define constant $preferred-buffer-size = 1024;

define method accessor-preferred-buffer-size
    (accessor :: <win32-tcp-accessor>)
 => (preferred-buffer-size :: <integer>)
  $preferred-buffer-size
end method accessor-preferred-buffer-size;

define C-function read-command
  result    command :: <C-string>;
  c-name: "read_command";
end C-function read-command;

define C-function read-char
  result    code :: <C-character>;
  c-name: "read_char";
end C-function read-char;

define method accessor-read-into!
    (accessor :: <win32-tcp-accessor>, stream :: <tcp-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>, end-of-file? :: <boolean>)
  if (accessor.tcp-connection-eof?)
    error("Stream closed")
  end;
  let buffer :: <buffer> = buffer | stream-input-buffer(stream);
  local method as-integer (a-string :: <string>)
	  let result = 0;
	  let exp = 1;
	  for (char in reverse(a-string))
	    result := exp * (as(<integer>, char) - 48) + result;
	    exp := exp * 10;
	  end;
	  result
	end method as-integer;
  //---*** Is this EOF condition correct?  I bet not
  let buffer-size = as-integer(as(<byte-string>, read-command()));
  if (buffer-size = 0)
    accessor.tcp-connection-eof? := #t;
    values(0, #t)
  else
    //---*** This sure looks as slow as hell
    for (index from 0 below buffer-size)
      buffer[index + offset] := as(<byte>, read-char());
    end;
    values(buffer-size, #f)
  end
end method accessor-read-into!;

define method accessor-write-from
    (accessor :: <win32-tcp-accessor>, stream :: <tcp-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nwritten :: <integer>, end-of-file? :: <boolean>)
  //---*** Implement this
end method accessor-write-from;

define method accessor-newline-sequence
    (accessor :: <unix-tcp-accessor>)
 => (string :: <string>)
  //---*** Implement this
end method accessor-newline-sequence;
