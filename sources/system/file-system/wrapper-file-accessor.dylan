Module:       system-internals
Synopsis:     Wrapper stream accessors
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <wrapper-file-accessor> (<external-file-accessor>)
  slot wrapper-file-accessor-stream, 
    init-keyword: stream:;
end class;

//---*** andrewa: don't like this sideways method...

define sideways method platform-accessor-class
    (type == #"file", locator :: <stream>)
 => (class :: singleton(<wrapper-file-accessor>))
  <wrapper-file-accessor>
end method platform-accessor-class;

define method accessor-open
    (accessor :: <wrapper-file-accessor>, 
     #key locator, direction, if-exists, if-does-not-exist,
     #all-keys)
 => ()
  wrapper-file-accessor-stream(accessor) := locator;
end method;

define method accessor-close
    (x :: <wrapper-file-accessor>, #rest keys, #key abort?, wait?)
 => (closed? :: <boolean>)
  let accessor-stream = wrapper-file-accessor-stream(x);
  apply(close, accessor-stream, keys)
end method;

define method accessor-open?
    (x :: <wrapper-file-accessor>)
 => (open? :: <boolean>)
  let accessor-stream = wrapper-file-accessor-stream(x);
  stream-open?(accessor-stream);
end method;

define method accessor-preferred-buffer-size
    (x :: <wrapper-file-accessor>)
 => (preferred-buffer-size :: <integer>)
  1024
end method;

define method accessor-read-into!
    (x :: <wrapper-file-accessor>, stream :: <external-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let accessor-stream = wrapper-file-accessor-stream(x);
  read-into!(accessor-stream, count, buffer, start: offset, on-end-of-stream: #f);
end method;

define method accessor-write-from
    (x :: <wrapper-file-accessor>, stream :: <external-stream>,
     offset :: <buffer-index>, count :: <buffer-index>, 
     #key buffer, return-fresh-buffer?)
 => (nwritten :: <integer>, new-buffer :: <buffer>)
  let buffer :: <buffer> = buffer | stream-output-buffer(stream);
  let accessor-stream = wrapper-file-accessor-stream(x);
  write(accessor-stream, buffer, start: offset, end: offset + count);
  force-output(accessor-stream);
  values(count, buffer);
end method;

define method accessor-force-output
    (x :: <wrapper-file-accessor>, stream :: <external-stream>)
 => ()
  let accessor-stream = wrapper-file-accessor-stream(x);
  force-output(accessor-stream);
end method;

define method accessor-wait-for-completion
    (x :: <wrapper-file-accessor>)
 => ()
  let accessor-stream = wrapper-file-accessor-stream(x);
  wait-for-io-completion(accessor-stream)
end method;

define method accessor-synchronize
    (x :: <wrapper-file-accessor>, stream :: <external-stream>)
 => ()
  let accessor-stream = wrapper-file-accessor-stream(x);
  synchronize-output(accessor-stream)
end method;

define method accessor-newline-sequence
    (x :: <wrapper-file-accessor>)
 => (newline-sequence :: <sequence>)
  let accessor-stream = wrapper-file-accessor-stream(x);
  newline-sequence(accessor-stream)
end method;

define method accessor-file-position
    (x :: <wrapper-file-accessor>)
 => (position :: <integer>)
  let accessor-stream = wrapper-file-accessor-stream(x);
  stream-position(accessor-stream)
end;

define method file-position
    (x :: <wrapper-file-accessor>)
 => (position :: <integer>)
  accessor-file-position(x)
end;

define method accessor-set-file-position
    (x :: <wrapper-file-accessor>, requested-position :: <integer>)
 => (ok? :: <boolean>)
  let accessor-stream = wrapper-file-accessor-stream(x);
  adjust-stream-position(accessor-stream, requested-position, from: #"start");
  #t
end method accessor-set-file-position;

define method file-size
    (x :: <wrapper-file-accessor>)
 => (size :: <integer>)
  let accessor-stream = wrapper-file-accessor-stream(x);
  stream-size(accessor-stream)
end method file-size;

