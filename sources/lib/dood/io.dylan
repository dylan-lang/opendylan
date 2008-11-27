Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Define this so can play around with replacements to this file
define constant <dood-stream> = <byte-multi-buffered-stream>;

define inline function dood-position (dood :: <dood>) => (res :: <address>)
  let position :: <address> = stream-position(dood-stream(dood));
  truncate/(position, $bytes-per-word);
end function;

define inline function dood-position-setter 
    (value :: <address>, dood :: <dood>)
  audit(dood, "%dP%d\n", value);
  let new-position = value * $bytes-per-word;
  multi-buffered-stream-position(dood-stream(dood)) := new-position;
end function;

define inline function dood-force-output (dood :: <dood>)
  force-output(dood-stream(dood));
end function;

/// READING

/*
define inline function dood-byte-read (dood :: <dood>)
  read-element(dood-stream(dood));
end function;

define inline function dood-read-string
    (dood :: <dood>, n :: <integer>)
  read(dood-stream(dood), n)
end function;
*/

define inline function dood-read-string-into!
    (dood :: <dood>, n :: <integer>, object)
  audit(dood, "%dS%d\n", n);
  read-into!(dood-stream(dood), n, object)
end function;

define inline function dood-read-word-at 
    (dood :: <dood>, address :: <address>) => (res :: <dood-word>)
  dood-position(dood) := address;
  dood-read-machine-word(dood);
end function;

define function dood-read (dood :: <dood>) => (res :: <address>)
  as(<address>, dood-read-machine-word(dood))
end function;

define inline function dood-read-at 
    (dood :: <dood>, address :: <address>) => (res :: <address>)
  as(<address>, dood-read-word-at(dood, address))
end function;

define inline function dood-read-machine-word-at 
    (dood :: <dood>, address :: <address>) => (res :: <dood-word>)
  dood-position(dood) := address;
  dood-read-machine-word(dood);
end function;

/// WRITING

/*
define inline function dood-byte-write (dood :: <dood>, value :: <integer>)
  write-element(dood-stream(dood), value);
end function;
*/

define inline function dood-write-string (dood :: <dood>, value)
  write(dood-stream(dood), value)
end function;

/*
define inline function dood-write-string-at
    (dood :: <dood>, value, address :: <address>)
  dood-position(dood) := address;
  dood-format("WRITING %= @ %d\n", value, dood-position(dood));
  dood-write-string(dood, value)
end function;
*/

define inline function dood-write-word-at 
    (dood :: <dood>, value :: <dood-word>, address :: <address>)
  dood-position(dood) := address;
  dood-write-machine-word(dood, value);
end function;

define function dood-write (dood :: <dood>, value :: <integer>)
  dood-write-machine-word(dood, as(<dood-word>, value))
end function;

define inline function dood-write-at 
    (dood :: <dood>, value :: <integer>, address :: <address>)
  dood-write-word-at(dood, as(<dood-word>, value), address)
end function;

define inline function dood-write-machine-word-at 
    (dood :: <dood>, value :: <machine-word>, address :: <address>)
  dood-position(dood) := address;
  dood-write-machine-word(dood, value);
end function;

// eof
