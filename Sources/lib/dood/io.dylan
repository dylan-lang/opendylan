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

define inline function encode-word-bytes
    (b1 :: <dood-word>, b2 :: <dood-word>, 
     b3 :: <dood-word>, b4 :: <dood-word>)
 => (res :: <dood-word>)
 dood-word-logior
    (dood-word-logior
       (dood-word-shift-left(b1, 24), dood-word-shift-left(b2, 16)),
     dood-word-logior
       (dood-word-shift-left(b3, 8),  b4))
end function;

define inline function dood-read-word
    (dood :: <dood>) => (res :: <dood-word>)
  dood-format("READING @ %d", dood-position(dood));
  let (b1, b2, b3, b4) = read-4-aligned-bytes(dood-stream(dood));
  // let b1 :: <integer> = dood-read-element(dood);
  // let b2 :: <integer> = dood-read-element(dood);
  // let b3 :: <integer> = dood-read-element(dood);
  // let b4 :: <integer> = dood-read-element(dood);
  audit(dood, "%dW\n");
  let value :: <dood-word> 
    = encode-word-bytes
        (as(<dood-word>, b1), as(<dood-word>, b2), 
	 as(<dood-word>, b3), as(<dood-word>, b4));
  dood-format(" %d\n", untag(as(<address>, value)));
  value 

  /*
  let value = read-4-aligned-bytes-as-word(dood-stream(dood));
  dood-format("READING %= @ %d\n", value, dood-position(dood));
  value
  */
end function; 

define inline function dood-read-word-at 
    (dood :: <dood>, address :: <address>) => (res :: <dood-word>)
  dood-position(dood) := address;
  dood-read-word(dood);
end function;

define function dood-read (dood :: <dood>) => (res :: <address>)
  as(<address>, dood-read-word(dood))
end function;

define inline function dood-read-at 
    (dood :: <dood>, address :: <address>) => (res :: <address>)
  as(<address>, dood-read-word-at(dood, address))
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

define inline function decode-word-bytes (value :: <dood-word>) 
 => (b1 :: <dood-word>, b2 :: <dood-word>, 
     b3 :: <dood-word>, b4 :: <dood-word>)
  let b1 = dood-word-logand
             (dood-word-shift-right(value, 24), as(<dood-word>, 255));
  let b2 = dood-word-logand
	     (dood-word-shift-right(value, 16), as(<dood-word>, 255));
  let b3 = dood-word-logand
	     (dood-word-shift-right(value,  8), as(<dood-word>, 255));
  let b4 = dood-word-logand(value, as(<dood-word>, 255));
  values(b1, b2, b3, b4); 
end function;

define inline function dood-write-word
    (dood :: <dood>, value :: <dood-word>)
  let (b1, b2, b3, b4) = decode-word-bytes(value);
  dood-format("WRITING %d @ %d [%d, %d, %d, %d]\n", 
              as(<integer>, value), dood-position(dood), 
	      as(<integer>, b1), as(<integer>, b2), 
	      as(<integer>, b3), as(<integer>, b4));
  write-4-aligned-bytes
    (dood-stream(dood), 
     as(<integer>, b1), as(<integer>, b2), 
     as(<integer>, b3), as(<integer>, b4));
  // dood-write-element(dood, as(<integer>, b1));
  // dood-write-element(dood, as(<integer>, b2));
  // dood-write-element(dood, as(<integer>, b3));
  // dood-write-element(dood, as(<integer>, b4));

  /*
  dood-format("WRITING %d @ %d\n", value, dood-position(dood));
  write-4-aligned-bytes-from-word
    (dood-stream(dood), value)
  */
end function;

define inline function dood-write-word-at 
    (dood :: <dood>, value :: <dood-word>, address :: <address>)
  dood-position(dood) := address;
  dood-write-word(dood, value);
end function;

define function dood-write (dood :: <dood>, value :: <integer>)
  dood-write-word(dood, as(<dood-word>, value))
end function;

define inline function dood-write-at 
    (dood :: <dood>, value :: <integer>, address :: <address>)
  dood-write-word-at(dood, as(<dood-word>, value), address)
end function;


// eof
