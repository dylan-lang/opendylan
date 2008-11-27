module: dood
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline method dood-compute-instance-size
    (dood :: <dood>, object == <machine-word>) => (address :: <address>)
  1 + 1 + 1 // 64 bits
end method;

define constant $word-size      = 64;
define constant $bytes-per-word = 8;

define inline function encode-machine-word-bytes
    (b1 :: <machine-word>, b2 :: <machine-word>, 
     b3 :: <machine-word>, b4 :: <machine-word>,
     b5 :: <machine-word>, b6 :: <machine-word>, 
     b7 :: <machine-word>, b8 :: <machine-word>)
 => (res :: <machine-word>)
 machine-word-logior
  (machine-word-logior
     (machine-word-logior
	(machine-word-shift-left-with-overflow(b1, 56), 
	 machine-word-shift-left-with-overflow(b2, 48)),
      machine-word-logior
	(machine-word-shift-left-with-overflow(b3, 40), 
	 machine-word-shift-left-with-overflow(b4, 32))),
   machine-word-logior
     (machine-word-logior
	(machine-word-shift-left-with-overflow(b5, 24), 
	 machine-word-shift-left-with-overflow(b6, 16)),
      machine-word-logior
	(machine-word-shift-left-with-overflow(b7, 8),  b8)))
end function;

define inline function dood-read-machine-word
    (dood :: <dood>) => (res :: <machine-word>)
  dood-format("READING WORD @ %d", dood-position(dood));
  let (b1, b2, b3, b4, b5, b6, b7, b8) = read-8-aligned-bytes(dood-stream(dood));
  // let b1 :: <integer> = read-element(dood);
  // let b2 :: <integer> = read-element(dood);
  // let b3 :: <integer> = read-element(dood);
  // let b4 :: <integer> = read-element(dood);
  // let b5 :: <integer> = read-element(dood);
  // let b6 :: <integer> = read-element(dood);
  // let b7 :: <integer> = read-element(dood);
  // let b8 :: <integer> = read-element(dood);
  let value :: <machine-word> 
    = encode-machine-word-bytes
        (as(<machine-word>, b1), as(<machine-word>, b2), 
	 as(<machine-word>, b3), as(<machine-word>, b4),
	 as(<machine-word>, b5), as(<machine-word>, b6), 
	 as(<machine-word>, b7), as(<machine-word>, b8));
  value 
end function; 

define inline function decode-machine-word-bytes (value :: <machine-word>) 
 => (b1 :: <machine-word>, b2 :: <machine-word>, 
     b3 :: <machine-word>, b4 :: <machine-word>,
     b5 :: <machine-word>, b6 :: <machine-word>, 
     b7 :: <machine-word>, b8 :: <machine-word>)
  let mask = as(<machine-word>, 255);
  let b1 = machine-word-logand(machine-word-shift-right(value, 56), mask);
  let b2 = machine-word-logand(machine-word-shift-right(value, 48), mask);
  let b3 = machine-word-logand(machine-word-shift-right(value, 40), mask);
  let b4 = machine-word-logand(machine-word-shift-right(value, 32), mask);
  let b5 = machine-word-logand(machine-word-shift-right(value, 24), mask);
  let b6 = machine-word-logand(machine-word-shift-right(value, 16), mask);
  let b7 = machine-word-logand(machine-word-shift-right(value,  8), mask);
  let b8 = machine-word-logand(value, mask);
  values(b1, b2, b3, b4, b5, b6, b7, b8); 
end function;

define inline function dood-write-machine-word
    (dood :: <dood>, value :: <machine-word>)
  let (b1, b2, b3, b4, b5, b6, b7, b8) = decode-machine-word-bytes(value);
  dood-format("WRITING %d @ %d [%d, %d, %d, %d, %d %d %d %d]\n", 
              value, dood-position(dood), b1, b2, b3, b4, b5, b6, b7, b8);
  write-8-aligned-bytes
    (dood-stream(dood), 
     as(<integer>, b1), as(<integer>, b2), as(<integer>, b3), as(<integer>, b4),
     as(<integer>, b5), as(<integer>, b6), as(<integer>, b7), as(<integer>, b8));
  // write-element(dood, as(<integer>, b1));
  // write-element(dood, as(<integer>, b2));
  // write-element(dood, as(<integer>, b3));
  // write-element(dood, as(<integer>, b4));
  // write-element(dood, as(<integer>, b5));
  // write-element(dood, as(<integer>, b6));
  // write-element(dood, as(<integer>, b7));
  // write-element(dood, as(<integer>, b8));
end function;

// eof
