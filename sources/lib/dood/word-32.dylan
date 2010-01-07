module: dood
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline method dood-compute-instance-size
    (dood :: <dood>, object == <machine-word>) => (address :: <address>)
  1 + 1 // 32 bits
end method;

define constant $word-size      = 32;
define constant $bytes-per-word = 4;

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

define inline function dood-write-machine-word
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

define inline function dood-read-machine-word
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
