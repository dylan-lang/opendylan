module:        tether-downloader-internals
synopsis:      Aligning the address pointers in static blocks.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// BLOCK-ALIGN-N
//    Aligns the block to the next n-byte boundary.

define method block-align-n
    (sb :: <static-block>, n :: <integer>) => ()
  let a = sb.static-block-current-address;
  let i = remote-value-low-order-bits(a, 20);
  let increment = 0;
  while (modulo(i, n) > 0)
    increment := increment + 1;
    i := i + 1;
  end while;
  sb.block-byte-cursor := sb.block-byte-cursor + increment;
  sb.static-block-remaining-size := sb.static-block-remaining-size - increment;
end method;


///// BLOCK-ALIGN-1

define method block-align-1 (sb :: <static-block>) => ()
  block-align-n(sb, 1)
end method;


///// BLOCK-ALIGN-2

define method block-align-2 (sb :: <static-block>) => ()
  block-align-n(sb, 2)
end method;


///// BLOCK-ALIGN-4

define method block-align-4 (sb :: <static-block>) => ()
  block-align-n(sb, 4)
end method;


///// BLOCK-ALIGN-8

define method block-align-8 (sb :: <static-block>) => ()
  block-align-n(sb, 8)
end method;


///// BLOCK-ALIGN-REMOTE-VALUE

define method block-align-remote-value (sb :: <static-block>) => ()
  block-align-n(sb, remote-value-byte-size(sb.block-access-path))
end method;

