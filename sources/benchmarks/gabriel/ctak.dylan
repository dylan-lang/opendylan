Module: gabriel-benchmarks
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function ctak (x, y, z)
  block (return)
    ctak-aux(x, y, z, return)
  end
end function ctak;

define function ctak-aux
    (x :: <integer>, y :: <integer>, z :: <integer>, r :: <function>)
  if (y >= x)
    r(z)
  else
    ctak-aux(block (return)
               ctak-aux(x - 1, y, z, return)
             end,
             block (return)
               ctak-aux(y - 1, z, x, return)
             end,
             block (return)
               ctak-aux(z - 1, x, y, return)
             end,
             r)
  end if
end function ctak-aux;

define function testctak ()
  ctak(18, 12, 6);
end function testctak;

define benchmark ctak = testctak;

