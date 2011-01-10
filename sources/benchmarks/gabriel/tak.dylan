Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: Tak -- Converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Note on the Dylan version:
///   No changes.  Direct translation.
define function tak (x :: <integer>, y :: <integer>, z :: <integer>)
  if (y >= x)
    z
  else
    tak(tak(x - 1, y, z),
	tak(y - 1, z, x),
	tak(z - 1, x, y))
  end if
end function tak;

define function testtak ()
  begin
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
    tak(18, 12, 6);
  end
end function testtak;

define benchmark tak = testtak;
