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

define benchmark tak-benchmark ()
  let result
    = benchmark-repeat (iterations: 500)
        tak(18, 12, 6)
      end;
  assert-equal(7, result);
end benchmark;

define method trtak (x :: <integer>, y :: <integer>, z :: <integer>)
  local method go-tak ()
          if (y >= x)
            z
          else
            let a = tak(x - 1, y, z);
            let b = tak(y - 1, z, x);
            z := tak(z - 1, x, y);
            x := a;
            y := b;
            go-tak();
          end if;
        end method go-tak;
  go-tak()
end method trtak;

define benchmark trtak-benchmark ()
  let result
    = benchmark-repeat (iterations: 500)
        trtak(18, 12, 6)
      end;
  assert-equal(7, result);
end benchmark;
