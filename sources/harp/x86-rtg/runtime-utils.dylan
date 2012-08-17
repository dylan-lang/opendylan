module:    harp-x86-rtg
Synopsis:  Utilities for the Dylan x86 runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method make-temp-register
    (be :: <x86-back-end>, n :: <integer>)
  select (n)
    1 => reg--tmp1;
    2 => reg--tmp2;
    3 => reg--tmp3;
  end;
end method;


define sideways method op--load-index
    (back-end :: <x86-back-end>, result, base, index, offset :: <integer>)
 => ()
  ins--ld-index(back-end, result, base, index, offset);
end method;


define sideways method op--store-index
    (back-end :: <x86-back-end>, value, base, index, offset :: <integer>)
 => ()
  ins--st-index(back-end, value, base, index, offset);
end method;


define sideways method op--load-index-scaled
    (back-end :: <x86-back-end>, result, base, scaled-index, offset :: <integer>)
 => ()
  ins--ld-index-scaled(back-end, result, base, scaled-index, offset);
end method;


define sideways method op--store-index-scaled
    (back-end :: <x86-back-end>, value, base, scaled-index, offset :: <integer>)
 => ()
  ins--st-index-scaled(back-end, value, base, scaled-index, offset);
end method;



define sideways method op--load-byte-index
    (back-end :: <x86-back-end>, result, base, index, offset :: <integer>)
 => ()
  ins--ldb-index(back-end, result, base, index, offset);
end method;


define sideways method op--store-byte-index
    (back-end :: <x86-back-end>, value, base, index, offset :: <integer>)
 => ()
  ins--stb-index(back-end, value, base, index, offset);
end method;
