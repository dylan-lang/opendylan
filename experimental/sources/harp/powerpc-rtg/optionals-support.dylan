module:    powerpc-rtg
Synopsis:  Support for entry points with optionals 
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// OP--COPY-WORDS-WITH-UPDATE

define sideways method op--copy-words-with-update
    (be :: <powerpc-back-end>, dest, to, from, copy-count)
  ins--copy-words-down-w(be, to, from, copy-count);
  if (dest) ins--move(be, dest, reg--tmp3) end;
end method;
