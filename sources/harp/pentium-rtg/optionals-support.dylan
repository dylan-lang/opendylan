module:    pentium-rtg
Synopsis:  Support for entry points with optionals 
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// ENSURE-SAFE-KEY-SPACE
// A hack for the Pentium. Register pressure is so great
// that the Pentium must recalculate the size of the key space.

define sideways method ensure-safe-key-space 
    (be :: <pentium-back-end>, key-space :: <register>) 
    => (r :: <register>);
  let keys-size = make-n-register(be);
  op--keywords-size(be, keys-size);
  keys-size;
end method;


// OP--COPY-WORDS-WITH-UPDATE

define sideways method op--copy-words-with-update
    (be :: <pentium-back-end>, dest, to, from, copy-count)
  // Force the colourer to use EDI in the way we want
  let wanted-to = edi;
  let wanted-from = esi;
  ins--move(be, wanted-to, to);     // dummy instruction
  ins--move(be, wanted-from, from);
  ins--copy-words-down-w(be, wanted-to, wanted-from, copy-count);
  if (dest) ins--move(be, dest, wanted-from) end;   // ESI contains the right result
end method;
