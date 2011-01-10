Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// define open abstract class <explicit-key-collection> ... end;


/////////////////
// IMPLEMENTATION
/////////////////

//
// Specialized inherited generic methods
//

//
// KEY-SEQUENCE
// 

define method key-sequence (collection :: <explicit-key-collection>)
    => (keys :: <sequence>)
  for (keys = #() then pair(k, keys),
       c keyed-by k in collection)
  finally 
    reverse!(keys)
  end
end method key-sequence;


//
// MAXIMUM-SEQUENCE-KEY
//

define method maximum-sequence-key(collection :: <explicit-key-collection>) 
    => key :: <integer>;
  let max-key = -1;
  for (e keyed-by k in collection)
    if (instance?(k, <integer>)) max-key := max(k, max-key) end if;
  end for;
  max-key
end method maximum-sequence-key;
 
