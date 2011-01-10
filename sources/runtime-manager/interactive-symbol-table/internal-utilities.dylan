module:        ist-implementation
synopsis:      Various internal functions to help with the implementation.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
///// ADDRESS-IN-RANGE-OF-TABLE? (Internal - Deprecated).
//    Given an address, works out whether any definitions in the table
//    might correspond to it.

define method address-in-range-of-table?
    (st :: <remote-library-subtable>, 
     page :: <integer>, offset :: <integer>)
        => (answer :: <boolean>)
  if (st.subtable-empty?)
    #f
  else
    let first-page = 
      head(st.symbols-by-address[0]);
    let first-range =
      tail(st.symbols-by-address[0]);
    let last-page = 
      head(st.symbols-by-address[size(st.symbols-by-address) - 1]);
    let last-range =
      tail(st.symbols-by-address[size(st.symbols-by-address) - 1]);
    let first-offset =
      head(first-range[0]);
    let last-offset =
      head(last-range[size(last-range) - 1]);
    if (page < first-page)
      #f
    elseif (page > (last-page + 1))
      #f
    elseif (page == first-page)
      offset >= first-offset
    else
      #t
    end if
  end if
end method;
*/

///// SORT-SYMBOL-TABLE-BY-ADDRESS (Internal)
//    Called after one or more new definitions have been written to the
//    table. This might look small, but it's potentially very expensive.

define method sort-symbol-table-by-address
    (st :: <interactive-symbol-table>) => ()

  local method compare-heads (x :: <pair>, y :: <pair>) => (z :: <boolean>)
    head(x) < head(y)
  end method;

  for (lib in st.known-symbol-libraries)
    let subtable = st.symbols-by-library[lib];
    for (page-list-pair in subtable.symbols-by-address)
      tail(page-list-pair) := sort!(tail(page-list-pair), test: compare-heads)
    end for;
    subtable.symbols-by-address := 
      sort!(subtable.symbols-by-address, test: compare-heads);
  end for;

end method;


///// FIND-SUBTABLE-FROM-ADDRESS (Internal)
//    Given an address, find the (only) <remote-library-subtable> that
//    could possibly have a definition at that address.

define method find-subtable-from-address
    (st :: <interactive-symbol-table>, address :: <remote-value>)
  => (subtable :: false-or(<remote-library-subtable>))
  let result = #f;
  block (exit)
    for (library in st.known-symbol-libraries)
      let subtable = st.symbols-by-library[library];
      for (region in subtable.subtable-address-boundaries)
        if (remote-value-<=(region.boundary-lower-address, address) &
            remote-value-<=(address, region.boundary-upper-address))
          result := subtable;
          exit();
        end if;
      end for;
    end for;
  end block;
  result;
end method;

 
///// SEARCH-IN-ORDERED-PAIR-SEQUENCE (Internal)
//    Given a sequence of pairs of the form (x :: <integer>, y :: <object>),
//    and a target value T, attempt to find a member M of the sequence
//    such that head(M) == T, assuming that at most one match is possible,
//    and that the elements E[i] of the sequence are in ascending order
//    according to head(E[i]).
//    If M is found, returns the position in the sequence of M, and
//    also the value of tail(M) - the latter purely for convenience, since
//    it can be calculated from the former.
//    Returns (#f, #f) if the search fails.
//    Employs a basic binary search.

define method search-in-ordered-pair-sequence
    (seq :: <sequence>, t :: <integer>)
       => (pos :: false-or(<integer>), tl :: <object>)
  let lo = 0;
  let hi = size(seq);
  let pos = #f;
  let tl = #f;
  block(exit)
    let mid-old = #f;
    while (hi > lo)
      let mid = truncate/(hi + lo, 2);
      if (mid-old & (mid-old == mid))
        exit()
      end if;
      let entry = seq[mid];
      let x = head(entry);
      if (x == t)
        pos := mid;
        tl := tail(entry);
        exit();
      elseif (x < t)
        mid-old := mid;
        lo := mid;
      else
        mid-old := mid;
        hi := mid;
      end if;
    end while;
  end block;
  values(pos, tl);
end method;
