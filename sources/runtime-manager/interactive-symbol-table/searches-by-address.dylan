module:        ist-implementation
synopsis:      Searching the interactive symbol table by address.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// SYMBOL-TABLE-REGISTER-REGION-FOR-LIBRARY (Exported)
//    This is used to inform the interactive symbol table that a particular
//    region of memory has been allocated to house symbolic definitions
//    associated with a specific <remote-library>.

define method symbol-table-register-region-for-library
   (st :: <interactive-symbol-table>, library :: <remote-library>,
    lower-bound :: <remote-value>, upper-bound :: <remote-value>,
    classification :: <symbol>)
  => ()

  // If no symbols (or address regions) have been entered into this
  // table for the given <remote-library>, then initialize the entry
  // first.

  unless (member?(library, st.known-symbol-libraries))
    add!(st.known-symbol-libraries, library);
    st.symbols-by-library[library] := make(<remote-library-subtable>);
  end unless;

  // Add these delimiting addresses to the known boundaries.

  let subtable = st.symbols-by-library[library];
  add!(subtable.subtable-address-boundaries,
       make(<subtable-address-boundary>, 
            from: lower-bound, to: upper-bound, 
            classification: classification));

end method;

/*
///// SYMBOL-TABLE-CLASSIFY-ADDRESS (Not currently used, and also unfinished!)
//    Determines whether an address lies within the any of the memory
//    regions that have been registered with the symbol table. If it
//    has, then this function returns the <remote-library> that is
//    associated with the memory region, and also the classification of
//    that region. Otherwise, this function returns #f in the first
//    position and #"heap" in the second position.
//    Defers to access-path information if necessary.

define method symbol-table-classify-address
    (table :: <interactive-symbol-table>, address :: <remote-value>)
 => (lib :: false-or(<remote-library>), classification :: <symbol>)
  let lib = #f;
  let classification = #"heap";
  block (exit)
    
  end block;
  values(lib, classification)
end method;
*/

///// SYMBOL-TABLE-SYMBOL-RELATIVE-ADDRESS (Exported)
//    Given an address, and a symbol table, attempts to find the closest
//    symbol whose definition is at or before that address. Returns the
//    symbol, and the offset from the definition address to the supplied
//    address.
//    Analogous to 'symbol-relative-address' as defined by the access-path
//    library.
//    The symbol table must be ordered, hence this function must not be
//    called within the DEFINING-SYMBOLS macro.

define method symbol-table-symbol-relative-address
    (st :: <interactive-symbol-table>, addr :: <remote-value>,
     #key library = #f)
       => (sym :: false-or(<remote-symbol>),
           offset :: <integer>)

  if (st.performing-multiple-definitions?)
    error("Attempted to search for a symbol in a potentially unordered table")
  end if;

  let path = st.symbol-table-access-path;
  let (page, page-offset) =  page-relative-address(path, addr);
  let subtable =
     if (library)
       element(st.symbols-by-library, library, default: #f)
     else
       find-subtable-from-address(st, addr)
     end if;

  // Here, things get pretty unpleasant...

  if (subtable)
    let sym = #f;
    let offset = 0;
    let actual-page = 0;
    let actual-page-offset = 0;
    block (return)
      let target-sublist = #f;
      let last-page-index = size(subtable.symbols-by-address) - 1;
      block(found-sublist)
        for (sublist-index from 0 to last-page-index)
          if (head(subtable.symbols-by-address[sublist-index]) > page)
            if (sublist-index > 0)
              target-sublist := 
                tail(subtable.symbols-by-address[sublist-index - 1]);
              actual-page := 
                head(subtable.symbols-by-address[sublist-index - 1]);
              found-sublist();
            end if
          elseif (head(subtable.symbols-by-address[sublist-index]) == page)
            let subl = tail(subtable.symbols-by-address[sublist-index]);
            let first-pair = subl[0];
            let first-offset = head(first-pair);
            if (first-offset <= page-offset)
              target-sublist := subl;
              actual-page := page;
              found-sublist();
            elseif (sublist-index > 0)
              target-sublist := 
                tail(subtable.symbols-by-address[sublist-index - 1]);
              actual-page := 
                head(subtable.symbols-by-address[sublist-index - 1]);
              found-sublist();
            end if
          elseif (sublist-index == last-page-index)
            target-sublist := 
              tail(subtable.symbols-by-address[last-page-index]);
            actual-page := 
              head(subtable.symbols-by-address[last-page-index]);
            found-sublist();
          end if;
        end for;
      end block;
      if (target-sublist)
        if (actual-page == page)
          let last-offset-index = size(target-sublist) - 1;
          for (offset-index from 0 to last-offset-index)
            if (head(target-sublist[offset-index]) > page-offset)
              if (offset-index > 0)
                sym := tail(target-sublist[offset-index - 1]);
                actual-page-offset := head(target-sublist[offset-index - 1]);
                return();
              end if
            elseif (offset-index == last-offset-index)
              sym := tail(target-sublist[last-offset-index]);
              actual-page-offset := head(target-sublist[last-offset-index]);
              return();
           end if
          end for;
        else
          // Invariant: actual-page < page.
          // We need the very last symbol in this sublist.
          let last-offset-index = size(target-sublist) - 1;
          sym := tail(target-sublist[last-offset-index]);
          actual-page-offset := head(target-sublist[last-offset-index]);
        end if;
      end if;
    end block;

    // If we found the symbol, we also know the page-relative-address of
    // the symbol's actual definition. This is enough for us to calculate
    // the offset from this to the address that was actually supplied to
    // us.
    // Invariant: actual-page <= page
    // Invariant: (actual-page = page) --> (actual-page-offset <= page-offset)

    if (sym)
      if (actual-page == page)
        offset := page-offset - actual-page-offset
      else
        let page-difference = page - actual-page;
        let page-byte-size = 
          remote-value-byte-size(path) * remote-virtual-page-size(path);
        if (actual-page-offset = page-offset)
          offset := page-difference * page-byte-size
        elseif (actual-page-offset < page-offset)
          offset := 
            (page-difference * page-byte-size) + 
                (page-offset - actual-page-offset);
        else
          offset :=
            (page-difference * page-byte-size) - 
                 (actual-page-offset - page-offset);
        end if
      end if
    end if;

    // Return the result of that lot.
    values(sym, offset);
  else
    symbol-relative-address(path, addr)
  end if;
end method;


///// SYMBOL-TABLE-NEAREST-SYMBOLS (Exported)
//    Given an address, and a symbol table, attempts to find the three
//    closest symbols to that address.
//    The symbol table must be ordered, hence this function must not be
//    called within the defining-symbols macro.
//    Note: this is not implemented properly yet. The debugger manager
//          doesn't use it, although the console debugger does.

define method symbol-table-nearest-symbols
    (st :: <interactive-symbol-table>, addr :: <remote-value>)
      => (closest :: false-or(<remote-symbol>),
          preceeding :: false-or(<remote-symbol>),
          following :: false-or(<remote-symbol>))

  if (st.performing-multiple-definitions?)
    error("Attempted to search for a symbol in a potentially unordered table")
  end if;
  nearest-symbols(st.symbol-table-access-path, addr);
end method;
