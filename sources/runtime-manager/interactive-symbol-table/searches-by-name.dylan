module:        ist-implementation
synopsis:      Searching the interactive symbol table by name.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// SYMBOL-TABLE-FIND-SYMBOL (Exported)
//    This is the only provision for by-name lookup. It is totally analogous
//    to the access-path's "find-symbol" API.

define method symbol-table-find-symbol
   (st :: <interactive-symbol-table>, name :: <string>,
    #key library = #f, local-lookup-only? = #f,
         file = #f)
      => (maybe-sym :: false-or(<remote-symbol>))

  // A quick test. If the symbol table is empty, don't even bother with
  // the most trivial checking of it. Just defer to access path.

  // Whenever we defer to access path, we need to take slightly different
  // action depending on whether 'library' was supplied to this call.
  // Since, if we pass 'library' on, it _must_ be a <remote-library> (and
  // not #f).

  if (st.symbol-table-empty?)
    if (local-lookup-only?)
      #f
    elseif (library)
      find-symbol(st.symbol-table-access-path, name, library: library)
    else
      find-symbol(st.symbol-table-access-path, name)
    end if
  else

    let local-sym =
      if (library)
        st-find-symbol-in-known-library(st, name, library, file: file)
      else
        let ls = #f;
        block (exit)
          for (lib in st.known-symbol-libraries)
            ls := st-find-symbol-in-known-library(st, name, lib, file: file);
            if (ls)
              exit()
            end if
          end for;
        end block;
        ls
      end if;

    // If that lookup succeeded, then that's a hit on the local symbol
    // table. If the lookup failed, defer to the access-path once again.

    if (local-sym)
      local-sym
    elseif (local-lookup-only?)
      #f
    elseif (library)
      find-symbol(st.symbol-table-access-path, name, library: library)
    else
      find-symbol(st.symbol-table-access-path, name)
    end if
  end if
end method;


///// ST-FIND-SYMBOL-IN-KNOWN-LIBRARY (Internal)
//    Deferred to by symbol-table-find-symbol

define method st-find-symbol-in-known-library
    (st :: <interactive-symbol-table>, name :: <string>,
     library :: <remote-library>,
     #key file = #f)
  => (sym :: false-or(<remote-symbol>))
  if (member?(library, st.known-symbol-libraries))
    if (file)
      if (member?(file, library.library-object-files))
        let subtable = st.symbols-by-library[library];
        if (subtable.subtable-empty?)
          #f
        else
          let statics = subtable.statics-by-object-file[file];
          element(statics, name, default: #f);
        end if
      else
        #f
      end if
    else
      let subtable = st.symbols-by-library[library];
      if (subtable.subtable-empty?)
        #f
      else
        element(subtable.symbols-by-name, name, default: #f);
      end if
    end if
  else
    #f
  end if;
end method;

