module:        ist-implementation
synopsis:      Spewing out the contents of an interactive symbol table.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DEBUG-DISPLAY-SYMBOL-TABLE
//    For debugging purposes, generates output for the entire contents of
//    the symbol table.

define method debug-display-symbol-table (st :: <interactive-symbol-table>)
  => ()
  format-out("Interactive Symbols Defined Over %s\n\n",
             st.symbol-table-access-path.access-path-application);
  if (st.symbol-table-empty?)
    format-out("(None)\n\n");
  else
    for (lib in st.known-symbol-libraries)
      let subtable = st.symbols-by-library[lib];
      format-out("Extending the DLL %s:\n",
                 lib.library-core-name);
      if (subtable.subtable-empty?)
        format-out("  (None)\n");
      else
        for (page-pair in subtable.symbols-by-address)
          let page = head(page-pair);
          let sublist = tail(page-pair);
          format-out
            ("  In Virtual Page: %d, %d symbols:\n", page, size(sublist));
          for (sym-pair in sublist)
            let offset = head(sym-pair);
            let sym = tail(sym-pair);
            format-out("    Symbol %s At Offset %d (Address = 0x%s)\n",
                       sym.remote-symbol-name, offset, 
                       remote-value-as-string
                          (st.symbol-table-access-path,
                           sym.remote-symbol-address,
                           16));
          end for;
          format-out("\n");
        end for;
        format-out("\n");
      end if
    end for
  end if
end method;
