module:        ist-implementation
synopsis:      Creating new symbols in the interactive symbol table.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DEFINING-SYMBOLS (Macro, Exported)
//    Executes a body of code, which is likely to contain several
//    symbol-table-define-symbol calls. After executing the body,
//    re-sorts all of the definitions so that further lookups will
//    work. If symbol definitions are made _outside_ of a call to this
//    macro, the table will be sorted after every individual
//    definition, which is likely to be expensive.

define macro defining-symbols
  {defining-symbols (?st:expression) 
     ?b:body 
   end}
   => 
     {begin 
        let tab = ?st;
        tab.performing-multiple-definitions? := #t;
        ?b;
        sort-symbol-table-by-address(tab);
        tab.performing-multiple-definitions? := #f;
      end}
end macro;


///// SYMBOL-TABLE-DEFINE-FILE
//    Defines a delimiting scope for static symbols.

define method symbol-table-define-file
    (st :: <interactive-symbol-table>, name :: <byte-string>,
     #key language = $symbol-language-C, library = #f,
          client-data = #f, source-extension = "c",
          object-extension = "obj", path = "")
  => (defined-file :: <remote-object-file>)

  let access-path = st.symbol-table-access-path;

  let newfile = make(<remote-object-file>,
                     name: name,
                     source-extension: source-extension,
                     object-extension: object-extension,
                     library: library,
                     language: language,
                     path: path,
                     client-data: client-data);

  // It is an error if no <remote-library> is supplied for this file to be
  // associated with.

  if (~library)
    error(make(<file-no-library-error>,
               table: st, name: name))
  end if;

  // If this <remote-library> is not yet known by the symbol table, create
  // an entry for it now.

  unless (member?(library, st.known-symbol-libraries))
    add!(st.known-symbol-libraries, library);
    st.symbols-by-library[library] := make(<remote-library-subtable>);
  end unless;

  // Extend the <remote-library> with this new object file.
  extend-remote-library(access-path, library, newfile);

  // And create a fresh <string-table> to hold static symbol definitions
  // that are made for this file.
  
  let subtable = st.symbols-by-library[library];
  subtable.statics-by-object-file[newfile] := make(<string-table>);

  // Return the newly-defined file
  newfile
end method;


///// SYMBOL-TABLE-DEFINE-SYMBOL
//    Defines a symbolic name, and pairs it with the given address. This
//    will perform redefinition implicitly, if the symbol table supports
//    it. Note that if there is already a symbol defined at the given
//    address, an error will be signalled. This is always an illegal
//    operation.

define method symbol-table-define-symbol
    (st :: <interactive-symbol-table>, 
     name :: <string>, 
     addr :: <remote-value>,
     #key language = $symbol-language-C, library = #f,
          file = #f, storage-status = #"public")
        => (defined-sym :: <remote-symbol>)

  let path = st.symbol-table-access-path;
  let (page, page-offset) = page-relative-address(path, addr);

  // Construct the <remote-symbol> itself.

  let newsym = make(<remote-symbol>,
                    name: name,
                    address: addr,
                    language: language,
                    library: library,
                    object-file: file,
                    storage-status: storage-status);

  // If this symbol table does not support redefinition of symbols, see
  // whether this call would indeed result in a redefinition. If so,
  // signal an error.

  if (~st.symbol-table-redefinition-allowed?)
    let existing-sym = find-symbol(path, name, library: library);
    if (existing-sym)
      error(make(<symbol-illegal-redefinition-by-name-error>,
                  table: st,
                  name: name,
                  symbol: existing-sym))
    end if;
  end if;

  // It is an error if no <remote-library> is supplied for the symbol.

  if (~library)
    error(make(<symbol-no-library-error>,
               table: st, name: name))
  end if;

  // It is an error if the symbol is declared to be static, but no
  // <remote-object-file> is given.

  if ((storage-status == #"static") & (~file))
    error(make(<symbol-static-no-file-error>,
               table: st, name: name))
  end if;

  // It is an error if the symbol is declared to be within a
  // <remote-object-file> that does not belong to the library.

  if (file)
    unless (member?(file, library.library-object-files))
      error(make(<symbol-attempted-definition-in-undefined-file-error>,
                 table: st, name: name, file: file, library: library))
    end unless
  end if;

  unless (member?(library, st.known-symbol-libraries))
    add!(st.known-symbol-libraries, library);
    st.symbols-by-library[library] := make(<remote-library-subtable>);
  end unless;

  let subtable = st.symbols-by-library[library];
  let index-into-pages = #f;
  let index-into-offsets = #f;

  // This block statement scans the existing state of the interactive
  // symbol table, and attempts to determine whether a symbol has already
  // been defined at this address. If it has, we will later signal an
  // error. If not, we will know whether or not there is already an entry
  // for symbols defined on the same virtual memory page.

  block(exit)
    for (i from 0 below size(subtable.symbols-by-address))
      if (head(subtable.symbols-by-address[i]) == page)
        index-into-pages := i;
        let list-i = tail(subtable.symbols-by-address[i]);
        for (j from 0 below size(list-i))
          if (head(list-i[j]) == page-offset)
            index-into-offsets := j;
            exit();
          end if
        end for;
        exit();
      end if
    end for;
  end block;

  if (index-into-offsets)
    // There is a definition which clashes with the one we are trying to
    // make.
/*
    let sublist = tail(subtable.symbols-by-address[index-into-pages]);
    let offset-symbol-pair = sublist[index-into-offsets];
    let offending-symbol = tail(offset-symbol-pair);
    signal(make(<symbol-illegal-redefinition-by-address-error>,
                table: st,
                name: name,
                symbol: offending-symbol));
*/
  elseif (index-into-pages)
    // There are already definitions whose addresses are on the same
    // virutal memory page as the one we are making. This is legal, but
    // results in different behaviour.
    let existing-entry = subtable.symbols-by-address[index-into-pages];
    add!(tail(existing-entry), pair(page-offset, newsym));
  else
    // New symbol, and a new memory page to add to the sequence.
    let new-entry = pair(page, make(<stretchy-vector>));
    add!(tail(new-entry), pair(page-offset, newsym));
    add!(subtable.symbols-by-address, new-entry);
  end if;

  // Also, add (or alter) the symbol's name->symbol mapping in the string
  // table.

  if (storage-status == #"static")
    let statics = subtable.statics-by-object-file[file];
    statics[name] := newsym;
  else
    subtable.symbols-by-name[name] := newsym;
  end if;

  // The symbols are now potentially out-of-order by address, since we added
  // our new definitions to the end of stretchy-vectors. However, if we
  // are currently making multiple definitions at once, it is not
  // desirable to perform a resort now.

  unless(st.performing-multiple-definitions?)
    sort-symbol-table-by-address(st)
  end unless;

  // The symbol table is no longer empty.
  if (st.symbol-table-empty?)
    st.symbol-table-empty? := #f
  end if;

  // And neither is the subtable.
  if (subtable.subtable-empty?)
    subtable.subtable-empty? := #f
  end if;

  // And return the <remote-symbol> itself.
  newsym;    
end method;


///// SYMBOL-TABLE-UNDEFINE-SYMBOL
//    Removes a symbolic definition from the interactive table. Note that
//    this can only work if the symbol actually _has_ a definition in
//    the table itself. If the definition exists only from access-path
//    information, it cannot be removed.
//    Status Note: This implementation is very suspect indeed due to the
//                 use of remove! on stretchy-vectors. We are assuming
//                 (perhaps wrongly), that the vectors are compacted
//                 automatically. If this does work, the symbol table
//                 should remain correctly ordered after the removal.

define method symbol-table-undefine-symbol
    (st :: <interactive-symbol-table>, name :: <string>,
     #key library = #f) => ()

  if (~library)
    error(make(<symbol-no-library-error>, table: st, name: name));
  end if;

  let subtable = st.symbols-by-library[library];
  let target-sym = element(subtable.symbols-by-name, name, default: #f);
  if (target-sym)
    let (page, page-offset) = 
      page-relative-address(st.symbol-table-access-path,
                            target-sym.remote-symbol-address);
    let (list-index, sublist) =
      search-in-ordered-pair-sequence(subtable.symbols-by-address, page);
    let (sublist-index, sym) =
      search-in-ordered-pair-sequence(sublist, page-offset);
    remove!(sublist, sublist[sublist-index]);
    if (size(sublist) == 0)
      remove!(subtable.symbols-by-address, 
              subtable.symbols-by-address[list-index]);
    end if;
    remove-key!(subtable.symbols-by-name, name);
  else
    error(make(<symbol-does-not-exist-error>,
               table: st,
               name: name));
  end if
end method;
