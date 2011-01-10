module:       interactive-downloader-internals
synopsis:     Resolving and defining symbols
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DEFINE-ALL-SYMBOLS
//    Defines all symbols in all coff files.

define method define-all-symbols (trans :: <interactive-transaction>) => ()
  let application = 
    trans.transaction-downloader-target.interactive-application;
  let public-table = 
    application.debug-target-symbol-table;
  defining-symbols(public-table)
    for (coff-file in trans.transaction-coff-file-sequence)
      define-coff-file-symbols(trans, public-table, coff-file);
    end for;
  end defining-symbols;
end method;


///// DEFINE-COFF-FILE-SYMBOLS
//    Responsible for going through the <coff-file>'s symbol table, and
//    entering each symbol's calculated address into the interactive
//    symbol table.
//    All defined symbols must have a section, whose base address will be
//    known, because the raw data will, by this time, have been downloaded
//    into the runtime. The symbol's actual address can then be calculated
//    as a byte offset from the base address.

define method define-coff-file-symbols
    (trans :: <interactive-transaction>, 
     public-table :: <interactive-symbol-table>,
     coff-file :: <coff-file>) => ()
  let static-file = trans.transaction-object-files[coff-file];
  for (coff-symbol in coff-file.symbols.ordered-data)
    define-this-symbol(trans, static-file, public-table, coff-symbol)
  end for;
end method;


///// GET-SECTION-BASE-ADDRESS
//    Returns the base address of the section in which the symbol is
//    defined.

define method get-section-base-address
    (trans :: <interactive-transaction>, section :: <coff-symbol-locator>)
       => (base :: false-or(<remote-value>))
  //
  // We are trying to find the address of a symbol defined outside of this
  // coff file (since it is not defined directly in a <coff-section>). 
  //
  #f
end method;

define method get-section-base-address
    (trans :: <interactive-transaction>, section :: <coff-section>)
       => (base :: false-or(<remote-value>))
  element(trans.transaction-section-addresses, section, default: #f)
end method;


///// DEFINE-THIS-SYMBOL
//    Enters a single symbol into the interactive table.

define method define-this-symbol
    (trans :: <interactive-transaction>, 
     static-file :: <remote-object-file>,
     public-table :: <interactive-symbol-table>,
     symbol :: <coff-symbol-record>) => ()
  //
  // Do nothing in this case.
  // We are only interested in symbols that are specific <coff-symbol>
  // instances.
  //
end method;

define method define-this-symbol
    (trans :: <interactive-transaction>, 
     static-file :: <remote-object-file>,
     public-table :: <interactive-symbol-table>,
     symbol :: <coff-symbol>) 
       => ()

  let lib = trans.transaction-library;

  local method import-symbol? (name :: <string>) => (ans :: <boolean>)
      (name.size > 5) &
      (name[0] == '_') &
      (name[1] == '_') &
      (name[2] == 'i') &
      (name[3] == 'm') &
      (name[4] == 'p') &
      (name[5] == '_')
  end method;

  local method non-data-symbol? (name :: <string>) => (ans :: <boolean>)
      (name.size > 0) &
      (name[0] == '.')
  end method;

  local method strip-import (name :: <string>) => (stripped :: <string>)
    let stripped = make(<byte-string>, size: name.size - 6);
    for (i from 0 below (name.size - 6))
      stripped[i] := name[i + 6]
    end for;
    stripped
  end method;

  let dt = trans.transaction-downloader-target.interactive-application;
  let imp-region = trans.transaction-downloader-target.dylan-import-table;

  ///// CREATE-AND-DEFINE-IMPORT
  //    An object file is referencing a symbol of type __imp_.

  local method create-and-define-import (name :: <string>) => ()
    unless(imp-region.region-searched-for-allocator?)
      initialize-interactive-region(dt, imp-region)
    end unless;
    let blk = find-block-in-region(trans, imp-region, 4, library: lib);
    let path = dt.debug-target-access-path;
    let newname = strip-import(name);
    let imp-from-somewhere = symbol-table-find-symbol(public-table, name);
    let actual-sym = 
      imp-from-somewhere | symbol-table-find-symbol(public-table, newname);
    let actual-val =
      if (imp-from-somewhere)
//      format-out("Import existed elsewhere. Performing indirection.\n");
        read-value(path, imp-from-somewhere.remote-symbol-address);
      elseif (actual-sym)
        actual-sym.remote-symbol-address
      else
//      format-out("+++ WARNING: Could not find true definition of %s\n",
//                 newname);
        as-remote-value(0)
      end if;
    let imp-addr = 
      if (blk)
        download-remote-value-into
           (path, blk, actual-val)
      else
//      format-out("+++ WARNING: Failure to define import symbol %s\n",
//                 name);
        as-remote-value(0)
      end if;
    symbol-table-define-symbol(public-table, name, imp-addr,
                               language: $symbol-language-Dylan,
                               library: lib);
//  format-out
//      ("Defined import symbol %s at 0x%s to be 0x%s\n",
//       name,
//       remote-value-as-string(path, imp-addr, 16),
//       remote-value-as-string(path, actual-val, 16));
  end method;

  // If this symbol is defined in a section in this coff file, then get the
  // base address at which the section's raw data was downloaded.

  let section-base-address = 
    get-section-base-address(trans, symbol.section);

  // section-base-address could be #f if this symbol is being referenced
  // but not defined, or if an earlier stage has failed. In either case,
  // do not enter this symbol into the interactive table.

  if (section-base-address & ~non-data-symbol?(symbol.symbol-name.string-data))

    // We are going to define the symbol, as long as its storage class
    // makes it "interesting".

    select (symbol.storage-class)
      $sym-external =>
        let symbolic-name = symbol.symbol-name.string-data;
        let symbolic-address =
          byte-indexed-remote-value
            (section-base-address, symbol.symbol-value);
        symbol-table-define-symbol
          (public-table, symbolic-name, symbolic-address,
           language: $symbol-language-Dylan, library: lib, file: static-file);

      $sym-static =>
        let symbolic-name = symbol.symbol-name.string-data;
        let symbolic-address =
          byte-indexed-remote-value(section-base-address, symbol.symbol-value);
        symbol-table-define-symbol
          (public-table, symbolic-name, symbolic-address,
           language: $symbol-language-Dylan, library: lib, file: static-file,
           storage-status: #"static");

      $sym-function =>
        let symbolic-name = symbol.symbol-name.string-data;
        let symbolic-address =
          byte-indexed-remote-value
             (section-base-address, symbol.symbol-value);
        symbol-table-define-symbol
          (public-table, symbolic-name, symbolic-address,
           language: $symbol-language-Dylan, library: lib, file: static-file);

    end select;
  else
    let symbolic-name = symbol.symbol-name.string-data;
    if (import-symbol?(symbolic-name))
      unless (symbol-table-find-symbol(public-table, symbolic-name,
                                       local-lookup-only?: #t, library: lib))
        create-and-define-import(symbolic-name);
      end unless;
    end if
  end if
end method;
