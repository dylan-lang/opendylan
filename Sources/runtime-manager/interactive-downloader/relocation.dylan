module:       interactive-downloader-internals
synopsis:     Performing relocations on downloaded code and data
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// PERFORM-ALL-RELOCATIONS
//    The top level function for performing relocations. Delegates to
//    PERFORM-COFF-FILE-RELOCATIONS on each coff file making up part of
//    the transaction.

define method perform-all-relocations (trans :: <interactive-transaction>)
    => ()
  for (coff-file in trans.transaction-coff-file-sequence)
    perform-coff-file-relocations(trans, coff-file)
  end for
end method;


///// PERFORM-COFF-FILE-RELOCATIONS
//    Goes through all of the sections in a coff file. For each one that
//    is known to have downloaded raw data, processes its relocations.

define method perform-coff-file-relocations
    (trans :: <interactive-transaction>, coff-file :: <coff-file>)
       => ()
  for (coff-section in coff-file.sections.ordered-data)
    let section-base-address = 
      get-section-base-address(trans, coff-section);
    if (section-base-address)
      perform-section-relocations
        (trans, coff-file, coff-section, section-base-address)
    end if
  end for
end method;


///// PERFORM-SECTION-RELOCATIONS
//    Goes through all of the relocations for a particular coff section.
//    Each relocation is calculated, and performed in-place on the downloaded
//    raw data. We have introduced some specific utilities in the debugger
//    nub and access-path to allow this.

define method perform-section-relocations
    (trans :: <interactive-transaction>, 
     coff-file :: <coff-file>,
     coff-section :: <coff-section>,
     section-base-address :: <remote-value>)
        => ()
  for (relocation in coff-section.relocations)
    perform-coff-relocation-in-section
      (trans, coff-file, coff-section, section-base-address, relocation)
  end for
end method;


///// PERFORM-COFF-RELOCATION-IN-SECTION
//    Performs a single relocation, based upon its type.

define method perform-coff-relocation-in-section
    (trans :: <interactive-transaction>, coff-file :: <coff-file>,
     coff-section :: <coff-section>, section-base-address :: <remote-value>,
     relocation :: <coff-relative-relocation>)
  => ()
  let target = trans.transaction-downloader-target;
  let access-path = target.interactive-application.debug-target-access-path;
  let application = target.interactive-application;
  let symbolic-name = 
    relocation.relocation-symbol.symbol-name.string-data;
  let remote-symbol =
    transaction-find-symbol(trans, coff-file, symbolic-name);
  if (remote-symbol)
    let symbolic-address = remote-symbol.remote-symbol-address;
    let relocation-address =
       byte-indexed-remote-value(section-base-address,
                                 relocation.index);
    perform-coff-relocation
      (access-path, relocation-address, symbolic-address, relative?: #t);
  else
    debug-message("+++ WARNING: Reference to unresolved symbol %s\n",
                     symbolic-name)
  end if
end method;

define method perform-coff-relocation-in-section
    (trans :: <interactive-transaction>, coff-file :: <coff-file>,
     coff-section :: <coff-section>, section-base-address :: <remote-value>,
     relocation :: <coff-absolute-relocation>)
  => ()
  let target = trans.transaction-downloader-target;
  let access-path = target.interactive-application.debug-target-access-path;
  let application = target.interactive-application;
  let symbolic-name = 
    relocation.relocation-symbol.symbol-name.string-data;
  let remote-symbol =
    transaction-find-symbol(trans, coff-file, symbolic-name);
  if (remote-symbol)
    let symbolic-address = remote-symbol.remote-symbol-address;
    let relocation-address =
       byte-indexed-remote-value(section-base-address,
                                 relocation.index);
    perform-coff-relocation
      (access-path, relocation-address, symbolic-address, relative?: #f);
  else
    debug-message("+++ WARNING: Reference to unresolved symbol %s\n",
                     symbolic-name)
  end if
end method;

define method perform-coff-relocation-in-section
    (trans :: <interactive-transaction>, coff-file :: <coff-file>,
     coff-section :: <coff-section>, section-base-address :: <remote-value>,
     relocation :: <coff-interactor-relocation>)
  => ()
  let target = trans.transaction-downloader-target;
  let access-path = target.interactive-application.debug-target-access-path;
  let application = target.interactive-application;
  let thread = trans.transaction-thread;

  ///// LOCAL: CREATE-CELL-FOR-INTERACTIVE-REFERENCE
  //           Allocates (and returns the address of) a value cell, into
  //           which an interactively-referenced dylan object (the
  //           "actual value" has been placed).

  local method create-cell-for-interactive-reference
                 (actual-value :: <remote-value>) => (addr :: <remote-value>)
          let i-region = target.dylan-interactive-reference-value-cells;
          unless (i-region.region-searched-for-allocator?)
            initialize-interactive-region(application, i-region);
          end unless;
          let illegal-address = as-remote-value(0);
          let (static-block, fresh?) = 
            find-block-in-region(trans, i-region, 4, 
                                 library: trans.transaction-library);
          if (static-block)
            // If this resulted in the allocation of a new block of
            // value cells, we need to fill the block with "unbound"
            // references, and then register its existence with the
            // runtime's garbage collector.
            if (fresh?)
              let base = static-block.static-block-base-address;
              let final = 
                byte-indexed-remote-value
                  (base, static-block.static-block-size);
              let number-of-cells =
                truncate/(static-block.static-block-size, 4);
              let unbound-markers =
                make(<vector>, 
                     size: number-of-cells,
                     fill: application.dylan-runtime-unbound-marker);
              download-remote-value-vector-into
                (access-path, static-block, unbound-markers);
              register-exact-roots(application, base, final, thread: thread);
              recycle-static-block(static-block);
            end if;
            download-remote-value-into
              (access-path, static-block, actual-value);
          else
            illegal-address
          end if;
        end method;

  let interactor-id = relocation.interactor-handle;
  let actual-value =
    if (instance?(interactor-id, <history-place-holder>))
      retrieve-object-from-thread-history
        (application, interactor-id.history-place-holder-thread,
         interactor-id.history-place-holder-index)
    else
      interactor-id;
    end if;
  let indirection-address =
    create-cell-for-interactive-reference(actual-value);
  let relocation-address =
    byte-indexed-remote-value(section-base-address,
                              relocation.index);
  perform-coff-relocation
    (access-path, relocation-address, indirection-address, relative?: #f);
  write-dylan-value
    (application, indirection-address, actual-value);
end method;
