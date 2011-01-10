module:      interactive-downloader-internals
synopsis:    Describing interactive transactions and dynamic contexts.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// Types of memory registration.
//    Exact        - A portion of memory known to be a contiguous array of
//                   root references to dylan objects.
//    Static       - A portion of memory known to contain statically
//                   heaped dylan objects.
//    Ambiguous    - A portion of memory that might contain some references
//                   to dylan objects.

define constant $registration-style-exact     = 1;
define constant $registration-style-static    = 2;
define constant $registration-style-ambiguous = 3;
define constant $registration-style-fixup     = 4;
define constant $registration-style-import    = 5;
define constant $registration-style-code      = 6;


///// <MEMORY-REGISTRATION>
//    A class that describes a portion of memory that requires registration
//    with the memory manager.

define class <memory-registration> (<object>)

  constant slot memory-registration-lower-address :: <remote-value>,
    required-init-keyword: lower-bound:;

  constant slot memory-registration-upper-address :: <remote-value>,
    required-init-keyword: upper-bound:;

  constant slot memory-registration-style :: <integer>,
    init-value: $registration-style-ambiguous,
    init-keyword: style:;

end class;


///// <INTERACTIVE-TRANSACTION>
//    Describes an "interactive transaction", associated with a
//    <downloader-target> (and hence a <debug-target>) and a
//    sequence of <coff-file> objects. This object stores all of the details
//    that are required to manage the downloading of the sequence of
//    coff files.

define class <interactive-transaction> (<object>)

  // A pointer to the <downloader-target> that describes all of the
  // interactive memory regions.

  constant slot transaction-downloader-target :: <downloader-target>,
    required-init-keyword: downloader-target:;

  // A sequence of <coff-file> objects that need to be downloaded for this
  // transaction.

  constant slot transaction-coff-file-sequence :: <sequence>,
    required-init-keyword: coff-file-sequence:;

  // A <remote-library> to lookup symbols in.

  constant slot transaction-library :: false-or(<remote-library>),
    init-value: #f,
    init-keyword: dll:;

  constant slot transaction-dylan-library :: false-or(<remote-library>),
    init-value: #f,
    init-keyword: dylan-dll:;

  // Each <coff-file> needs its own <remote-object-file>, exclusively
  // used to scope the _static_ symbols that it defines.
  // This table, keyed on <coff-file>, stores those object files.

  constant slot transaction-object-files :: <table> = make(<table>);

  // We will be downloading raw data for a number of <coff-section>s.
  // This table, keyed on <coff-section>, stores the base address at
  // which its data was downloaded. This address will be a 
  // <remote-value>.

  constant slot transaction-section-addresses :: <table> = make(<table>);

  // This table, keyed on <coff-section>, stores a symbol that abstractly
  // describes the section, based on its name.

  constant slot transaction-section-types :: <table> = make(<table>);

  // While downloading raw data, it will be necessary to remember how to
  // register it with the garbage collector. (This will be the last thing
  // to happen, after all of the relocations have been processed).
  // This sequence of <memory-registration> descriptors will be the means
  // for recording these deferred registrations.

  constant slot transaction-deferred-registrations :: <sequence>
     = make(<stretchy-vector>, size: 0);

  // Record the thread which we will use for all spy functions, and
  // also for running the interactive code itself.

  constant slot transaction-thread :: <remote-thread>,
    required-init-keyword: thread:;

end class;


///// OPEN-INTERACTIVE-TRANSACTION
//    Allocates and initializes an <interactive-transaction>, given a
//    <debug-target> and a sequence of <coff-file> objects.

define method open-interactive-transaction
     (application :: <debug-target>, coff-files :: <sequence>,
      #key dll-name = "dylan", thread = #f)
         => (transaction :: <interactive-transaction>)

  // NAME-INFORMATION-FROM-COFF-FILENAME
  // Splits up an arbitrary filename into path, name, extension. This
  // will break if the filename is illegal.

  local method name-information-from-coff-filename (n :: <byte-string>)
           => (name :: <byte-string>, path :: <byte-string>,
               source-ext :: <byte-string>, obj-ext :: <byte-string>)
          let index-slash = -1;
          let index-dot = n.size;
          let limit = n.size;
          for (i from 0 below limit)
            if ((n[i] == '/') | (n[i] == '\\'))
              index-slash := i
            elseif (n[i] == '.')
              index-dot := i
            end if;
          end for;
          let path = make(<byte-string>, size: index-slash + 1);
          let source-ext = 
            if (limit == index-dot)
              ""
            else 
              make(<byte-string>, size: limit - index-dot - 1)
            end if;
          let obj-ext = "obj";
          let name = make(<byte-string>, size: index-dot - index-slash - 1);
          let j = 0;
          for (i from 0 to index-slash)
            path[j] := n[i]; j := j + 1
          end for;
          let j = 0;
          for (i from (index-slash + 1) below index-dot)
            name[j] := n[i]; j := j + 1
          end for;
          let j = 0;
          for (i from (index-dot + 1) below limit)
            source-ext[j] := n[i]; j := j + 1
          end for;
          values(name, path, source-ext, obj-ext);
        end method;

  // NAME-INFORMATION-FROM-COFF-FILE

  local method name-information-from-coff-file (f :: <coff-file>)
           => (name :: <byte-string>, path :: <byte-string>,
               source-ext :: <byte-string>, obj-ext :: <byte-string>)
          let file-sym =
            element(f.symbols.table-data, ".file", default: #f);
          if (file-sym)
            let file-aux =
              element(file-sym.aux-symbols, 0, default: #f);
            if (file-aux)
              name-information-from-coff-filename(file-aux.auxiliary-string)
            else
              values("unknown", "", "dylan", "obj");
            end if
          else
            values("unknown", "", "dylan", "obj");
          end if
        end method;

  // Find (or create) a <downloader-target> to correspond to the
  // <debug-target>.

  let dt = find-downloader-target(application);
  let dll = find-library-called(application, dll-name);
  let dylan-dll = find-library-called(application, "dylan");
  let table = application.debug-target-symbol-table;

  // If an associated thread was not supplied, use whichever is available
  // for running spy functions.
  unless (thread)
    thread := select-thread-for-spy(application)
  end unless;

  // Allocate the <interactive-transaction> itself, storing in the
  // debug target, and the sequence of COFF file descriptors.

  let transaction =
     make(<interactive-transaction>,
          downloader-target: dt, coff-file-sequence: coff-files, 
          dll: dll, dylan-dll: dylan-dll, thread: thread);

  // Define a <remote-object-file> to hold the static symbols of
  // each coff file.
  // Also, classify each coff section.

  for (cf in coff-files)
    let (core-name, path, source-extension, object-extension)
      = name-information-from-coff-file(cf);
    transaction.transaction-object-files[cf] :=
      symbol-table-define-file
        (table, core-name,
         language: $symbol-language-Dylan,
         library: dll,
         client-data: #f,
         source-extension: source-extension,
         object-extension: object-extension,
         path: path);
    for (sect in cf.sections.ordered-data)
      transaction.transaction-section-types[sect] :=
        classify-coff-section(sect);
    end for
  end for;

  // Return the initialized transaction
  transaction
end method;


///// TRANSACTION-FIND-SYMBOL
//    A convenient abstraction over the process of locating a symbol.
//    This is done in the context of a <coff-file>, which will define
//    a number of static symbols. The underlying interactive symbol table
//    will also define a number of public symbols. This will return the
//    correct static symbol if this <coff-file> defines it, otherwise it
//    will look in the publics.

define method transaction-find-symbol
   (trans :: <interactive-transaction>, coff-file :: <coff-file>,
    name :: <string>)
      => (maybe-sym :: false-or(<remote-symbol>))
  let local-file = trans.transaction-object-files[coff-file];
  let target = trans.transaction-downloader-target.interactive-application;
  let global-table = target.debug-target-symbol-table;
  let library = trans.transaction-library;
  symbol-table-find-symbol(global-table, name, library: library,
                           local-lookup-only?: #t, file: local-file) |
  symbol-table-find-symbol(global-table, name, library: library) |
  symbol-table-find-symbol(global-table, name, 
                           library: trans.transaction-dylan-library) |
  symbol-table-find-symbol(global-table, name)
end method;



