module:     access-path-implementation
synopsis:   Functions for looking up remote symbols
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// <REMOTE-OBJECT-FILE>
//    An abstraction over a delimiting scope for static symbols.
//    A <remote-object-file> must belong to a <remote-library>. It is
//    permitted to create new <remote-object-file>s while a particular
//    <access-path> is open, and associate them with a <remote-library>.

define class <remote-object-file> (<object>)

  constant slot remote-object-file-core-name :: <byte-string>,
    required-init-keyword: name:;

  constant slot remote-object-file-source-extension :: <byte-string>,
    init-value: "c",
    init-keyword: source-extension:;

  constant slot remote-object-file-object-extension :: <byte-string>,
    init-value: "obj",
    init-keyword: object-extension:;

  constant slot remote-object-file-path :: <byte-string>,
    init-value: "",
    init-keyword: path:;

  constant slot remote-object-file-library :: <remote-library>,
    required-init-keyword: library:;

  constant slot remote-object-file-language :: <integer>,
    init-value: $symbol-language-C,
    init-keyword: language:;

  // Clients of <access-path> are permitted to allocate new
  // <remote-object-file> instances. Therefore, an untyped slot is
  // provided for storage of client data.

  constant slot remote-object-file-client-data :: <object>,
    init-value: #f,
    init-keyword: client-data:;

end class;


///// EXTEND-REMOTE-LIBRARY
//    Adds a new <remote-object-file> to a <remote-library>.

define method extend-remote-library
    (path :: <access-path>, library :: <remote-library>,
     file :: <remote-object-file>)
  => ()
  unless (member?(file, library.library-object-files))
    add!(library.library-object-files, file)
  end unless
end method;


///// <REMOTE-SYMBOL>

define class <remote-symbol> (<object>)

  constant slot remote-symbol-name :: <string>,
    required-init-keyword: name:;

  constant slot remote-symbol-address :: <remote-value>,
    required-init-keyword: address:;

  constant slot remote-symbol-library :: false-or(<remote-library>),
    init-value: #f,
    init-keyword: library:;

  constant slot remote-symbol-object-file :: false-or(<remote-object-file>),
    init-value: #f,
    init-keyword: object-file:;

  constant slot remote-symbol-language :: <integer>,
    required-init-keyword: language:;

  constant slot remote-symbol-storage-status :: 
                             one-of(#"public", #"static", #"exported"),
    init-value: #"public",
    init-keyword: storage-status:;

  slot remote-symbol-source-location-map :: false-or(<source-location-map>),
    init-value: #f;

  slot definitely-no-source-locations :: <boolean>,
    init-value: #f;

end class;


///// LANGUAGES

define constant $symbol-language-C = 0;
define constant $symbol-language-C++ = 1;
define constant $symbol-language-Fortran = 2;
define constant $symbol-language-MASM = 3;
define constant $symbol-language-Pascal = 4;
define constant $symbol-language-BASIC = 5;
define constant $symbol-language-COBOL = 6;
define constant $symbol-language-Dylan = 9;


///// <REMOTE-FUNCTION>
//    A special <remote-symbol> that is known to denote the entry point
//    of a function in the runtime. Slightly more information is
//    available about a <remote-function>: the 'debug-start' and
//    'debug-end' addresses, which inform us when the function's
//    stack frame is set up.

define class <remote-function> (<remote-symbol>)

  // These are both 'absolute', not offsets from the function's
  // remote-symbol-address. The debugger nub does the calculation,
  // since this will be more efficient.

  constant slot remote-function-debug-start :: <remote-value>,
    required-init-keyword: debug-start:;

  constant slot remote-function-debug-end :: <remote-value>,
    required-init-keyword: debug-end:;

  constant slot remote-function-end :: <remote-value>,
    required-init-keyword: absolute-end:;

end class;


///// EXPORTED GENERIC FUNCTIONS


define generic do-symbols 
    (function :: <function>, ap :: <access-path>,
     #key library = #f, matching = #f, type = #f) => ();

define generic nearest-symbols 
    (ap :: <access-path>, ra :: <remote-value>)
      => (symbol :: false-or(<remote-symbol>),
          previous :: false-or(<remote-symbol>),
          next :: false-or(<remote-symbol>));

define generic find-symbol 
    (ap :: <access-path>, name :: <string>,
     #key library = #f, type = #f)
       => (maybe-sym :: false-or(<remote-symbol>));


///// DO-SYMBOLS
//    This function is currently unsupported. In order to make
//    nearest-symbols and find-symbol work with acceptable performance,
//    the lookup algorithms were put (back) into the debugger nub
//    implementation. The debugger manager doesn't even use do-symbols,
//    so this isn't a problem, but we should probably re-think this and
//    get it back on line.

define method do-symbols 
    (function :: <function>, ap :: <access-path>,
     #key library = #f, matching = #f, type = #f) => ()

  // If the keyword library is supplied, we want to work on that
  // library only, otherwise iterate over all libraries...
/*
  if (library)
    do-symbols-in-library (function, ap, library,
                           matching: matching, type: type);
  else
    // update-access-path-libraries (ap);
    for (this-library in ap.libraries)
      do-symbols-in-library (function, ap, this-library,
                             matching: matching, type: type);
    end for;
  end if;
*/
end method;


///// NEAREST-SYMBOLS-FROM-NUB

define open generic nearest-symbols-from-nub 
    (conn :: <access-connection>, address :: <remote-value>) 
 => (any-luck? :: <boolean>,
     where-is-it? :: <NUBLIBRARY>,
     gimme-handle :: <NUBHANDLE>);


///// COLLECT-NEAREST-SYMBOLS

define open generic collect-nearest-symbols 
    (conn :: <access-connection>, lib :: <remote-library>,
     lookups :: <NUBHANDLE>)
 => (closest :: <remote-symbol>,
     prev :: <remote-symbol>,
     nxt :: <remote-symbol>);


///// NEAREST-SYMBOLS

define method nearest-symbols 
     (ap :: <access-path>, address :: <remote-value>)
       => (symbol :: false-or (<remote-symbol>),
           previous :: false-or (<remote-symbol>),
           next :: false-or (<remote-symbol>))

  let (success, lib, table) 
    = nearest-symbols-from-nub (ap.connection, address);
  if (success)
    let remote-lib = find-or-make-library (ap, lib);
    collect-nearest-symbols (ap.connection, remote-lib, table);
  else
    values (#f, #f, #f);
  end if;        
end method;


///// FIND-SYMBOL-IN-LIBRARY

define open generic find-symbol-in-library 
    (conn :: <access-connection>,
     lib :: <remote-library>,
     name :: <string>) 
 => (maybe-sym :: false-or(<remote-symbol>));


///// FIND-SYMBOL

define method find-symbol 
    (ap :: <access-path>, name :: <string>,
     #key library = #f, type = #f)
        => (maybe-sym :: false-or (<remote-symbol>))

  if (library)
    find-symbol-in-library (ap.connection, library, name)
  else
    let i = 0;
    let found = #f;
    while ((~found) & (i < size (ap.libraries)))
      found := find-symbol-in-library 
                 (ap.connection, ap.libraries[i], name);
      i := i + 1;
    end while;
    found;
  end if;       
end method;


///// SYMBOL-RELATIVE-ADDRESS
//    Given an address as a <remote-value>, attempts to convert this to
//    a pair of <remote-symbol> and integer byte offset.

define method symbol-relative-address
  (ap :: <access-path>, address :: <remote-value>)
     => (sym-if-found :: false-or(<remote-symbol>),
         offset       :: <integer>)
  let (sym, unsanitized-offset) =
    symbol-relative-address-on-connection(ap.connection, ap, address);
  let offset =
    if (instance?(unsanitized-offset, <integer>))
      unsanitized-offset
    else
      0
    end if;
  values(sym, offset)
end method;


define open generic symbol-relative-address-on-connection
  (conn :: <access-connection>, 
   path :: <access-path>, address :: <remote-value>)
 => (sym-if-found :: false-or(<remote-symbol>),
     offset);

///// FIRST-FRAME-BREAKABLE-ADDRESS
//    Given a remote symbol, which will already have an address, this function
//    returns a more appropriate position (if one exists) for an entry
//    breakpoint.

define method first-frame-breakable-address
    (symbol :: <remote-symbol>) => (addr :: <remote-value>)
  remote-symbol-address(symbol);
end method;

define method first-frame-breakable-address
    (symbol :: <remote-function>) => (addr :: <remote-value>)
  remote-function-debug-start(symbol);
end method;


///// LAST-FRAME-BREAKABLE-ADDRESS
//    Give a remote symbol, returns the address of the last code location
//    where the frame is still set up, and the function result is available.
//    If this information is not known for the symbol, the symbol's address
//    is returned.

define method last-frame-breakable-address
    (symbol :: <remote-symbol>) => (addr :: <remote-value>)
  remote-symbol-address(symbol);
end method;

define method last-frame-breakable-address
    (symbol :: <remote-function>) => (addr :: <remote-value>)
  remote-function-debug-end(symbol);
end method;


///// ADDRESS-WITHIN-DEFINITION?
//    Takes a <remote-symbol> and a <remote-value> address. If the address
//    is _known_ to be within the .

define method address-within-definition?
    (symbol :: <remote-symbol>, addr :: <remote-value>)
      => (answer :: <boolean>)
  symbol.remote-symbol-address = addr;
end method;

define method address-within-definition?
    (symbol :: <remote-function>, addr :: <remote-value>)
       => (answer :: <boolean>)
  if (symbol.remote-symbol-address = addr)
    #t
  elseif (symbol.remote-symbol-address < addr)
    (addr < symbol.remote-function-end) | (addr = symbol.remote-function-end);
  else
    #f
  end if;
end method;


///// FUNCTION-BOUNDING-ADDRESSES
//    Returns the addresses that delimit the definition of a function.

define method function-bounding-addresses
    (ap :: <access-path>, address :: <remote-value>)
       => (lowerbound :: <remote-value>, upperbound :: <remote-value>)
  function-bounding-addresses-on-connection(ap.connection, address);
end method;

define open generic function-bounding-addresses-on-connection
    (conn :: <access-connection>, address :: <remote-value>)
 => (lowerbound :: <remote-value>, upperbound :: <remote-value>);
