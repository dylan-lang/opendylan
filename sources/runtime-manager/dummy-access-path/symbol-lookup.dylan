module:     access-path-implementation
synopsis:   Functions for looking up remote symbols
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// CONSTANTS
//    These represent the languages that can define a symbol in the
//    runtime.

define constant $symbol-language-C = 0;
define constant $symbol-language-C++ = 1;
define constant $symbol-language-Fortran = 2;
define constant $symbol-language-MASM = 3;
define constant $symbol-language-Pascal = 4;
define constant $symbol-language-BASIC = 5;
define constant $symbol-language-COBOL = 6;
define constant $symbol-language-Dylan = 9;


///// <REMOTE-SYMBOL>
//    Models a symbol in the application.

define class <remote-symbol> (<object>)

       slot remote-symbol-name :: <string>,
            required-init-keyword: name:;

       slot remote-symbol-address :: <remote-value>,
            required-init-keyword: address:;

       slot remote-symbol-library :: false-or(<remote-library>),
            init-value: #f,
            init-keyword: library:;

       slot remote-symbol-source-location-map,
            init-value: #f;

       slot definitely-no-source-locations :: <boolean>,
            init-value: #f;

       slot remote-symbol-language :: <integer>,
            init-value: $symbol-language-Dylan;

end class;


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

       slot remote-function-debug-start :: <remote-value>,
            required-init-keyword: debug-start:;

       slot remote-function-debug-end :: <remote-value>,
            required-init-keyword: debug-end:;

end class;

///// SUPREME HACK DUE TO KNACKERED CLOSURES IN SCC
///// NEED TO CHANGE AFTER BUG FIX

define variable *closest* = #f;
define variable *closest-above* = #f;
define variable *closest-below* = #f;
define variable *symbol-found* = #f;


///// EXPORTED GENERIC FUNCTIONS


define generic do-symbols 
    (function :: <function>, ap :: <access-path>,
     #key library = #f, matching = #f, type = #f) => ();

define generic nearest-symbols 
    (ap :: <access-path>, ra :: <remote-value>)
       => (symbol :: <remote-symbol>,
           previous :: <remote-symbol>,
           next :: <remote-symbol>);

define generic find-symbol (ap :: <access-path>, name :: <string>,
                            #key library = #f, type = #f)
                            => (_ :: false-or(<remote-symbol>));

///// DO-SYMBOLS

define method address-distance (a1 :: <remote-value>, a2 :: <remote-value>)
     => (_ :: <integer>)
  let i1 = as-integer(a1);
  let i2 = as-integer(a2);
  abs (i1 - i2);
end method;


define method do-symbols 
    (function :: <function>, ap :: <access-path>,
     #key library = #f, matching = #f, type = #f) => ()
end method;


///// NEAREST-SYMBOLS

define method nearest-symbols 
    (ap :: <access-path>, address :: <remote-value>)
      => (symbol :: false-or (<remote-symbol>),
          previous :: false-or (<remote-symbol>),
          next :: false-or (<remote-symbol>))

  let closest = #f;
  let prev = #f;
  let nxt = #f;
  let i = 0;
  let lim = size(ap.connection.process.debug-info);
  while ((~closest) & (i < lim))
    let sym = ap.connection.process.debug-info[i];
    if (instance?(sym, <simulation-function-description>))
      if ((sym.compiled-address <= address) &
          (address < sym.compiled-address + size(sym.code-vector)))
        closest := make(<remote-function>,
                        name: sym.symbolic-name,
                        address: sym.compiled-address,
                        library: *the-only-library*,
                        debug-start: sym.debug-start-offset,
                        debug-end: sym.debug-end-offset);
      else
        i := i + 1;
      end if;
    elseif (instance?(sym, <simulation-variable-description>))
      if ((sym.dumped-address <= address) &
          (address < sym.dumped-address + size(sym.representation)))
        closest := make(<remote-symbol>,
                        name: sym.symbolic-name,
                        address: sym.dumped-address,
                        library: *the-only-library*);
      else
        i := i + 1;
      end if;
    else
      i := i + 1;
    end if
  end while;

  if (closest)
    if (i > 0)
      let sym = ap.connection.process.debug-info[i - 1];
      if (instance?(sym, <simulation-function-description>))
        prev := make(<remote-function>,
                     name: sym.symbolic-name,
                     address: sym.compiled-address,
                     library: *the-only-library*,
                     debug-start: sym.debug-start-offset,
                     debug-end: sym.debug-end-offset);
      elseif (instance?(sym, <simulation-variable-description>))
        prev := make(<remote-symbol>,
                     name: sym.symbolic-name,
                     address: sym.dumped-address,
                     library: *the-only-library*);
      else
        prev := #f;
      end if
    end if;

    if ((i + 1) < size(ap.connection.process.debug-info))
      let sym = ap.connection.process.debug-info[i + 1];
      if (instance?(sym, <simulation-function-description>))
        nxt := make(<remote-function>,
                    name: sym.symbolic-name,
                    address: sym.compiled-address,
                    library: *the-only-library*,
                    debug-start: sym.debug-start-offset,
                    debug-end: sym.debug-end-offset);
      elseif (instance?(sym, <simulation-variable-description>))
        nxt := make(<remote-symbol>,
                    name: sym.symbolic-name,
                    address: sym.dumped-address,
                    library: *the-only-library*);
      else
        nxt := #f;
      end if;
    end if

  end if;

  values (closest, prev, nxt);
end method;


///// FIND-SYMBOL

define method find-symbol (ap :: <access-path>, name :: <string>,
                           #key library = #f, type = #f)
                           => (_ :: false-or (<remote-symbol>))

   let i = 0;
   let found = #f;
   while ((~found) & (i < size(ap.connection.process.debug-info)))
     let sym = ap.connection.process.debug-info[i];
     if (name = sym.symbolic-name)
       if (instance?(sym, <simulation-function-description>))
         found := make(<remote-function>,
                       name: name,
                       address: sym.compiled-address,
                       library: *the-only-library*,
                       debug-start: sym.debug-start-offset,
                       debug-end: sym.debug-end-offset);
       elseif (instance?(sym, <simulation-variable-description>))
         found := make(<remote-symbol>,
                       name: name,
                       address: sym.dumped-address,
                       library: *the-only-library*);
       else
         found := #f;
       end if;
     else
       i := i + 1;
     end if
   end while;
   found;     
end method;


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
    #f
  else
    #f
  end if;
end method;


///// SYMBOL-RELATIVE-ADDRESS
//    Given an instruction pointer, returns the most closely defined
//    <remote-symbol>, and a byte offset from the actual address of the
//    definition.

define method symbol-relative-address
    (ap :: <access-path>, address :: <remote-value>)
      => (symbol :: false-or (<remote-symbol>),
          offset :: <integer>)
  let closest = #f;
  let prev = #f;
  let nxt = #f;
  let i = 0;
  let lim = size(ap.connection.process.debug-info);
  while ((~closest) & (i < lim))
    let sym = ap.connection.process.debug-info[i];
    if (instance?(sym, <simulation-function-description>))
      if ((sym.compiled-address <= address) &
          (address < sym.compiled-address + size(sym.code-vector)))
        closest := make(<remote-function>,
                        name: sym.symbolic-name,
                        address: sym.compiled-address,
                        library: *the-only-library*,
                        debug-start: sym.debug-start-offset,
                        debug-end: sym.debug-end-offset);
      else
        i := i + 1;
      end if;
    elseif (instance?(sym, <simulation-variable-description>))
      if ((sym.dumped-address <= address) &
          (address < sym.dumped-address + size(sym.representation)))
        closest := make(<remote-symbol>,
                        name: sym.symbolic-name,
                        address: sym.dumped-address,
                        library: *the-only-library*);
      else
        i := i + 1;
      end if;
    else
      i := i + 1;
    end if
  end while;

  if (closest)
    values(closest, address - closest.remote-symbol-address);
  else
    values(#f, 0);
  end if;
end method;


///// FUNCTION-BOUNDING-ADDRESSES
//    Given an instruction pointer, returns the two addresses which delimit
//    the function definition containing it.

define method function-bounding-addresses
    (ap :: <access-path>, address :: <remote-value>)
       => (start-addr :: <remote-value>, end-addr :: <remote-value>)
  let lowerbound = #f;
  let upperbound = #f;
  let i = 0;
  let lim = size(ap.connection.process.debug-info);
  while ((~lowerbound) & (i < lim))
    let sym = ap.connection.process.debug-info[i];
    if (instance?(sym, <simulation-function-description>))
      if ((sym.compiled-address <= address) &
          (address < sym.compiled-address + size(sym.code-vector)))
        lowerbound := sym.compiled-address;
        upperbound := sym.compiled-address + size(sym.code-vector) - 1;
      else
        i := i + 1;
      end if;
    else
      i := i + 1;
    end if
  end while;

  if (~lowerbound)
    lowerbound := address;
    upperbound := address;
  end if;

  values(lowerbound, upperbound);
end method;

