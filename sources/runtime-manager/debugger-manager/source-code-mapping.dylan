module:     dm-internals
synopsis:   Mapping between "source positions" in a compilation context,
            and runtime instruction addresses.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// RESOLVE-SOURCE-CODE-POSITION
//    A more general function to map a position in a source file to a 
//    remote instruction address, possibly with the help of information
//    from the compiler.

define method resolve-source-code-position
    (application :: <debug-target>, compilation-context :: <object>,
     sr :: <source-record>, line-number :: <integer>,
     #key column-number = 0, interactive-only? = #t)
  => (address-if-found :: false-or(<remote-value>),
      relative-symbol :: false-or(<remote-symbol>),
      relative-offset :: false-or(<integer>))

  let address = #f;
  let relative-symbol = #f;
  let offset = #f;
  let table = application.debug-target-symbol-table;

  // A function to do the best we can with runtime debug information.

  local method defer-to-runtime-information () => ()
          let path = application.debug-target-access-path;
          let lib =
            if (compilation-context)
              compilation-context-component(application, compilation-context)
	    else
              #f
	    end if;
          let (addr, exact) =
            resolve-source-location
              (path, 
               sr.source-record-name | "unnamed-source-record.dylan", 
               line: line-number, 
               column: column-number,
               library: lib);
          if (addr)
            let (sym, byte-offset) = 
               symbol-table-symbol-relative-address(table, addr);
            if (sym)
              relative-symbol := sym;
              offset := byte-offset;
	    end if;
            address := addr
	  end if;
	end method;

  // If the compilation context in which to do the search has not been
  // passed in, then just use whatever compilation context is currently
  // associated with the debug target. Note that this can still be #f.

  unless (compilation-context)
    compilation-context := application.debug-target-compilation-context
  end unless;

  if (compilation-context)
    let lib = compilation-context-component(application, compilation-context);
    let (lambda, byte-offset) =
      source-position-compiled-lambda
        (compilation-context, 
         sr, 
         line-number,
         interactive-only?: interactive-only?);
    if (lambda & byte-offset)
      let sym-name = 
        compiled-lambda-symbolic-name(compilation-context, lambda);
      let sym = symbol-table-find-symbol(table, sym-name, library: lib);
      if (sym)
        relative-symbol := sym;
        address := 
          byte-indexed-remote-value(sym.remote-symbol-address, byte-offset);
        offset := byte-offset;
      end if;
    else
      defer-to-runtime-information();
    end if
  else
    defer-to-runtime-information();
  end if;
  values(address, relative-symbol, offset);
end method;


///// SOURCE-LOCATION-REMOTE-ADDRESS
//    The exported utility in the debugger-manager to map a source location
//    to a remote address.

define method source-location-remote-address
     (application :: <debug-target>, source-location :: <source-location>,
      #key line-only? = #t, interactive-only? = #t,
           entry-point-only? = #f, compilation-context = #f)
  => (address :: false-or(<remote-value>))
  let source-record = source-location.source-location-source-record;
  let (filename, line-number) = 
    source-line-location(source-record, 
                         source-location.source-location-start-line);
  unless (filename)
    filename := "no-one-has-a-file-with-this-name.dylan"
  end unless;
  let (address, symbol, offset) =
    resolve-source-code-position(application, 
                                 compilation-context, 
                                 source-record, 
                                 line-number,
                                 interactive-only?: interactive-only?);
  if (entry-point-only?)
    symbol & symbol.remote-symbol-address;
  else
    address;
  end if;
end method;


///// REMOTE-ADDRESS-SOURCE-LOCATION
//    Maps the other way.
//    TODO: This function currently can't defer to runtime information,
//          due to the apparent inability to create source records at
//          random from named files.

define method remote-address-source-location
    (application :: <debug-target>, address :: <remote-value>,
     #key line-only? = #t, interactive-only? = #f, exact-only? = #f)
  => (source-location :: false-or(<source-location>), exact? :: <boolean>)

  // Initialize the return value pessimistically.
  let source-location = #f;
  let exact? = #f;

  // We need a symbolic name via which to access debug info, so try to find
  // a symbol corresponding to this address.

  let interactive-table = application.debug-target-symbol-table;
  let (sym, offset) = 
    symbol-table-symbol-relative-address(interactive-table, address);

  if (sym)
    // Our only possible route to the source location is via the
    // <compiled-lambda> object whose definition encloses this address.
    // Call the utility function to find the compiled lambda and its context.

    let (context, lambda) =
      compiled-lambda-in-context-from-symbol(application, sym);

    // If we have this, it should be a simple matter to get the source
    // location from the compiler database.

    if (lambda)
      let (location, ex) =
        compiled-lambda-source-location
          (lambda,
           offset,
           exact?: exact-only?,
           line-only?: line-only?,
           interactive-only?: interactive-only?);
      source-location := location;
      exact? := ex;
    end if;
  end if;

  // Return whatever we found.
  values(source-location, exact?);
end method;
