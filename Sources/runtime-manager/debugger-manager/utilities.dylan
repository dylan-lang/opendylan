module:     dm-internals
synopsis:   A set of utility functions for the debugger manager
            implementation.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// COMPILATION-CONTEXT-COMPONENT
//    Given a compilation context, try to obtain a <REMOTE-LIBRARY> into
//    which its definitions were emitted.

define method compilation-context-component
    (application :: <debug-target>, context :: <object>)
  => (lib :: <remote-library>)
  element(application.library-component-names,
          context.compilation-context-library-name,
          default: application.application-executable)
end method;


///// COMPILATION-CONTEXT-FROM-LIBRARY
//    Given a <remote-library>, attempt to get the corresponding
//    <compilation-context> from the compiler debug info.

define method compilation-context-from-library
    (application :: <debug-target>, lib :: <remote-library>)
 => (context)
  if (application.debug-target-compilation-context)
    let component-name = lib.library-core-name;
    component-name-context
      (application.debug-target-compilation-context, 
       application, 
       component-name);
  else
    #f
  end if
end method;


///// COMPILATION-CONTEXT-FROM-SYMBOL
//    Given a <remote-symbol>, attempt to get the corresponding
//    <compilation-context> from the compiler debug info.

define method compilation-context-from-symbol
    (application :: <debug-target>, symbol :: <remote-symbol>)
 => (context)
  if (application.debug-target-compilation-context)
    let dylan-libname =
      demangler-extract-library-name
        ($demangler, symbol.remote-symbol-name);
    if (dylan-libname)
      library-name-context
        (application.debug-target-compilation-context, 
         application,
         dylan-libname)
    else
      compilation-context-from-library
        (application, symbol.remote-symbol-library)
    end if
  else
   #f
  end if
end method;


///// COMPILED-LAMBDA-IN-CONTEXT-FROM-SYMBOL
//    Given a <remote-symbol>, attempt to find both the <compilation-context>
//    and the <compiled-lambda> that correspond to it.

define method compiled-lambda-in-context-from-symbol
    (application :: <debug-target>, symbol :: <remote-symbol>)
 => (context,
     lambda :: false-or(<compiled-lambda>))

  local method make-runtime-compiled-lambda ()
	 => (lambda :: false-or(<runtime-compiled-lambda>))
          if (symbol.remote-symbol-language == $symbol-language-Dylan)
            make(<runtime-dylan-compiled-lambda>,
                 target: application, entry-point-symbol: symbol)
	  else
            make(<runtime-foreign-compiled-lambda>,
                 target: application, entry-point-symbol: symbol)
	  end if;
	end method;

  let context = compilation-context-from-symbol(application, symbol);

  let object-filename =
     if (symbol.remote-symbol-object-file)
       symbol.remote-symbol-object-file.remote-object-file-core-name
     else
       #f
     end if;

  if (context)
    values(context,
           symbolic-name-compiled-lambda(context,
                                         symbol.remote-symbol-name,
                                         file-name: object-filename) |
           make-runtime-compiled-lambda())
  else
    values(#f, make-runtime-compiled-lambda())
  end if
end method;


///// CALL-FRAME-RECORDED-LOCATIONS
//    A utility function used by the stepping and source-alignment
//    implementations. Given any function-frame in a stack trace, returns
//    a sequence of addresses at which breakpoints can be set in order
//    to capture the runtime at a recorded (and optionally an interactive)
//    location within that frame.
//    The function also returns the address that can be used to capture
//    the frame's return.
//    This will use compiler debug-info preferentially, but will resort to
//    runtime source-locator-maps if necessary.

define method call-frame-recorded-locations
    (application :: <debug-target>, call-frame :: <call-frame>,
     #key interactive? = #f)
  => (frame-pointer-context :: <remote-value>,
      caller-frame-pointer-context :: <remote-value>,
      step-out-location :: <remote-value>,
      step-over-locations :: <sequence>)

  let path = application.debug-target-access-path;
  let ap-frame = call-frame-description(application, call-frame);
  let frame-pointer-context = frame-pointer(path, ap-frame);
  let calling-ap-frame = previous-frame(path, ap-frame);
  let caller-frame-pointer-context =
    if (calling-ap-frame) 
      frame-pointer(path, calling-ap-frame)
    else
      frame-pointer-context // Is better than zero!!
    end if;
  let step-out-location = call-frame-return-address(application, call-frame);
  let (symbol, lambda-object, gf-object) =
    call-frame-function(application, call-frame);
  let step-over-locations = #[];
  let function-base =
    if (symbol) symbol.remote-symbol-address else as-remote-value(0) end if;


  // A convenience function to turn a sequence of byte-offsets into a
  // sequence of actual instruction pointers.

  local method make-absolute-addresses (byte-offsets :: <sequence>)
	 => (absolute-addresses :: <sequence>)
          map(curry(byte-indexed-remote-value, function-base),
              byte-offsets);
	end method;

  // The convenience function to be punted to if compiler debug-info access
  // gets blocked, or is unavailable to start with.

  local method defer-to-runtime-information () => ()
          if (symbol)
            // Call the access-path function to lookup runtime data.
            let (fname, base-line, base-addr, lines, byte-offsets)
              = function-recorded-source-locations(path, symbol);
            step-over-locations := 
              make-absolute-addresses(remove-duplicates(byte-offsets));
	  end if;
	end method;

  if (dylan-call-frame?(application, call-frame))
    // This is a dylan call frame. The first thing to try is to get
    // a <compiled-lambda> and a <compilation-context> that will allow
    // us to lookup this information via the compiler.
    let (context, lambda) =
      compiled-lambda-in-context-from-symbol(application, symbol);
    if (lambda & context)
      // If we have a compiled-lambda and its context, we can now commit
      // ourselves to compiler-info lookup.
      let mapped-offsets = compiled-lambda-mapped-code-offsets
                             (lambda, interactive-only?: interactive?);
      step-over-locations := 
        make-absolute-addresses(remove-duplicates(mapped-offsets));
    else
      defer-to-runtime-information();
    end if; 
  else
    // If this is a foreign call frame, then none of its recorded
    // code-locations can be considered safe for interaction, so don't even
    // bother to look them up if the caller is filtering out non-interactive
    // locations.
    unless (interactive?)
      defer-to-runtime-information();
    end unless;
  end if;

  values(frame-pointer-context,
         caller-frame-pointer-context,
         step-out-location,
         step-over-locations)
end method;
