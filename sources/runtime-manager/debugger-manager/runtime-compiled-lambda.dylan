module:        dm-internals
synopsis:      Describing a compiled lambda, all of whose debugging information
               is going to be obtained from the runtime debug data.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <RUNTIME-COMPILED-LAMBDA>
//    A subclass of <compiled-lambda>, describing a dylan function that
//    has no compiler-generated debugging information.

define abstract class <runtime-compiled-lambda> (<compiled-lambda>)
/*
  slot lambda-debug-target :: <debug-target>,
    required-init-keyword: target:;

  slot lambda-entry-point-symbol :: <remote-symbol>,
    required-init-keyword: entry-point-symbol:;

  slot lambda-runtime-recorded-offsets :: false-or(<sequence>),
    init-value: #f,
    init-keyword: recorded-offsets:;

  slot lambda-runtime-filename :: false-or(<byte-string>),
    init-value: #f,
    init-keyword: filename:;

  slot lambda-runtime-linenumber :: false-or(<integer>),
    init-value: #f,
    init-keyword: line:;

  slot lambda-no-source-information :: <boolean>,
    init-value: #f;
*/
end class;


///// <RUNTIME-DYLAN-COMPILED-LAMBDA>

define class <runtime-dylan-compiled-lambda> (<runtime-compiled-lambda>)
/*
  slot lambda-runtime-interactive-offsets :: false-or(<sequence>),
    init-value: #f,
    init-keyword: interactive-offsets:;
*/
end class;


///// <RUNTIME-FOREIGN-COMPILED-LAMBDA>

define class <runtime-foreign-compiled-lambda> (<runtime-compiled-lambda>)
end class;

/*
///// GET-LAMBDA-SOURCE-INFORMATION-IF-NECESSARY
//    Using the access path, gets as much source code information as
//    possible about a compiled lambda.

define method get-lambda-source-information-if-necessary
    (lambda :: <runtime-compiled-lambda>) => ()
  unless (lambda.lambda-runtime-filename | lambda.lambda-no-source-information)
    let application = lambda.lambda-debug-target;
    let path = application.debug-target-access-path;
    let sym = lambda.lambda-entry-point-symbol;
    let (fname, baseline, baseaddr, line-positions, code-offsets)
      = function-recorded-source-locations(path, sym);
    if (fname)
      lambda.lambda-runtime-filename := fname;
      lambda.lambda-runtime-recorded-offsets := 
        map(pair, line-positions, code-offsets);
      lambda.lambda-runtime-linenumber := baseline;
    else
      lambda.lambda-no-source-information := #t
    end if;
  end unless
end method;

// Note.
// This method also fills in the interactive code locations. At the moment,
// for dylan code, we assume all recorded locations to be interactive.

define method get-lambda-source-information-if-necessary
    (lambda :: <runtime-dylan-compiled-lambda>) => ()
  next-method();
  lambda.lambda-runtime-interactive-offsets :=
                   lambda.lambda-runtime-recorded-offsets;
end method;


///// COMPILED-LAMBDA-SYMBOLIC-NAME
//    Returns the mangled runtime name for the function.

define method compiled-lambda-symbolic-name
    (cl :: <runtime-compiled-lambda>) => (sn :: <symbolic-name>)
  cl.lambda-entry-point-symbol.remote-symbol-name
end method;


///// SYMBOLIC-NAME-COMPILED-LAMBDA
//    Given a symbolic name in a <debug-target> context, attempts to
//    create a <runtime-compiled-lambda>.

define method symbolic-name-compiled-lambda
    (context :: <debug-target>, name :: <symbolic-name>)
       => (maybe-lambda :: false-or(<compiled-lambda>))
  let application = context;  // TODO: Placeholder. Since 'context' may not
                              // be as convenient as <debug-target>.
  let component = symbolic-name-component-name(context, name);
  let table = application.debug-target-symbol-table;
  let library =
     if (component)
       find-library-called(application, component)
     else
       #f
     end if;
  let sym = symbol-table-find-symbol(table, name, library: library);
  if (sym)
    let language = sym.remote-symbol-language;
    if (language == $symbol-language-Dylan)
      make(<runtime-dylan-compiled-lambda>, 
           target: application, entry-point-symbol: sym);
    else
      make(<runtime-foreign-compiled-lambda>, 
           target: application, entry-point-symbol: sym);
    end if
  else
    #f
  end if
end method;


///// COMPILED-LAMBDA-SOURCE-LOCATION
//    Given a compiled lambda and a code offset, returns a <source-location>
//    and a flag indicating whether the source location is exact.

define method compiled-lambda-source-location
    (lambda :: <runtime-compiled-lambda>, offset :: <integer>,
     #key exact? = #t, line-only? = #t, interactive-only? = #t)
        => (source-loc :: false-or(<any-source-location>),
            is-exact? :: <boolean>)
  values(#f, #f);
end method;


///// COMPILED-LAMBDA-MAPPED-SOURCE-LOCATIONS
//    Returns an ordered sequence of the recorded <source-location>s for
//    a compiled lambda. This sequence can be filtered according to the
//    keyword arguments.

define method compiled-lambda-mapped-source-locations
    (lambda :: <runtime-compiled-lambda>,
     #key line-only? = #t, interactive-only? = #t)
        => (sequence-of-locations :: <sequence>)
  #[]
end method;


///// COMPILED-LAMBDA-MAPPED-CODE-OFFSETS
//    Returns an ordered sequence of the recorded code offsets for a
//    compiled lambda. This sequence can be filtered according to the
//    keyword arguments.

define method compiled-lambda-mapped-code-offsets
    (lambda :: <runtime-compiled-lambda>,
     #key line-only? = #t, interactive-only? = #t)
        => (sequence-of-offsets :: <sequence>)
  #[]
end method;
*/

