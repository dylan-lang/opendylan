module:    main-harp
Synopsis:  The HARP entry point
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable check-consistency-by-default = #f;

define variable print-harp-debug-by-default = #f;

define variable export-publics-by-default = #t;

define variable export-call-ins-by-default = #f;

define method invoke-harp
    (backend :: <harp-back-end>, continuation :: <function>, function-name,
     #key outputter = #f,
          section = #"code",   // Only used if we supply an outputter
          defasm = #f,         // true if we're not obeying normal stack conventions
          call-in = #f,        // true if we're a foreign callable function
          static = #f,         // true if we're a static (file local) function
          public = ~ static,   // true if we're a public function
          export = unsupplied(), 
                               // true if lambda is exported
          source-locator = #f, // #f, or a source location for the lambda start
          harp-debug = unsupplied(),   
                               // if true, then record debug output in compiled lambda
          check = check-consistency-by-default)
   => (compiled-lambda :: <compiled-lambda>)
  let harp-debug = if (supplied?(harp-debug)) harp-debug else print-harp-debug-by-default end if;
  let export = if (supplied?(export)) export
	       else
		 (~ call-in & public & export-publics-by-default)
                   | (call-in & export-call-ins-by-default)
	       end if;
  let old-vars = backend.variables;
  let code-gen-complete? = #f;
  block ()
    let new-vars = make-harp-variables(backend, function-name, 
                                       defasm: defasm, call-in: call-in,
				       prototype: old-vars);
    backend.variables := new-vars;
    pre-cg-lambda(backend);
    continuation(backend);
    /*
    format-out("\n### Generating code for %= with %= virtuals\n",
	       function-name,
	       backend.variables.vreg-state.vr-vect.size);
    */
    let source-locator =
      if (source-locator & dummy-harp-source-locator?(source-locator))
	#f
      else
	source-locator
      end if;
    if (check) harp-consistency-check(backend) end;
    let compiled-lambda =
      post-cg-lambda(backend, outputter, harp-debug, public, export, source-locator, section);
    copy-shared-variables(old-vars, new-vars);
    code-gen-complete? := #t;
    compiled-lambda;
  cleanup
    backend.variables := old-vars;
    clear-shared-variables(backend, all?: ~code-gen-complete?);
  end block;
end method;

define method invoke-harp-asm
    (backend :: <harp-back-end>, continuation :: <function>, function-name,
     #rest all-keys)
   => (compiled-lambda :: <compiled-lambda>)
  apply(invoke-harp, backend, continuation, function-name, defasm: #t, all-keys);
end method;
