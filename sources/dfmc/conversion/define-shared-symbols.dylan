Module:   dfmc-conversion
Synopsis: The shared-symbols definition processor.
Author:   Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Shared Symbols modeling.

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <shared-symbols-definition>) => ()
  unless (form-dynamic?(form))
    for (shared-symbol in form-shared-symbols(form))
      model-definition(shared-symbol) := form;
    end for;
    define-model-object // HACK: DUMMY INITIAL VALUE
      (form-variable-name(form), make-compile-time-literal(0))
  end unless;
end method;

// eof
