Module:       dylan-user
Synopsis:     A test suite for compiler warnings
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module protocols
  create undefined-binding;
end module protocols;

define module warnings-test-suite
  use common-dylan,
    exclude: { reduce };
  use protocols;

  // Buggy imports
  use unimported-module;
  use dylan,
    import: { unimported-binding },
    rename: { another-unimported-binding => a-binding };
  use common-extensions,
    import: { \false-or },
    export: { \one-of };

  // Buggy renamings
  use dylan,
    import: { reduce, reduce1 },
    rename: { reduce  => name-clash,
	      reduce1 => name-clash };
  use dylan,
    import: { reduce, reduce1 },
    rename: { reduce1 => reduce };

  // Buggy excludes
  use common-extensions,
    exclude: { no-such-import };
  use common-extensions,
    import: { \false-or },
    exclude: { \false-or };

  // Buggy exports
  export no-method-exported-generic;
  export undefined-function;
  export <object>;

  // We have to export macros to suppress defined-but-not used because
  // ignore doesn't currently handle them.
  export statement-macro-name-mismatch,
         function-macro-name-mismatch,
         local-declaration-macro-name-mismatch,
         body-macro-name-mismatch-definer,
         list-macro-name-mismatch-definer;

  export non-definer-definition-definerz;

  export inconsistent-non-definer-rules, 
         inconsistent-definer-rules-definer;

  export unconstrained-name-macro,
         duplicate-names-macro,
         element-as-sequence-macro,
         sequence-as-element-macro,
         splicer-macro;
end module warnings-test-suite;

define module unreferenced-module
  use common-dylan;
end module unreferenced-module;
