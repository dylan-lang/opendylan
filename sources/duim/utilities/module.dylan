Module:       Dylan-User
Synopsis:     DUIM utilities
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-imports
  // Export useful functionality that DUIM needs from other libraries
  use common-extensions,
    exclude: { position },
    export: all;
  use dylan-extensions,
    import: { <byte-character>,
              <keyboard-interrupt>,
	      \without-bounds-checks,
	      element-no-bounds-check,
	      element-no-bounds-check-setter,
	      element-range-error },
    export: all;
  use simple-debugging, export: all;
  use simple-io, export: all;
  use threads, export: all;
  use transcendentals, export: all;
  use table-extensions, exclude: { table }, export: all;
  use plists, export: all;
end module duim-imports;

define module duim-utilities
  use dylan;
  use duim-imports;

  // Debug support
  export *debug-duim-function*,
         duim-debug-message;

  //--- Per-thread (i.e., dynamically bindable) slots
  export \thread-slot-definer;

  //--- A bit more support for threads
  export destroy-thread;

  // Floating point constants
  export $pi, $2pi, $pi/2;

  // "Declarations"
  export dynamic-extent;

  // Protocol definer
  export \protocol-class-definer,
         \protocol-predicate-definer,
	 \protocol-definer;

  // Useful stuff
  export \inc!, \dec!,
         \min!, \max!,
         \push!, \pop!,
	 \swap!,
         \destructuring-let,
         \simple-restart-loop,
	 \with-abort-restart,
         \with-abort-restart-loop,
         \with-restart,
         \with-simple-restart,
         warn;

  // Characters and strings
  create char-equal?,   char-less?,   char-greater?,
         string-equal?, string-less?, string-greater?,
         alpha-char?, digit-char?, alphanumeric-char?,
         upper-case?, lower-case?,
         graphic-char?, standard-char?, whitespace-char?,
         string-capitalize, string-capitalize!,
	 string-pluralize, string-a-or-an;

  // Stack allocation
  export \with-stack-list, evacuate-list,
	 \with-stack-object, evacuate-object,
         \with-stack-vector, evacuate-vector;

  export gethash, gethash-setter,
         remhash,
	 <string-or-object-table>,
         substitute, substitute!,
         insert-at!, remove-at!,
	 find-pair,
	 position, count,
         make-array-from-contents, fill-array!,
	 range-check;

  //--- Need a way to cheaply get the current time in microseconds
  export get-internal-real-time;

end module duim-utilities;
