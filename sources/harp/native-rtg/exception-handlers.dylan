module:    harp-native-rtg
Synopsis:  Exception handling
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Exceptions get handled at the SEH level by the code in collector.c.
/// Arithmetic exceptions transfer control to the C call-ins defined
/// here. Their job is then to signal an appropriate error.

define mw-iep runtime-external dylan-integer-overflow-error
   = "machine-word-overflow";

define extensions-iep runtime-external dylan-integer-divide-by-0-error
   = "integer-divide-by-0";

define extensions-iep runtime-external dylan-float-divide-by-0-error
   = "float-divide-by-0";

define extensions-iep runtime-external dylan-float-invalid-error
   = "float-invalid";

define extensions-iep runtime-external dylan-float-overflow-error
   = "float-overflow";

define extensions-iep runtime-external dylan-float-underflow-error
   = "float-underflow";


define c-runtime-primitive dylan-integer-overflow-handler
  // On entry: no args
  //
  //   Calls a Dylan error reporting function
  // On exit:
  //  Should never exit directly (only via a NLX)

  op--call-iep(be, dylan-integer-overflow-error);

  // Control should never get here - but code the tail anyway
  ins--rts(be);
end c-runtime-primitive;


define c-runtime-primitive dylan-integer-divide-0-handler
  // On entry: no args
  //
  //   Calls a Dylan error reporting function
  // On exit:
  //  Should never exit directly (only via a NLX)

  op--call-iep(be, dylan-integer-divide-by-0-error);

  // Control should never get here - but code the tail anyway
  ins--rts(be);
end c-runtime-primitive;


define c-runtime-primitive dylan-float-divide-0-handler
  // On entry: no args
  //
  //   Calls a Dylan error reporting function
  // On exit:
  //  Should never exit directly (only via a NLX)

  ins--clear-float-exceptions(be);
  op--call-iep(be, dylan-float-divide-by-0-error);

  // Control should never get here - but code the tail anyway
  ins--rts(be);
end c-runtime-primitive;


define c-runtime-primitive dylan-float-invalid-handler
  // On entry: no args
  //
  //   Calls a Dylan error reporting function
  // On exit:
  //  Should never exit directly (only via a NLX)

  ins--clear-float-exceptions(be);
  op--call-iep(be, dylan-float-invalid-error);

  // Control should never get here - but code the tail anyway
  ins--rts(be);
end c-runtime-primitive;


define c-runtime-primitive dylan-float-overflow-handler
  // On entry: no args
  //
  //   Calls a Dylan error reporting function
  // On exit:
  //  Should never exit directly (only via a NLX)

  ins--clear-float-exceptions(be);
  op--call-iep(be, dylan-float-overflow-error);

  // Control should never get here - but code the tail anyway
  ins--rts(be);
end c-runtime-primitive;


define c-runtime-primitive dylan-float-underflow-handler
  // On entry: no args
  //
  //   Calls a Dylan error reporting function
  // On exit:
  //  Should never exit directly (only via a NLX)

  ins--clear-float-exceptions(be);
  op--call-iep(be, dylan-float-underflow-error);

  // Control should never get here - but code the tail anyway
  ins--rts(be);
end c-runtime-primitive;
