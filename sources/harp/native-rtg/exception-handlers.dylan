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


/// Floating-point Classification


define runtime-literal unsupported-float = "unsupported-float", data: "unsupported";
define runtime-literal nan-float = "nan-float", data: "nan";
define runtime-literal normal-float = "normal-float", data: "normal";
define runtime-literal infinity-float = "infinity-float", data: "infinity";
define runtime-literal zero-float = "zero-float", data: "zero";
define runtime-literal empty-float = "empty-float", data: "empty";
define runtime-literal denormal-float = "denormal-float", data: "denormal";

define raw runtime-literal floating-point-classes 
  =  "floating-point-classes",
     data: vector( #x0, unsupported-float, #x100, nan-float,
	           #x400, normal-float,    #x500, infinity-float,
	           #x4000, zero-float,     #x4100, empty-float,
	           #x4400, denormal-float );


define runtime-primitive float-class

/*
  On entry:
    float  - a raw single-float
  On exit:
    Dylan string describing class of float
*/

  float-arg0 float;
  result result;
  nreg count, code1, code2;
  tag find, found;

  ins--classify-float(be, code1, float);
  ins--move(be, count, 0);

  ins--tag(be, find);
  ins--ld(be, code2, floating-point-classes, count);
  ins--beq(be, found, code1, code2);
  ins--add(be, count, count, 8);
  ins--bra(be, find);

  ins--tag(be, found);
  ins--add(be, count, count, 4);
  ins--ld(be, result, floating-point-classes, count);
  ins--rts-and-drop(be, 0);

end runtime-primitive;
