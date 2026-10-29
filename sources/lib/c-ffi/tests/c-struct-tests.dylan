Module:    c-ffi-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define c-struct <maximal>
  slot max-1 :: <minimal>, address-getter: max-1*;
  slot max-2 :: <C-unsigned-char>, address-getter: max-2*;
  array slot max-3 :: <minimal>, length: 7, address-getter: max-3*;
  // bitfield slot max-4 :: <c-unsigned-int>, width: 4;
  // bitfield slot max-5 :: <c-unsigned-int>, width: 12;
  slot max-6 :: <C-unsigned-char>, address-getter: max-6*, setter: #f;
  slot max-7 :: <C-unsigned-char*>, address-getter: max-7*;
  pointer-type-name: <maximal*>;
end;
ignorable(max-1, max-1-setter);
ignorable(max-3, max-3-setter, max-3*);
ignorable(max-6, max-6*);
ignorable(max-7, max-7-setter, max-7*);

define c-struct <minimal>
  slot slot-1 :: <C-unsigned-long>;
  slot slot-2 :: <C-unsigned-long*>;
  pointer-type-name: <minimal*>, c-name: "subliminal";
end;

define c-pointer-type <Pminimal> => <minimal>;

/// ask C what it thinks the size of the minimal struct is.
define c-function struct-minimal-size
  result r :: <C-size-t>;
  c-name: "struct_minimal_size";
end;

/// ask C what it thinks the size of the maximal struct is.
define c-function struct-maximal-size
  result r :: <C-size-t>;
  c-name: "struct_maximal_size";
end;

/// try some C-struct in different ways
/// Could always use some more tests here.
define test c-struct-test ()
  check-equal("<minimal> struct size",
              struct-minimal-size(), size-of(<minimal>));
  let minimal-struct = make(<minimal*>);
  check-equal("set simple struct-slot", (minimal-struct.slot-1 := 777), 777);
  check-equal("simple struct slot value", minimal-struct.slot-1, 777);
  check-equal( "set struct simple address valued slot",
              minimal-struct.slot-2 := $foos-address,
              $foos-address);
  check-equal("struct simple address valued slot value",
              pointer-address($foos-address),
              pointer-address(minimal-struct.slot-2));
  check-equal("struct set pointer value address valued slot",
          pointer-value(minimal-struct.slot-2) := 12345,
          12345);
  check-equal("struct pointer value check alias",
              foo-value(), pointer-value(minimal-struct.slot-2));
  check-equal("struct pointer value check value",
              pointer-value(minimal-struct.slot-2), 12345);
  check-equal("define c-pointer-type does correct aliasing",
              <minimal*>, <Pminimal>);

  // do some tests here that deal with maximal
  check-equal("<maximal> struct size",
              struct-maximal-size(), size-of(<maximal>));
  let maximal-struct = make(<maximal*>);
  check-equal("set maximal struct-slot", (maximal-struct.max-2 := 77), 77);
  check-equal("maximal struct slot value", maximal-struct.max-2, 77);
  let max-2-addr = maximal-struct.max-2*;
  check-equal("struct field pointer indirection matches set value",
              77, C-unsigned-char-at(max-2-addr));
  let maximal-struct-minimal = maximal-struct.max-1*;
  maximal-struct-minimal.slot-1 := 20261029;
  check-equal("embedded simple struct slot value",
              20261029, maximal-struct-minimal.slot-1);

  // !@#$ Need more tests to see if accessors get the same thing as C code
  //      for corresponding C structs
end;

//
// -------------
// tests for bug 134

define C-struct <LARGE-INTEGER>
  slot LowPart-value  :: <C-unsigned-long>;
  slot HighPart-value :: <C-signed-long>;
end;
ignorable(LowPart-value, LowPart-value-setter);
ignorable(HighPart-value, HighPart-value-setter);

define C-union <variant-union>
  slot lVal-value  :: <C-long>;
  slot bVal-value  :: <C-unsigned-char>;
  slot iVal-value  :: <C-short>;
  slot cyVal-value :: <LARGE-INTEGER>;
  slot ptr-value :: <C-pointer>;
end;
ignorable(lVal-value, lVal-value-setter);
ignorable(bVal-value, bVal-value-setter);
ignorable(iVal-value, iVal-value-setter);
ignorable(cyVal-value, cyVal-value-setter);
ignorable(ptr-value, ptr-value-setter);

define test bug-134 (expected-to-fail-reason:
                       "https://github.com/dylan-lang/opendylan/issues/1845")
  check-true("bug 134 union has full size",
             size-of(<variant-union>) >= size-of(<LARGE-INTEGER>));
  check-true("bug 134 LARGE-INTEGER has full size",
             size-of(<LARGE-INTEGER>)
               >= size-of(<C-unsigned-long>) + size-of(<C-signed-long>));
end;

//
// -------------
// tests for bug 213

define c-struct <HENV-213>
  slot whatever :: <C-signed-int>;
  pointer-type-name: <HENV-213*>;
end C-struct;
ignorable(whatever, whatever-setter);

define test bug-213 ()
  check-instance?("bug 213 make returns",
                  <HENV-213*>, make(<HENV-213*>));
end;

//
// -------------
// tests for bug 414

define C-struct <OLEMENUGROUPWIDTHS>
  array slot width-array :: <C-long>, length: 6,
    address-getter: width-value,
    getter: #f, setter: #f;
  pointer-type-name: <LPOLEMENUGROUPWIDTHS>;
  c-name: "struct tagOleMenuGroupWidths";
end C-struct <OLEMENUGROUPWIDTHS>;

define test bug-414 ()
  let menugroupwidths = make(<LPOLEMENUGROUPWIDTHS>);
  // should not generate any warnings when compiling,
  // or errors when running
  check-true("bug 414 no warnings or errors", menugroupwidths.width-value);
  destroy(menugroupwidths);
end;
