Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define byte-vector constant-test <byte> ()
  //---*** Fill this in...
end constant-test <byte>;

define sideways method make-test-instance
    (class == <byte-vector>) => (object)
  make(<byte-vector>, size: 1, fill: 0);
end method make-test-instance;

define byte-vector class-test <byte-vector> ()
  check-condition("make(<byte-vector>, size: 0, fill: 256) signals <type-error>",
                  <type-error>, make(<byte-vector>, size: 0, fill: 256));
  check-condition("make(<byte-vector>, size: 1, fill: 256) signals <type-error>",
                  <type-error>, make(<byte-vector>, size: 1, fill: 256));

  test-byte-vector-size-and-fill(0, 0);
  test-byte-vector-size-and-fill(0, 1);
  test-byte-vector-size-and-fill(0, 255);
  test-byte-vector-size-and-fill(1, 0);
  test-byte-vector-size-and-fill(1, 1);
  test-byte-vector-size-and-fill(1, 255);
  test-byte-vector-size-and-fill(2, 0);
  test-byte-vector-size-and-fill(2, 1);
  test-byte-vector-size-and-fill(2, 255);
  test-byte-vector-size-and-fill(3, 0);
  test-byte-vector-size-and-fill(3, 1);
  test-byte-vector-size-and-fill(3, 255);
end class-test <byte-vector>;

define method test-byte-vector-size-and-fill
    (vector-size :: <integer>, fill :: <byte>)
 => ()
    let v = make(<byte-vector>, size: vector-size, fill: fill);
    let name = format-to-string("vector-size-%d-fill-%d", vector-size, fill);
    check-equal(format-to-string("%s empty?", name), v.empty?, vector-size == 0);
    check-equal(format-to-string("%s.size = %d", name, vector-size), v.size, vector-size);
    check-true(format-to-string("%s fill = %d", name, fill),
               every?(method (b) b == fill end, v));
end method test-byte-vector-size-and-fill;

define byte-vector function-test copy-bytes ()
  // ---*** Fill this in.
end function-test copy-bytes;

define byte-vector function-test byte-vector-fill ()
  // ---*** Fill this in.
end function-test byte-vector-fill;

define byte-vector function-test byte-vector-ref ()
  // ---*** Fill this in.
end function-test byte-vector-ref;

define byte-vector function-test byte-vector-ref-setter ()
  // ---*** Fill this in.
end function-test byte-vector-ref-setter;
