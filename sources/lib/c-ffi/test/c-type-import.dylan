Module:    c-ffi-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define c-function import-unsigned-long
  result r :: <C-unsigned-long>;
  c-name: "import_unsigned_long";
end;

define c-function import-signed-long
  result r :: <C-signed-long>;
  c-name: "import_signed_long";
end;

define c-function import-unsigned-int
  result r :: <C-unsigned-int>;
  c-name: "import_unsigned_int";
end;

define c-function import-signed-int
  result r :: <C-signed-int>;
  c-name: "import_signed_int";
end;

define c-function import-unsigned-short
  result r :: <C-unsigned-short>;
  c-name: "import_unsigned_short";
end;

define c-function import-signed-short
  result r :: <C-signed-short>;
  c-name: "import_signed_short";
end;

define c-function import-unsigned-char
  result r :: <C-unsigned-char>;
  c-name: "import_unsigned_char";
end;

define c-function import-signed-char
  result r :: <C-signed-char>;
  c-name: "import_signed_char";
end;

define c-function import-size-t
  result r :: <C-size-t>;
  c-name: "import_size_t";
end;

define c-function import-ssize-t
  result r :: <C-ssize-t>;
  c-name: "import_ssize_t";
end;

define test c-type-import ()
  assert-equal(size-of(<C-unsigned-long>) * 8,
               integer-length(import-unsigned-long()));
  assert-equal(-1, import-signed-long());
  assert-equal(size-of(<C-unsigned-int>) * 8,
               integer-length(import-unsigned-int()));
  assert-equal(-1, import-signed-int());
  assert-equal(size-of(<C-unsigned-short>) * 8,
               integer-length(import-unsigned-short()));
  assert-equal(-1, import-signed-short());
  assert-equal(size-of(<C-unsigned-char>) * 8,
               integer-length(import-unsigned-char()));
  assert-equal(-1, import-signed-char());
  assert-equal(size-of(<C-size-t>) * 8,
               integer-length(import-size-t()));
  assert-equal(-1, import-ssize-t());
end test;
