module: disasm-test
author: Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Test harness for disassembler for i386, i486 and i586 code

define method as-byte-vector(result-class == <byte-vector>, arg :: <vector>)
  => (bytes :: <byte-vector>)
  let res = make(<byte-vector>, size: arg.size);
  map-into(res, identity, arg);
  res
end method as-byte-vector;

define constant code-vector = as-byte-vector(<byte-vector>, #[#x90, #x04, #x19, #x05, #x19, #x1a, #x1b, #x1c, #x66, #x05, #x19, #x1a, #x80, #xc3, #x1a, #x80, #x03, #x1a, #x80, #x05, #x19, #x1a, #x1b, #x1c, #xff, #x00, #x13, #x01, #x1a, #x66, #x01, #x1a, #x02, #x13, #x03, #x1a, #x66, #x03, #x1a,
#x0c, #x19, #x0d, #x19, #x1a, #x1b, #x1c, #x66, #x0d, #x19, #x1a, #x80, #xcb, #x1a, #x80, #x0b, #x1a, #x80, #x0d, #x19, #x1a, #x1b, #x1c, #xff, #x08, #x13, #x09, #x1a, #x66, #x09, #x1a, #x0a, #x13, #x0b, #x1a, #x66, #x0b, #x1a,
#x34, #x90, #x31, #x53, #x8a, #x33, #x93, #x2a, #x39, #x48, #x57,
#x2b, #x1c, #x0f, #x29, #x3c, #x0d, #x2a, #x39, #x48, #x57,
#x1b, #x34, #x40, #x1b, #x34, #xb9, #x1b, #x34, #xf2,
#x1b, #x34, #x45, #x36, #x1b, #x34, #xb9,
#x1b, #x4c, #x00, #x80,
#x77, #x00,
#x77, #x08,
#x77, #xfc,
#x0f, #x87, #x00, #x00, #x00, #x80,
#x90, #x90, #xd6, #x90, #x90, #xc2, #x00, #x00,
#x0f, #xb6, #x00,
#xf0, #x0f, #xc1, #x5f, #x08,
#xd9, #x04, #xbb,
#x0f, #xba, #xe2, #x01,
#xd9, #x46, #x04,
#xd9, #x5d, #xfc,
#xd9, #xc0,
#x0f, #xbe, #x00,
#x0f, #xbf, #x00,
#x0f, #xb6, #x00,
#x0f, #xb7, #x00
]);

/*define constant code-vector = as-byte-vector(<byte-vector>, #[#xd6, #x90]);*/

define method test-decode-opcodes () => (res :: <list>)
  let (res, new-index) = decode-opcodes(code-vector, 0, code-vector.size);
  format-out("Opcode decoding fails at index %d\n", new-index);
  res
end method test-decode-opcodes;

define method table-lookup(addr :: <object>) => (obj :: <object>)
  #f
end method table-lookup;

define method test-opcodes-to-string(x :: <list>, addr :: <integer>) => ()
  for (y in opcodes-to-string(x, addr, table-lookup))
    format-out("%s\n", y)
  end for;
end method test-opcodes-to-string;

test-opcodes-to-string(begin let (res, new-index) = test-decode-opcodes(); res end, 0);
