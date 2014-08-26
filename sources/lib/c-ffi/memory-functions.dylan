Module:    c-ffi-implementation
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline-only function convert-bytes-to-words (bytes :: <integer>)
 => (words :: <integer>)
  let word-size-in-bytes = raw-as-integer(primitive-word-size());
  truncate/(bytes, word-size-in-bytes);
end function;

define inline function copy-into! (destination :: <C-pointer>,
    source :: <C-pointer>, size-in-bytes :: <integer>) => ()
  let size = convert-bytes-to-words(size-in-bytes);
  let raw-destination = primitive-unwrap-c-pointer(destination);
  let raw-source = primitive-unwrap-c-pointer(source);
  let raw-zero = primitive-unwrap-machine-word(as(<machine-word>, 0));
  let raw-size = primitive-unwrap-machine-word(as(<machine-word>, size));
  primitive-replace!(raw-destination, raw-zero, raw-zero,
                     raw-source, raw-zero, raw-zero,
                     raw-size);
end function;

define inline function copy-bytes! (destination :: <C-pointer>,
    source :: <C-pointer>, size :: <integer>) => ()
  let raw-destination = primitive-unwrap-c-pointer(destination);
  let raw-source = primitive-unwrap-c-pointer(source);
  let raw-zero = primitive-unwrap-machine-word(as(<machine-word>, 0));
  let raw-size = primitive-unwrap-machine-word(as(<machine-word>, size));
  primitive-replace-bytes!(raw-destination, raw-zero, raw-zero,
                           raw-source, raw-zero, raw-zero,
                           raw-size);
end function;

define inline function equal-memory? (pointer-1 :: <C-pointer>,
    pointer-2 :: <C-pointer>, size-in-bytes :: <integer>)
 => (equal? :: <boolean>)
  let size = convert-bytes-to-words(size-in-bytes);
  let raw-pointer-1 = primitive-unwrap-c-pointer(pointer-1);
  let raw-pointer-2 = primitive-unwrap-c-pointer(pointer-2);
  let raw-zero = primitive-unwrap-machine-word(as(<machine-word>, 0));
  let raw-size = primitive-unwrap-machine-word(as(<machine-word>, size));
  primitive-compare-words(raw-pointer-1, raw-zero,
                          raw-pointer-2, raw-zero,
                          raw-size);
end function;

define inline function clear-memory! (pointer :: <C-pointer>,
    size-in-bytes :: <integer>) => ()
  let size = convert-bytes-to-words(size-in-bytes);
  let raw-pointer = primitive-unwrap-c-pointer(pointer);
  let raw-zero = primitive-unwrap-machine-word(as(<machine-word>, 0));
  let raw-size = primitive-unwrap-machine-word(as(<machine-word>, size));
  primitive-fill!(raw-pointer, raw-zero, raw-zero, raw-size,
                  primitive-cast-raw-as-pointer(raw-zero));
end function;
