module: dfmc-back-end
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define symbolic-class <&raw-type-descriptor> (<object>) using dylan-value
  constant slot %raw-type-size, init-keyword: size:;
  constant slot %raw-type-alignment, init-keyword: alignment:;
  constant slot raw-type-signed? :: <boolean>, init-keyword: signed?:;
  // accessors for raw value given address, base, and offset
  symbolic slot raw-type-getter,
    required-init-keyword: getter-name:;
  symbolic slot raw-type-setter,
    required-init-keyword: setter-name:;
  // boxed representation class in dylan
  symbolic slot raw-type-boxed-class,
    required-init-keyword: boxed-class-name:;
  // box/unbox to/from boxed class
  symbolic slot raw-type-boxer,
    required-init-keyword: boxer-name:;
  symbolic slot raw-type-unboxer,
    required-init-keyword: unboxer-name:;
  // c only
  constant slot raw-type-c-name :: <byte-string>, init-keyword: c-name:;
end symbolic-class <&raw-type-descriptor>;

define method compute-size (size :: type-union(<integer>, singleton(#"w")))
  if (size == #"w")
    word-size()
  else
    size
  end if
end method;

define method raw-type-size
    (type :: <&raw-type-descriptor>) => (res :: <integer>)
  compute-size(type.%raw-type-size);
end method;

define method raw-type-alignment
    (type :: <&raw-type-descriptor>) => (res :: <integer>)
  compute-size(type.%raw-type-alignment);
end method;

define macro raw-type-descriptor-definer
  { define raw-type-descriptor "raw-" ## ?:name
      (?back-end:name :: ?back-end-type:expression,
       #key ?c-name:expression, ?size, ?alignment, ?signed:expression,
            ?boxer, ?unboxer) }
    => { begin
           define constant "raw-" ## ?name ## "-descriptor"
              = make(<&raw-type-descriptor>,
                     size:
                       ?size,
                     alignment:
                       ?alignment,
                     signed?:
                       ?signed,
                     getter-name:
                       coagulate-name("%raw-" ## ?name ## "-at"),
                     setter-name:
                       coagulate-name("%raw-" ## ?name ## "-at-setter"),
                     c-name:
                       ?c-name,
                     // TODO: ONLY VALID FOR CERTAIN DYLAN RAW-TYPES
                     boxed-class-name:
                       coagulate-name("<" ## ?name ## ">"),
                     boxer-name:
                       ?boxer,
                     unboxer-name:
                       ?unboxer);
           define sideways method "raw-" ## ?name (?back-end :: ?back-end-type)
            => (rtd :: <&raw-type-descriptor>)
             "raw-" ## ?name ## "-descriptor"
           end;
         end }
alignment:
  { w }             => { #"w" }
  { ?x:expression } => { ?x }
size:
  { w }             => { #"w" }
  { ?x:expression } => { ?x }
// HACK: SHOULD DO THIS SOME OTHER WAY
boxer:
  { raw-machine-word }
    => { #"primitive-wrap-machine-word" }
  { raw-byte }
    => { #"raw-as-integer" }
  { raw-double-byte }
    => { #"raw-as-integer" }
  { "raw-" ## ?:name }
    => { coagulate-name("primitive-raw-as-" ## ?name) }
unboxer:
  { raw-machine-word }
    => { #"primitive-unwrap-machine-word" }
  { raw-byte }
    => { #"integer-as-raw" }
  { raw-double-byte }
    => { #"integer-as-raw" }
  { "raw-" ## ?:name }
    => { coagulate-name("primitive-" ## ?name ## "-as-raw") }
end macro raw-type-descriptor-definer;

define macro raw-type-descriptors-aux-definer
  { define raw-type-descriptors-aux
        (?back-end:name :: ?back-end-type:expression)
      ?:name, ?c-name:expression, ?size:*, ?alignment:*, ?signed:*; ?stuff:*
    end }
    => { begin
           define raw-type-descriptor ?name
             (?back-end :: ?back-end-type,
              c-name: ?c-name, size: ?size, alignment: ?alignment,
              signed: ?signed, boxer: ?name, unboxer: ?name);
           define raw-type-descriptors-aux (?back-end :: ?back-end-type)
             ?stuff
           end
         end }
  { define raw-type-descriptors-aux
      (?back-end:name :: ?back-end-type:expression) end }
    => { }
end macro raw-type-descriptors-aux-definer;

define macro raw-type-descriptors-definer
  { define raw-type-descriptors (?back-end:name :: ?back-end-type:expression)
      ?raw-type-descriptor-definitions:*
    end }
    => { define raw-type-descriptors-aux (?back-end :: ?back-end-type)
           ?raw-type-descriptor-definitions
         end }
end macro raw-type-descriptors-definer;

define raw-type-descriptors (back-end :: <back-end>)
  // concrete raw types

  /*
  raw-signed-8-bit-integer,    "INT8",   1, 1;
  raw-unsigned-8-bit-integer,  "UINT8",  1, 1;
  raw-signed-16-bit-integer,   "INT16",  2, 2;
  raw-unsigned-16-bit-integer, "UINT16", 2, 2;
  raw-signed-32-bit-integer,   "INT32",  4, 4;
  raw-unsigned-32-bit-integer, "UINT32", 4, 4;
  raw-signed-64-bit-integer,   "INT64",  8, 8;
  raw-unsigned-64-bit-integer, "UINT64", 8, 8;
  raw-ieee-single-float,       "SFLT",   4, 4;
  raw-ieee-double-float,       "DFLT",   8, 8;
  raw-ieee-extended-float,     "EFLT",  16,16;
  */

  // c raw types

  raw-c-signed-char,        "signed char",        1, 1, #t;
  raw-c-unsigned-char,      "unsigned char",      1, 1, #f;
  // raw-c-char             "char",               1, 1, #t;
  raw-c-signed-short,       "short",              2, 2, #t;
  raw-c-unsigned-short,     "unsigned short",     2, 2, #f;
  raw-c-signed-int,         "int",                4, 4, #t;
  raw-c-unsigned-int,       "unsigned int",       4, 4, #f;
  raw-c-signed-long,        "long",               w, w, #t;
  raw-c-unsigned-long,      "unsigned long",      w, w, #f;
  raw-c-signed-long-long,   "long_long",          8, 8, #t;
  raw-c-unsigned-long-long, "unsigned_long_long", 8, 8, #t;
  raw-c-float,              "float",              4, 4, #t;
  raw-c-double,             "double",             8, 8, #t;
  raw-c-long-double,        "long_double",       16,16, #t;
  raw-c-void,               "void",               0, 0, #f;
  raw-c-pointer,            "void*",              w, w, #f;
  raw-c-size-t,             "size_t",             w, w, #f;
  raw-c-ssize-t,            "ssize_t",            w, w, #t;

  // dylan raw types

  raw-boolean,                       "DBOOL",   1, 1, #f;
  raw-byte-character,                "DBCHR",   1, 1, #f;
  raw-unicode-character,             "DWORD",   w, w, #f;
  raw-byte,                          "DBYTE",   1, 1, #f;
  raw-double-byte,                   "DDBYTE",  2, 2, #f;
  raw-integer,                       "DSINT",   w, w, #t;
  raw-machine-word,                  "DWORD",   w, w, #f;
  raw-single-float,                  "DSFLT",   4, 4, #t;
  raw-double-float,                  "DDFLT",   8, 8, #t;
  raw-extended-float,                "DEFLT",  16,16, #t;
  raw-pointer,                       "D",       w, w, #f;
  raw-address,                       "DADDR",   w, w, #f;
  raw-byte-string,                   "DBSTR",   w, w, #f;

end raw-type-descriptors;

