Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-type> (<object>)
  // Nothing yet
end class;

define constant <llvm-primitive-type-kind>
  = one-of(#"VOID", #"FLOAT", #"DOUBLE", #"LABEL", #"OPAQUE",
           #"X86_FP80", #"FP128", #"PPC_FP128", #"METADATA");

define class <llvm-primitive-type> (<llvm-type>)
  constant slot llvm-primitive-type-kind :: <llvm-primitive-type-kind>,
    required-init-keyword: kind:;
end class;

define class <llvm-integer-type> (<llvm-type>)
  constant slot llvm-integer-type-width :: <integer>,
    required-init-keyword: width:;
end class;

define class <llvm-pointer-type> (<llvm-type>)
  constant slot llvm-pointer-type-pointee :: <llvm-type>,
    required-init-keyword: pointee:;
  constant slot llvm-pointer-type-address-space :: <integer>,
    required-init-keyword: address-space:;
end class;

define class <llvm-function-type> (<llvm-type>)
  constant slot llvm-function-type-return-type :: <llvm-type>,
    required-init-keyword: return-type:;
  constant slot llvm-function-type-parameter-types :: <sequence>,
    required-init-keyword: parameter-types:;
  constant slot llvm-function-type-varargs? :: <boolean>,
    required-init-keyword: varargs?:;
end class;

define abstract class <llvm-aggregate-type> (<llvm-type>)
end class;

define class <llvm-struct-type> (<llvm-aggregate-type>)
  constant slot llvm-struct-type-packed? :: <boolean>,
    required-init-keyword: packed?:;
  constant slot llvm-struct-type-elements :: <sequence>,
    required-init-keyword: elements:;
end class;

define class <llvm-array-type> (<llvm-aggregate-type>)
  constant slot llvm-array-type-size :: <integer>,
    required-init-keyword: size:;
  constant slot llvm-array-type-element-type :: <llvm-type>,
    required-init-keyword: element-type:;
end class;

define class <llvm-vector-type> (<llvm-type>)
  constant slot llvm-vector-type-size :: <integer>,
    required-init-keyword: size:;
  constant slot llvm-vector-type-element-type :: <llvm-type>,
    required-init-keyword: element-type:;
end class;

define abstract class <llvm-placeholder-type> (<llvm-type>)
  slot llvm-placeholder-type-forward :: <llvm-type>;
end class;

define class <llvm-upval-type> (<llvm-placeholder-type>)
  constant slot llvm-upval-type-index :: <integer>,
    required-init-keyword: index:;
end class;

define class <llvm-symbolic-type> (<llvm-placeholder-type>)
  constant slot llvm-symbolic-type-name :: <string>,
    required-init-keyword: name:;
end class;

define class <llvm-unnamed-type> (<llvm-placeholder-type>)
  constant slot llvm-unnamed-type-index :: <integer>,
    required-init-keyword: index:;
end class;
