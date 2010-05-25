Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-instruction> (<llvm-value>)
  constant slot llvm-instruction-operands :: <sequence>,
    init-value: #[], init-keyword: operands:;
end class;

define class <llvm-binop-instruction> (<llvm-instruction>,
                                       <llvm-binary-operator-flags-mixin>)
  constant slot llvm-binop-instruction-operator :: <llvm-binary-operator>,
    required-init-keyword: operator:;
end class;

define method llvm-value-type
    (value :: <llvm-binop-instruction>)
 => (type :: <llvm-type>);
  llvm-value-type(value.llvm-instruction-operands[0])
end method;

define class <llvm-cast-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-cast-instruction-operator :: <llvm-cast-operator>,
    required-init-keyword: operator:;
end class;

define class <llvm-gep-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type> = make(<llvm-opaque-type>),
    init-keyword: type:;
  constant slot llvm-gep-instruction-in-bounds? :: <boolean>,
    required-init-keyword: in-bounds?:;
end class;

define class <llvm-select-instruction> (<llvm-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-select-instruction>)
 => (type :: <llvm-type>);
  llvm-value-type(value.llvm-instruction-operands[1])
end method;

define class <llvm-extractelement-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type> = make(<llvm-opaque-type>),
    init-keyword: type:;
end class;

define class <llvm-insertelement-instruction> (<llvm-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-insertelement-instruction>)
 => (type :: <llvm-type>);
  llvm-value-type(value.llvm-instruction-operands[0])
end method;

define class <llvm-shufflevector-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type> = make(<llvm-opaque-type>),
    init-keyword: type:;
end class;

define abstract class <llvm-cmp-instruction> (<llvm-instruction>,
                                              <llvm-cmp-mixin>)
  constant slot llvm-value-type :: <llvm-type> = make(<llvm-opaque-type>),
    init-keyword: type:;
end class;

define class <llvm-icmp-instruction> (<llvm-cmp-instruction>,
                                      <llvm-icmp-mixin>)
end class;

define class <llvm-fcmp-instruction> (<llvm-cmp-instruction>,
                                      <llvm-fcmp-mixin>)
end class;

define abstract class <llvm-terminator-instruction> (<llvm-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-terminator-instruction>)
 => (type :: <llvm-type>);
  $llvm-void-type
end method;

define class <llvm-return-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-branch-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-switch-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-invoke-instruction> (<llvm-terminator-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-invoke-instruction-attribute-list :: <llvm-attribute-list>,
    required-init-keyword: attribute-list:;
  constant slot llvm-invoke-instruction-calling-convention :: <integer>,
    init-value: 0, init-keyword: calling-convention:;
end class;

define class <llvm-unwind-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-unreachable-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-indirect-branch-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-phi-node> (<llvm-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-phi-node>)
 => (type :: <llvm-type>);
  llvm-value-type(value.llvm-instruction-operands[0])
end method;

define class <llvm-alloca-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-alloca-instruction-alignment :: <integer>,
    init-value: 0, init-keyword: alignment:;
end class;

define abstract class <llvm-memory-instruction> (<llvm-instruction>)
  constant slot llvm-memory-instruction-alignment :: <integer>,
    init-value: 0, init-keyword: alignment:;
  constant slot llvm-memory-instruction-volatile? :: <boolean>,
    required-init-keyword: volatile?:;
end class;

define class <llvm-load-instruction> (<llvm-memory-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-load-instruction>)
 => (type :: <llvm-type>);
  let pointer-type
    = type-forward(llvm-value-type(value.llvm-instruction-operands[0]));
  type-forward(pointer-type.llvm-pointer-type-pointee)
end method;

define class <llvm-store-instruction> (<llvm-memory-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-store-instruction>)
 => (type :: <llvm-type>);
  $llvm-void-type
end method;

define class <llvm-call-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-call-instruction-attribute-list :: <llvm-attribute-list>,
    required-init-keyword: attribute-list:;
  constant slot llvm-call-instruction-calling-convention :: <integer>,
    init-value: 0, init-keyword: calling-convention:;
  constant slot llvm-call-instruction-tail-call? :: <boolean>,
    init-value: #f, init-keyword: tail-call?:;
end class;

define class <llvm-va-arg-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
end class;

define abstract class <llvm-aggregate-instruction> (<llvm-instruction>)
  constant slot llvm-aggregate-instruction-indices :: <sequence>,
    required-init-keyword: indices:;
end class;

define class <llvm-extract-value-instruction> (<llvm-aggregate-instruction>)
  constant slot llvm-value-type :: <llvm-type> = make(<llvm-opaque-type>),
    init-keyword: type:;
end class;

define class <llvm-insert-value-instruction> (<llvm-aggregate-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-insert-value-instruction>)
 => (type :: <llvm-type>);
  llvm-value-type(value.llvm-instruction-operands[0])
end method;
