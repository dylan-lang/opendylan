Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-instruction> (<llvm-value>)
  constant slot llvm-instruction-operands :: <sequence>,
    init-value: #[], init-keyword: operands:;
  constant slot llvm-instruction-metadata :: <list>,
    init-value: #(), init-keyword: metadata:;
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
  slot %llvm-value-type :: <llvm-type>;
  constant slot llvm-gep-instruction-in-bounds? :: <boolean>,
    init-value: #f, init-keyword: in-bounds?:;
end class;

define method llvm-value-type
    (value :: <llvm-gep-instruction>)
 => (type :: <llvm-type>);
  if (slot-initialized?(value, %llvm-value-type))
    value.%llvm-value-type
  else
    let operands = value.llvm-instruction-operands;
    let pointer-type
      = type-forward(llvm-value-type(operands[0]));
    value.%llvm-value-type
      := if (operands.size < 2)
           pointer-type
         else
           for (i from 2 below operands.size,
                type = type-forward(pointer-type.llvm-pointer-type-pointee)
                  then type-forward(llvm-gep-index(type, operands[i])))
           finally
             make(<llvm-pointer-type>,
                  pointee: type,
                  address-space: pointer-type.llvm-pointer-type-address-space)
           end for
         end if
  end if
end method;

define method llvm-gep-index
    (type :: <llvm-array-type>, index :: <llvm-value>)
 => (indexed-type :: <llvm-type>)
  type.llvm-array-type-element-type
end method;

define method llvm-gep-index
    (type :: <llvm-vector-type>, index :: <llvm-value>)
 => (indexed-type :: <llvm-type>)
  type.llvm-vector-type-element-type
end method;

define method llvm-gep-index
    (type :: <llvm-struct-type>, index :: <llvm-integer-constant>)
 => (indexed-type :: <llvm-type>)
  llvm-constrain-type(llvm-value-type(index), $llvm-i32-type);
  type.llvm-struct-type-elements[index.llvm-integer-constant-integer]
end method;

define method llvm-gep-index
    (type :: <llvm-union-type>, index :: <llvm-integer-constant>)
 => (indexed-type :: <llvm-type>)
  llvm-constrain-type(llvm-value-type(index), $llvm-i32-type);
  type.llvm-union-type-elements[index.llvm-integer-constant-integer]
end method;

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
  slot %llvm-value-type :: <llvm-type>,
    init-keyword: type:;
end class;

define method llvm-value-type
    (value :: <llvm-cmp-instruction>)
 => (type :: <llvm-type>);
  if (slot-initialized?(value, %llvm-value-type))
    value.%llvm-value-type
  else
    let operands = value.llvm-instruction-operands;
    let operand-type = type-forward(llvm-value-type(operands[0]));
    if (instance?(operand-type, <llvm-vector-type>))
      value.%llvm-value-type
        := make(<llvm-vector-type>,
                size: operand-type.llvm-vector-type-size,
                element-type: $llvm-i1-type)
    else
      value.%llvm-value-type := $llvm-i1-type
    end if
  end if
end method;

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
  constant slot llvm-invoke-instruction-attribute-list :: <llvm-attribute-list>
    = $llvm-empty-attribute-list, init-keyword: attribute-list:;
  constant slot llvm-invoke-instruction-calling-convention :: <integer>,
    init-value: $llvm-calling-convention-c, init-keyword: calling-convention:;
end class;

define class <llvm-unreachable-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-indirect-branch-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-resume-instruction> (<llvm-terminator-instruction>)
end class;

define class <llvm-phi-node> (<llvm-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-phi-node>)
 => (type :: <llvm-type>);
  llvm-value-type(value.llvm-instruction-operands[0])
end method;

define class <llvm-landingpad-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-landingpad-instruction-cleanup? :: <boolean>,
    init-value: #f, init-keyword: cleanup?:;
end class;

define class <llvm-alloca-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-alloca-instruction-alignment :: <integer>,
    init-value: 0, init-keyword: alignment:;
end class;

define abstract class <llvm-volatile-instruction> (<llvm-instruction>)
  constant slot llvm-instruction-volatile? :: <boolean>,
    init-value: #f, init-keyword: volatile?:;
end class;

define abstract class <llvm-memory-instruction> (<llvm-volatile-instruction>)
  constant slot llvm-memory-instruction-alignment :: <integer>,
    init-value: 0, init-keyword: alignment:;
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

define constant <llvm-synchronization-scope>
  = one-of(#"single-thread", #"cross-thread");
define constant <llvm-atomic-ordering>
  = one-of(#"not-atomic",
           #"unordered",
           #"monotonic",
           #"acquire",
           #"release",
           #"acquire-release",
           #"sequentially-consistent");

define abstract class <llvm-atomic-instruction> (<llvm-instruction>)
  constant slot llvm-atomic-instruction-scope :: <llvm-synchronization-scope>,
    init-value: #"cross-thread", init-keyword: scope:;
  constant slot llvm-atomic-instruction-ordering :: <llvm-atomic-ordering>,
    init-value: #"not-atomic", init-keyword: ordering:;
end class;

define class <llvm-atomic-load-instruction> (<llvm-load-instruction>,
                                             <llvm-atomic-instruction>)
end class;

define class <llvm-atomic-store-instruction> (<llvm-store-instruction>,
                                              <llvm-atomic-instruction>)
end class;

define class <llvm-fence-instruction> (<llvm-atomic-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-fence-instruction>)
 => (type :: <llvm-type>);
  $llvm-void-type
end method;

define class <llvm-cmpxchg-instruction> (<llvm-atomic-instruction>,
                                         <llvm-volatile-instruction>)
  slot %llvm-value-type :: <llvm-type>;
  constant slot llvm-cmpxchg-instruction-failure-ordering :: <llvm-atomic-ordering>,
    init-value: #"not-atomic", init-keyword: failure-ordering:;
  constant slot llvm-cmpxchg-instruction-weak? :: <boolean>,
    init-value: #f, init-keyword: weak?:;
end class;

define method llvm-value-type
    (value :: <llvm-cmpxchg-instruction>)
 => (type :: <llvm-type>);
  if (slot-initialized?(value, %llvm-value-type))
    value.%llvm-value-type
  else
    let memory-type = llvm-value-type(value.llvm-instruction-operands[1]);
    value.%llvm-value-type
      := make(<llvm-struct-type>,
              elements: vector(memory-type, $llvm-i1-type))
  end if
end method;

define constant <llvm-atomicrmw-operation>
  = one-of(#"xchg",
           #"add",
           #"sub",
           #"and",
           #"nand",
           #"or",
           #"xor",
           #"max",
           #"min",
           #"umax",
           #"umin");

define class <llvm-atomicrmw-instruction> (<llvm-atomic-instruction>,
                                           <llvm-volatile-instruction>)
  constant slot llvm-atomicrmw-instruction-operation :: <llvm-atomicrmw-operation>,
    required-init-keyword: operation:;
end class;

define method llvm-value-type
    (value :: <llvm-atomicrmw-instruction>)
 => (type :: <llvm-type>);
  let pointer-type
    = type-forward(llvm-value-type(value.llvm-instruction-operands[0]));
  type-forward(pointer-type.llvm-pointer-type-pointee)
end method;

define class <llvm-call-instruction> (<llvm-instruction>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-call-instruction-attribute-list :: <llvm-attribute-list>
    = $llvm-empty-attribute-list, init-keyword: attribute-list:;
  constant slot llvm-call-instruction-calling-convention :: <integer>,
    init-value: $llvm-calling-convention-c, init-keyword: calling-convention:;
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
  slot %llvm-value-type :: <llvm-type>,
    init-keyword: type:;
end class;

define method llvm-value-type
    (value :: <llvm-extract-value-instruction>)
 => (type :: <llvm-type>);
  if (slot-initialized?(value, %llvm-value-type))
    value.%llvm-value-type
  else
    let operands = value.llvm-instruction-operands;
    let operand-type = type-forward(llvm-value-type(operands[0]));
    value.%llvm-value-type
      := for (index in value.llvm-aggregate-instruction-indices,
              type = operand-type then llvm-aggregate-index(type, index))
         finally
           type
         end
  end if
end method;

define method llvm-aggregate-index
    (type :: <llvm-array-type>, index :: <integer>)
 => (indexed-type :: <llvm-type>)
  type.llvm-array-type-element-type
end method;

define method llvm-aggregate-index
    (type :: <llvm-struct-type>, index :: <integer>)
 => (indexed-type :: <llvm-type>)
  type.llvm-struct-type-elements[index]
end method;

define class <llvm-insert-value-instruction> (<llvm-aggregate-instruction>)
end class;

define method llvm-value-type
    (value :: <llvm-insert-value-instruction>)
 => (type :: <llvm-type>);
  llvm-value-type(value.llvm-instruction-operands[0])
end method;
