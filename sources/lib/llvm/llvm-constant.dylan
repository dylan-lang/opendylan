Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-constant-value> (<llvm-value>)
  constant slot llvm-value-type :: <llvm-type> = make(<llvm-opaque-type>),
    init-keyword: type:;
end class;

define method value-partition-key
    (value :: <llvm-constant-value>)
 => (key :: <vector>);
  vector(object-class(value))
end method;

define class <llvm-null-constant> (<llvm-constant-value>)
end class;

define class <llvm-undef-constant> (<llvm-constant-value>)
end class;

define class <llvm-integer-constant> (<llvm-constant-value>)
  constant slot llvm-integer-constant-integer :: <abstract-integer>,
    required-init-keyword: integer:;
end class;

define method value-partition-key
    (value :: <llvm-integer-constant>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-integer-constant-integer)
end method;

define class <llvm-float-constant> (<llvm-constant-value>)
  constant slot llvm-float-constant-float :: <float>,
    required-init-keyword: float:;
end class;

define method value-partition-key
    (value :: <llvm-float-constant>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-float-constant-float)
end method;

define class <llvm-aggregate-constant> (<llvm-constant-value>)
  constant slot llvm-aggregate-constant-values :: <sequence>,
    required-init-keyword: aggregate-values:
end class;

define method value-partition-key
    (value :: <llvm-aggregate-constant>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-aggregate-constant-values.size)
end method;

define method value-referenced-values
    (value :: <llvm-aggregate-constant>)
 => (referenced :: <vector>);
  as(<vector>, value.llvm-aggregate-constant-values)
end method;

define class <llvm-asm-constant> (<llvm-constant-value>)
  constant slot llvm-asm-constant-asm-string :: <string>,
    required-init-keyword: asm-string:;
  constant slot llvm-asm-constant-constraint :: <string>,
    required-init-keyword: constraint:;
  constant slot llvm-asm-constant-side-effect? :: <boolean>,
    init-value: #f, init-keyword: side-effect?:;
  constant slot llvm-asm-constant-align-stack? :: <boolean>,
    init-value: #f, init-keyword: align-stack?:;
end class;

define method value-partition-key
    (value :: <llvm-asm-constant>)
 => (key :: <vector>);
  vector(object-class(value),
         value.llvm-asm-constant-asm-string,
         value.llvm-asm-constant-constraint,
         value.llvm-asm-constant-side-effect?,
         value.llvm-asm-constant-align-stack?)
end method;


/// Constant expressions

define abstract class <llvm-expression-constant> (<llvm-constant-value>)
  constant slot llvm-expression-constant-operands :: <sequence>,
    required-init-keyword: operands:;
end class;

define method value-referenced-values
    (value :: <llvm-expression-constant>)
 => (referenced :: <vector>);
  as(<vector>, value.llvm-expression-constant-operands)
end method;

define class <llvm-binop-constant> (<llvm-expression-constant>,
                                    <llvm-binary-operator-flags-mixin>)
  constant slot llvm-binop-constant-operator :: <llvm-binary-operator>,
    required-init-keyword: operator:;
end class;

define method value-partition-key
    (value :: <llvm-binop-constant>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-binop-constant-operator)
end method;

define class <llvm-cast-constant> (<llvm-expression-constant>)
  constant slot llvm-cast-constant-operator :: <llvm-cast-operator>,
    required-init-keyword: operator:;
end class;

define method value-partition-key
    (value :: <llvm-cast-constant>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-cast-constant-operator)
end method;

define class <llvm-gep-constant> (<llvm-expression-constant>)
  constant slot llvm-gep-constant-in-bounds? :: <boolean>,
    required-init-keyword: in-bounds?:;
end class;

define method value-partition-key
    (value :: <llvm-gep-constant>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-gep-constant-in-bounds?)
end method;

define abstract class <llvm-cmp-constant> (<llvm-expression-constant>,
                                           <llvm-cmp-mixin>)
end class;

define method value-partition-key
    (value :: <llvm-cmp-constant>)
 => (key :: <vector>);
  vector(object-class(value), value.llvm-cmp-predicate)
end method;

define class <llvm-icmp-constant> (<llvm-cmp-constant>, <llvm-icmp-mixin>)
end class;

define class <llvm-fcmp-constant> (<llvm-cmp-constant>, <llvm-fcmp-mixin>)
end class;

// FIXME:
// record CE_SELECT     = 13;  // CE_SELECT:     [opval, opval, opval]
// record CE_EXTRACTELT = 14;  // CE_EXTRACTELT: [opty, opval, opval]
// record CE_INSERTELT  = 15;  // CE_INSERTELT:  [opval, opval, opval]
// record CE_SHUFFLEVEC = 16;  // CE_SHUFFLEVEC: [opval, opval, opval]
// record CE_CMP        = 17;  // CE_CMP:        [opty, opval, opval, pred]
// record CE_SHUFVEC_EX = 19;  // SHUFVEC_EX:    [opty, opval, opval, opval]
// record BLOCKADDRESS  = 21;  // BLOCKADDRESS:  [fnty, fnval, bb#]


/// Constant placeholders

define class <llvm-symbolic-constant> (<llvm-constant-value>,
                                       <llvm-placeholder-value>)
  constant slot llvm-symbolic-constant-name :: type-union(<string>, <integer>),
    required-init-keyword: name:;
end class;

define method value-forward
    (value :: <llvm-symbolic-constant>)
 => (value :: <llvm-constant-value>);
  if (slot-initialized?(value, llvm-placeholder-value-forward))
    value-forward(value.llvm-placeholder-value-forward)
  else
    error("value @%s is not defined", value.llvm-symbolic-constant-name);
  end if
end method;
