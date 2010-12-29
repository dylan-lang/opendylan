Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library llvm
  use dylan;
  use common-dylan;
  use collections;
  use io;
  use system;
  use generic-arithmetic;
  use big-integers;

  export llvm;
  export llvm-builder;
end library;

define module llvm
  create
    <llvm-type>,
    <llvm-primitive-type>,
    <llvm-integer-type>,
    llvm-integer-type-width,
    <llvm-pointer-type>,
    llvm-pointer-type-pointee,
    <llvm-function-type>,
    llvm-function-type-return-type,
    llvm-function-type-parameter-types,
    llvm-function-type-varargs?,
    <llvm-struct-type>,
    llvm-struct-type-elements,
    <llvm-union-type>,
    <llvm-array-type>,
    <llvm-vector-type>,

    <llvm-placeholder-type>,
    llvm-placeholder-type-forward-setter,
    llvm-type-forward,

    llvm-constrain-type,

    <llvm-upval-type>,
    llvm-type-resolve-upvals,

    <llvm-opaque-type>,

    <llvm-symbolic-type>,

    llvm-void-type?,

    $llvm-label-type,
    $llvm-void-type,
    $llvm-metadata-type,
    $llvm-float-type,
    $llvm-double-type,
    $llvm-i1-type,
    $llvm-i8-type,
    $llvm-i8*-type,
    $llvm-i16-type,
    $llvm-i32-type,
    $llvm-i64-type,

    <llvm-value>,
    llvm-value-type,

    <llvm-placeholder-value>,
    llvm-placeholder-value-forward-setter,
    llvm-value-forward,

    <llvm-symbolic-value>,
    <llvm-symbolic-constant>,

    <llvm-constant-value>,
    <llvm-null-constant>,
    <llvm-undef-constant>,
    <llvm-integer-constant>,
    <llvm-float-constant>,
    <llvm-aggregate-constant>,
    <llvm-asm-constant>,
    <llvm-binop-constant>,
    <llvm-cast-constant>,
    <llvm-gep-constant>,
    <llvm-icmp-constant>,
    <llvm-fcmp-constant>,

    $llvm-false,
    $llvm-true,

    <llvm-global-value>,
    llvm-global-name,
    <llvm-global-variable>,
    <llvm-global-alias>,

    <llvm-attributes>,
    $llvm-attribute-none,
    $llvm-attribute-zext,
    $llvm-attribute-sext,
    $llvm-attribute-noreturn,
    $llvm-attribute-inreg,
    $llvm-attribute-sret,
    $llvm-attribute-nounwind,
    $llvm-attribute-noalias,
    $llvm-attribute-byval,
    $llvm-attribute-nest,
    $llvm-attribute-readnone,
    $llvm-attribute-readonly,
    $llvm-attribute-noinline,
    $llvm-attribute-alwaysinline,
    $llvm-attribute-optsize,
    $llvm-attribute-ssp,
    $llvm-attribute-sspreq,
    llvm-attribute-alignment,
    $llvm-attribute-nocapture,
    $llvm-attribute-noredzone,
    $llvm-attribute-noimplicitfloat,
    $llvm-attribute-naked,
    $llvm-attribute-inlinehint,
    llvm-attribute-stack-alignment,
    llvm-attribute-merge,
    <llvm-attribute-list>,

    $llvm-calling-convention-c,
    $llvm-calling-convention-fast,
    $llvm-calling-convention-cold,
    $llvm-calling-convention-ghc,
    $llvm-calling-convention-x86-stdcall,
    $llvm-calling-convention-x86-fastcall,
    $llvm-calling-convention-arm-apcs,
    $llvm-calling-convention-arm-aapcs,
    $llvm-calling-convention-arm-vfp,
    $llvm-calling-convention-msp430-intr,
    $llvm-calling-convention-x86-thiscall,
    $llvm-calling-convention-ptx-kernel,
    $llvm-calling-convention-ptx-device,
    
    <llvm-function>,
    llvm-function-arguments,
    llvm-function-basic-blocks,
    llvm-function-value-table,
    llvm-function-calling-convention,
    llvm-function-attribute-list,

    <llvm-argument>,
    llvm-argument-name,
    llvm-argument-attributes,
    llvm-argument-index,

    <llvm-basic-block>,
    llvm-basic-block-instructions,

    <llvm-instruction>,
    <llvm-binop-instruction>,
    <llvm-cast-instruction>,
    <llvm-gep-instruction>,
    <llvm-select-instruction>,
    <llvm-extractelement-instruction>,
    <llvm-insertelement-instruction>,
    <llvm-shufflevector-instruction>,
    <llvm-cmp-instruction>,
    <llvm-icmp-instruction>,
    <llvm-fcmp-instruction>,
    <llvm-terminator-instruction>,
    <llvm-return-instruction>,
    <llvm-branch-instruction>,
    <llvm-switch-instruction>,
    <llvm-invoke-instruction>,
    <llvm-unwind-instruction>,
    <llvm-unreachable-instruction>,
    <llvm-indirect-branch-instruction>,
    <llvm-phi-node>,
    <llvm-alloca-instruction>,
    <llvm-load-instruction>,
    <llvm-store-instruction>,
    <llvm-call-instruction>,
    <llvm-va-arg-instruction>,
    <llvm-extract-value-instruction>,
    <llvm-insert-value-instruction>,
    
    <llvm-module>,
    llvm-module-name,
    llvm-module-target-triple,
    llvm-module-target-triple-setter,
    llvm-module-data-layout,
    llvm-module-data-layout-setter,
    llvm-module-asm,
    llvm-module-asm-setter,
    llvm-module-globals,
    llvm-module-functions,
    llvm-module-aliases,
    llvm-module-dependent-libraries,
    llvm-type-table,
    llvm-global-table,

    llvm-save-bitcode-file;
end module;

define module llvm-builder
  create
    <llvm-builder>,
    llvm-builder-module,
    llvm-builder-module-setter,
    llvm-builder-function,
    llvm-builder-function-setter,
    llvm-builder-basic-block,
    llvm-builder-basic-block-setter,

    llvm-builder-value,
    llvm-builder-value-function,

    llvm-builder-define-global,
    llvm-builder-declare-global,
    llvm-builder-global,

    ins--local,
    llvm-builder-local,
    
    ins--block,

    ins--add,
    ins--fadd,
    ins--sub,
    ins--fsub,
    ins--mul,
    ins--fmul,
    ins--udiv,
    ins--sdiv,
    ins--fdiv,
    ins--urem,
    ins--srem,
    ins--frem,
    ins--shl,
    ins--lshr,
    ins--ashr,
    ins--and,
    ins--or,
    ins--xor,

    ins--trunc,
    ins--zext,
    ins--sext,
    ins--fptoui,
    ins--fptosi,
    ins--uitofp,
    ins--sitofp,
    ins--fptrunc,
    ins--fpext,
    ins--ptrtoint,
    ins--inttoptr,
    ins--bitcast,

    ins--icmp-eq,
    ins--icmp-ne,
    ins--icmp-slt,
    ins--icmp-sgt,
    ins--icmp-sle,
    ins--icmp-sge,
    ins--icmp-ult,
    ins--icmp-ugt,
    ins--icmp-ule,
    ins--icmp-uge,

    ins--fcmp-oeq,
    ins--fcmp-one,
    ins--fcmp-olt,
    ins--fcmp-ogt,
    ins--fcmp-ole,
    ins--fcmp-oge,
    ins--fcmp-ord,
    ins--fcmp-uno,
    ins--fcmp-ueq,
    ins--fcmp-une,
    ins--fcmp-ult,
    ins--fcmp-ugt,
    ins--fcmp-ule,
    ins--fcmp-uge,
    ins--fcmp-true,
    ins--fcmp-false,

    ins--gep,
    ins--gep-inbounds,

    ins--select,
    ins--va-arg,

    ins--extractelement,
    ins--insertelement,
    ins--shufflevector,
    
    ins--phi,
    
    ins--call,
    ins--tail-call,
    ins--call-intrinsic,
    ins--alloca,
    ins--load,
    ins--store,
    
    ins--insertvalue,
    ins--extractvalue,
    
    ins--ret,
    ins--br,
    ins--switch,
    ins--invoke,
    ins--unwind,
    ins--unreachable;
end module;

define module llvm-internals
  use dylan-extensions,
    import: { <double-integer>, %double-integer-low, %double-integer-high,
              decode-single-float, decode-double-float,
              encode-single-float, encode-double-float };
  use machine-word-lowlevel;
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use file-system;
  use locators;
  use machine-words;
  use byte-vector;
  use operating-system;
  use threads;
  use big-integers,
    prefix: "generic-",
    rename: { <integer> => <abstract-integer> };

  use llvm,
    rename: { llvm-type-forward => type-forward,
              llvm-value-forward => value-forward };
  use llvm-builder;
end module;
