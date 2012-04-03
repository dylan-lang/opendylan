Module:       llvm-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library-spec llvm ()
  module llvm;
  module llvm-builder;

  suite llvm-asm-suite;
end library-spec llvm;

define module-spec llvm ()
  instantiable class <llvm-module> (<object>);

  constant $llvm-label-type :: <llvm-type>;
  constant $llvm-void-type :: <llvm-type>;
  constant $llvm-metadata-type :: <llvm-type>;
  constant $llvm-float-type :: <llvm-type>;
  constant $llvm-double-type :: <llvm-type>;
  constant $llvm-i1-type :: <llvm-type>;
  constant $llvm-i8-type :: <llvm-type>;
  constant $llvm-i8*-type :: <llvm-type>;
  constant $llvm-i16-type :: <llvm-type>;
  constant $llvm-i32-type :: <llvm-type>;
  constant $llvm-i64-type :: <llvm-type>;
end module-spec llvm;

define module-spec llvm-builder ()
  constant <llvm-local-value> :: <type>;

  open abstract primary instantiable class <llvm-builder> (<object>);

  function llvm-builder-basic-block
      (<llvm-builder>) => (false-or(<llvm-basic-block>));
  function llvm-builder-basic-block-setter
      (false-or(<llvm-basic-block>), <llvm-builder>)
   => (false-or(<llvm-basic-block>));
  function llvm-builder-function
      (<llvm-builder>) => (false-or(<llvm-function>));
  function llvm-builder-function-setter
      (false-or(<llvm-function>), <llvm-builder>)
   => (false-or(<llvm-function>));
  function llvm-builder-module (<llvm-builder>) => (false-or(<llvm-module>));
  function llvm-builder-module-setter
      (false-or(<llvm-module>), <llvm-builder>) => (false-or(<llvm-module>));

  function llvm-builder-define-global
      (<llvm-builder>, <string>, <llvm-constant-value>)
   => (<llvm-constant-value>);
  function llvm-builder-declare-global
      (<llvm-builder>, <string>, <llvm-constant-value>)
   => (<llvm-constant-value>);
  function llvm-builder-global
      (<llvm-builder>, <string>) => (<llvm-constant-value>);

  function ins--local
      (<llvm-builder>, <string>, <llvm-local-value>)
   => (<llvm-local-value>);
  function llvm-builder-local (<llvm-builder>, <string>) => (<llvm-value>);

  function ins--block
      (<llvm-builder>, <llvm-basic-block>) => (<llvm-basic-block>);

  function ins--dbg
      (<llvm-builder>, <integer>, <integer>,
       <llvm-metadata-value>, <llvm-metadata-value>)
   => ();

  function ins--add
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--fadd
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--sub
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--fsub
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--mul
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--fmul
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--udiv
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--sdiv
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--fdiv
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--urem
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--srem
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--frem
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--shl
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--lshr
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--ashr
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--and
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--or
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);
  function ins--xor
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-binop-instruction>);

  function ins--trunc
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--zext
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--sext
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--fptoui
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--fptosi
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--uitofp
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--sitofp
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--fptrunc
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--fpext
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--ptrtoint
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--inttoptr
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);
  function ins--bitcast
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-cast-instruction>);

  function ins--icmp-eq
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-ne
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-slt
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-sgt
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-sle
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-sge
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-ult
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-ugt
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-ule
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);
  function ins--icmp-uge
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-icmp-instruction>);

  function ins--fcmp-oeq
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-one
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-olt
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-ogt
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-ole
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-oge
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-ord
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-uno
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-ueq
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-une
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-ult
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-ugt
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-ule
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-uge
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-true
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);
  function ins--fcmp-false
      (<llvm-builder>, <llvm-value>, <llvm-value>)
   => (<llvm-fcmp-instruction>);

  function ins--gep
      (<llvm-builder>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--gep-inbounds
      (<llvm-builder>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--select
      (<llvm-builder>, <llvm-value>, <llvm-value>, <llvm-value>)
   => (<llvm-instruction>);

  function ins--va-arg
      (<llvm-builder>, <llvm-value>, <llvm-type>) => (<llvm-instruction>);
  function ins--extractelement
      (<llvm-builder>, <llvm-value>, <llvm-value>) => (<llvm-instruction>);
  function ins--insertelement
      (<llvm-builder>, <llvm-value>, <llvm-value>, <llvm-value>)
   => (<llvm-instruction>);
  function ins--shufflevector
      (<llvm-builder>, <llvm-value>, <llvm-value>, <llvm-value>)
   => (<llvm-instruction>);
  function ins--phi (<llvm-builder>, #"rest") => (<llvm-instruction>);
  function ins--landingpad
      (<llvm-builder>, <llvm-type>, <llvm-value>, <sequence>,
       #"key", #"cleanup?", #"metadata")
   => (<llvm-instruction>);

  function ins--call
      (<llvm-builder>, <llvm-value>, <sequence>, #"rest")
   => (<llvm-instruction>);
  function ins--tail-call
      (<llvm-builder>, <llvm-value>, <sequence>, #"rest")
   => (<llvm-instruction>);
  function ins--call-intrinsic
      (<llvm-builder>, <string>, <sequence>, #"rest")
   => (<llvm-instruction>);
  function ins--alloca
      (<llvm-builder>, <llvm-type>, <llvm-value>, #"key", #"alignment")
   => (<llvm-instruction>);

  function ins--load
      (<llvm-builder>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--store
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-instruction>);

  function ins--cmpxchg
      (<llvm-builder>, <llvm-value>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-instruction>);

  function ins--atomicrmw-xchg
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-add
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-sub
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-and
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-nand
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-or
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-xor
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-max
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-min
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-umax
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);
  function ins--atomicrmw-umin
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest") => (<llvm-instruction>);

  function ins--fence (<llvm-builder>, #"rest") => (<llvm-instruction>);

  function ins--insertvalue
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-instruction>);
  function ins--extractvalue
      (<llvm-builder>, <llvm-value>, #"rest") => (<llvm-instruction>);
  
  function ins--ret (<llvm-builder>, #"rest") => (<llvm-instruction>);
  function ins--br (<llvm-builder>, #"rest") => (<llvm-instruction>);
  function ins--switch
      (<llvm-builder>, <llvm-value>, <llvm-value>, #"rest")
   => (<llvm-instruction>);

  function ins--invoke
      (<llvm-builder>, <llvm-value>, <llvm-value>, <llvm-value>, <sequence>,
       #"rest")
   => (<llvm-instruction>);

  function ins--unwind (<llvm-builder>, #"key", #"metadata") => (<llvm-instruction>);
  function ins--unreachable
     (<llvm-builder>, #"key", #"metadata")
  => (<llvm-instruction>);
  function ins--resume
      (<llvm-builder>, <llvm-value>, #"key" #"metadata")
   => (<llvm-instruction>);
end module-spec llvm-builder;
