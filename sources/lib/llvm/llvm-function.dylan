Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $llvm-calling-convention-c            =  0; // C (default)
define constant $llvm-calling-convention-fast         =  8; // Fast
define constant $llvm-calling-convention-cold         =  9; // Cold
define constant $llvm-calling-convention-ghc          = 10; // GHC
define constant $llvm-calling-convention-x86-stdcall  = 64; // X86_StdCall
define constant $llvm-calling-convention-x86-fastcall = 65; // X86_StdCall
define constant $llvm-calling-convention-arm-apcs     = 66; // ARM_APCS
define constant $llvm-calling-convention-arm-aapcs    = 67; // ARM_AAPCS
define constant $llvm-calling-convention-arm-vfp      = 68; // ARM_VFP
define constant $llvm-calling-convention-msp430-intr  = 69; // MSP430_INTR
define constant $llvm-calling-convention-x86-thiscall = 70; // X86_StdCall
define constant $llvm-calling-convention-ptx-kernel   = 71; // PTX_Kernel
define constant $llvm-calling-convention-ptx-device   = 72; // PTX_Device

define class <llvm-function> (<llvm-global-value>)
  constant slot llvm-function-calling-convention :: <integer>,
    init-value: $llvm-calling-convention-c, init-keyword: calling-convention:;
  constant slot llvm-function-garbage-collector :: false-or(<string>),
    init-value: #f, init-keyword: garbage-collector:;
  constant slot llvm-function-arguments :: <sequence>,
    init-value: #[], init-keyword: arguments:;
  constant slot llvm-function-attribute-list :: <llvm-attribute-list>
    = $llvm-empty-attribute-list, init-keyword: attribute-list:;
  constant slot llvm-function-basic-blocks :: <sequence>
    = make(<stretchy-object-vector>);
  constant slot llvm-function-value-table :: <mutable-explicit-key-collection>
    = make(<string-table>);
end class;

define class <llvm-argument> (<llvm-value>)
  constant slot llvm-value-type :: <llvm-type>,
    required-init-keyword: type:;
  constant slot llvm-argument-name :: false-or(<string>),
    init-value: #f, init-keyword: name:;
  constant slot llvm-argument-attributes :: <llvm-attributes>,
    init-value: $llvm-attribute-none, init-keyword: attributes:;
  constant slot llvm-argument-index :: <integer>,
    required-init-keyword: index:;
end class;
