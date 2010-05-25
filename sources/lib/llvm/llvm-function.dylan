Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <llvm-function> (<llvm-global-value>)
  constant slot llvm-function-calling-convention :: <integer>,
    init-value: 0, init-keyword: calling-convention:;
  constant slot llvm-function-garbage-collector :: false-or(<string>),
    init-value: #f, init-keyword: garbage-collector:;
  constant slot llvm-function-arguments :: <sequence>,
    init-value: #[], init-keyword: arguments:;
  constant slot llvm-function-attribute-list :: <llvm-attribute-list>
    = make(<llvm-attribute-list>), init-keyword: attribute-list:;
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
