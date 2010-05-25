Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <llvm-basic-block> (<llvm-value>)
  constant slot llvm-basic-block-name :: false-or(<string>),
    init-value: #f, init-keyword: name:;
  constant slot llvm-basic-block-instructions :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
end class;

define method llvm-value-type
    (value :: <llvm-basic-block>) => (type :: <llvm-type>);
  $llvm-label-type
end method;


