Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-back-end> (<back-end>, <llvm-builder>)
end;

define generic llvm-back-end-target-triple
    (back-end :: <llvm-back-end>) => (triple :: <string>);

define generic llvm-back-end-data-layout
    (back-end :: <llvm-back-end>) => (layout :: <string>);

