Module:    dw
Synopsis:  Pentium compilation driving
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define macro elf-linker-options
  { elf-linker-options () }
    =>
    { ?=linker := #"elf"; }
end macro;

define constant $gnu-backend-class  = <pentium-back-end>;
define constant $elf-backend-class  = <pentium-linux-back-end>;
define constant $masm-backend-class = <pentium-back-end>;
