Module:    dw
Synopsis:  PowerPC compilation driving
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define macro elf-linker-options
  { elf-linker-options () }
    =>
    { ?=linker := #"ppc-elf"; ?=combine-dlls? := #t; }
end macro;

///---*** NOTE: There's no such backend yet so define it to prevent compiler warnings ...
define constant <powerpc-back-end> = #f;

define constant $gnu-backend-class  = <powerpc-back-end>;
define constant $elf-backend-class  = <powerpc-linux-back-end>;
define constant $masm-backend-class = <powerpc-back-end>;
