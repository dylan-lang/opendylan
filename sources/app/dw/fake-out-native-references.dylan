Module:    dw
Synopsis:  Batch mode compilation handling
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function current-native-back-end-setter (new-value) => (new-value)
  new-value
end function current-native-back-end-setter;

define variable *trace-harp?* = #f;

define macro elf-linker-options
  { elf-linker-options () }
    =>
    { ?=linker := #"elf"; }
end macro;

define constant $gnu-backend-class  = #f;
define constant $elf-backend-class  = #f;
define constant $masm-backend-class = #f;
