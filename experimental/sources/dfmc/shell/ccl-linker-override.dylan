Module:    dfmc-shell
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The C backend always wants to use its own "linker" and separate "DLLs" ...

$override-default-linker     := #"ccl";

// $override-default-dll-policy := #"separate-dlls";
