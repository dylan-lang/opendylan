module:    linux-rtg
Synopsis:  Non-local exit primitives for the Dylan Linux runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// OP--POP-ANY-SEH-HANDLERS

define sideways method op--pop-any-SEH-handlers
    (be :: <native-linux-back-end>, new-stack-ptr :: <register>)
end method;
