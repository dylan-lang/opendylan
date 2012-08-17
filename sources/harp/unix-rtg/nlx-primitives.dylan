module:    harp-unix-rtg
Synopsis:  Non-local exit primitives for the Dylan Unix runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// OP--POP-ANY-SEH-HANDLERS

define sideways method op--pop-any-SEH-handlers
    (be :: <native-unix-back-end>, new-stack-ptr :: <register>)
end method;
