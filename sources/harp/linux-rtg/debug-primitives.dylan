module:    linux-rtg
Synopsis:  Debugger Primitives for the Dylan Linux runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method op--output-debug-string
    (be :: <native-unix-back-end>, string :: <register>) => ()
end method;
