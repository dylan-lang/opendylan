module:    powerpc-rtg
Synopsis:  Non-local exit primitives for the Dylan runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method op--leaf-call
    (be :: <powerpc-back-end>, code :: <register>) => ()
  ins--call(be, code, 0);
end method;
