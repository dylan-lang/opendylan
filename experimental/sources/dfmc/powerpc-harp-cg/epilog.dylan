Module: dfmc-powerpc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method rts-dropping-args(back-end :: <powerpc-back-end>) => ()
  ins--rts-and-drop(back-end,
                    back-end.cg-variables.count-vreg
                    | back-end.cg-variables.args-to-be-dropped);
end method rts-dropping-args;
