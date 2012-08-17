Module: dfmc-harp-native-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method rts-dropping-args(back-end :: <harp-native-back-end>) => ()
  ins--rts-and-drop(back-end,
                    back-end.cg-variables.count-vreg
                    | back-end.cg-variables.args-to-be-dropped);
end method rts-dropping-args;
