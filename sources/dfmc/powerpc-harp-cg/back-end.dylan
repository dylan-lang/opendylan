Module: dfmc-powerpc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


current-native-back-end() := <powerpc-back-end>;

define sideways method op--cleanup-preserve-state-entry
    (back-end :: <powerpc-back-end>) => ()
  ins--save-return-address(back-end, #f)
end method;

define sideways method op--cleanup-preserve-state-exit
    (back-end :: <powerpc-back-end>) => ()
  ins--restore-return-address(back-end, #f)
end method;


