module:    powerpc-rtg
Synopsis:  Apply entry point generation for the POWERPC RTG
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define sideways method op--preserve-return-address-for-apply
    (be :: <powerpc-back-end>, req-index)
  ins--save-return-address(be, req-index);
end method;

define sideways method op--restore-return-address-for-apply
    (be :: <powerpc-back-end>)
  ins--restore-return-address(be, #f);
end method;
