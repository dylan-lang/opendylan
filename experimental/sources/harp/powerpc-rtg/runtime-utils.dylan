module:    powerpc-rtg
Synopsis:  Utilities for the Dylan Pentium runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method make-temp-register
    (be :: <powerpc-back-end>, n :: <integer>)
  next-method();
  /*
  select (n)
    1 => reg--tmp1;
    2 => reg--tmp2;
    3 => reg--tmp3;
  end;
  */
end method;
