module:    powerpc-rtg
Synopsis:  Complex Primitives for the Dylan PowerPC runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method op--push-registers-for-remove-optionals
    (be :: <powerpc-back-end>) => (bytes-pushed :: <integer>)
  with-harp (be)
    arg0 arg0;
    arg-count argc;
    mlist mlist;
    function function;
  
    ins--push(be, function);
    ins--push(be, mlist);

    2 * 4
  end with-harp;
end method;

define sideways method op--pop-registers-for-remove-optionals
    (be :: <powerpc-back-end>) => ()
  with-harp (be)
    arg0 arg0;
    arg-count argc;
    mlist mlist;
    function function;

    ins--pop(be, mlist);
    ins--pop(be, function);
  end with-harp;
end method;
