Module:    dfmc-testing-support
Copyright: Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define function compile-library-until-optimized (lib)
  block()
    compile-library-from-definitions(lib, force?: #t, skip-link?: #t,
                                     compile-if-built?: #t, skip-heaping?: #t);
  exception (e :: <abort-compilation>)
  end
end function;
