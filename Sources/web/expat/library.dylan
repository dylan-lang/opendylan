Module:       Dylan-User
Synopsis:     Dylan FFI wrapper for Expat
Author:       Evan Williams, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library expat
  use functional-dylan;
  use c-ffi;

  export expat,
	 expat-internals;
end library expat;
