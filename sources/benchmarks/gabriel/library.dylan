Module:   dylan-user
Synopsis: Gabriel benchmarks in Dylan
Author:   Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gabriel-benchmarks
  use functional-dylan;
  export gabriel-benchmarks;
end library gabriel-benchmarks;

define module gabriel-benchmarks
  use functional-dylan;
  use transcendentals,
    import: { sin, cos, $single-pi, $double-pi };
  use threads,
    import: { dynamic-bind };
  use simple-format;
  use simple-random;
end module gabriel-benchmarks;
