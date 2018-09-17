Module:   dylan-user
Synopsis: Gabriel benchmarks in Dylan
Author:   Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gabriel-benchmarks
  use common-dylan;
  export gabriel-benchmarks;
end library gabriel-benchmarks;

define module gabriel-benchmarks
  use common-dylan;
  use transcendentals,
    import: { sin, cos, $single-pi, $double-pi };
  use threads,
    import: { dynamic-bind };
  use simple-format;
  use simple-random;
  use simple-profiling;
end module gabriel-benchmarks;
