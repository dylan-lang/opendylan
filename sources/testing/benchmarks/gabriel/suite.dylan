Module:   gabriel-benchmarks
Synopsis: Gabriel benchmarks in Dylan
Author:   Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite gabriel-benchmarks ()
  benchmark boyer-benchmark;
  benchmark browse-benchmark;
  benchmark dderiv-benchmark;
  benchmark deriv-benchmark;
  benchmark destructive-benchmark;
  benchmark div2-test-1-benchmark;
  benchmark div2-test-2-benchmark;
  benchmark fft-benchmark;
  benchmark frpoly/fixnum-benchmark;
  //benchmark frpoly/bignum-benchmark;
  benchmark frpoly/float-benchmark;
  benchmark puzzle-benchmark;
  benchmark tak-benchmark;
  benchmark trtak-benchmark;
  benchmark ctak-benchmark;
  benchmark takl-benchmark;
  benchmark stak-benchmark;
  benchmark traverse-benchmark;
  benchmark triangle-benchmark;
end suite;
