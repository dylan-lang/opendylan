module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dispatch-profiler
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use system;
  export dispatch-profiler;
end library;

define module dispatch-profiler
  use functional-dylan;
  use dylan-extensions;
  use dylan-primitives;
  use dispatch-engine;
  // use simple-format;
  use streams;
  use file-system;
  use standard-io;
  use format;
  use format-out;
  use generic-arithmetic,
    prefix: "generic/";
  export
    decache-all-generics,
    clear-dispatch-profiling,
    make-dispatch-statistics,
    clear-dispatch-statistics!,
    collect-dispatch-statistics,
    print-dispatch-statistics,
    enable-generic-caches-only,
    enable-partial-dispatch-only,
    enable-call-site-caches-only
    ;
end module;

