Module:    Dylan-User
Synopsis:  The profiling tool provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-profiler
  use environment-imports,
    exclude: { info-name };

  use duim;
  use duim-internals,
    import: { string-pluralize };

  use environment-protocols;
  use environment-reports;
  use environment-manager;
  use environment-framework;
  use environment-tools;

  export <profiler>;
end module environment-profiler;
