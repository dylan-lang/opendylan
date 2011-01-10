Module:    Dylan-User
Synopsis:  dfmc-environment-reports test module
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment-reports-test
  use environment-imports;
  use environment-protocols;
  use standard-io;

  use dfmc-project-compilation;
  use dfmc-derived-information,
    rename: { <source-location> => dfmc/<source-location>,
              do-library-modules => dfmc/do-library-modules };
  use registry-projects-internal;
  use projects-implementation,
    rename: { close-project => projects/close-project };

  use dfmc-environment-reports;

  export test-report;
end module dfmc-environment-reports-test;
