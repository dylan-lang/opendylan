Module:    Dylan-User
Synopsis:  DFM compiler database testing
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment-database-test
  use environment-imports;
  use environment-protocols;

  use dfmc-environment-database;
  use dfmc-derived-information,
    rename: { <source-location> => dfmc/<source-location>,
              do-library-modules => dfmc/do-library-modules };
  use registry-projects-internal;
  use projects-implementation;
end module dfmc-environment-database-test;
