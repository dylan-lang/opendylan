Module:    Dylan-User
Synopsis:  DFMC report generation
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment-reports
  use functional-dylan;
  use streams;
  use format;

  use dfmc-derived-information;
  use source-records;

  export <report>,
         do-report;
end module dfmc-environment-reports;
