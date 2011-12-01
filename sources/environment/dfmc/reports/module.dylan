Module:    Dylan-User
Synopsis:  DFMC report generation
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment-reports
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;

  use dfmc-derived-information;
  use source-records;

  export <report>,
         do-report;
end module dfmc-environment-reports;
