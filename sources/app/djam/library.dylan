Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library djam
  use common-dylan;
  use io;
  use system;
  use file-source-records;
  use jam;
end library;

define module djam
  use common-dylan, exclude: { format-to-string };
  use format-out;
  use format;
  use streams;
  use standard-io;
  use file-system;
  use locators;
  use operating-system;
  use date;
  use file-source-records;
  use jam;
end;
