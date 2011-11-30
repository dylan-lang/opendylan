Module:    dylan-user
Synopsis:  Report directory contents and compare directory sizes
Author:    Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library disk-usage
  use functional-dylan;
  use io;
  use system;
  use generic-arithmetic;
  use big-integers;
  export disk-usage;
end library disk-usage;

define module disk-usage
  use generic-arithmetic-common-dylan;
  use simple-format;
  use operating-system;
  use file-system;
  use locators;
  use threads;
  use streams;
end module disk-usage;


