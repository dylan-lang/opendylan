Module:    Dylan-User
Synopsis:  Dylan sectionizer
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sectionizer
  use dylan;
  use functional-extensions;
  use equal-table;
  use streams;
  use format;
  use locators;
  use operating-system;
  use file-system;
  use date;

  export sectionizer;
end library sectionizer;
