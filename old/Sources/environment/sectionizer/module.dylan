Module:    Dylan-User
Synopsis:  Dylan sectionizer
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module sectionizer
  use dylan;
  use functional-extensions;
  use equal-table;
  use streams;
  use format;
  use locators;
  use operating-system;
  use file-system;
  use date;

  export <source-container>,
         source-container-locator,
         source-container-timestamp,
         source-container-sections,
         <section>,
         section-name,
         section-identifier,
         section-source-container,
         section-start-line,
         section-end-line,
         section-defining-line,
         section-lines,
         sectionize-file;

  //--- Should be part of a File System library...
  export file-modification-time;
end module sectionizer;
