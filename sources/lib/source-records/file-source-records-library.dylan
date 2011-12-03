Module:    dylan-user
Synopsis:  Library and module definitions for file source records
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library file-source-records
  use common-dylan;
  use collections;
  use io;
  use system;

  use source-records;

  export file-source-records;
end library;

define module file-source-records
  use source-records, export: all;

  create read-file-header,
         read-header-from-stream;

  create <file-source-record>,
         <badly-formed-file-header>,
         file-source-record-ids;

  create <flat-file-source-record>;
end module;

define module file-source-records-implementation
  use common-dylan, exclude: { format-to-string };
  use threads;
  use locators;
  // Probably don't need all this, sort it out later
  // use collectors;
  use byte-vector;
  use set;
  use streams;
  use format;
  use print;
  use standard-io;
  use format-out;
  use operating-system;
  use file-system;
  use date;
  use source-records-implementation;
  use file-source-records;
end module;
