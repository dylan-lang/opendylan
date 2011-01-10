module:         dylan-user
synopsis:       The tty user interface for the devel debugger
author:         Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library mm-debugger
  use functional-dylan;
  use collections;
  use c-ffi;
  use system;
  use io;
  use access-path;
  use debugger-manager;
  use collections;
  use tether-downloader;
  use interactive-symbol-table;
  use coff-manager;
  use coff-debug;
  use simple-downloader;
  use disasm;
  use projects;
  use registry-projects;
  use source-records;
  use user-projects;
  use release-info;
  use dfmc-browser-support;
  use dfmc-harp-browser-support;
  use dfmc-pentium-file-compiler;
  export devel-dbg-ui;
end library;

define module devel-dbg-ui
  use functional-dylan;
  use collectors;
  use c-ffi;
  use operating-system, 
     exclude: {load-library},
     rename: {application-filename => os-application-filename};
  use streams;
  use standard-io;
  use format;
  use print;
  use format-out;
  use threads, 
    rename: {thread-name => threads-thread-name};
  use access-path,
    exclude: {kill-application, debugger-message};
  use debugger-manager,
    exclude: {debugger-message};
  use tether-downloader;
  use coff-representation;
  use coff-reader;
  use coff-print;
  use byte-vector;
  use interactive-symbol-table;
  use interactive-downloader;
  use disasm;
  use projects;
  use projects-implementation;
  use source-records;
end module;

