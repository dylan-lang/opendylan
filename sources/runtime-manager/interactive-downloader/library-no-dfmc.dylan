module:        dylan-user
synopsis:      The internals for managing interactive execution
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library simple-downloader
  use common-dylan;
  use io;
  use collections;
  use access-path;
  use debugger-manager;
  use tether-downloader;
  use interactive-symbol-table;
  use coff-manager;
  export
     interactive-downloader;
end library;

define module interactive-downloader
  use common-dylan, exclude: { format-to-string }, rename: {table => hqnex-table};
  use format;
  use format-out;
  use access-path;
  use byte-vector;
  use tether-downloader;
  use interactive-symbol-table;
  use debugger-manager, rename: {kill-application => dm-kill-application};
  use coff-representation;
  use coff-sizes;
  create
     download-object-files,
     download-for-interactive-execution;
end module;

define module interactive-downloader-internals
  use common-dylan, exclude: { format-to-string }, rename: {table => hqnex-table};
  use format;
  use format-out;
  use access-path;
  use byte-vector;
  use tether-downloader;
  use interactive-symbol-table;
  use debugger-manager, rename: {kill-application => dm-kill-application};
  use coff-representation;
  use coff-sizes;
  use interactive-downloader;
end module;

