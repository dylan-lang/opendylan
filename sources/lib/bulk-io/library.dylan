Module:    dylan-user
Author:    Toby Weinberg & Seth LaForge
Synopsis:  bulk-io, we haul <byte>s...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library bulk-io
  use functional-dylan;
  use io;
  use system;
  use win32-common;
  use win32-kernel;
  use C-ffi;
  export bulk-io;
end library bulk-io;


define module bulk-io
  use functional-dylan;
    // export: element, element-setter, size, forward-iteration-vertical;
  //use bulk-io-internal;

  // Basic classes
  create 
    write-file, wait-for-write-completion, write-file-error-check,
    <write-file-error>, write-file-error-locator, write-file-error-thread,
    <write-file-errors>, write-file-errors-errors;
  create <mapped-memory>, <memory-mapped-file>, 
    flush-mapped-memory, close-mapped-memory,
    word-32-element, word-32-element-setter, 
    word-64-element, word-64-element-setter;
  create <mm-dood-stream>, read-byte-string, 
    read-word-32, write-word-32, read-word-64, write-word-64;
end module bulk-io;


define module bulk-io-internal
  use functional-dylan;
  use dylan-primitives;
  use streams;
  use streams-internals;
  use format-out;
  use dylan-extensions;
  use machine-word-lowlevel;
  use win32-common;
  use win32-kernel;
  use c-ffi;
  use byte-vector;
  use operating-system;
  use threads;
  use format;

  use bulk-io;
end module bulk-io-internal;
