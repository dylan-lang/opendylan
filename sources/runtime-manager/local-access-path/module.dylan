module:       dylan-user
synopsis:     Module definitions for the access-path library
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module access-path-implementation
  use common-dylan, exclude: { format-to-string };
  use dylan-extensions,
     import: {<machine-word>, 
              <double-integer>, 
              $minimum-unsigned-machine-word, integer-as-raw},
     export: all;
  use dylan-primitives;
  use machine-word-lowlevel;
  use threads, exclude: {thread-name};
  use format;
  use format-out;
  use print;
  use streams, import: {<file-stream>, <stream>, force-output, close};
  use locators, import: {<file-locator>};
  use file-system;
  use byte-vector;
  use table-extensions, import: {<string-table>};
  use c-ffi;

  use access-path;
  use access-path-nub;
end module;
