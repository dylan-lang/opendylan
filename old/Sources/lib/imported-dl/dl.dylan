Module: imported-dl
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define C-type-alias <DL-handle> = <C-char>.pointer-type;

define C-function dlopen
  parameter path   :: <C-string>;
  parameter mode   :: <C-int>;
  result    handle :: <DL-handle>;
  c-name: "dlopen";
end C-function;

define C-function dlsym
  parameter handle      :: <DL-handle>;
  parameter symbol-name :: <C-string>;
  result    address     :: <C-char>.pointer-type;
  c-name: "dlsym";
end C-function;

define C-function dlclose
  parameter handle :: <DL-handle>;
  result    code   :: <C-int>;
  c-name: "dlclose";
end C-function;

define C-function dlerror
  result description :: <C-string>;
  c-name: "dlerror";
end C-function;

define constant $RTLD_LAZY = 1;
define constant $RTLD_NOW = 2;

// define C-link-requirements "-ldl" end;
// define C-link-requirements "/usr/lib/libdl.so" end;

// eof
