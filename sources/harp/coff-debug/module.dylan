module:    dylan-user
Synopsis:  The module definitions for the COFF-DEBUG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module coff-reader
  use dylan;
  use dylan-extensions;
  use big-integers, prefix: "generic-";
  use streams, exclude: {count, name}, export: all;
  use standard-io, import: {*standard-output*}, export: all;
  use file-system, import: {<file-stream>}, export: all;
  use format, export: all;
  use print, export: all;
  use locators;
  use coff-representation;
  use coff-sizes;
  use byte-vector;

  export read-coff-file,
         read-char,
         read-byte,
         read-signed-short,
         read-short,
         read-signed-word,
         read-word;
end module;


define module coff-print
  use dylan;
  use streams, export: all;
  use standard-io, import: {*standard-output*}, export: all;
  use format, export: all;
  use print, export: all;
  use coff-representation;
  use coff-reader, export: all;
  use byte-vector;

  export print-coff-file;
end module;


define module coff-debug
  use dylan;
  use streams, export: all;
  use standard-io, import: {*standard-output*}, export: all;
  use file-system, import: {<file-stream>}, export: all;
  use format, export: all;
  use print, export: all;
  use locators;
  use coff-representation;
  use coff-reader, export: all;
  use coff-print, export: all;
  use coff-writer, export: all;

  export write-coff-file;
end module;
