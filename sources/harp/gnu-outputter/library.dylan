module:    dylan-user
Synopsis:  GNU mnemonic assembler output from harp for i486
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library gnu-outputter
  use dylan;
  use common-dylan;
  use collections;
  use io;
  use big-integers;
  use system;
  use harp;
  use binary-manager;
  use binary-builder;
  use binary-outputter;

  export gnu-outputter;
end library;

define module gnu-outputter
  use common-dylan, exclude: { format-to-string };
  use dylan-extensions;
  use byte-vector;
  use format;
  use format-out;
  use streams;
  use streams-internals, import: {<byte-file-stream>, <byte-char-file-stream>};
  use file-system;
  use file-system-internals;
  use operating-system;
  use harp-for-extenders;
  use binary-manager;
  use binary-builder;
  use binary-outputter;

end module;
