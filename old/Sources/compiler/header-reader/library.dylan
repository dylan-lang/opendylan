Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library header-reader
  use dylan;
  use streams;
  export header-reader;
end library;

define module header-reader
  use dylan;
  use streams;
  export
    read-file-header;
end module;

// eof
