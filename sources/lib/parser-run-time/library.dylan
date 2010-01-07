Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library parser-run-time
  use functional-dylan;
  export 
    parser-run-time;
end library;

define module parser-run-time
  use functional-dylan;
  use dylan-extensions,
    import: { vector-element, vector-element-setter, pointer-id? };
  use simple-format;
  export
    <parser>, run-parser;
end module;
