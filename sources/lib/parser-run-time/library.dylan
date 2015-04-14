Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library parser-run-time
  use dylan;
  use common-dylan;
  export
    parser-run-time;
end library;

define module parser-run-time
  use common-dylan;
  use dylan-extensions,
    import: { vector-element, vector-element-setter, pointer-id? };
  use simple-format;
  export
    <parser>, run-parser;
end module;
