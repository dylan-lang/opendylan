module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library streams-doss
  use dylan;
  use streams;
  use doss;
end;

define module streams-doss
  use dylan;
  use functional-dylan;
  use core-stream-accessors,
        import: {<external-file-accessor>,
                 file-descriptor};
  use streams;
  use mop;
  use doss;
end;
