Module:    Dylan-User
Synopsis:  Environment Protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-protocols
  use functional-dylan;
  use io;
  use source-records;
  use file-source-records;
  use system;
  use channels;
  use release-info;
  use collections;
  use memory-manager;
  use cl, import: { cl-strings };

  export environment-protocols;
  export environment-imports;
end library environment-protocols;
