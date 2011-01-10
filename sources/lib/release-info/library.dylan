Module:    Dylan-User
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library release-info
  use functional-dylan;
  use system;
  use io;
  use file-source-records;

  export release-info;
  export release-info-internals;
end library release-info;
