Module:    dylan-user
Synopsis:  Microsoft Visual Source Safe Type Library
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library SourceSafeTypeLib
  use functional-dylan;
  use system;
  use c-ffi;
  use win32-common;
  use ole-automation;
  export SourceSafeTypeLib;
end library SourceSafeTypeLib;

