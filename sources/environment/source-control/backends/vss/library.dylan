Module:    dylan-user
Synopsis:  Visual SourceSafe backend for Dylan environment
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library source-control-vss-backend
  use common-dylan;
  use system;
  use commands;
  use c-ffi;
  use win32-registry;
  use ole-automation;
  use SourceSafeTypeLib;
  use source-control-manager;
  export source-control-vss-backend;
end library source-control-vss-backend;
