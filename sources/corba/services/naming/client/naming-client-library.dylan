Module:    dylan-user
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library naming-client
  use common-dylan;
  use dylan-orb;
  use naming-stubs;
  export
    naming-client;
end library naming-client;

define module naming-client
  use common-dylan;
  use dylan-orb;
  use naming-stubs,
    export: all;
end module naming-client;
