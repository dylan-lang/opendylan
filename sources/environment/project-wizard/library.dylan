Module:    dylan-user
Author:    Hugh Greene, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-project-wizard
  use common-dylan;
  use duim;
  use win32-duim;
  use win32-user;
  use io;
  use system;
  use collections;

  use release-info;
  use environment-manager;
  use build-system;
  use motley;

  export environment-project-wizard;
end library;

