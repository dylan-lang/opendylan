Module:    dylan-user
Author:    Hugh Greene, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This library is the same as the normal ones, except we don't use motley.

define library basic-environment-project-wizard
  use functional-dylan;
  use duim;
  use win32-duim;
  use io;
  use system;
  use collections;

  use release-info;
  use environment-manager;
  use build-system;

  export environment-project-wizard;
end library;
