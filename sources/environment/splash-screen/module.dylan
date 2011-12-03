Module:    dylan-user
Synopsis:  environment splash screen
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-splash-screen
  use common-dylan;
  use threads;
  use duim;
  use win32-duim;

  export $splash-screen-bitmap,
         display-splash-screen,
         fork-splash-screen;
end module environment-splash-screen;
