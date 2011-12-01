Module:    Dylan-user
Author:    Bill Chiles, Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-debugger
  use common-dylan;

  use duim;
  use deuce;
  use duim-deuce;

  use environment-protocols;
  use environment-manager;
  use environment-framework;
  use environment-tools;
  use environment-deuce;

  // For stop-reasons
  use dfmc-environment-application;

  export environment-debugger;
end library environment-debugger;
