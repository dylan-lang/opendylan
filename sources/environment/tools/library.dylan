Module:    Dylan-User
Synopsis:  Environment tools
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-tools
  use environment-protocols;
  use environment-manager;
  use editor-manager;
  use source-control-manager;
  use environment-reports;
  use environment-framework;
  use environment-server;

  use duim-core;

  use deuce;
  use duim-deuce;	// We need to use duim-deuce to share its images

  export environment-tools;
end library environment-tools;
