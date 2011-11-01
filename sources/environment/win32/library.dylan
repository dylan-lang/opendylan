Module:    Dylan-User
Synopsis:  Win32 Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-environment
  use functional-dylan;
  use system;

  use release-info;

  use c-ffi;
  use win32-common;
  use win32-user;
  use win32-shell;

  use duim;
  use win32-duim;

  // Use these libraries just to get bitmap and color constants
  use deuce;
  use duim-deuce;

  use environment-commands;
  use environment-application-commands;
  use environment-property-pages;
  use environment-deuce;
  use environment-protocols;
  use environment-framework;
  use environment-tools;
  use environment-project-wizard;
  use environment-debugger;
  use environment-profiler;
  use environment-internal-commands;

  use editor-manager;
  use editor-deuce-backend;

  use source-control-manager;
  use source-control-vss-backend;

  // Back-ends
  use dfmc-back-end-implementations;

  // Project manager plug-ins
  use motley;
  use tool-scepter;
  use tool-parser-generator;

  // Plug-in for Remote Debugging
  use remote-access-path;

  export win32-environment;
end library win32-environment;
