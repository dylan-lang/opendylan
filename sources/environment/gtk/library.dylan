Module:    Dylan-User
Synopsis:  GTK Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk-environment
  use functional-dylan;
  use system;

  use release-info;

  use c-ffi;
  use gtk;

  use duim-core;
  use gtk-duim;

  // Use these libraries just to get bitmap and color constants
  use deuce;
  use duim-deuce;

  use dylan-environment;
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
  use source-control-hope-backend;

  // Project manager plug-ins
  use motley;
  use tool-scepter;
  use tool-parser-generator;

  // Plug-in for Remote Debugging
  use remote-access-path;

  export gtk-environment;
end library gtk-environment;

