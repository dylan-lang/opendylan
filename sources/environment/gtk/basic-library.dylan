Module:    Dylan-User
Synopsis:  GTK Environment Basic Edition
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library basic-gtk-environment
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
  use basic-environment-project-wizard;
  use environment-debugger;

  use editor-manager;
  use editor-deuce-backend;

  use source-control-manager;

  export gtk-environment;
end library basic-gtk-environment;
