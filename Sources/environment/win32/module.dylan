Module:    Dylan-User
Synopsis:  Win32 Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module win32-environment
  use environment-imports;	// this gets functional-dylan
  use operating-system,
    import: { application-name,
	      application-arguments,
	      run-application,
	      exit-application,
              environment-variable, environment-variable-setter },
    rename: { run-application => run-os-application };
  use file-system,
    import: { file-exists?,
              working-directory };

  use licensing;

  use c-ffi;
  use win32-common,
    rename: { <point> => w/<point> };
  use win32-user,
    exclude: { hIcon-value };
  use win32-shell,
    import: { $SHGFI-ICON,
              $SHGFI-SMALLICON,
              $SHGFI-LARGEICON,
              SHGetFileInfo,
              ShellExecute,
	      <LPSHFILEINFOA>,
	      <SHFILEINFO>,
	      hIcon-value };

  use duim-internals,
    exclude: { position };
  use win32-duim;

  // Use these modules just to get bitmap and color constants
  use deuce-internals,
    import: { make-color,
	      $region-marking-color, 
	      $dylan-definition-line-color },
    rename: { make-color  => deuce/make-color };
  use duim-deuce;

  use environment-protocols,
    exclude: { application-arguments };
  use environment-framework;
  use environment-tools;
  use editor-manager;
  use source-control-manager;
  use environment-debugger;
  use environment-project-wizard,
    import: { $check-mark-icon,
	      $uncheck-mark-icon };

  use editor-deuce-backend;
end module win32-environment;
