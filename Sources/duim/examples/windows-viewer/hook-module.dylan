Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Windows hook
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module windows-hook
  use functional-dylan;
  use simple-format;

  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user;

  create $false, $true;

  create $message-id,
         $description-id;

  create $hook-class,
         $hook-window;

  create $application,
         $version,
         $copyright,
         $dll-name;

  export Hook-Proc;
end module windows-hook;
