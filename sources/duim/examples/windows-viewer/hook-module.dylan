Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Windows hook
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module windows-hook
  use common-dylan;
  use simple-io;

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
