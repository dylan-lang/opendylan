Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library console-scepter
  use functional-dylan;
  use io;
  use system;
  use scepter;
  use scepter-core;
end library;

define module console-scepter
  use functional-dylan;
  use simple-debugging;
  use file-system;
  use format;
  use locators;
  use operating-system;
  use standard-io;
  use scepter;
  use scepter-driver;
  use scepter-front-end;
  use scepter-back-end;
  use scepter-error;
  use scepter-utilities;
end module;
