Module:    dylan-user
Synopsis:  Definition of the Hello World library and module
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library hello-world
  use common-dylan;
  export hello-world;
end library;

define module hello-world
  use common-dylan;
  use simple-io;
end module;
