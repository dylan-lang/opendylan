Module:    dylan-user
Synopsis:  Program to generate GUID numbers.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library create-id
  use common-dylan;
  use system;
  use COM;
end library;

define module create-id
  use common-dylan;
  use operating-system;
  use simple-io;
  use COM;
end module;
