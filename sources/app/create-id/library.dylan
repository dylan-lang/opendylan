Module:    dylan-user
Synopsis:  Program to generate GUID numbers.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library create-id
  use functional-dylan;
  use system;
  use COM;
end library;

define module create-id
  use functional-dylan;
  use operating-system;
  use simple-format;
  use COM;
end module;
