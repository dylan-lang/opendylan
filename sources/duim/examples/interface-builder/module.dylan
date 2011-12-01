Module:       dylan-user
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module interface-builder
  use common-dylan, exclude: { format-to-string };
  use threads;
  use streams;
  use file-system;
  use format;

  use duim;

  export <interface-builder>,
         start-interface-builder;
end module interface-builder;
