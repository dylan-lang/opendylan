module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library cgi-pizza
  use dylan;
  use io;
  use cgi;
end library;

define module cgi-pizza
  use dylan;
  use format-out;
  use forms;
  use environment-variables;
end module;
