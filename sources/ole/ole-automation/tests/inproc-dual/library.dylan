Module:    Dylan-User
Synopsis:  Tests a dual interface as an in-process server.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library inproc-dual
  use functional-dylan;
  use OLE-Automation;
end library;

define module inproc-dual
  use functional-dylan;
  use simple-format;
  use OLE-Automation;
end module;
