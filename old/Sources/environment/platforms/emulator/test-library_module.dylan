Module:    Dylan-User
Synopsis:  Environment-Editor Interface Test -- platform-specific part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library test-editor-manager
  use dylan;
  use functional-extensions;
//  use io;
  use testworks-plus;

  use test-editor-manager-common;

  export test-editor-manager;
end library test-editor-manager;

define module test-editor-manager
  use dylan;
  use functional-extensions;
//  use streams;
  use testworks-plus;

  use test-editor-manager-common;
end module test-editor-manager;
