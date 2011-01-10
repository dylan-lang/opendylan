Module:    Dylan-User
Synopsis:  Environment-LispWorks Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module editor-lispworks-backend
  use functional-dylan;
  use commands;

  use editor-manager-internals;

  export <lispworks-editor>;
end module editor-lispworks-backend;
