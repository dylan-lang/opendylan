Module:    Dylan-User
Synopsis:  Environment-Vi Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module editor-vi-backend
  use common-dylan;
  use commands;

  use editor-manager-internals;
  use editor-exe-backend;

  export <vi-editor>;
end module editor-vi-backend;
