Module:    Dylan-User
Synopsis:  Environment-Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library editor-manager
  use common-dylan;
  use commands;

  export editor-manager,
	 editor-manager-internals;
end library editor-manager;
