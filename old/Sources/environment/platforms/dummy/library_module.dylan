Module:    Dylan-User
Synopsis:  Environment-Editor Interface pulled together
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library editor-manager
  use functional-dylan;

  export editor-manager;
end library editor-manager;

define module editor-manager
  use functional-dylan;

  export editor-call;

  export editor-register-all-factories,
	 editor-init;
end module editor-manager;
