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

  use editor-common, export: all;

  use editor-vi-backend;
  use editor-emacsserver-backend;
  use editor-gnuserv-backend;
  use editor-lispworks-backend;
  use editor-deuce-backend;

  export editor-manager;
end library editor-manager;

define module editor-manager
  use functional-dylan;

  use environment-to-editor-user, export: all;

  use editor-vi-backend;
  use editor-emacsserver-backend;
  use editor-gnuserv-backend;
  use editor-lispworks-backend;
  use editor-deuce-backend;

  export editor-register-all-factories,
	 editor-init;
end module editor-manager;
