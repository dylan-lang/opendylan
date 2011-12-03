Module:    Dylan-User
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library test-editor-manager-common
  use common-dylan;
  use io;
  use gui-testworks; //-plus;

  use editor-common;
  use editor-exe-backend;
  use editor-manager;

  export test-editor-manager-common;
end library test-editor-manager-common;

define module test-editor-manager-common
  use common-dylan;
  use gui-testworks; //-plus;

  use environment-to-editor-hacker;
  use editor-exe-backend;
  use editor-manager;

  export *editor-call*, // Assign a <function> for "editor-call" to this. 
	 *test-file-path*, // Assign <string>s to these two, to point to
	 *test-file-name*, // a text file which exists.
	 suite-test-frontend,
	 suite-test-default-backend,
	 suite-test-all-backends;
end module test-editor-manager-common;
