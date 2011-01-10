Module:    test-editor-manager-common
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A suite to test every aspect of the front-end of the editor
/// call-out mechanism.

/// WARNING
// Some tests in this suite rebind module variables used by later
// tests in the suite.  Therefore the tests here should not be relied
// upon in isolation or in a different order.
/// WARNING

define function test-frontend-cleanup ()
  editor-deregister-factory(*test-custom-editor-factory*);
  editor-uncache-instances(#"test-custom-editor");
end function;

define suite suite-test-frontend
    (description:
       "Test the front (environment) end of the call-out mechanism.",
     cleanup-function: test-frontend-cleanup)
  test test-editor-command-factories;
  test test-editor-substitution;
  test test-editors;
  test test-editor-call-initialize;
  test test-editor-translate-command;
  test test-editor-call;
end suite;
