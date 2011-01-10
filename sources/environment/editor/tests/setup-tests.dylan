Module:    test-editor-manager-common
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Variables which control the semantics of the tests.
define variable *editor-call* :: <function> = editor-call;
/* [obsolete?]
define function test-editor-call (#rest args) => (success? :: <boolean>)
  apply(*editor-call*, args);
  #t
end;
*/

define variable *test-file-path* :: <string> = "";
define variable *test-file-name* :: <string> = "";
