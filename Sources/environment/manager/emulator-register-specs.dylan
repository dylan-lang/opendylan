Module:    environment-manager
Author:    Hugh Greene
Synopsis:  Functions the Environment provides to external callers. -- EMU
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

begin
  register-command-function-spec
    (#"properties", 4, #f,
     vector(<object>, <string>, <string>, <vector>, <string>));
  register-command-function-spec
    (#"complete", 4, #t,
     vector(<object>, <string>, <string>, <vector>, <string>));
  register-command-function-spec
    (#"edit-definitions", 4, #f,
     vector(<object>, <string>, <string>, <vector>, <string>));
  register-command-function-spec
    (#"compile", 5, #f,
     vector(<object>, <string>, <string>, <vector>, <string>, <symbol>));
  register-command-function-spec
    (#"documentation", 4, #f,
     vector(<object>, <string>, <string>, <vector>, <string>));
  register-command-function-spec
    (#"browse", 4, #f,
     vector(<object>, <string>, <string>, <vector>, <string>));
  register-command-function-spec
    (#"browse-type", 4, #f,
     vector(<object>, <string>, <string>, <vector>, <string>));
end;
