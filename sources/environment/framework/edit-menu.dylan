Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Edit menu

define command-table *edit-command-table* (*global-command-table*)
  include *undo-command-table*;
  include *clipboard-command-table*;
  include *selection-command-table*;
  include *searching-command-table*;
end command-table *edit-command-table*;
