Module:    Dylan-User
Synopsis:  Environment-Deuce Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module editor-deuce-backend
  use functional-dylan;
  use threads;
  use commands;
  use locators;

  use duim,
    import: { <frame>,
	      apply-in-frame,
	      raise-frame,
	      deiconify-frame,
	      exit-frame };
  use deuce-internals,
    exclude: { <editor>,
	       browse-class,
               browse-object,
	       browse-function,
	       show-documentation,
	       position },
    rename: { execute-command => deuce/execute-command,
	      undo-command    => deuce/undo-command,
	      redo-command    => deuce/redo-command };
  use duim-deuce;

  use editor-manager-internals;
  use environment-protocols,
    import: { <project-object> };
  use environment-manager;
  use environment-tools;
  use environment-deuce;

  export <deuce-editor>;
end module editor-deuce-backend;
