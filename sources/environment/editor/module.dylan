Module:    Dylan-User
Synopsis:  Environment-Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module editor-manager
  create <editor>,
	 editor-name,
	 editor-title,
	 find-editor-of-class,
	 register-editor-class,
	 unregister-editor-class,
	 current-editor,
	 current-editor-setter;

  create <editor-condition>,
	 <editor-error>,
	 <editor-warning>,
	 <editor-unavailable>,
	 <editor-unsupported-command>,
	 <editor-command-failed>;

  create <editor-command>,
	 <editor-window-command>,
	 <editor-open-command>,
	 <editor-close-command>,
	 <editor-file-command>,
	 <editor-open-file-command>,
	 <editor-new-file-command>,
	 <editor-close-file-command>,
	 <editor-insert-text-command>,
	 <editor-delete-text-command>,
	 <editor-project-command>,
	 <editor-edit-definitions-command>,
	 <editor-save-files-command>;
end module editor-manager;

define module editor-manager-internals
  use functional-dylan;
  use simple-format;
  use threads;
  use commands;
  use editor-manager, export: all;

  export class-for-editor-command,
	 editor-command-title,
	 editor-command-frame,
	 editor-command-pathname,
	 editor-command-start-line,
	 editor-command-start-column,
	 editor-command-end-line,
	 editor-command-end-column,
	 editor-command-text,
	 editor-command-project,
	 editor-command-definitions,
	 editor-command-pathnames,
	 editor-command-reason,
	 editor-command-exit-label;

  export *current-editor*,
	 note-editor-selected;
end module editor-manager-internals;
