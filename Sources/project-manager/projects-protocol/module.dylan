Module:    dylan-user
Synopsis:  Project manager protocol library
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module projects-protocol
  // Constants
  create <library-name>,
         <project-name>,
         <processor>,
         <operating-system>,
         <project-target-type>;

  // Workspaces
  create <project-workspace>,
         default-workspace, default-workspace-setter,
         workspace-processor,
         workspace-operating-system,
         workspace-projects,
         workspace-build-request,
         find-project,
         open-project;

  // Projects
  create <project>,
         project-name,
         project-title,
         project-workspace,
         build-project,
         open-project-database,
         project-read-only?,
         project-can-be-closed?, project-can-be-closed?-setter,
         project-file-location,
         project-file-date, project-file-date-setter,
         project-library-name, project-library-name-setter,
         project-source-files, project-source-files-setter,
         project-minor-version, project-minor-version-setter,
         project-major-version, project-major-version-setter,
         project-target, project-target-setter,
         project-targets, project-targets-setter,
         project-user-settings, project-user-settings-setter,
         save-project,
         save-project-as-lid-file;

  // Build targets
  create <build-target>,
         <target-source-record>,
         remove-target-build-products,
         target-project,
         target-read-only?, target-read-only?-setter,
         target-source-files,
         target-source-records,
         target-subtargets, target-subtargets-setter,
         target-canonical-source-records,
         target-file-canonical-source-record,
         target-workspace;

  // Target settings
  create target-processor, target-processor-setter,
	 target-operating-system, target-operating-system-setter,
	 target-compilation-mode, target-compilation-mode-setter,
	 target-copy-sources?, target-copy-sources?-setter,
	 target-build-directory, target-build-directory-setter,
	 target-database-directory, target-database-directory-setter,
	 target-profile-directory, target-profile-directory-setter,
	 target-bin-directory, target-bin-directory-setter,
	 target-release-directory, target-release-directory-setter,
	 target-library-loose-bindings, target-library-loose-bindings-setter,
	 target-library-tight-bindings, target-library-tight-bindings-setter,
	 target-type, target-type-setter,
	 target-filename, target-filename-setter,
	 target-linker, target-linker-setter,
	 target-linker-options, target-linker-options-setter,
	 target-base-address-string, target-base-address-string-setter,
	 target-debug-command, target-debug-command-setter,
	 target-debug-arguments, target-debug-arguments-setter,
	 target-debug-machine, target-debug-machine-setter,
	 target-debug-directory, target-debug-directory-setter,
	 target-start-function, target-start-function-setter;

  // Build requests
  create <build-request>,
         build-target,
         build-compile?,
         build-link?,
         build-release?,
         build-subprojects?,
         build-clean?,
         build-copy-sources?,
         build-compilation-mode,
         build-save?,
         build-linker,
         build-target-type,
         build-unify?,
         build-exports?,
         build-abort-reason,
         build-debug-output,
         build-progress-callback;

  // File extensions
  create dylan-file-extension,
         project-file-extension,
         lid-file-extension,
         database-file-extension,
         profile-file-extension,
         project-cache-file-extension;

  // File information
  create <file-information>,
         file-location;

  // Basic file information
  create <basic-file-information>,
         <dylan-file-information>,
         <project-file-information>,
         <linker-file-information>,
         <release-file-information>;

  // Warnings
  create <project-warning>,
         <project-serious-warning>,
         warning-project;

  // Build warnings
  create <build-warning>,
         <build-serious-warning>,
end module projects-protocol;

define module project-tools
  create <import-file-information>,
         read-file-information,
         read-input-files,
         generate-files;

  create <specification-file-information>,
         file-input-files;
end module project-tools;

define module project-templates
  create <project-template>;
end module project-templates;

define module projects-protocol-internals
  use functional-dylan;
  use machine-words;
  use operating-system;
  use file-system;
  use date;
  use streams;
  use format;
  use format-out;
  use locators;
  use print;

  use release-info;
  use build-system;
  use path-utilities;
  use source-records;
  use file-source-records;

  use projects-protocol, export: all;
  use project-tools,     export: all;
  use project-templates, export: all;

  // Useful constants
  export $project-file-format-version;

  // Workspaces
  export \with-workspace-build,
         make-workspace-project,
         make-workspace-build-target,
         note-workspace-build-operation-started,
         note-workspace-build-operation-finished;

  // Projects
  export <basic-project>,
         project-owners,
         read-project-file,
         project-source-files-of-type;

  // Building
  export <basic-build-target>,
         <basic-build-state>,
         do-build-targets,
         target-build-state, target-build-state-setter,
         targets-to-recompile,
         targets-to-relink,
         compile-target-library,
         generate-link-makefile,
         link-target-executable;

  // Progress
  export build-message,
         note-build-progress;

  // Warnings
  export project-error,
         project-warning,
         project-serious-warning,
         resignal-project-warning;

  // Build warnings
  export build-warning,
         build-serious-warning,
         resignal-build-warning;

  // Project file IO
  export read-lid-data,
         lid-project-settings,
         make-lid-build-target,
         write-comment,
         write-lid-library-info,
         write-lid-linker-info,
         write-list-value,
         write-source-files;
end module projects-protocol-internals;
