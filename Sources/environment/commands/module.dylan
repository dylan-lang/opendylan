Module:    Dylan-User
Synopsis:  The commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module command-lines
  use functional-dylan;
  use dylan-extensions,
    import: { <keyboard-interrupt>,
	      keyboard-interrupt?, keyboard-interrupt?-setter,
	      keyboard-interrupt-polling?-setter,
	      keyboard-interrupt-polling-thread?-setter };
  use commands;
  use file-system;
  use operating-system;
  use streams;
  use format;
  use locators;

  // Command line servers
  export <command-line-server>,
         <command-line-server-error>,
         server-context,
         server-input-stream,
         server-output-stream,
         server-debugger?, server-debugger?-setter,
         server-profile-commands?, server-profile-commands?-setter;

  // Utilities
  export message,
         display-condition,
         print-table;

  // Contexts
  export <server-context>,
         context-server,
         context-command-group,
         context-command-prefix;

  // Command info
  export \command-line-definer,
         \command-group-definer,
         <command-group>,
         <command-line>,
         <basic-command-line>,
         add-command-group,
         class-for-command-line,
         collect-command-info,
         command-complete?,
         ensure-command-available,
         command-info-name,
         command-info-title,
         command-info-summary,
         command-info-documentation;

  // Properties
  export \command-property-definer,
         <command-property>,
         <set-property-error>,
         context-property-setter,
         context-named-property-setter,
         ensure-property-available,
         set-property,
         set-named-property,
         show-property,
         show-named-property,
         set-error;

  // States
  export register-state-type,
         describe-state,
         find-state-value;

  // Command lines
  export <command-error>,
         command-error,
         command-line-for-command,
         command-line-for-command-class,
         command-line-loop,
         display-command-prompt,
         execute-command-line,
         execute-server-command;

  // Parsing
  export <parse-error>,
         parameter-type-name,
         parse-command-line,
         parse-next-argument,
         parse-next-word,
         parse-error,
         $keyword-list-type;

  // Basic commands
  export $basic-command-group,
         <exit-command>,
         <help-command>,
         display-help;

  // Property commands
  export $property-command-group,
         <show-property-command>,
         <set-property-command>;

  // File system commands
  export $system-command-group;
end module command-lines;

define module environment-commands
  use environment-imports;
  use environment-protocols,
    exclude: { <optional-parameter>,
	       parameter-name,
	       parameter-type,
	       parameter-keyword };
  use environment-manager;
  use environment-reports;

  use commands;
  use command-lines;

  // Environment context
  export <environment-context>,
         context-notification,
         context-project;
       
  // Project context
  export <project-context>,
         context-module,
         context-properties, context-properties-setter,
         context-project-context;

  // Environment commands
  export $environment-command-group,
         <environment-command>,
         command-title,
         make-environment-command-line-server;

  // Environment command libraries
  export <command-library>,
         register-default-command-library,
         command-library-prompt,
         command-library-default-command-class;

  // Project commands
  export $project-command-group,
         <project-command>,
         <open-project-command>,
         <import-project-command>,
         <close-project-command>;

  // Build commands
  export $build-command-group,
         <build-project-command>,
         <link-project-command>;

  // Properties
  export <environment-property>,
         <project-property>;
end module environment-commands;
