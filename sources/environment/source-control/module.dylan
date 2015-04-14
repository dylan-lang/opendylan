Module:    Dylan-User
Synopsis:  Environment-Source Control Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module source-control-manager
  create <source-control-system>,
         sccs-name,
         sccs-label,
         sccs-title,
         default-source-control-system,
         find-source-control-system-named,
         register-source-control-class,
         unregister-source-control-class,
         current-source-control-system,
         current-source-control-system-setter;

  create <source-control-options>,
         <source-control-login-options>,
         <source-control-command-options>,
         command-pathname;

  create <source-control-option>,
         option-label,
         option-keyword,
         option-type,
         option-getter,
         option-documentation,
         option-default,
         option-required?;

  create <source-control-info>,
         <source-control-login-info>,
         <source-control-command-info>,
         command-title,
         command-class,
         command-options,
         source-control-login-info,
         source-control-login,
         source-control-command-info;

  create <source-control-condition>,
         <source-control-error>,
         <source-control-warning>,
         <source-control-unavailable>,
         <source-control-unsupported-command>,
         <source-control-command-failed>;

  create <source-code-control-command>,
         sccs-command-implemented?,
         <sccs-claim-command>,     *claim-command-string*,
         <sccs-check-out-command>, *check-out-command-string*,
         <sccs-check-in-command>,  *check-in-command-string*,
         <sccs-abandon-command>,   *abandon-command-string*,
         <sccs-merge-command>,     *merge-command-string*,
         <sccs-diff-command>,      *diff-command-string*,
         <sccs-report-command>,    *report-command-string*,
         <sccs-add-command>,       *add-command-string*,
         <sccs-remove-command>,    *remove-command-string*;
end module source-control-manager;

define module source-control-manager-internals
  use dylan;
  use common-extensions;
  use threads;
  use locators;
  use settings;
  use commands;
  use release-info;
  use source-control-manager, export: all;

  export class-for-sccs-command,
         sccs-command-options,
         sccs-command-title;

  export *current-source-control-system*,
         note-source-control-system-selected;
end module source-control-manager-internals;
