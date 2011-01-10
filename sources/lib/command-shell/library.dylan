Module:    dylan-user
Synopsis:  Generic purpose interactive shell functionality
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library command-shell
  use functional-dylan;
  use system;
  use io;

  // Use the internal release info
  use release-info;

  export command-shell;
end library;

define module command-args
  use functional-dylan;
  use threads;
  use operating-system;
  use streams;
  use standard-io;
  use format;
  use format-out;
  use print;
  export
    <command>,
    <basic-command>,
    <command-alias>,
    command-id,
    command-name,
    command-alias,
    command-arguments,
    command-function, command-function-setter,
    command-hidden?,
    command-description,
    command-documentation,
    <command-argument>,
    command-argument-keyword,
    command-argument-flags,
    <illegal-option>,
    <missing-value>,
    <illegal-syntax>,
    <illegal-argument>,
    find-argument,
// for the sake of the emulator
    register-argument,
    command-argument-spec-definer,
    command-argument-definer,
    command-flag-definer,
    build-command-call,
    print-usage,
    is-option?, parse-option;
//    tokenize-command-string;
end module;

define module shell
  use functional-dylan;
  use threads;
  use operating-system, prefix: "os/", export: { os/tokenize-command-string };
  use streams;
  use standard-io;
  use format;
  use format-out;
  use release-info;
  use command-args;
  export
    <command-loop>,
    <command-loop-continuation>,
    command-table,
    display-banner,
    exit-command-loop,
    level,
    <end-of-loop>,
// exported for emulator's sake
    register-command,
    register-command-alias,
//
    *prompt-prefix*, *top-prompt*, *top-level-loop*,
    dylan-banner,

    %source, %source-setter, %sink, %sink-setter,

    user-message, ask-user,

    command-list,
    display-help,
    display-command-help,
    imported-shell-command-definer,
    dylan-shell-command-definer,
    shell-command-definer,
    shell-command-alias-definer,
    run-command-loop,
    run-command-loop-internal,
    execute-command;
end module;

define module command-shell
  use functional-dylan;
  use threads;
  use streams;
  use standard-io;
  use format;
  use format-out;
  use print;
  use command-args, export: all;
  use shell, export: all;
  export
    condition-handler,
    restart-handler,
    restart-decliner,
    print-restart-options,
    signal-error,
    invoke-restart,
    display-condition,


    $help-command, $help-all-command,
    $exit-command,
    $continue-command, $debug-command, $abort-command, $restarts-command


  ;
end module;
    

