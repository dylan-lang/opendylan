Module:    source-control-manager-internals
Synopsis:  Environment-Source Control Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Source control system commands

define open abstract class <source-code-control-command> (<command>)
  sealed each-subclass slot sccs-command-title :: false-or(<string>) = #f,
    init-keyword: title:;
  sealed constant slot sccs-command-options :: <source-control-command-options>,
    required-init-keyword: options:;
end class <source-code-control-command>;

define method make
    (class :: subclass(<source-code-control-command>), #rest initargs, #key server)
 => (command :: <source-code-control-command>)
  let sccs  = server | current-source-control-system();
  let class = class-for-sccs-command(sccs, class);
  if (class)
    apply(next-method, class, server: sccs, initargs)
  else
    error(make(<source-control-unsupported-command>,
	       source-control: sccs, command: class))
  end
end method make;

define method initialize
    (command :: <source-code-control-command>, #key pathname) => ()
  ignore(pathname);
  next-method()
end method initialize;

define open generic class-for-sccs-command
    (sccs :: <source-control-system>, class :: subclass(<source-code-control-command>))
 => (class :: false-or(subclass(<source-code-control-command>)));

define method class-for-sccs-command
    (sccs :: <source-control-system>, class :: subclass(<source-code-control-command>))
 => (class == #f)
  #f
end method class-for-sccs-command;

define function sccs-command-implemented?
    (sccs :: <source-control-system>, class :: subclass(<source-code-control-command>))
 => (implemented? :: <boolean>)
  class-for-sccs-command(sccs, class) ~= #f
end function sccs-command-implemented?;


/// Source control options

define open abstract class <source-control-options> (<object>)
end class <source-control-options>;

define open abstract class <source-control-login-options> (<source-control-options>)
  keyword pathname:;
end class <source-control-login-options>;

define open abstract class <source-control-command-options> (<source-control-options>)
  constant slot command-pathname :: false-or(<file-locator>) = #f,
    init-keyword: pathname:;
end class <source-control-command-options>;

define class <source-control-option> (<object>)
  constant slot option-label :: <string>,
    required-init-keyword: label:;
  constant slot option-keyword :: <symbol>,
    required-init-keyword: keyword:;
  constant slot option-type :: type-union(<type>, <symbol>),
    required-init-keyword: type:;
  constant slot option-getter :: <function>,
    required-init-keyword: getter:;
  constant slot option-documentation :: false-or(<string>) = #f,
    init-keyword: documentation:;
  constant slot option-default :: <object> = #f,
    init-keyword: default:;
  constant slot option-required? :: <boolean> = #f,
    init-keyword: required?:;
end class <source-control-option>;

define open abstract class <source-control-info> (<object>)
  constant slot command-title :: <string>,
    required-init-keyword: title:;
  constant slot command-class :: subclass(<source-control-options>),
    required-init-keyword: class:;
  constant slot command-options :: <vector>,
    required-init-keyword: options:;
end class <source-control-info>;

define class <source-control-login-info> (<source-control-info>)
end class <source-control-login-info>;

define class <source-control-command-info> (<source-control-info>)
end class <source-control-command-info>;

define open generic source-control-login-info
    (sccs :: <source-control-system>,
     command-class :: subclass(<source-code-control-command>),
     #key defaults :: false-or(<source-control-login-options>),
          pathname :: false-or(<file-locator>))
 => (options :: false-or(<source-control-login-info>));

define open generic source-control-login
    (sccs :: <source-control-system>, options :: <source-control-login-options>)
 => (logged-in? :: <boolean>, message :: false-or(<string>));

define open generic source-control-command-info
    (sccs :: <source-control-system>,
     command-class :: subclass(<source-code-control-command>),
     #key defaults :: false-or(<source-control-command-options>),
          pathname :: false-or(<file-locator>))
 => (options :: <source-control-command-info>);

define method source-control-login-info
    (sccs :: <source-control-system>,
     command-class :: subclass(<source-code-control-command>),
     #key defaults :: false-or(<source-control-login-info>),
          pathname :: false-or(<file-locator>))
 => (options == #f)
  #f
end method source-control-login-info;


/// Command patterns

// The idea is that source control back-ends implement a method for
// 'note-source-control-system-selected' that fills in the default values
// for the command strings.  We use Settings to save the settings for
// (1) which source control back-end is being used, and (2) the strings
// for each back-end.  We save only the back-end strings so that they
// persist even when somebody changes between different back-ends.

define variable *claim-command-string*
  = "checkout -claim soft $(compound)$(unit)$(branch)$(reason)";

define open abstract class <sccs-claim-command> (<source-code-control-command>)
  keyword title: = "Checks out the files from source code control";
end class <sccs-claim-command>;


define variable *check-out-command-string*
  = "checkout $(compound)$(unit)$(branch)";

define open abstract class <sccs-check-out-command> (<source-code-control-command>)
  keyword title: = "Gets the latest version of files from source code control";
end class <sccs-check-out-command>;


define variable *check-in-command-string*
  = "checkin $(compound)$(unit)$(branch)$(reason)";

define open abstract class <sccs-check-in-command> (<source-code-control-command>)
  keyword title: = "Checks in the files to source code control";
end class <sccs-check-in-command>;


define variable *abandon-command-string*
  = "abandon $(compound)$(unit)$(branch)";

define open abstract class <sccs-abandon-command> (<source-code-control-command>)
  keyword title: = "Undoes the checkout of files, without retaining changes";
end class <sccs-abandon-command>;


define variable *merge-command-string*
  = "merge $(compound)$(unit)$(branch)$(reason)";

define open abstract class <sccs-merge-command> (<source-code-control-command>)
  keyword title: = "Merges the files with the latest versions under source code control";
end class <sccs-merge-command>;


define variable *diff-command-string*
  = "diff $(compound)$(unit)$(branch)";

define open abstract class <sccs-diff-command> (<source-code-control-command>)
  keyword title: = "Shows the changes made to a file since it was checked out";
end class <sccs-diff-command>;


define variable *report-command-string*
  = "report $(compound)$(unit)$(branch)";

define open abstract class <sccs-report-command> (<source-code-control-command>)
  keyword title: = "Shows the history of a file or files under source code control";
end class <sccs-report-command>;


define variable *add-command-string*
  = "add $(compound)$(unit)$(branch)$(reason)";

define open abstract class <sccs-add-command> (<source-code-control-command>)
  keyword title: = "Puts project files under source code control";
end class <sccs-add-command>;


define variable *remove-command-string*
  = "remove $(compound)$(unit)$(branch)$(reason)";

define open abstract class <sccs-remove-command> (<source-code-control-command>)
  keyword title: = "Removes project files from source code control";
end class <sccs-remove-command>;
