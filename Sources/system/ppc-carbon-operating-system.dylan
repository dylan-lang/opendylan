Module:       system-internals
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $architecture-little-endian? :: <boolean> = #f;

define constant $machine-name = #"ppc";
define constant $os-name      = #"carbon";

define constant $os-variant = $os-name;
define constant $os-version = "Unknown";

//---*** andrewa: is this correct?
define constant $command-line-option-prefix = '/';

define function command-line-option-prefix
    () => (prefix :: <character>)
  $command-line-option-prefix
end function command-line-option-prefix;

///---*** NOTE: Provide a non-null implementation when time permits...
define function login-name () => (name :: false-or(<string>))
  #f
end function login-name;

///---*** NOTE: Provide a non-null implementation when time permits...
define function login-group () => (group :: false-or(<string>))
  #f
end function login-group;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-name () => (name :: false-or(<string>))
  #f
end function owner-name;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-organization () => (organization :: false-or(<string>))
  #f
end function owner-organization;

/// Use the MPW convention of a comma-separated list ...
define constant $environment-variable-delimiter = ',';

define variable *environment-variables* = make(<string-table>);

define function environment-variable
    (name :: <byte-string>) => (value :: false-or(<byte-string>))
  element(*environment-variables*, name, default: #f)
end function environment-variable;

define function environment-variable-setter
    (new-value :: false-or(<byte-string>), name :: <byte-string>)
 => (new-value :: false-or(<byte-string>))
  if (new-value)
    *environment-variables*[name] := new-value
  else
    remove-key!(*environment-variables*, name);
    #f
  end
end function environment-variable-setter;

define function load-library
    (name :: <string>) => (module)
  #f
end function load-library;

define function create-application-event
    (event :: <string>) => (event-object :: <machine-word>)
  as(<machine-word>, 0)
end function create-application-event;

define constant $INFINITE_TIMEOUT = -1;

define function wait-for-application-event
    (event-object :: <machine-word>, #key timeout :: <integer> = $INFINITE_TIMEOUT)
 => (success? :: <boolean>)
  #f
end function wait-for-application-event;

define function signal-application-event
    (event :: <string>) => (success? :: <boolean>)
  #f
end function signal-application-event;

define function run-application (command :: <string>,
				 #key under-shell? = #f,
				      inherit-console? = #t,
				      activate? = #t,
				      minimize? = #f,
				      outputter :: false-or(<function>),
				      asynchronous? = #f)
 => (status :: <integer>)
  -1
end function run-application;
