Module:    source-control-manager-internals
Synopsis:  Environment-Source Control Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Source control systems

define variable *all-source-control-systems* :: <stretchy-object-vector>
    = make(<stretchy-vector>);

// 'sccs' = "source code control system"
define open abstract primary class <source-control-system> (<object>)
  sealed constant slot sccs-name  :: <symbol>,
    required-init-keyword: name:;
  sealed constant slot sccs-label  :: <string>,
    required-init-keyword: label:;
  sealed constant slot sccs-title :: <string>,
    required-init-keyword: title:;
end class <source-control-system>;

define method find-source-control-system-named
    (name :: <symbol>) => (sccs :: false-or(<source-control-system>))
  find-value(*all-source-control-systems*, method (sccs) sccs-name(sccs) == name end)
end method find-source-control-system-named;

define method register-source-control-class
    (class :: subclass(<source-control-system>), #rest initargs) 
 => (sccs :: <source-control-system>)
  let sccs
    = find-value(*all-source-control-systems*, method (e) object-class(e) == class end);
  if (sccs)
    sccs
  else
    let sccs = apply(make, class, initargs);
    add!(*all-source-control-systems*, sccs);
    sccs
  end
end method register-source-control-class;

define method unregister-source-control-class
    (class :: subclass(<source-control-system>)) => ()
  let sccs
    = find-value(*all-source-control-systems*, method (e) object-class(e) == class end);
  when (sccs)
    remove!(*all-source-control-systems*, sccs)
  end
end method unregister-source-control-class;


define settings <source-control-user-settings> (<open-dylan-user-settings>)
  key-name "Source Control";
  slot default-sccs :: <symbol> = #"none";
end settings <source-control-user-settings>;

define constant $source-control-user-settings = make(<source-control-user-settings>);

define function default-source-control-system
    () => (sccs :: false-or(<source-control-system>))
  let sccs = $source-control-user-settings.default-sccs;
  sccs ~= #"none" & find-source-control-system-named(sccs)
end function default-source-control-system;


define variable *current-source-control-system* :: false-or(<source-control-system>) = #f;
define variable *cscs-initialized?* :: <boolean> = #f;

///---*** TODO: Need a better mechanism to pick a default SCCS ...
define function current-source-control-system
    () => (sccs :: false-or(<source-control-system>))
  unless (*cscs-initialized?*)
    let default-sccs = default-source-control-system();
    case
      default-sccs => 
	*current-source-control-system* := default-sccs;
      otherwise =>
	*current-source-control-system* := find-source-control-system-named(#"HOPE")
                                           | find-source-control-system-named(#"SourceSafe");
      end
    end;
    *cscs-initialized?* := #t
  end;
  *current-source-control-system*
end function current-source-control-system;

define function current-source-control-system-setter
    (sccs :: <source-control-system>) => (sccs :: <source-control-system>)
  *current-source-control-system* := sccs;
  *cscs-initialized?* := #t;
  note-source-control-system-selected(sccs);
  sccs
end function current-source-control-system-setter;

define open generic note-source-control-system-selected
    (sccs :: <source-control-system>) => ();

define method note-source-control-system-selected
    (sccs :: <source-control-system>) => ()
  #f
end method note-source-control-system-selected;


/// Error classes

define open abstract class <source-control-condition> (<condition>)
end class <source-control-condition>;

define open abstract class <source-control-error>
    (<source-control-condition>, <error>)
end class <source-control-error>;

define open abstract class <source-control-warning>
    (<source-control-condition>, <warning>)
end class <source-control-warning>;

// Nobody has selected a source control system to use
define sealed class <source-control-unavailable>
    (<source-control-error>)
end class <source-control-unavailable>;

define abstract class <source-control-command-error-mixin> (<object>)
  sealed constant slot %source-control,
    required-init-keyword: source-control:;
  sealed constant slot %command,
    required-init-keyword: command:;
end class <source-control-command-error-mixin>;
  
// The selected source control system doesn't support this command
define sealed class <source-control-unsupported-command>
    (<source-control-command-error-mixin>, <source-control-error>)
end class <source-control-unsupported-command>;

// The command didn't successfully run
define sealed class <source-control-command-failed>
    (<source-control-command-error-mixin>, <source-control-warning>)
end class <source-control-command-failed>;
