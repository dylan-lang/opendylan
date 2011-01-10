Module:    editor-manager-internals
Synopsis:  Environment-Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Editors

define variable *all-editors* :: <stretchy-object-vector>
    = make(<stretchy-vector>);

define open abstract primary class <editor> (<object>)
  sealed constant slot editor-name  :: <symbol>,
    required-init-keyword: name:;
  sealed constant slot editor-title :: <string>,
    required-init-keyword: title:;
end class <editor>;

define method find-editor-of-class
    (class :: subclass(<editor>)) => (editor :: <editor>)
  let editor
    = find-value(*all-editors*, method (e) object-class(e) == class end);
  editor
  | error(make(<editor-unavailable>))
end method find-editor-of-class;

define method register-editor-class
    (class :: subclass(<editor>), #rest initargs)
 => (editor :: <editor>)
  let editor
    = find-value(*all-editors*, method (e) object-class(e) == class end);
  if (editor)
    editor
  else
    let editor = apply(make, class, initargs);
    add!(*all-editors*, editor);
    editor
  end
end method register-editor-class;

define method unregister-editor-class
    (class :: subclass(<editor>)) => ()
  let editor
    = find-value(*all-editors*, method (e) object-class(e) == class end);
  when (editor)
    remove!(*all-editors*, editor)
  end
end method unregister-editor-class;


define variable *current-editor* :: false-or(<editor>) = #f;

define function current-editor
    () => (editor :: <editor>)
  *current-editor*
  | error(make(<editor-unavailable>))
end function current-editor;

define function current-editor-setter
    (editor :: <editor>) => (editor :: <editor>)
  *current-editor* := editor;
  note-editor-selected(editor);
  editor
end function current-editor-setter;

define open generic note-editor-selected
    (editor :: <editor>) => ();

define method note-editor-selected
    (editor :: <editor>) => ()
  #f
end method note-editor-selected;


/// Error classes

define open abstract class <editor-condition> (<condition>)
end class <editor-condition>;

define open abstract class <editor-error>
    (<editor-condition>, <error>)
end class <editor-error>;

define open abstract class <editor-warning>
    (<editor-condition>, <warning>)
end class <editor-warning>;

// Nobody has selected an editor to use
define sealed class <editor-unavailable>
    (<editor-error>)
end class <editor-unavailable>;

define abstract class <editor-command-error-mixin> (<object>)
  sealed constant slot %editor,
    required-init-keyword: editor:;
  sealed constant slot %command,
    required-init-keyword: command:;
end class <editor-command-error-mixin>;
  
// The selected editor doesn't support this command
define sealed class <editor-unsupported-command>
    (<editor-command-error-mixin>, <editor-error>)
end class <editor-unsupported-command>;

// The command didn't successfully run
define sealed class <editor-command-failed>
    (<editor-command-error-mixin>, <editor-warning>)
end class <editor-command-failed>;
