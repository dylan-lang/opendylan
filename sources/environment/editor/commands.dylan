Module:    editor-manager-internals
Synopsis:  Environment-Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Editor commands

define open abstract class <editor-command> (<command>)
  sealed each-subclass slot editor-command-title :: false-or(<string>) = #f,
    init-keyword: title:;
  // For choosing among multiple editor frames...
  sealed slot editor-command-frame = #f,
    init-keyword: editor-frame:;
end class <editor-command>;

define method make
    (class :: subclass(<editor-command>), #rest initargs, #key server)
 => (command :: <editor-command>)
  let editor = server | current-editor();
  let class  = class-for-editor-command(editor, class);
  if (class)
    apply(next-method, class, server: editor, initargs)
  else
    error(make(<editor-unsupported-command>,
	       editor: editor, command: class))
  end
end method make;

define open generic class-for-editor-command
    (editor :: <editor>, class :: subclass(<editor-command>))
 => (class :: false-or(subclass(<editor-command>)));

define method class-for-editor-command
    (editor :: <editor>, class :: subclass(<editor-command>))
 => (class == #f)
  #f
end method class-for-editor-command;


/// Commands for hacking editor windows

define open abstract class <editor-window-command> (<editor-command>)
end class <editor-window-command>;

// Open a new editor window
define open abstract class <editor-open-command> (<editor-window-command>)
  keyword title: = "Open a new editor window";
end class <editor-open-command>;

// Close an editor window
define open abstract class <editor-close-command> (<editor-window-command>)
  keyword title: = "Open an existing editor window";
end class <editor-close-command>;


/// Commands for hacking files

define open abstract class <editor-file-command> (<editor-command>)
  sealed constant slot editor-command-pathname   :: false-or(<string>),
    required-init-keyword: pathname:;
  sealed slot editor-command-start-line   :: <integer> = 0,
    init-keyword: start-line:;
  sealed slot editor-command-start-column :: <integer> = 0,
    init-keyword: start-column:;
  sealed slot editor-command-end-line     :: <integer> = 0,
    init-keyword: end-line:;
  sealed slot editor-command-end-column   :: <integer> = 0,
    init-keyword: end-column:;
end class <editor-file-command>;

define method initialize
    (command :: <editor-file-command>, #key end-line, end-column) => ()
  unless (end-line)
    editor-command-end-line(command) := editor-command-start-line(command)
  end;
  unless (end-column)
    editor-command-end-column(command)  := editor-command-start-column(command)
  end;
end method initialize;

// Open a file within an editor
define open abstract class <editor-open-file-command> (<editor-file-command>)
  keyword title: = "Open the given file at the given location";
end class <editor-open-file-command>;

// Open a new file within an editor
define open abstract class <editor-new-file-command> (<editor-file-command>)
  keyword title: = "Create a new file";
end class <editor-new-file-command>;

// Close a file within an editor
define open abstract class <editor-close-file-command> (<editor-file-command>)
  keyword title: = "Close the given file";
end class <editor-close-file-command>;

// Insert some text
define open abstract class <editor-insert-text-command> (<editor-file-command>)
  sealed constant slot editor-command-text :: <string>,
    required-init-keyword: text:;
  keyword title: = "Insert the given text at the given location";
end class <editor-insert-text-command>;

// Delete some text
define open abstract class <editor-delete-text-command> (<editor-file-command>)
  keyword title: = "Delete the given selection";
end class <editor-delete-text-command>;


/// Commands for hacking projects

define open abstract class <editor-project-command> (<editor-command>)
  sealed constant slot editor-command-project,
    required-init-keyword: project:;
end class <editor-project-command>;

// Edit a set of definitions
define open abstract class <editor-edit-definitions-command> (<editor-project-command>)
  sealed constant slot editor-command-definitions :: <sequence>,
    required-init-keyword: definitions:;
  keyword title: = "Edit the source for the given definitions";
end class <editor-edit-definitions-command>;

// Save some files within an editor
define open abstract class <editor-save-files-command> (<editor-project-command>)
  sealed constant slot editor-command-pathnames :: <sequence> = #[],
    init-keyword: pathnames:;
  sealed constant slot editor-command-reason     :: false-or(<string>) = #f,
    init-keyword: reason:;
  sealed constant slot editor-command-exit-label :: false-or(<string>) = #f,
    init-keyword: exit-label:;
  keyword title: = "Save the given files for the given reason";
end class <editor-save-files-command>;
