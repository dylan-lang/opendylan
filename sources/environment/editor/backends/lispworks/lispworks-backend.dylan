Module:    editor-lispworks-backend
Synopsis:  Editor Back-End for the LispWorks Editor (in the Emulator)
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <lispworks-editor> (<editor>)
  keyword name:  = #"lispworks";
  keyword title: = "LispWorks Editor";
end class <lispworks-editor>;

define sealed domain make (singleton(<lispworks-editor>));
define sealed domain initialize (<lispworks-editor>);

// Get hold of a way to access the LispWorks editor.
import-cl-functions
  (cl-user(lispworks-editor-open-editor) (as: lispworks-editor-open-editor),
   cl-user(lispworks-editor-open-file)   (as: lispworks-editor-show-location));


define open abstract class <lispworks-editor-command>
    (<editor-command>, <basic-command>)
end class <lispworks-editor-command>;

// *** WARNING ***
// If $(start-line) is anything other than 0, this backend will currently
// produce the cryptic error message "No next character." and fail.
// However, the DylanWorks environment does not yet use anything other
// than column 0.
// *** WARNING ***

define macro lispworks-command-definer
  { define lispworks-command ?:name; }
    => { define sealed class "<lispworks-" ## ?name ## "-command>"
	     ("<editor-" ## ?name ## "-command>", <lispworks-editor-command>)
	 end class;
	 define sealed domain make (singleton("<lispworks-" ## ?name ## "-command>"));
	 define sealed domain initialize ("<lispworks-" ## ?name ## "-command>");
	 define sealed method class-for-editor-command
	     (editor :: <lispworks-editor>, class == "<editor-" ## ?name ## "-command>")
	  => (class == "<lispworks-" ## ?name ## "-command>")
	   "<lispworks-" ## ?name ## "-command>"
	 end method class-for-editor-command; }
end macro lispworks-command-definer;

define lispworks-command open;

// Note that we don't specialize 'do-execute-command' because we don't have
// a meaningful server yet
define sealed method execute-command
    (editor :: <lispworks-editor>, command :: <lispworks-open-command>)
 => (#rest values)
  lispworks-editor-open-editor();
end method do-execute-command;

//--- What about 'close'?

//--- What about 'new-file'?

define lispworks-command open-file;

define sealed method do-execute-command
    (editor :: <lispworks-editor>, command :: <lispworks-open-file-command>)
 => (#rest values)
  let pathname   = editor-command-pathname(command);
  let start-line = editor-command-start-line(command)   | #();
  let start-col  = editor-command-start-column(command) | #();
  lispworks-editor-show-location(pathname, start-line, start-col)
end method do-execute-command;

//--- What about 'close-file'?

//--- What about 'insert-text'?

//--- What about 'delete-text'?

//--- What about 'edit-definitions'?

//--- What about 'save-files'?
