Module:    editor-exe-backend
Synopsis:  Environment-Exe Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Backend for the class of "command-line" editors

define sealed class <exe-editor> (<editor>)
  // The filename required to launch this editor's executable
  constant slot editor-image :: <string>,
    required-init-keyword: image:;
  // A slot to help us identify each running instance
  slot editor-process-id :: false-or(<integer>) = #f,
    init-keyword: process-id:;
  keyword name:  = #"generic-exe";
  keyword title: = "Generic Command-Line Editor";
end class <exe-editor>;

define sealed domain make (singleton(<exe-editor>));
define sealed domain initialize (<exe-editor>);


define open abstract class <exe-editor-command>
    (<editor-command>, <basic-string-command>)
end class <exe-editor-command>;

define method execute-command
    (command :: <exe-command>) => (#rest values)
  let application = editor-image(editor-command-editor(command));
  let cmd-string  = string-for-command(command);
  //--- What about error handling for this?
  run-application(concatenate-as(<string>, application, " ", cmd-string))
end method execute-command;
