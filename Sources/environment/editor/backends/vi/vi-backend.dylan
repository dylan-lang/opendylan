Module:    editor-vi-backend
Synopsis:  Backend for the Vi Editor
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Vi editor back-end, for Unix only

define sealed class <vi-editor> (<exe-editor>)
  keyword name:  = #"vi";
  keyword title: = "VI in an XTerm Window";
  keyword image: = "xterm -e vi";
end class <vi-editor>;

define sealed domain make (singleton(<vi-editor>));
define sealed domain initialize (<vi-editor>);


define open abstract class <vi-editor-command>
    (<editor-command>, <basic-string-command>)
end class <vi-editor-command>;


define macro vi-command-definer
  { define vi-command ?:name ?slots:* end }
    => { define sealed string-command "<vi-" ## ?name ## "-command>"
	     ("<editor-" ## ?name ## "-command>", <vi-editor-command>)
	   ?slots
	 end;
	 define sealed domain make (singleton("<vi-" ## ?name ## "-command>"));
	 define sealed domain initialize ("<vi-" ## ?name ## "-command>");
	 define sealed method class-for-editor-command
	     (editor :: <vi-editor>, class == "<editor-" ## ?name ## "-command>")
	  => (class == "<vi-" ## ?name ## "-command>")
	   "<vi-" ## ?name ## "-command>"
	 end method class-for-editor-command; }
end macro vi-command-definer;

define vi-command open
  keyword pattern-string: = "&"
end;

//--- What about 'close'?

//--- What about 'new-file'?

define vi-command open-file
  inherited named-argument pathname     is editor-command-pathname;
  inherited named-argument start-line   is editor-command-start-line;
  inherited named-argument start-column is editor-command-start-column;
  keyword pattern-string: = "-c $(start-line) $(pathname) &"
end;

//--- What about 'close-file'?

//--- What about 'insert-text'?

//--- What about 'delete-text'?

//--- What about 'edit-definitions'?

//--- What about 'save-files'?
