Module:    editor-emacsserver-backend
Synopsis:  Editor Back-end for Emacs via Emacsserver
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <emacsserver-editor> (<exe-editor>)
  keyword name:  = #"emacsserver";
  keyword title: = "Emacs via Emacsserver";
  keyword image: = "";
end class <emacsserver-editor>;

define sealed domain make (singleton(<emacsserver-editor>));
define sealed domain initialize (<emacsserver-editor>);


define open abstract class <emacsserver-editor-command>
    (<editor-command>, <basic-string-command>)
end class <emacsserver-editor-command>;


// *** WARNING ***
// The commands for the Emacsserver backend are slightly unusual,
// because the Emacsserver will unfortunately not try launch Emacs if
// Emacs is not already running.  To circumvent this, the "||"
// pipeline separator of sh/bash is used.  So, if the operating-system
// library changes to launch programs via a different shell (e.g.,
// csh) or some non-shell mechanism, this may break.
// *** WARNING ***

define macro emacsserver-command-definer
  { define emacsserver-command ?:name ?slots:* end }
    => { define sealed string-command "<emacsserver-" ## ?name ## "-command>"
	     ("<editor-" ## ?name ## "-command>", <emacsserver-editor-command>)
	   ?slots
	 end;
	 define sealed domain make (singleton("<emacsserver-" ## ?name ## "-command>"));
	 define sealed domain initialize ("<emacsserver-" ## ?name ## "-command>");
	 define sealed method class-for-editor-command
	     (editor :: <emacsserver-editor>, class == "<editor-" ## ?name ## "-command>")
	  => (class == "<emacsserver-" ## ?name ## "-command>")
	   "<emacsserver-" ## ?name ## "-command>"
	 end method class-for-editor-command; }
end macro emacsserver-command-definer;


define emacsserver-command open
  keyword pattern-string: = "emacsclient19 \"\" >/dev/null 2>&1 || emacs19 &";
end;


//--- What about 'close'?

//--- What about 'new-file'?

define emacsserver-command open-file
  inherited named-argument pathname     is editor-command-pathname;
  inherited named-argument start-line   is editor-command-start-line;
  inherited named-argument start-column is editor-command-start-column;
  keyword pattern-string: = "emacsclient19 +$(start-line) $(pathname)"
			    " >/dev/null 2>&1"
			    " || emacs19 +$(start-line) $(pathname) &"
end;

//--- What about 'close-file'?

//--- What about 'insert-text'?

//--- What about 'delete-text'?

//--- What about 'edit-definitions'?

//--- What about 'save-files'?
