Module:    editor-gnuserv-backend
Synopsis:  Editor Back-end for Emacs via Gnuserv
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <gnuserv-editor> (<exe-editor>)
  keyword name:  = #"gnuserv";
  keyword title: = "Emacs via Gnuserv";
  keyword image: = "gnudoit -q";
end class <gnuserv-editor>;

define sealed domain make (singleton(<gnuserv-editor>));
define sealed domain initialize (<gnuserv-editor>);


define open abstract class <gnuserv-editor-command>
    (<editor-command>, <basic-string-command>)
end class <gnuserv-editor-command>;

// *** NOTE ***
// Although Gnuserv is available for UNIX as well as Win32, this backend
// works better with Win32.  This is because the Win32 port of Gnuserv will
// attempt to launch Emacs if it is not already running, whereas the UNIX
// version will not.  Aside from this, however, both will work just as well.
// *** NOTE ***

// *** NOTE ***
// The Gnuserv server process must be running in Emacs for this
// backend to work.  See the documentation for the Gnuserv package,
// and remember to set the HOME and/or LOGNAME variables under Win32,
// or Emacs may not find its init file.
// *** NOTE ***

define macro gnuserv-command-definer
  { define gnuserv-command ?:name ?slots:* end }
    => { define sealed string-command "<gnuserv-" ## ?name ## "-command>"
	     ("<editor-" ## ?name ## "-command>", <gnuserv-editor-command>)
	   ?slots
	 end;
	 define sealed domain make (singleton("<gnuserv-" ## ?name ## "-command>"));
	 define sealed domain initialize ("<gnuserv-" ## ?name ## "-command>");
	 define sealed method class-for-editor-command
	     (editor :: <gnuserv-editor>, class == "<editor-" ## ?name ## "-command>")
	  => (class == "<gnuserv-" ## ?name ## "-command>")
	   "<gnuserv-" ## ?name ## "-command>"
	 end method class-for-editor-command; }
end macro gnuserv-command-definer;

define gnuserv-command open
  keyword pattern-string: = "\"\""
end;

//--- What about 'close'?

//--- What about 'new-file'?

define gnuserv-command open-file
  inherited named-argument pathname     is editor-command-pathname;
  inherited named-argument start-line   is editor-command-start-line;
  inherited named-argument start-column is editor-command-start-column;
  keyword pattern-string: = "\"(find-file \\\"$(pathname)\\\")"
			    " (goto-line $(start-line))"
			    " (beginning-of-line)"
			    " (forward-char $(start-column))\""
end;

//--- What about 'close-file'?

//--- What about 'insert-text'?

//--- What about 'delete-text'?

//--- What about 'edit-definitions'?

//--- What about 'save-files'?

// Gnuserv-specific method for 'string-for-argument'.
// This forces the '\' character to be escaped, i.e. become "\\", in
// the replacement of $(pathname), since otherwise gnuserv under MS-DOS
// interprets the '\'s in MS-DOS filenames as escaping the following
// character -- so "C:\bar.txt" is "Car.txt"!
define method string-for-argument
    (server :: <gnuserv-editor>, command :: <gnuserv-editor-command>,
     name == #"pathname", value :: <string>)
 => (string :: <string>)
  let value
    = begin
	let result :: <stretchy-object-vector> = make(<stretchy-vector>);
	for (char :: <character> in value)
	  when (char == '\\')
	    add!(result, '\\')
	  end;
	  add!(result, char)
	end;
	as(<string>, result)
      end;
  next-method(server, command, name, value)
end method string-for-argument;
