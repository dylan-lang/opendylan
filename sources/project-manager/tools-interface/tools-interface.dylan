Module: 	tools-interface
Author:		1997-07-17: Seth LaForge
Synopsis:	Interface between spec file tools and the project manager.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Overview of how tools work:
//   Tools are Dylan libraries which contain a top-level expression to
//   register themselves via tool-register.  Tools are linked into the
//   environment and batch compiler by being "use"d in
//   D-environment-win32!*-library.dylan and D-app-dw!*-library.dylan.
// 
// When a build is initiated, the project manager first iterates over all
// files in the project, calling tool-name-from-specification() and
// tool-find() for each to determine is a tool processes that file type.
// For files for which tool-find() finds a tool:
//   The project manager calls the function returned, passing the
//   location of the file and the project file, and the date of the last
//   time the file was processed.  This function reprocesses the file if
//   necessary, and returns a list of modified projects.
// The project manager then rereads any project files which tools have
// modified, and continues with the rest of the build.
//
// Any warnings or errors are reported by the tool by signaling
// <tool-warning-condition>.  The tool can also ask yes-no questions by
// signalling <tool-yes-no-question>.


define variable tool-registry :: false-or(<table>) = #f;


// Register a tool.  Invokee should have signature:
//      (spec-file :: <locator>, project-file :: false-or(<locator>), 
//	 last-run :: false-or(<date>)
//	 #key clean-build? :: <boolean> = #f)
//   => (success? :: <boolean>,
//	 modified-projects :: <sequence> /* of: <locator> */)
// modified-projects is a <sequence> of <locator>s of projects which have been
// (or may have been) modified.
// project-file should be #f only when the tool is invoked from the tool-tester
// with one argument.
// last-run is a date immediately after the previous run of the tool
// on this specification file.
// clean-build? indicates whether to force regeneration of all build products.
// If success? is false, the build is aborted.
//
// Each tool should register itself via a top-level expression.

define function tool-register (tool-name :: <symbol>, 
			       invokee :: <function>) => ()
  if (~ tool-registry)
    tool-registry := make(<table>);
  end if;

  tool-registry[tool-name] := invokee;
end function tool-register;


// Extract the tool name from a specification file.  For .spec files, reads
// the file to find a tool name - for others, simply returns the file
// extension.
// If there is an error, a nonrecoverable <tool-warning-condition> will 
// be signalled.

define function tool-name-from-specification (spec-file :: <locator>) 
					  => (tool-name :: <symbol>)
  // Get the file extension:
  let ext :: <string> = spec-file.locator-extension | "";
  let extsym
    = if (empty?(ext))
	#"dylan"
      else
	as(<symbol>, ext)
      end;

  //---*** andrewa: bootstrapping nastiness, only the first should be necessary
  if (extsym == #"spec" | extsym == #".spec")
    // Read the header of the file:
    let h = read-keyword-pair-file(spec-file);
    let tool = element(h, origin:, default: #());
    if (tool.size ~= 1)
      tool-error("specification file must contain a single \"origin:\" "
		 "declaration in its header",
		 file: spec-file, serious?: #t);
    end if;
    as(<symbol>, tool.first.keyword-file-element-value)
  else
    extsym
  end if
end;


// Find a tool invokee by name - used by project manager.
// The name should have been returned by tool-name-from-specification.

define function tool-find (tool-name :: <symbol>) 
		       => (invokee :: false-or(<function>))
  if (tool-registry)
    element(tool-registry, tool-name, default: #f)
  end if;
end function tool-find;


// If the tool wishes to ask a yes-no question (for example, whether
// to abort the translation when translation products have been
// modified), it should signal this condition.  The project manager
// should catch the condition, present a yes/no dialog box, and return
// #t/#f.

define class <tool-yes-no-question> (<simple-warning>)
end class <tool-yes-no-question>;

define function tool-ask-yes-no-question (format-string :: <string>, 
					  #rest format-args) 
				      => (answer? :: <boolean>)
  signal(make(<tool-yes-no-question>, 
	      format-string: format-string, format-arguments: format-args));
end function tool-ask-yes-no-question;


// Warnings should be communicated to the project manager by
// signalling the following condition.  The project manager should
// handle the condition, save the warning for future presentation to
// the user somehow, and return.

define class <tool-warning-condition> (<simple-warning>)
  // The message in the inherited format-string and format-arguments slots is
  // the body of the warning.
  slot tool-warning-serious? :: <boolean> = #f, init-keyword: serious?:;
  slot tool-warning-recoverable? :: <boolean> = #t, init-keyword: recoverable?:;
  slot tool-warning-file :: false-or(type-union(<string>, <locator>)) = #f, 
	  init-keyword: file:;
  slot tool-warning-line :: false-or(<integer>) = #f, init-keyword: line:;
  slot tool-warning-library :: false-or(<string>) = #f, init-keyword: library:;
  slot tool-warning-definition :: false-or(<string>) = #f, 
	  init-keyword: definition:;
end class <tool-warning-condition>;


define function tool-warning (format-string :: <string>, #rest keys) => ()
  signal(apply(make, <tool-warning-condition>, format-string: format-string, 
	       keys));
end function tool-warning;

define function tool-error (format-string :: <string>, #rest keys)
  error(apply(make, <tool-warning-condition>, format-string: format-string, 
	      recoverable?: #f, keys));
end function tool-error;


define method condition-to-string (this :: <tool-warning-condition>) 
			       => (r :: <string>)
  format-to-string("%s: %s%s%s%s",
	if (this.tool-warning-recoverable?)
	  if (this.tool-warning-serious?) "Serious warning" else "Warning" 
	  end if
	else "Error" end if,
	if (this.tool-warning-file)
	  format-to-string("file %s%s: ", as(<string>, this.tool-warning-file),
			   if (this.tool-warning-line)
			     concatenate(":", integer-to-string(
			     				this.tool-warning-line))
			   else "" end if)
	else "" end if,
	next-method(),
	if (this.tool-warning-definition)
	  concatenate(" in ", this.tool-warning-definition)
	else "" end if,
	if (this.tool-warning-library)
	  concatenate(" in library ", this.tool-warning-library)
	else "" end if)
end method condition-to-string;
