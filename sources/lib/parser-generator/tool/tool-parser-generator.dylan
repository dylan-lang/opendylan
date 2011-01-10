Module:    tool-parser-generator
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

tool-register(#"parser", parser-generator-invoke);

define method parser-generator-invoke
   (spec-file :: <locator>, project-file :: false-or(<locator>), last-run :: false-or(<date>),
    #key clean-build? :: <boolean> = #f)
 => (success? :: <boolean>, modified-projects :: <sequence> /* of: <locator> */)

  debug-message("Parser Generator Tool invoked.  Grammar: %=, project: %=, last: %=, clean: %=\n",
  		spec-file, project-file, last-run & date-as-string(last-run),
		clean-build?);

  let keyval = keyword-file-element-value;
  let keyline = keyword-file-element-line;
  let spec = read-keyword-pair-file(spec-file);
  local method key (sym :: <symbol>) element(spec, sym, default: #()) end;
  local method single (sym :: <symbol>, #key default = $unsupplied)
    if (sym.key.size = 1)
      sym.key.first.keyval
    elseif (default.unsupplied?)
      tool-error("exactly one %s must be specified", 
		 format-arguments: list(as(<string>, sym)),
		 file: spec-file, 
		 line: if (sym.key.size > 1) sym.key.second.keyline end if);
    elseif (sym.key.size > 1)
      tool-error("no more than one %s may be specified", 
		 format-arguments: list(as(<string>, sym)),
		 file: spec-file, line: sym.key.second.keyline);
    else
      default
    end if;
  end method single;

  let spec-file-modification-date = file-property(spec-file, #"modification-date");
  if (last-run & spec-file-modification-date > last-run)
    // Spec file has changed - we'd better regenerate stuff.
    clean-build? := #t;
  end if;
  let parser-file = merge-locators(as(<file-locator>, single(#"parser")), spec-file);
  let parser-file-modification-date = 
    block ()
      file-property(parser-file, #"modification-date")
    exception (c :: <file-system-error>)
      tool-error("could not open parser file \"%s\"", format-arguments: list(as(<string>, parser-file)));
    end block;
  if (last-run & parser-file-modification-date > last-run)
    // Parser file has changed - we'd better regenerate stuff.
    clean-build? := #t;
  end if;

  let output-file = single(#"output", default: #f);
  if (output-file)
    output-file := merge-locators(as(<file-locator>, output-file), spec-file);
  else
    output-file := make(<file-locator>,
			directory: locator-directory(parser-file),
			base: locator-base(parser-file),
			extension: "dylan");
  end if;
  if (file-exists?(output-file))
    let output-file-modification-date = file-property(output-file, #"modification-date");
    if (parser-file-modification-date > output-file-modification-date)
      clean-build? := #t;
    end if;
  else
    clean-build? := #t;
  end if;

  if (clean-build?)
    compile-grammar-file(parser-file, output-file, report-grammar-conflict,
			 terminal-string: "define constant $%s-token = %d;");
  end if;

  // Maybe add output-file to project?
  let modified-projects = #();
  if (project-file)
    let project = read-project-file(project-file);
    let files = project.project-information-files;
    let ab-output-file = relative-locator(output-file, project-file);
    unless (member?(ab-output-file, files, test: \=))
      debug-message("Adding parser generator output file to project");
      project.project-information-files := concatenate!(files, list(ab-output-file));
      write-project-file(project-file, project);
      modified-projects := add(modified-projects, project-file);
    end unless;
  end if;

  values(#t, modified-projects);
end method;

define function report-grammar-conflict (c :: <grammar-conflict>)
  let msg
    = with-output-to-string(out)
	local method format-rule (rule :: <sequence>, position :: <integer>)
		let s = format(out, "%s:", rule.rule-name);
		for (tkn in rule.rule-production, pos from 0)
		  if (pos == position) format(out, " .") end;
		  format(out, " %s", tkn);
		end for;
	      end method;

	format(out, "Conflict for terminal %s\n", c.grammar-conflict-terminal);
	format(out, "    Action %s in ", c.grammar-conflict-action-1);
	format-rule(c.grammar-conflict-rule-1, c.grammar-conflict-position-1);
	format(out, "\n    Action %s in ", c.grammar-conflict-action-2);
	format-rule(c.grammar-conflict-rule-2, c.grammar-conflict-position-2);
	format(out, "\n  Using the first action ...\n");
      end;
  tool-warning("%s", format-arguments: list(msg));
end;

define function date-as-string (date :: false-or(<date>)) => (r :: <string>)
  if (date)
    date := date + make(<day/time-duration>, days: 0);	// Stupid way to copy
    date.date-time-zone-offset := local-time-zone-offset();
    let (year, month, day, hours, minutes) = decode-date(date);
    format-to-string("%d:%02d %d-%02d-%02d %s", 
		     hours, minutes, year, month, day, local-time-zone-name())
  else
    "<no date>"
  end if
end function date-as-string;

