Module: parser-compiler
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

let $default-terminal-string = "define constant $%s-token = %d;";

define function usage ()
  format(*standard-error*,
	 "Usage: %s [-t term-string] input-file [ output-file ]\n"
	   "Finds and processes the 'define parser' form in the input file,\n"
	   "creating an output file with that form replaced by the compiled\n"
           "parser definition\n"
	   "\n"
	   "The -t flag specifies the format string used to format terminal\n"
	   "definitions.  The first parameter is the token and the second is\n"
           "an integer code assigned to it by the parser compiler. The default\n"
	   "is: -t \"%s\"\n"
	   "\n"
	   "If output file is not specified, writes to standard output\n",
	 application-name(), $default-terminal-string);
end;

define function main () => ();
  let args = application-arguments();
  let nargs = args.size;
  let terminal-string = $default-terminal-string;
  iterate loop (index = 0)
    if (index == nargs)
      usage()
    else
      let arg :: <string> = args[index];
      if (arg.size > 1 & arg[0] = command-line-option-prefix())
	select (as-uppercase(arg[1]))
	  'T' =>
	    if (index + 1 == nargs)
	      usage()
	    else
	      terminal-string := args[index + 1];
	      loop(index + 2)
	    end;
	  otherwise =>
	    format(*standard-error*, "Unknown flag: %s\n", arg);
	    usage();
	end;
      elseif (nargs > index + 2)
	usage();
      elseif (~file-exists?(args[index]))
        format(*standard-error*, "File not found: %s\n", args[index]);
	usage();
      else
	compile-grammar-file(args[index],
			     if (nargs > index + 1)
			       args[index + 1]
			     else
			       *standard-output*
			     end,
			     report-grammar-conflict,
			     terminal-string: terminal-string)
      end;
    end;
  end;
end;

define function report-grammar-conflict (c :: <grammar-conflict>)
  let out = *standard-error*;
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
