Module:    parser-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function output-dylan-parser (out :: <stream>,
				     name :: <string>,
				     grammar :: <grammar>,
				    #key terminal-string :: false-or(<string>))
  when (terminal-string)
    output-terminal-definitions(out, grammar, terminal-string);
  end;
  let action-codes = output-action-definitions(out, grammar, name);
  format(out, "define constant %s :: <parser>\n  = make(<parser>,\n", name);
  output-action-table(out, grammar);
  format(out, ",\n");
  output-goto-table(out, grammar);
  format(out, ",\n");
  output-action-function-table(out, grammar, action-codes);
  format(out, ",\n");
  output-action-nargs-table(out, grammar);
  format(out, ",\n");
  output-action-nt-table(out, grammar);
  format(out, ");\n");
end;

define function output-terminal-definitions
    (out :: <stream>, grammar :: <grammar>, format-string :: <string>)
  let v = grammar.grammar-terminals;
  for (index from 0 below v.size)
    format(out, format-string, v[index], index);
    format(out, "\n");
  end;
  format(out, "\n");
end;

define function output-action-definitions
    (out :: <stream>, grammar :: <grammar>, prefix :: <byte-string>)
 => (action-codes :: <table>)
  let action-codes = make(<string-table>);
  local method intern (prefix :: <byte-string>,
		       index :: <integer>,
		       action :: <byte-string>)
	  unless (element(action-codes, action, default: #f))
	    let name = concatenate(prefix, integer-to-string(index));
	    format(out, "define function %s %s\nend %s;\n\n",
                        name, action, name);
	    action-codes[action] := name;
	  end;
	end;

  let action-prefix = concatenate(prefix, "-action");
  let rules = grammar.grammar-rules;
  for (i from 0 below rules.size)
    intern(action-prefix, i, rules[i].rule-action);
  end;

  let error-prefix = concatenate(prefix, "-error-action");
  let error-rules = grammar.grammar-error-rules;
  for (i from 0 below error-rules.size)
    intern(error-prefix, i, rules[i].rule-action);
  end;

  action-codes
end;

define function canonicalize-body (nargs :: <integer>, body :: <byte-string>)
  // TODO: strip indentation, strip begin/end
  concatenate("(", action-variables(nargs), ") => (value)\n  ", body)
end;

define function action-variables (n)
  if (n < 10)
    element(#["",
	      "arg$1",
	      "arg$1, arg$2",
	      "arg$1, arg$2, arg$3",
	      "arg$1, arg$2, arg$3, arg$4",
	      "arg$1, arg$2, arg$3, arg$4, arg$5",
	      "arg$1, arg$2, arg$3, arg$4, arg$5, arg$6",
	      "arg$1, arg$2, arg$3, arg$4, arg$5, arg$6, arg$7",
	      "arg$1, arg$2, arg$3, arg$4, arg$5, arg$6, arg$7, arg$8",
	      "arg$1, arg$2, arg$3, arg$4, arg$5, arg$6, arg$7, arg$8, arg$9"],
	    n);
  else
    concatenate(action-variables(n - 1), ", arg$", integer-to-string(n));
  end;
end;

define function output-action-table (out :: <stream>, grammar :: <grammar>)
  output-vector-table(out, action-table: grammar.grammar-action-table);
end;

define function output-goto-table (out :: <stream>, grammar :: <grammar>)
  output-vector-table(out, goto-table: grammar.grammar-goto-table);
end;

define function output-vector-table (out :: <stream>,
				     name :: <symbol>,
				     table :: <simple-object-vector>)
  format(out, "  %s:\n      #[", name);
  for (pv :: <simple-object-vector> in table, open = "#[" then ",\n\t#[")
    format(out, open);
    for (item in pv, comma = "" then ", ")
      format(out, "%s%=", comma, item)
    end;
    format(out, "]");
  end;
  format(out, "]");
end;

define function output-action-function-table (out :: <stream>,
					      grammar :: <grammar>,
					      action-codes :: <table>)
  let rules = grammar.grammar-rules;
  if (empty?(rules))
    format(out, "  action-function-table:\n      #[],\n");
  else
    format(out, "  action-function-table:\n\t vector(");
    for (rule in rules, prefix = "" then ",\n\t\t")
      format(out, "%s%s", prefix, action-codes[rule.rule-action]);
    end;
    format(out, ")");
  end;
end output-action-function-table;

define function output-action-nargs-table (out :: <stream>,
					   grammar :: <grammar>)
  format(out, "  action-nargs-table: #[");
  for (rule in grammar.grammar-rules, comma = "" then ", ")
    format(out, "%s%d", comma, rule.rule-production.size)
  end;
  format(out, "]");
end output-action-nargs-table;

define function output-action-nt-table (out :: <stream>, grammar :: <grammar>)
  format(out, "  action-nt-table: #[");
  for (code in grammar.grammar-rule-reduction-table, comma = "" then ", ")
    format(out, "%s%s", comma, code);
  end;
  format(out, "]");
end output-action-nt-table;

