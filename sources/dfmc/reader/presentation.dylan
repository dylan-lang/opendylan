Module:    dfmc-reader
Synopsis:  Fragment presentation and print methods.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Fragment presentation.

define method present (f , s :: <stream>)
  format(s, "{UNKNOWN-FRAGMENT %s}", object-class(f));
end method;

define method present (f :: <list> , s :: <stream>)
  present-fragments(f, s);
end method;

define method present (f :: <sequence-fragment> , s :: <stream>)
  present-fragments(fragment-fragments(f), s);
end method;

define method present (f :: <fragment>, s :: <stream>)
  printing-object (f, s) end;
end method;

define method present (f :: <literal-fragment>, s :: <stream>)
  format(s, "%=", fragment-value(f));
end method;

define method present (f :: <symbol-syntax-symbol-fragment>, s :: <stream>)
  format(s, "#\"%s\"", as-lowercase(as(<string>, fragment-value(f))));
end method;

define method present (f :: <keyword-syntax-symbol-fragment>, s :: <stream>)
  format(s, "%s:", as-lowercase(as(<string>, fragment-value(f))));
end method;

define method present (f :: <dot-fragment>, s :: <stream>)
  write-element(s, '.');
end method;

define method present (f :: <comma-fragment>, s :: <stream>)
  write-element(s, ',');
  pprint-newline(#"linear", s);
end method;

define method present (f :: <semicolon-fragment>, s :: <stream>)
  write-element(s, ';');
  pprint-newline(#"linear", s);
end method;

define method present (f :: <hash-next-fragment>, s :: <stream>)
  write(s, "#next");
end method;

define method present (f :: <hash-rest-fragment>, s :: <stream>)
  write(s, "#rest");
end method;

define method present (f :: <hash-key-fragment>, s :: <stream>)
  write(s, "#key");
end method;

define method present (f :: <hash-all-keys-fragment>, s :: <stream>)
  write(s, "#all-keys");
end method;

define method present (f :: <lparen-fragment>, s :: <stream>)
  write-element(s, '(');
end method;

define method present (f :: <rparen-fragment>, s :: <stream>)
  write-element(s, ')');
  // pprint-newline(#"fill", s);
end method;

define method present (f :: <lbracket-fragment>, s :: <stream>)
  write-element(s, '[');
end method;

define method present (f :: <rbracket-fragment>, s :: <stream>)
  write-element(s, ']');
  // pprint-newline(#"fill", s);
end method;

define method present (f :: <lbrace-fragment>, s :: <stream>)
  write-element(s, '{');
end method;

define method present (f :: <rbrace-fragment>, s :: <stream>)
  write-element(s, '}');
  // pprint-newline(#"fill", s);
end method;

define method present (f :: <hash-lparen-fragment>, s :: <stream>)
  write(s, "#(");
end method;

define method present (f :: <hash-lbracket-fragment>, s :: <stream>)
  write(s, "#[");
end method;

define method present (f :: <colon-colon-fragment>, s :: <stream>)
  write(s, "::");
end method;

define method present (f :: <equal-greater-fragment>, s :: <stream>)
  write(s, "=>");
end method;

define method present (f :: <name-fragment>, s :: <stream>)
  write(s, as-lowercase(as(<string>, fragment-name(f))));
  if (fragment-origin(f))
    // write(s, "@macro");
  end;
end method;

define method present (f :: <escaped-name-fragment>, s :: <stream>)
  write-element(s, '\\');
  next-method();
end method;

define method present (f :: <operator-fragment>, s :: <stream>)
  write(s, as-lowercase(as(<string>, fragment-name(f))));
end method;

define method present (f :: <prefix-call-fragment>, s :: <stream>)
  present(fragment-function(f), s);
  write-element(s, '(');
  present-list(fragment-arguments(f), s);
  write-element(s, ')');
end method;

define method present (f :: <array-call-fragment>, s :: <stream>)
  present(fragment-arguments(f).head, s);
  write-element(s, '[');
  present-list(fragment-arguments(f).tail, s);
  write-element(s, ']');
end method;

define method present (f :: <dot-call-fragment>, s :: <stream>)
  present(fragment-arguments(f).head, s);
  write-element(s, '.');
  present(fragment-function(f), s);
end method;

define method present (f :: <binary-operator-call-fragment>, s :: <stream>)
  write-element(s, '(');
  present(fragment-arguments(f).first, s);
  write-element(s, ' ');
  present(fragment-function(f), s);  
  write-element(s, ' ');
  present(fragment-arguments(f).second, s);
  write-element(s, ')');
end method;

define method present (f :: <unary-operator-call-fragment>, s :: <stream>)
  write-element(s, '(');
  present(fragment-function(f), s);  
  present(fragment-arguments(f).first, s);
  write-element(s, ')');
end method;

define method present (f :: <parens-fragment>, s :: <stream>)
  printing-logical-block (s, prefix: "(", suffix: ")")
    present-fragments(fragment-nested-fragments(f), s);
  end;
  // pprint-newline(#"fill", s);
  /*
  write-element(s, '(');
  present-fragments(fragment-nested-fragments(f), s);
  write-element(s, ')');
  */
end method;

define method present (f :: <brackets-fragment>, s :: <stream>)
  printing-logical-block (s, prefix: "[", suffix: "]")
    present-fragments(fragment-nested-fragments(f), s);
  end;
  // pprint-newline(#"fill", s);
  /*
  write-element(s, '[');
  present-fragments(fragment-nested-fragments(f), s);
  write-element(s, ']');
  */
end method;

define method present (f :: <braces-fragment>, s :: <stream>)
  printing-logical-block (s, prefix: "{", suffix: "}")
    present-fragments(fragment-nested-fragments(f), s);
  end;
  // pprint-newline(#"fill", s);
  /*
  write(s, "{ ");
  present-fragments(fragment-nested-fragments(f), s);
  write(s, " }");
  */
end method;

define method present (f :: <body-fragment>, s :: <stream>)
  write(s, "(begin ");
  present-constituents(fragment-constituents(f), s);
  write(s, " end)");
  pprint-newline(#"linear", s);
end method;

define method present (f :: <local-declaration-fragment>, s :: <stream>)
  present(fragment-macro(f), s);
  write-element(s, ' ');
  present-fragments(fragment-list-fragment(f), s);
end method;

define method present (f :: <local-declaration-call-fragment>, s :: <stream>)
  write(s, "(begin ");
  present(fragment-declaration-fragment(f), s);
  write(s, "; ");
  present(fragment-body-fragment(f), s);
  write(s, " end)");
end method;

define method present (f :: <macro-call-fragment>, s :: <stream>)
  present(fragment-macro(f), s);
  write(s, " ");
  present-fragments(fragment-argument(f), s);
  write(s, " end");
end method;

define method present (f :: <function-macro-fragment>, s :: <stream>)
  present(fragment-macro(f), s);
  write(s, "(");
  present-fragments(fragment-argument(f), s);
  write(s, ")");
end method;

define method present (f :: <body-definition-fragment>, s :: <stream>)
  write(s, "define ");
  let mods = fragment-modifiers(f);
  if (mods) 
    present-fragments(mods, s);
    write(s, " ");
  end;
  present(fragment-define-word(f), s);
  write(s, " ");
  present(fragment-body-fragment(f), s);
  write(s, " end");
end method;

define method present (f :: <list-definition-fragment>, s :: <stream>)
  write(s, "define ");
  let mods = fragment-modifiers(f);
  if (mods) 
    present-fragments(mods, s);
    write(s, " ");
  end;
  write(s, " ");
  present(fragment-define-word(f), s);
  write(s, " ");
  present(fragment-list-fragment(f), s);
end method;

define method present 
    (f :: <constrained-name-fragment>, s :: <stream>)
  format(s, "%s:%s", 
         as-lowercase(as(<string>, fragment-name(f))), 
         as-lowercase(as(<string>, fragment-constraint(f))));
end method;

define method present (f :: <query-fragment>, s :: <stream>)
  write(s, "?");
end method;

define method present (f :: <query-query-fragment>, s :: <stream>)
  write(s, "??");
end method;

define method present (f :: <query-equal-fragment>, s :: <stream>)
  write(s, "?=");
end method;

define method present 
    (f :: <spliced-pattern-variable-fragment>, s :: <stream>)
  if (f.fragment-prefix)
    format(s, "%= ## ", f.fragment-prefix);
  end;
  write-element(s, '?');
  present(fragment-pattern-variable(f), s);
  if (f.fragment-suffix)
    format(s, " ## %=", f.fragment-prefix);
  end;
end method;

define method present 
    (f :: <ellipsis-fragment>, s :: <stream>)
  write(s, "...");
end method;

define method present-list (l :: <fragment>, s)
  present-list(list(l), s);
end method;

define method present-list (l, s)
  for (first = #t then #f, f in l)
    if (~first) 
      write(s, ", ");
      pprint-newline(#"linear", s)
    end;
    present(f, s);
  end;
end method;

define method present-constituents (l, s)
  for (first = #t then #f, f in l)
    if (~first) 
      write(s, "; "); 
      pprint-newline(#"linear", s)
    end;
    present(f, s);
  end;
end method;

define method print-object (f :: <fragment>, s :: <stream>) => ()
  if (*print-escape?*) format(s, "#P{ ") end;
  present(f, s);
  if (*print-escape?*) format(s, " }") end;
end method;

//// Template presentation.

// U -> "Unparsed"
// S -> "Shallow-parsed"

define method print-object (f :: <template>, s :: <stream>) => ()
  if (*print-escape?*) format(s, "{ ") end;
  dynamic-bind (*print-pretty?* = #t)
    printing-logical-block (s)
      present-fragments(template-fragments(f), s);
    end;
  end;
  if (*print-escape?*) format(s, " }") end;
end method;

define method present
    (f :: <template>, s :: <stream>)
  present-fragments(template-fragments(f), s);
end method;

define method print-object (f :: <sequence-fragment>, s :: <stream>) => ()
  if (*print-escape?*) format(s, "``") end;
  present-fragments(fragment-fragments(f), s);
  if (*print-escape?*) format(s, "''") end;
end method;

define method present-fragments (f* :: <list>, s :: <stream>);
  let last = #f;
  local method walk (sub-f*)
    if (~empty?(sub-f*))
      for (f in sub-f*)
        if (instance?(f, <list>))
          // A nested respresentation used in templates to avoid
          // the copying required to flatten.
          walk(f);
        else
          if (last & present-with-preceding-space?(last, f))
            write-element(s, ' ');
          end;
         last := f;
         present(f, s);
        end;
      end;
    end;
  end;
  walk(f*);
end method;

// TODO: CORRECTNESS: How does a non-list fragment turn up here?

define method present-fragments (f :: <fragment>, s :: <stream>);
  present(f, s);
end method;

/*
define method present (f :: <template>, s :: <stream>)
  write(s, "?(");
  print-object(f, s);
  write-element(s, ')');
end method;
*/

define method present-with-preceding-space? (last, f)
  #t
end method;

define method present-with-preceding-space? 
    (last :: <fragment>, f :: <separator-fragment>) 
  #f
end method;

// Overrides the above...
define method present-with-preceding-space? 
    (last :: <fragment>, f :: <binary-operator-fragment>) 
  #t
end method;

define method present-with-preceding-space? 
    (last :: <fragment>, f :: <close-paren-fragment>) 
  #f
end method;

define method present-with-preceding-space? 
    (last :: <open-paren-fragment>, f :: <fragment>) 
  #f
end method;

define method present-with-preceding-space? 
    (last :: <fragment>, f :: <dot-fragment>) 
  #f
end method;

define method present-with-preceding-space? 
    (last :: <dot-fragment>, f :: <fragment>) 
  #f
end method;

//// Source location presentation.

define method print-object (f :: <compiler-range-source-location>, s :: <stream>) => ()
  printing-object (f, s)
    format(s, "%= (%d, %d) - (%d, %d)", 
           f.source-location-source-record,
           f.source-location-start-offset.source-offset-line, 
           f.source-location-start-offset.source-offset-column, 
           f.source-location-end-offset.source-offset-line, 
           f.source-location-end-offset.source-offset-column);
  end;
end method;

define method print-source-record-source-location
    (sr :: <source-record>, loc :: <compiler-range-source-location>, stream) => ();
  let start-offset = source-location-start-offset(loc);
  let start-line = source-offset-line(start-offset);
  let (lines, upper-dec, lower-dec) = extract-lines(loc);
  if (~lines)
    print-source-line-location(sr, start-line, stream);
  else
    format(stream, "\n");
    print-source-line-location(sr, start-line, stream);
    format(stream, ": %s\n", upper-dec);
    if (lines.size < 5)
      for (line in lines, number from start-line)
	print-source-line-location(sr, number, stream);
	format(stream, ": %s\n", line);
      finally
	print-source-line-location(sr, number - 1, stream);
	format(stream, ": %s\n", lower-dec);
      end;
    else
      print-source-line-location(sr, start-line, stream);
      format(stream, ": %s\n", lines[0]);
      print-source-line-location
	(sr, start-line + 1, stream);
      // format(stream, ": ...\n");
      format(stream, ": %s\n", lines[1]);
      print-source-line-location
       (sr, start-line + lines.size - 2, stream);
      format(stream, ": [...]\n");
      // format(stream, "\n");
      print-source-line-location
	(sr, start-line + lines.size - 1, stream);
      format(stream, ": %s\n", lines.last);
      print-source-line-location
	(sr, start-line + lines.size, stream);
      format(stream, ": %s\n", lower-dec);
    end;
  end;
end method;
