Module:    infix-reader
Language:  infix-dylan
Synopsis:  Code fragment printers for debug
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Generic fragments

define method print (f :: <fragment>, #key stream)
  format(stream, "#{ ");
  print-code(f, stream: stream);
  format(stream, " }");
end method;

define method print-code (f :: <object>, #key stream)
  format(stream, "{instance of ~s}", f.object-class);
end method;

define method print-code (f :: <bracketed-fragment>, #key stream)
  format(stream, "(");
  print-code-separated(f.fragments, " ", stream);
  format(stream, ")");
end method;

define method print-code (f :: <sequence-fragment>, #key stream)
  print-code-separated(f.fragments, " ", stream);
end method;

define method print-code (f :: <punctuation-fragment>, #key stream)
  format(stream, "~a", f.token-value);
end method;

define method print-code (f :: <literal-fragment>, #key stream)
  format(stream, "~s", f.object);
end method;

define method print-code (f :: <parsed-fragment>, #key stream)
  format(stream, "`~s'", f.token-value);
end method;

define method print-code (f :: <modified-fragment>, #key stream)
  print-code(f.modifiers, stream: stream);
  format(stream, " <define-word> ");
  print-code(f.fragment, stream: stream);
end method;

//// Misc printers

define method print (p :: <pattern-object>, #key stream)
  format(stream, "#{ ");
  print-code(p, stream: stream);
  format(stream, " }");
end method;

define method print-code (p :: <generic-pattern>, #key stream)
  print-code-separated(p.patterns, " ", stream);
end method;

define method print-code (p :: <statement-rule-pattern>, #key stream)
  print-code(p.pattern, stream: stream);
end method;

define method print-code (p :: <define-rule-pattern>, #key stream)
  print-code(p.modifiers-pattern, stream: stream);
  format(stream, " <define-word> ");
  print-code(p.pattern, stream: stream);
end method;

define method print-code (p :: <function-rule-pattern>, #key stream)
  print-code(p.pattern, stream: stream);
end method;

define method print-code (p :: <pattern-variable>, #key stream)
  let name = p.name;
  if (ellipsis-pattern?(p))
    format(stream, "...");
  else
    format(stream, "?~s:*", name);
  end;
  p;
end method;

define method print-code (p :: <constrained-pattern-variable>, #key stream)
  if (p.name == p.type)
    format(stream, "?:~s", p.type);
  else
    format(stream, "?~s:~s", p.name, p.type);
  end;
  p;
end method;

define method print-code (p :: <variable-pattern>, #key stream)
  print-code(p.name, stream: stream);
  if (p.type)
    format(stream," :: ");
    print-code(p.type, stream: stream);
  end;
  p;
end method;

define method print-code (p :: <pattern-sequence-pattern>, #key stream)
  let patterns = p.patterns;
  unless (empty?(patterns))
    for (l = patterns then l.tail, until empty?(l))
      print-code(l.head, stream: stream);
      unless (empty?(l.tail))
        format(stream, " ");
      end;
    end;
  end;
end method;

define method print-code (p :: <spliced-pattern-variable>, #key stream)
  if (p.before)
    format(stream, "~s ## ", p.before);
  end;
  print-code(p.pattern, stream: stream);
  if (p.after)
    format(stream, " ## ~s", p.after);
  end;
end method;

define method print-code (p :: <string-pattern-variable-pattern>, #key stream)
  format(stream, "?\"~s\"", p.name.name);
end method;

define method print-code (p :: <symbol-pattern-variable-pattern>, #key stream)
  format(stream, "?#\"~s\"", p.name.name);
end method;

define method print-code (r :: <statement-rule>, #key stream)
  format(stream, "~s => ~s", r.pattern, r.template);
end method; 

define method print-code (p :: <property-list-pattern>, #key stream)
  if (p.rest?)
    format(stream, "#rest ~s", p.rest-pattern);
  end;
  if (p.key?)
    if (p.rest?)
      format(stream, ", ");
    end;
    format(stream, "#key");
    if (~empty?(p.key-patterns))
      format(stream, " ");
      print-code-separated(p.key-patterns, ", ", stream);
    end;
  end;
  if (p.all-keys?)
    if (p.rest? | p.key?)
      format(stream, ", ");
    end;
    format(stream, "#all-keys");
  end;
end method;

//// Misc printers

define method print (t :: <template>, #key stream)
  format(stream, "#T{ ");
  iterate walk (tokens = t.tokens)
    case
      empty?(tokens) 
        => #t;
      instance?(tokens.first, <pattern-variable>)
        => format(stream, "~s ", tokens.first);
           walk(tokens.tail);
      instance?(tokens.first, <sequence-pattern-variable>)
        => format(stream, "~s ~a ...", tokens.first, tokens.first.separator);
           walk(tokens.tail);
      otherwise
        => format(stream, "~s ", tokens.second);
           walk(tokens.tail.tail);
    end;
  end;
  format(stream, "}");
  t;
end method;

define method print (t :: <template-closure>, #key stream)
  format(stream, "{closure of ~s}", t.template);
  t;
end method;

//// Local declarations.

// These aren't really reconstructable at the moment.

define method print (m :: <let-marker>, #key stream)
  format(stream, "let (");
  print-separated(m.bindings-of.first, ", ", stream);
  format(stream, ") = `~s'", m.bindings-of.second);
end method;

define method print (m :: <let-handler-marker>, #key stream)
  format(stream, "let handler ~s = `~s'", m.exception-of, m.expression-of);
end method;

define method print (m :: <local-marker>, #key stream)
  format(stream, "local ~s", m.methods-of);
end method;
  
// Utils

define method print-separated (list, sep, stream)
  unless (empty?(list))
    format(stream, "~s", list.first);
    for (item in list.tail)
      format(stream, "~a~s", sep, item);
    end;
  end;
end method;

define method print-code-separated (list, sep, stream)
  unless (empty?(list))
    print-code(list.first, stream: stream);
    for (item in list.tail)
      format(stream, "~a", sep);
      print-code(item, stream: stream);
    end;
  end;
end method;

// eof
