Module: infix-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *trace-depth* = 0;
define variable *tracing?* = #f;

define method trace-macros ()
  *tracing?* := #t;
  *trace-depth* := 0;
end method;

define method untrace-macros ()
  *tracing?* := #f;
  *trace-depth* := 0;
end method;

define method trace-indent ()
  for (i from 0 below *trace-depth* * 2) format(#t, " ") end;
end method;

define method trace-format (#rest stuff)
  trace-indent();
  apply(format, #t, stuff);
end method;

define method prettify (f* :: <list>)
  sequence-fragment(fragments: f*)
end method;

define method prettify (f :: <fragment>)
  f
end method;

define method trace-binding (var, val)
  when (*tracing?*)
    trace-format("BIND: ~s = ~s~%", var, prettify(val));
  end;
end method;

define method trace-main-rule-call (name, main-rule, f)
  when (*tracing?*)
    trace-format
      ("~s MACRO: ~s: ~s > ~s~%", 
       *trace-depth*, name, main-rule.pattern, prettify(f));
    *trace-depth* := *trace-depth* + 1;
  end;
end method;

define method trace-main-rule-return (name, main-rule, t)
  when (*tracing?*)
    *trace-depth* := *trace-depth* - 1;
    trace-format("~s MACRO: ~s: ~s < #{ ~a}~%", 
                 *trace-depth*, name, main-rule.pattern, 
                 reparse(t, constraint: fragment:, parser: run-null-parser));
  end;
end method;

define method trace-main-rule-fail (name, main-rule)
  when (*tracing?*)
    *trace-depth* := *trace-depth* - 1;
    trace-format("~s MACRO: ~s: ~s < // Match failed~%", 
                 *trace-depth*, name, main-rule.pattern);
  end;
end method;

define method trace-aux-rule-call (name, aux-rule, f)
  when (*tracing?*)
    trace-format
      ("~s AUX-RULE: ~s: ~s > ~s~%", 
       *trace-depth*, name, aux-rule.pattern, prettify(f));
    *trace-depth* := *trace-depth* + 1;
  end;
end method;

define method trace-aux-rule-return (name, aux-rule, t)
  when (*tracing?*)
    *trace-depth* := *trace-depth* - 1;
    trace-format("~s AUX-RULE: ~s: ~s < #{ ~a}~%", 
                 *trace-depth*, name, aux-rule.pattern, 
                 reparse(t, constraint: fragment:, parser: run-null-parser));
  end;
end method;

define method trace-aux-rule-fail (name, aux-rule)
  when (*tracing?*)
    *trace-depth* := *trace-depth* - 1;
    trace-format("~s AUX-RULE: ~s: ~s < // Match failed~%", 
                 *trace-depth*, name, aux-rule.pattern);
  end;
end method;

// eof
