Module:    dfmc-macro-expander
Synopsis:  Objects describing a pattern match specification.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <pattern-match> (<object>) 
  constant slot match-source-location = #f,
    init-keyword: source-location:;
end class;

define sealed domain make (subclass(<pattern-match>));
define sealed domain initialize (<pattern-match>);

define abstract class <binding-match> (<pattern-match>)
  slot match-symbol-name,
    required-init-keyword: symbol-name:;
  slot match-variable-name,
    required-init-keyword: variable-name:;
  slot match-constraint,
    required-init-keyword: constraint:;
  slot match-env-index :: <integer> = 0;
end class;

define class <simple-match> (<binding-match>) end;
define class <sequence-match> (<binding-match>) end;

define abstract class <structure-match> (<pattern-match>) end;

define abstract class <nested-match> (<structure-match>)
  constant slot match-nested-pattern,
    required-init-keyword: nested-pattern:;
end class;

define class <paren-match> (<nested-match>) end;
define class <bracket-match> (<nested-match>) end;
define class <brace-match> (<nested-match>) end;

define class <variable-match> (<structure-match>)
  constant slot match-variable-name-pattern,
    required-init-keyword: variable-name-pattern:;
  constant slot match-type-expression-pattern,
    required-init-keyword: type-expression-pattern:;
end class;

define class <property-list-match> (<structure-match>)
  constant slot match-rest-pattern = #f,
    init-keyword: rest-pattern:;
  constant slot match-key-patterns = #(),
    init-keyword: key-patterns:;
end class;

define class <rest-match> (<binding-match>) end;

define class <key-match> (<binding-match>) 
  constant slot match-default-expression = #f,
    init-keyword: default-expression:;
end class;

define class <key-sequence-match> (<sequence-match>) 
  constant slot match-default-expression = #f,
    init-keyword: default-expression:;
end class;

define class <splicing-match> (<structure-match>)
  constant slot match-nested-pattern,
    required-init-keyword: nested-pattern:;
  constant slot match-prefix :: false-or(<string>),
    required-init-keyword: prefix:;
  constant slot match-suffix :: false-or(<string>),
    required-init-keyword: suffix:;
end class;

define method wildcard-constraint? (constraint)
  constraint == #"*"
end method;

define method bounded-constraint? (constraint)
  constraint == #"body" | constraint == #"case-body" | constraint == #"body!"
end method;

/*
// TODO: Not yet used.

//// Main rule patterns.

define abstract class <main-rule-pattern> (<structure-match>) 
  constant slot match-main-pattern,
    required-init-keyword: main-pattern:;
end class;

define abstract class <define-rule-pattern> (<main-rule-pattern>)
  constant slot match-modifiers-pattern,
    required-init-keyword: modifiers-pattern:;
end class;

define class <define-body-pattern> (<define-rule-pattern>) end;
define class <define-list-pattern> (<define-rule-pattern>) end;

*/

//// Pattern traversal.

define method compute-binding-matches (m*)
  collecting ()
    do-binding-matches
      (method (name) collect(name) end, m*);
  end;
end method;

define method compute-bound-variable-names (m*)
  collecting ()
    do-binding-matches
      (method (name) collect(match-variable-name(name)) end, m*);
  end;
end method;

define method do-binding-matches (f, m*)
  for (m in m*) do-match-binding-matches(f, m) end;
end method;

define method do-match-binding-matches 
    (f, m :: type-union(<fragment>, <pattern-match>))
end method;

define method do-match-binding-matches (f, m :: <binding-match>)
  f(m);
end method;

define method do-match-binding-matches (f, m :: <nested-match>)
  do-binding-matches(f, match-nested-pattern(m));
end method;

define method do-match-binding-matches (f, m :: <variable-match>)
  do-match-binding-matches(f, match-variable-name-pattern(m));
  do-match-binding-matches(f, match-type-expression-pattern(m));
end method;

define method do-match-binding-matches (f, m :: <splicing-match>)
  do-match-binding-matches(f, match-nested-pattern(m));
end method;

define method do-match-binding-matches (f, m :: <property-list-match>)
  if (match-rest-pattern(m))
    do-match-binding-matches(f, match-rest-pattern(m));
  end;
  do-binding-matches(f, match-key-patterns(m));
end method;

//// Body pattern tail traversal.

define method do-body-match-tails (f :: <function>, m*) => ()
  for (m-tail = m* then m-tail.tail, until: empty?(m-tail))
    do-match-body-match-tails(f, m-tail.head, m-tail.tail);
  end;
end method;

define method do-match-body-match-tails 
    (f :: <function>, m :: type-union(<fragment>, <pattern-match>), m-tail)
 => ()
end method;

define method do-match-body-match-tails 
    (f :: <function>, m :: <simple-match>, m-tail) => ()
  if (bounded-constraint?(match-constraint(m)))
    f(m, m-tail);
  end;
end method;

define method do-match-body-match-tails 
    (f :: <function>, m :: <nested-match>, m-tail) => ()
  do-body-match-tails(f, match-nested-pattern(m));
end method;

 //// Valid constraints.

define constant $valid-constraints
  = #[#"*", #"token", #"name", #"expression", #"variable", #"body", #"body!",
      #"case-body", #"macro", #"symbol"];

define function valid-match-constraint? (constraint) => (well? :: <boolean>)
  constraint == #f // none specified
    | member?(constraint, $valid-constraints);
end function;

// eof
