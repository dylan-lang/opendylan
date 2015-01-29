Module:    dfmc-macro-expander
Synopsis:  Classes describing template substitution operations.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Template elements.

// Template elements are either substitutions or just fragments.

//// Substitutions.

define abstract class <substitution> (<object>)
  constant slot element-source-location = #f,
    init-keyword: source-location:;
end class;

define sealed domain make (subclass(<substitution>));
define sealed domain initialize (<substitution>);

define abstract class <variable-substitution> (<substitution>)
  slot element-variable-name,
    required-init-keyword: variable-name:;
  slot element-env-index :: <integer> = -1;
end class;

// Some substitutions are guaranteed to be non-empty, in particular
// the substitutions involving name splicing or coercion.

define abstract class <non-empty-substitution> (<substitution>) end;
define abstract class <maybe-empty-substitution> (<substitution>) end;

define abstract class <element-substitution>
    (<maybe-empty-substitution>, <variable-substitution>)
end class;
define abstract class <sequence-substitution>
    (<maybe-empty-substitution>, <variable-substitution>)
end class;

define class <simple-element-substitution> (<element-substitution>) end;

define abstract class <name-substitution>
    (<non-empty-substitution>, <variable-substitution>)
end class;

define class <as-string-substitution> (<name-substitution>) end;
define class <as-symbol-substitution> (<name-substitution>) end;

// A splicing substitution encloses one of the above.

define class <splicing-substitution> (<name-substitution>)
  constant slot element-name-substitution,
    required-init-keyword: name-substitution:;
  constant slot element-prefix :: <string> = "",
    init-keyword: prefix:;
  constant slot element-suffix :: <string> = "",
    init-keyword: suffix:;
end class;

define class <simple-sequence-substitution> (<sequence-substitution>)
  constant slot element-separator,
    required-init-keyword: separator:;
end class;

define abstract class <call-substitution> (<substitution>)
  constant slot element-template,
    required-init-keyword: template:;
  slot element-compiled-template;
end class;

define class <macro-call-substitution> (<call-substitution>) end;

define class <aux-rule-call-substitution> (<call-substitution>)
  constant slot element-rule-name,
    required-init-keyword: rule-name:;
  slot element-aux-rule-env :: <simple-object-vector>;
  slot element-aux-rule-index :: <integer>;
end class;

//// Iteration.

define method do-template-substitutions
    (f :: <function>, seq :: <sequence>) => ()
  for (elt in seq) do-template-substitutions(f, elt) end;
end method;

define method do-template-substitutions
    (f :: <function>, elt :: <object>) => ()
  // do nothing
end method;

define method do-template-substitutions
    (f :: <function>, elt :: <substitution>) => ()
  f(elt);
end method;

define method do-template-substitutions
    (f :: <function>, elt :: <splicing-substitution>) => ()
  do-template-substitutions(f, element-name-substitution(elt));
end method;

define method do-template-substitutions
    (f :: <function>, elt :: <nested-fragment>) => ()
  do-template-substitutions(f, fragment-nested-fragments(elt));
end method;
