Module:    infix-reader
Synopsis:  Constrained pattern variables
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Constraint protocol

define generic constraint-name? (name);
define generic wildcard-constraint-name? (name);

// Defaults

define method constraint-name? (name :: <symbol>)
  #f;
end method;

define method constraint-class (name :: <symbol>)
  as-keyword(concatenate(as(<string>, name), "-constraint"))
end method;

define method parsed-constraint-class (name :: <symbol>)
  as-keyword(concatenate("parsed-", as(<string>, name)))
end method;

define method wildcard-constraint-name? (name :: <symbol>)
  #f;
end method;

//// Standard constraint types

// Tokens

define method constraint-name? (name == #"token")
  #t;
end method;

define method parse-constraint (type == #"token", tokens)
  if (empty?(tokens) | ~instance?(tokens.first, <literal-fragment>)) 
    values(#f, tokens)
  else
    values(list(tokens.head), tokens.tail)
  end
end method;

// Expressions

define method constraint-name? (name == #"expression")
  #t;
end method;

// Variables

define method constraint-name? (name == #"variable")
  #t;
end method;

define method parse-constraint (type == #"variable", tokens)
  let (parsed-f, remains) = next-method();
  if (parsed-f)
    values(fragmentize-variable(parsed-f.token-value), remains);
  else
    values(parsed-f, remains)
  end
end method;

// Names

define method constraint-name? (name == #"name")
  #t;
end method;

define method parse-constraint (type == #"name", tokens)
  if (~empty?(tokens) & name-fragment?(tokens.first))
    values(parsed-fragment
            (token-class: parsed-name:, 
             token-value: name-symbol(tokens.first)),
           tokens.tail)
  else
    next-method()
  end
end method;

// Symbols (Functional Objects extension)

define method constraint-name? (name == #"symbol")
  #t
end method constraint-name?;

define method keyword-literal? (token)
  #f
end method keyword-literal?;

define method keyword-literal? (token :: <literal-fragment>)
  token.token-class == #"<keyword>"
  | (token.token-class == #"<literal>"
     & instance?(token.token-value, <symbol>))
end method keyword-literal?;

define method keyword-literal? (token :: <parsed-fragment>)
  token.token-class == parsed-expression:
  | begin
      let sexpr = token.token-value;
      instance?(sexpr, <pair>) 
      & sexpr.head == #"quote"
      & instance?(sexpr.second, <symbol>)
    end
end method keyword-literal?;

define method parse-constraint (type == #"symbol", tokens)
  if (~empty?(tokens) & keyword-literal?(tokens.first))
    values(list(tokens.head), tokens.tail)
  else
    values(#f, tokens)
  end if
end method parse-constraint;

// Bodies

define method constraint-name? (name == #"body")
  #t;
end method;

// Case bodies

define method constraint-name? (name == #"case-body")
  #t;
end method;

// Strip the "parsed form" wrapper so that it can be further destructured.

define method parse-constraint (type == #"case-body", tokens)
  let (parsed-f, remains) = next-method();
  values(fragmentize-case-body(parsed-f.token-value), remains);
end method;

// Wildcards

define method constraint-name? (name == #"*")
  #t;
end method;

define method wildcard-constraint-name? (name == #"*")
  #t;
end method;

// Macro expansion

define method constraint-name? (name == #"macro")
  #t;
end method;

define method parsed-constraint-class (name == #"macro")
  parsed-expression:
end method;

define method expanded-constraint (form)
  let m = #f;
  if (instance?(form, <pair>) & (m := lookup-macro(form.head, default: #f)))
    expand-template-macro-call
      (m, form, constraint: fragment-constraint:)
  else
    reader-error(#f, "Form %= is not a macro call.", form);
  end
end method;

define method expanded-macro-call (form)
  if (instance?(form, <pair>) & lookup-expander(form.head, default: #f))
    expand-template-macro-call
     (element($macros, form.head), form,
      constraint: fragment-constraint:, parser: run-null-parser)
  else
    form
  end
end method;

define method expand-template-macro-call (m, form, #rest options)
  if (instance?(m.main-rules.first, <function-rule>))
    apply(expand-template-macro, 
          m, sequence-fragment(fragments: fragmentize-list(form.tail)),
          options)
  elseif (instance?(form.second, <vector>))
    let f 
      = form.second;
    let modifiers 
      = sequence-fragment(fragments: fragmentize-sequence(f.first));
    let body-fragment 
      = f.second;
    apply(expand-template-macro, m, 
          modified-fragment(modifiers: modifiers, fragment: body-fragment),
          options);
  else
    apply(expand-template-macro, m, form.second, options)
  end
end method;

define method parse-constraint (type == #"macro", tokens)
  let (parsed-f, tokens) = next-method();
  if (parsed-f & instance?(parsed-f.token-value, <sequence-fragment>))
    values(parsed-f.token-value.fragments, tokens)
  else
    values(parsed-f, tokens)
  end
end method;

// eof
