Module:    dfmc-macro-expander
Synopsis:  Generate a function for template substitution operations.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define serious-program-warning
    <substitution-of-unbound-pattern-variable> (<manual-parser-error>)
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string
    "Reference to unbound pattern variable %= while expanding macro call.";
  format-arguments
    variable-name;
end serious-program-warning;

define macro substitution-method
  { substitution-method (?subst:expression) ?:body end }
    => { let ?=index = element-env-index(?subst);
         if (?=index >= 0)
           match-method ?body end
         else
           let name = element-variable-name(?subst);
           let loc = element-source-location(?subst);
           match-method
             note(<substitution-of-unbound-pattern-variable>,
                  source-location: loc,
                  variable-name:   name);
           end
         end }
end macro;

define method generate-substitution-function
    (subst :: <simple-element-substitution>)
 => (code :: <substitution-method>)
  substitution-method (subst)
    import-to-template(lookup-match(env, index))
  end;
end method;

define method generate-substitution-function
    (subst :: <as-string-substitution>)
 => (code :: <substitution-method>)
  substitution-method (subst)
    substitute-as-string(lookup-match(env, index))
  end;
end method;

define method generate-substitution-function
    (subst :: <as-symbol-substitution>)
 => (code :: <substitution-method>)
  substitution-method (subst)
    substitute-as-symbol(lookup-match(env, index))
  end;
end method;

define method generate-substitution-function
    (subst :: <splicing-substitution>)
 => (code :: <substitution-method>)
  let inner = element-name-substitution(subst);
  let prefix = element-prefix(subst);
  let suffix = element-suffix(subst);
  let splicer = splicing-function(inner);
  substitution-method (inner)
    splicer(prefix, lookup-match(env, index), suffix)
  end;
end method;

define method splicing-function
    (subst :: <as-string-substitution>) => (function :: <function>)
  substitute-spliced-as-string
end method;

define method splicing-function
   (subst :: <as-symbol-substitution>) => (function :: <function>)
  substitute-spliced-as-symbol
end method;

define method splicing-function
    (subst :: <simple-element-substitution>) => (function :: <function>)
  substitute-spliced-as-name
end method;

define method generate-substitution-function
    (subst :: <simple-sequence-substitution>)
 => (code :: <substitution-method>)
  let separator = element-separator(subst);
  if (separator)
    let separator = generate-constructor-function(separator);
    substitution-method (subst)
      substitute-sequence-separated
        (lookup-match(env, index), separator(env))
    end;
  else
    substitution-method (subst)
      substitute-sequence(lookup-match(env, index))
    end;
  end;
end method;

define method generate-substitution-function
    (subst :: <aux-rule-call-substitution>)
 => (code :: <substitution-method>)
  let aux-rule-env = element-aux-rule-env(subst);
  let aux-rule-index = element-aux-rule-index(subst);
  let argument-function = element-compiled-template(subst);
  match-method
    let argument = argument-function(env);
    let rewrite = lookup-aux-rule-function(aux-rule-env, aux-rule-index);
    rewrite(as-fragment-tokens(argument));
  end;
end method;

define serious-program-warning
    <non-macro-in-template-macro-call> (<manual-parser-error>)
  slot condition-fragment,
    required-init-keyword: fragment:;
  format-string
    "Template macro call ?@{ %s } is not to a known macro.";
  format-arguments
    fragment;
end serious-program-warning;

define method generate-substitution-function
    (subst :: <macro-call-substitution>)
 => (code :: <substitution-method>)
  let argument-function = element-compiled-template(subst);
  match-method
    let macro-fragment = argument-function(env);
    let (failed?, f*, expansion)
      = match-macro-constraint(as-fragment-tokens(macro-fragment));
    if (failed? | ~empty?(f*))
      note(<non-macro-in-template-macro-call>,
           source-location: element-source-location(subst),
           fragment: macro-fragment);
    else
      expansion
    end;
  end;
end method;
