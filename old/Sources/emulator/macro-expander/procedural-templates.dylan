Module: infix-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Compilation.

define class <ptc-state> (<object>)
  slot pattern-variables = #();
  slot pattern-variable-values = #();
  slot sequence-pattern-variables = #();
  slot sequence-pattern-variable-values = #();
end class;

define method compile-procedural-template 
    (t :: <template>) => (free-names)
  let state = make(<ptc-state>);
  for (token in t.tokens)
    compile-template-token(token, state)
  end;
  values(state.pattern-variables,
         state.pattern-variable-values,
         state.sequence-pattern-variables,
         state.sequence-pattern-variable-values)
end method;

define method compile-template-token 
    (token, state) => ()
end method;

define method compile-template-token 
    (token :: <pattern-variable>, state) => ()
  state.pattern-variables 
    := add-new!(state.pattern-variables, token.name);
  state.pattern-variable-values 
    := add-new!(state.pattern-variable-values, token.name);
end method;

define method compile-template-token 
    (token :: <sequence-pattern-variable>, state) => ()
  state.sequence-pattern-variables 
    := add-new!(state.sequence-pattern-variables, token.name);
  state.sequence-pattern-variable-values 
    := add-new!(state.sequence-pattern-variable-values, token.name);
end method;

define method compile-template-token 
    (token :: <spliced-pattern-variable>, state) => ()
  compile-template-token(token.pattern, state);
end method;

define method compile-template-token 
    (token :: <coercing-substitution>, state) => ()
  compile-template-token(token.name, state);
end method;

define method compile-template-token 
    (token :: <expression-substitution>, state) => ()
  let new-name = generate-name();
  token.name := new-name;
  state.pattern-variables
    := add!(state.pattern-variables, new-name);
  state.pattern-variable-values
    := add!(state.pattern-variable-values, token.expression);
end method;

define method compile-template-token 
    (token :: <sequence-expression-substitution>, state) => ()
  let new-name = generate-name();
  token.name := new-name;
  state.sequence-pattern-variables 
    := add-new!(state.sequence-pattern-variables, new-name);
  state.sequence-pattern-variable-values 
    := add-new!(state.sequence-pattern-variable-values, token.expression);
end method;

define variable *generated-name-count* = -1;

define method generate-name ()
  *generated-name-count* := *generated-name-count* + 1;
  as(<symbol>, 
     format(#f, "anonymous-subtitution-~a", *generated-name-count*));
end method;

// Evaluation.

define method evaluate-procedural-template 
    (t :: <template>, names, values, seq-names, seq-values) 
      => (fragment)
  let env = make(<env>);
  let name-bindings
    = map(method (name, val)
            pair(name, as-fragment(val).fragments)
          end, 
          names, values);
  let name-seq-bindings
    = map(method (name, vals)
            pair(name, 
                 map-as(<vector>, compose(fragments, as-fragment), vals))
          end, 
          seq-names, seq-values);
  env.bindings := concatenate(name-bindings, name-seq-bindings);
  /*
  externalise-fragment
    (reparse(template-closure(t, env), constraint: fragment-constraint:));
  */
  template-closure(t, env);
end method;

define method as-fragment (val :: <sequence-fragment>)
  val
end method;

define method as-fragment (val :: <fragment>)
  sequence-fragment(fragments: list(val))
end method;

define method as-fragment (val :: <parsed-fragment>)
  sequence-fragment(fragments: list(val))
end method;

define method as-fragment (val :: <literal-fragment>)
  sequence-fragment(fragments: list(val))
end method;

define method as-fragment (val :: <object>)
  as(<literal>, val)
end method;

// T'other way.

define method as-object (f :: <literal-fragment>)
  f.token-value
end method;

define method as (class == <symbol>, f :: <literal-fragment>)
  select (f.token-class)
    (#"<symbol>", #"<keyword>") =>  f.token-value;
    #"<literal>" => f.token-value;
  end;
end method;

define method externalise-fragment (fragment)
  fragment
end method;

define method externalise-fragment (f :: <sequence-fragment>)
  if (f.fragments.size = 1) externalise-fragment(f.fragments.first) else f end;
end method;

// Abstract syntax.

define abstract class <form> (<object>) end;

define abstract class <parsed-form> (<form>) end;

define abstract class <body> (<parsed-form>) end;
define abstract class <expression> (<parsed-form>) end;
define abstract class <literal> (<parsed-form>) end;

define abstract class <fragment-form> (<form>) end;

// Dreadful!

define constant hack-var = make(<pattern-variable>, name: #"hack");
define constant hack-t = make(<template>, tokens: list(hack-var));
define constant hack-env = make(<env>);

define method parse (form :: <sequence-fragment>)
  bind!(hack-env, hack-var, form.fragments);
  reparse(template-closure(hack-t, hack-env));
end method;

define method reparse 
    (form :: <fragment>, #rest options, #key constraint)
  bind!(hack-env, hack-var, form.as-fragment.fragments);
  reparse(template-closure(hack-t, hack-env), constraint: constraint);
end method;

define method as (class == <expression>, form :: <sequence-fragment>)
  parse(form)
end method;

define method parsed-literal (val)
  sequence-fragment
    (fragments:
       list(parsed-fragment(token-class: parsed-literal:, token-value: val)))
end method;

define method as (class == <literal>, r :: <real>)
  parsed-literal(r)
end method;

define method as (class == <literal>, r :: <list>)
  parsed-literal(r)
end method;

define method as (class == <literal>, r :: <simple-object-vector>)
  parsed-literal(r)
end method;

define method as (class == <literal>, r :: <byte-string>)
  parsed-literal(r)
end method;

define method as (class == <literal>, r :: <character>)
  parsed-literal(r)
end method;

define method as (class == <literal>, r :: <boolean>)
  parsed-literal(r)
end method;

define method as (class == <literal>, r :: <symbol>)
  parsed-literal(r)
end method;

// Utility.

define method same-pattern-variable?
    (a :: <pattern-variable>, b :: <pattern-variable>) => (well? :: <boolean>)
  a.name == b.name
end method;

define method same-pattern-variable?
    (a :: <sequence-pattern-variable>, b :: <sequence-pattern-variable>) 
      => (well? :: <boolean>)
  a.name == b.name
end method;

// eof
