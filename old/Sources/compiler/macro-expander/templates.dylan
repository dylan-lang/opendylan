Module:    infix-reader
Synopsis:  Templates
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*

  A template is simply a preserved token sequence - that is we remember 
  all of the information about the tokens that appear in the template so 
  that we can feed them back into the parser again later.

  So, given a template with no pattern variable in it such as:

    { let a = 1; let b = 2; a + b }

  we just record the token sequence classes and values:

    [let:, "let"] [<symbol>, a] [<binds>, "="] ...

  and then play them back at the parser when inserted by a macro call. 

  Because "free" pattern variables can appear in templates, we create a
  closure-like object around the raw token sequence at each macro call
  containing the pattern variable binding environment of the call. 
  Pattern variable substitution is done with respect to this environment
  at play back time.

  On finding a pattern variable when feeding a recorded token sequence
  back to the parser, we look up the variable's binding in the 
  accompanying environment. It can be bound to one of two things:

    - A template closure

      A template closure can appear as a value in another template
      closure's environment through the action of an auxilliary
      rule on a program fragment appearing in the macro call. In
      this case the inserted closure "takes over", feeding its tokens
      to the parser, until it is exhausted at which point we resume 
      feeding the parser with the tokens of the original closure. 

      Loosely, the effect is as if to splice in the tokens of the 
      inserted closure.

    - A parsed program fragment

      There are a couple of possible strategies for this. The best 
      seems to be to feed the parsed form to the grammar, augmenting
      the grammar to recognise classes of pre-parsed form. For 
      example:

         expression:
               binary-operand-series
         expression:
               PARSED-EXPRESSION

      2 + 2 
        -> {an <expression-fragment>}
          -> [parsed-expression: {an <expression-fragment>}]

      Parsed-xxx clauses simply return the value of the parsed 
      expression unchanged. Note also though perhaps:

         expression:
               PARSED-BODY

      which might be responsible for wrapping the body into a 
      begin ... end statement program fragment.

      A possible alternative would be to decompose the program 
      fragment back into its component tokens and feed those to
      the parser. This is (a) less efficient and (b) probably breaks
      things because it doesn't protect against token 
      juxtaposition errors (see p16 of the draft spec).

  The approach of making template closures rather than performing
  immediate substitution of environment variables should
  significantly reduces consing, particularly where templates contain
  a large number of tokens before or between pattern variables. It 
  should also save time assuming the same token sequence isn't
  inserted into the same template a large number of times. [?]

*/

define class <template> (<object>)
  slot tokens :: <list>,
    required-init-keyword: tokens:;
end class;

define method template (tokens)
  make(<template>, tokens: tokens);
end method;

define class <template-closure> (<object>)
  slot environment,
    required-init-keyword: environment:;
  slot template :: <template>,
    required-init-keyword: template:;
end class;

// TODO: Lazy template parsing can be a big win where large numbers of
// unparsed fragments are involved. Imagine iterating over the export
// clauses of a hefty module definition - if RHS's are eagerly processed,
// their output, much the same as their input, will get processed again
// and again as template results get spliced together.

// The wrinkle is in spotting empty substitution cases correctly.
// There's also an issue with the unfriendliness of a procedural template
// in the procedural macro system, but that could be solved with better 
// print methods.

// Thing is, lazy template parsing in the way it's currently done
// can also lose if, say, multiple substitutions are performed
// of a rewritten template.

// A compromise possibility is to expand a template eagerly, but to
// leave in references to substituted templates that have also
// been eagerly expanded rather than to expand them in line. The
// only cost here over the completely lazy approach is consing
// up the local expansion of the template and holding it in memory
// rather than making a template closure. However, it allows the
// original closure values in the environment to be released, so
// maybe that's not so bad. I suspect this gives us the best of both
// worlds.

// All this stuff is exceedingly dumb anyway - you shouldn't have to 
// walk though searching for substitutions. We can split a template 
// into sequences of literal tokens and substitutions once and
// for all ahead of time.

define method template-closure (t :: <template>, env)
  let closure = make(<template-closure>, environment: env, template: t);
  if (empty?(env.bindings))
    closure
  else
    let form = make(<template-closure>, 
                    environment: #f,
                    template:
                      make(<template>, tokens: expand-template(closure)));
    form;
  end;
end method;

// Expands out a template, performing substitution, separator folding, etc.
// The resulting token sequence is in the template token property list format
// plus references to non-empty inserted subtemplates. The subtemplates
// are expanded by reparse. This is so that we don't have to splice 
// subtemplate tokens at their insertion point, avoiding a bunch of
// unnecessary copying, particularly in aux-rule templates. 

define method expand-template (tc :: <template-closure>)
  // format(#t, "Expanding - "); force-output();
  let env = tc.environment;
  let the-tokens = tc.template.tokens;
  let sep-class = #f;
  let sep-value = #f;
  let expanded = #();
  block (return)
  local token-values (class, value)
    if (class == eoi:)
      // format(#t, "Done expanding~%"); force-output();
      return(reverse!(expanded))
    else
      expanded := pair(value, pair(class, expanded));
    end;
  end;
  local subtemplate (subt)
    expanded := pair(subt, expanded);
  end;
  while (#t)
    // format(#t, "Tokens: ~s~%", the-tokens); force-output();
    case
      the-tokens == #()
        => if (sep-class)
             let cls = sep-class;
             sep-class := #f;
             token-values(cls, sep-value);
           else
             token-values(eoi:, eoi:);
           end;
      instance?(the-tokens.first, <fragment>)
        => // format(#t, "frag ~s~%", the-tokens.first);
           let val = the-tokens.first;
           let val-tokens = fragment-tokens(val);
           the-tokens := the-tokens.tail;
           if (empty?(val-tokens))
             sep-class := #f;
           else
             the-tokens := concatenate(val-tokens, the-tokens);
             if (sep-class)
               the-tokens := pair(sep-class, pair(sep-value, the-tokens));
               sep-class := #f;
             end;
           end;
      instance?(the-tokens.first, <pattern-variable>)
        => let var = the-tokens.first; the-tokens := the-tokens.tail;
           let val = lookup(env, var);
           let (new-tokens, empty-substitution?) 
             = add-substituted-tokens(val, the-tokens);
           if (empty-substitution?)
             sep-class := #f;
           else
             the-tokens := new-tokens;
           end;
      instance?(the-tokens.first, <sequence-pattern-variable>)
        => let var = the-tokens.first; the-tokens := the-tokens.tail;
           let vals = bound-sequence(lookup(env, var));
           if (empty?(vals))
             sep-class := #f;
           else
             for (val in reverse(vals.tail))
               the-tokens := add-substituted-tokens(val, the-tokens);
               if (var.separator)
                 the-tokens 
                   := pair(var.separator.first, 
                           pair(var.separator.second, the-tokens));
               end;
             end;
             the-tokens := add-substituted-tokens(vals.head, the-tokens);
           end;
      instance?(the-tokens.first, <spliced-pattern-variable>)
        => let splice = the-tokens.first;
           let val = lookup(env, splice.pattern.underlying-pattern-variable);
           the-tokens := pair(splice-tokens(val, splice), the-tokens.tail);
      instance?(the-tokens.first, <coercing-substitution>)
        => let subst = the-tokens.first;
           let val = lookup(env, subst.name);
           the-tokens := pair(coerce-token(val, subst), the-tokens.tail);
      otherwise
        => if (sep-class)
             // We would've output a token so we know we should output
             // the separator.
             let cls = sep-class;
             sep-class := #f;
             token-values(cls, sep-value);
           elseif (instance?(the-tokens.first, <template-closure>))
             subtemplate(the-tokens.first);
             the-tokens := the-tokens.tail;
           else
             let token-class = the-tokens.first;
             let token-value = the-tokens.second;
             the-tokens := the-tokens.tail.tail;
             // Reclassification is done when reparsed.
             case
               token-class == #"<var-sep>" | 
                 token-class == #"<binary-operator>" |
                   token-class == #"<statement-sep>"
                 => sep-class := token-class;
                    sep-value := token-value;
               otherwise
                 => token-values(token-class, token-value);
             end;
           end if;
    end case;
  end while;
  end block;
end method;

// Given an expanded template closure, feed its contents to the parser.
// The only interesting thing here is the stack-based subexpansion 
// of inserted subtemplates.

define method reparse 
    (tc :: <template-closure>, 
       #key constraint = body-constraint:, parser = run-parser)
  // format(#t, "Parsing - "); force-output();
  let env = tc.environment;
  let the-tokens = wrap-for-reparse(tc.template.tokens, constraint);
  let env-stack = #();
  let token-stack = #();
  local lexer ()
    case
      the-tokens == #()
        => if (token-stack == #())
             // format(#t, "Done parsing~%"); force-output();
             token-values(eoi:, eoi:);
           else
             the-tokens := token-stack.head.tail;
             env := env-stack.head;
             token-stack := token-stack.tail;
             env-stack := env-stack.tail;
             lexer();
           end;
      instance?(the-tokens.first, <template-closure>)
        => // format(#t, "template ~s~%", the-tokens.first);
           let val = the-tokens.first;
           token-stack := pair(the-tokens, token-stack);
           env-stack := pair(env, env-stack);
           the-tokens := val.template.tokens;
           env := val.environment;
           lexer();
      otherwise
        => let token-class = the-tokens.first;
           let token-value = the-tokens.second;
           the-tokens := the-tokens.tail.tail;
             case
               reclassify-using-value?(token-class)
                 => tokenize-symbol(as(<string>, token-value));
               reclassify-using-class?(token-class)
                 => tokenize-symbol(as(<string>, token-class));
               otherwise
                 => token-values(token-class, token-value);
             end;
    end case;
  end;
  parser(#f, infix-dylan-parser, make(<lexer>, function: lexer));
end method;

define method reparse (t :: <template>, #rest options, #key)
  apply(reparse, template-closure(t, #()), options);
end method;

// Access the pattern variable underlying a spliceable substitution.

define method underlying-pattern-variable 
    (p :: <pattern-variable>)
  p
end method;

define method underlying-pattern-variable 
    (p :: <string-pattern-variable-pattern>)
  p.name
end method;

define method underlying-pattern-variable 
    (p :: <symbol-pattern-variable-pattern>)
  p.name
end method;

// Convert between the template and fragment token representations.

define method add-substituted-tokens (val :: <template-closure>, the-tokens)
  values(pair(val, the-tokens), empty?(val.template.tokens));
end method;

define method add-substituted-tokens (val :: <fragment>, tokens)
  let val-tokens = fragment-tokens(val);
  values(concatenate(val-tokens, tokens), empty?(val-tokens));
end method;

define method add-substituted-tokens (val :: <list>, tokens)
  let new-tokens = tokens;
  for (subval in reverse(val))
    new-tokens := add-substituted-tokens(subval, new-tokens);
  end;
  values(new-tokens, empty?(val));
end method;

define method convert-tokens (val == #())
  #()
end;

define method convert-tokens (val)
  let converted = #();
  for (ltok in val)
    if (instance?(ltok, <literal-fragment>))
      converted := pair(ltok.token-value, pair(ltok.token-class, converted));
    else
      converted := pair(ltok, converted);
    end;
  end;
  reverse!(converted);
end method;

define method coerce-token (f, s)
  error("Don't know how to coerce ~s for ~s substitution", f, s)
end method;

define method coerce-token (f :: <list>, s)
  if (size(f) = 1)
    coerce-token(f.first, s)
  else
    next-method()
  end
end method;

// Only called after a successful splice.

define method coerce-token 
    (f :: <fragment>, s :: <pattern-variable>)
  f
end method;

define method coerce-token 
    (f :: <fragment>, s :: <string-pattern-variable-pattern>)
  if (name-fragment?(f))
    make(<parsed-fragment>,
         token-class: #"<string>",
         token-value: as(<string>, f.name-symbol))
  else
    next-method()
  end
end method;

define method coerce-token 
    (f :: <fragment>, s :: <symbol-pattern-variable-pattern>)
  if (name-fragment?(f))
    make(<parsed-fragment>,
         token-class: #"<string>",
         token-value: f.name-symbol)
  else
    next-method()
  end
end method;

// Hack!!! We should use a specialist token here and reinstate
// semicolon as a separator in the above sep-removal treatment.

define constant $tokens-before = #(#"<simple-begin-word>", #"begin");
define constant $tokens-after = #(end:, "end", #"<statement-sep>", ";");

define method wrap-for-reparse (tokens, constraint)
  // concatenate($tokens-before, tokens, $tokens-after);
  concatenate(list(constraint, "begin"), tokens, list(end:, "end"))
end method;

define method fragment-tokens (f :: <bracketed-fragment>)
  concatenate(list(#"<lbracket>", "("),
              convert-tokens(f.fragments), 
              list(#"<rbracket>", ")"));
end method;

define method fragment-tokens (f :: <sbracketed-fragment>)
  concatenate(list(#"<lsbracket>", "["),
              convert-tokens(f.fragments), 
              list(#"<rsbracket>", "]"));
end method;

define method fragment-tokens (f :: <cbracketed-fragment>)
  concatenate(list(#"<lcbracket>", "{"),
              convert-tokens(f.fragments), 
              list(#"<rcbracket>", "}"));
end method;

define method fragment-tokens (f :: <grouped-fragment>)
  convert-tokens(f.fragments)
end method;

define method fragment-tokens (f :: <parsed-fragment>)
  list(f.token-class, f.token-value);
end method;

define method fragment-tokens (f :: <literal-fragment>)
  list(f.token-class, f.token-value);
end method;

define method splice-tokens (f, s)
  error("Don't know how to coerce ~s for ~s substitution.", f, s)
end method;

define method splice-tokens (f :: <list>, splice)
  if (size(f) = 1)
    splice-tokens(f.first, splice)
  else
    next-method()
  end
end method;

define method splice-tokens (f :: <fragment>, splice)
  if (~name-fragment?(f))
    next-method();
  else
    let spliced-symbol 
      = concatenate-symbols
          (splice.before | "", f.name-symbol, splice.after | "");
    let spliced-symbol-fragment
      = make(<parsed-fragment>, 
             token-class: parsed-name:,
             token-value: spliced-symbol);
    coerce-token(spliced-symbol-fragment, splice.pattern);
  end
end method;

// Misplaced utilities.

define method name-fragment? (f :: <object>)
  #f
end method;

define method name-fragment? (f :: <literal-fragment>)
  f.token-class == #"<symbol>"
end method;

define method name-fragment? (f :: <parsed-fragment>)
  f.token-class == (parsed-name:) 
    | (f.token-class == (parsed-expression:) 
         & instance?(f.token-value, <symbol>))
end method;

define method name-symbol (f :: <literal-fragment>)
  f.token-value
end method;

define method name-symbol (f :: <parsed-fragment>)
  f.token-value
end method;

// A hack "null" parser used for debugging macro expansions.

define method run-null-parser (id, grammar, lexer)
  let next-token = function-of(lexer);
  let tokens = #();
  block (return)
    next-token(); // This will be "begin" or equivalent.
    while (#t)
      let (type, value, string) = next-token();
      select (type)
        eoi: 
          => return();
        (parsed-expression:, parsed-literal:, parsed-body:, parsed-name:)
          => tokens := pair(format(#f, "`~s'", value), tokens);
        parsed-local-declaration:
          => tokens := pair(format(#f, "`~s'", value), tokens);
        #"<string>"
          => tokens := pair(format(#f, "~s", string), tokens);
        #"<keyword>"
          => tokens := pair(format(#f, "~a:", string), tokens);
        otherwise
          => tokens := pair(format(#f, "~a", string), tokens);
      end
    end;
  end;
  tokens := tokens.tail; // This will be "end" or equivalent.
  reduce(method (acc, string) concatenate(acc, string, " ") end, 
         "", reverse!(tokens));
end method;

// eof
