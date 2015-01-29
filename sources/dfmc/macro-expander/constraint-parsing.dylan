Module:    dfmc-macro-expander
Synopsis:  Drive the parser to parse constraints during matching.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method parse-constraint
    (constraint-start :: <fragment>, tokens :: <fragment-list>)
 => (failure, remains, parsed)
  let all-tokens = pair(constraint-start, tokens);
  let token-stack = all-tokens;
  let nesting-stack = #();
  // TODO: PERORMANCE: If we only count top level tokens, we could win in
  // certain situations - if you fail anywhere within a nested fragment, no
  // subsequence of the nested fragment will win, so rewind back to the
  // next top level token.
  let limit = -1;
  let count = 0;
  local method lexer () => (f :: <fragment>)
    if (count == limit)
      $end-constraint;
    elseif (empty?(token-stack))
      if (empty?(nesting-stack))
        $end-constraint;
      else
        let close/tokens = nesting-stack.head;
        nesting-stack := nesting-stack.tail;
        token-stack := close/tokens.tail;
        count := count + 1;
        close/tokens.head;
      end;
    else
      let next-token = token-stack.head;
      token-stack := token-stack.tail;
      count := count + 1;
      if (instance?(next-token, <nested-fragment>))
        nesting-stack
          := pair(pair(fragment-right-delimiter(next-token), token-stack),
                  nesting-stack);
        token-stack := fragment-nested-fragments(next-token);
        fragment-left-delimiter(next-token);
      else
        next-token
      end;
    end;
  end;
  block (return)
    while (#t)
      block (retry)
        token-stack := all-tokens;
        nesting-stack := #();
        count := 0;
        let form = re-read-fragments(lexer, on-error: retry);
        return(#f, token-stack, form);
      end block;
      // Warning: Do not optimise to (count > limit) here. Just because
      // all the tokens were consumed does not imply a substring won't
      // parse (e.g. { 1 + }).
      if (limit == 0)
        return(#t, #f, #f);
      else
        // Try one less token than at the failure point.
        limit := count - 1;
      end if;
    end while;
  end block;
end method;

// TODO: CORRECTNESS: The way boundedness is checked is buggy.

define method parse-bounded-constraint
    (constraint-start :: <fragment>,
       tokens :: <fragment-list>, stop? :: <function>, stop-arg)
 => (failure, remains :: false-or(<fragment-list>), parsed)
  let all-tokens = pair(constraint-start, tokens);
  let token-stack = all-tokens;
  let nesting-stack = #();
  // TODO: PERORMANCE: If we only count top level tokens, we could win in
  // certain situations - if you fail anywhere within a nested fragment, no
  // subsequence of the nested fragment will win, so rewind back to the
  // next top level token.
  let limit = -1;
  let count = 0;
  local method lexer () => (f :: <fragment>)
    if (count == limit)
      $end-constraint;
    elseif (empty?(token-stack))
      if (empty?(nesting-stack))
        $end-constraint;
      else
        let close/tokens = nesting-stack.head;
        nesting-stack := nesting-stack.tail;
        token-stack := close/tokens.tail;
        count := count + 1;
        close/tokens.head;
      end;
    else
      let next-token = token-stack.head;
      if (instance?(next-token, <nested-fragment>))
        token-stack := token-stack.tail;
        count := count + 1;
        nesting-stack
          := pair(pair(fragment-right-delimiter(next-token), token-stack),
                  nesting-stack);
        token-stack := fragment-nested-fragments(next-token);
        fragment-left-delimiter(next-token);
      elseif (nesting-stack == #() & stop?(next-token, stop-arg))
        $end-constraint;
      else
        token-stack := token-stack.tail;
        count := count + 1;
        next-token
      end;
    end;
  end;
  block (return)
    while (#t)
      block (retry)
        token-stack := all-tokens;
        nesting-stack := #();
        count := 0;
        let form = re-read-fragments(lexer, on-error: retry);
        return(#f, token-stack, form);
      end block;
      // Warning: Do not optimise to (count > limit) here. Just because
      // all the tokens were consumed does not imply a substring won't
      // parse (e.g. { 1 + }).
      if (limit == 0)
        return(#t, #f, #f);
      else
        // Try one less token than at the failure point.
        limit := count - 1;
      end if;
    end while;
  end block;
end method;

define method parse-bounded-constraint-no-backtracking
    (constraint-start :: <fragment>,
       tokens :: <fragment-list>, stop? :: <function>, stop-arg)
 => (failure, remains :: false-or(<fragment-list>), parsed)
  let all-tokens = pair(constraint-start, tokens);
  let token-stack = all-tokens;
  let nesting-stack = #();
  // TODO: PERORMANCE: If we only count top level tokens, we could win in
  // certain situations - if you fail anywhere within a nested fragment, no
  // subsequence of the nested fragment will win, so rewind back to the
  // next top level token.
  local method lexer () => (f :: <fragment>)
    if (empty?(token-stack))
      if (empty?(nesting-stack))
        $end-constraint;
      else
        let close/tokens = nesting-stack.head;
        nesting-stack := nesting-stack.tail;
        token-stack := close/tokens.tail;
        close/tokens.head;
      end;
    else
      let next-token = token-stack.head;
      if (instance?(next-token, <nested-fragment>))
        token-stack := token-stack.tail;
        nesting-stack
          := pair(pair(fragment-right-delimiter(next-token), token-stack),
                  nesting-stack);
        token-stack := fragment-nested-fragments(next-token);
        fragment-left-delimiter(next-token);
      elseif (nesting-stack == #() & stop?(next-token, stop-arg))
        $end-constraint;
      else
        token-stack := token-stack.tail;
        next-token
      end;
    end;
  end;
  token-stack := all-tokens;
  nesting-stack := #();
  let form = re-read-fragments(lexer);
  values(#f, token-stack, form);
end method;

// Template parsing is simpler, not having delimited nested fragments
// to account for, and not requiring backtracking.

define serious-program-warning <template-parser-error> (<reader-error>)
  slot condition-token,
    required-init-keyword: token:;
  slot condition-template,
    required-init-keyword: template:;
  format-string
    "Unexpected token %= encountered in macro expansion: %=.";
  format-arguments
    token, template;
end serious-program-warning;

define serious-program-warning <unexpected-end-of-template-error>
    (<reader-error>)
  slot condition-template,
    required-init-keyword: template:;
  format-string
    "Unexpected end of macro expansion: %=.";
  format-arguments
    template;
end serious-program-warning;

define method parse-template-fragments-as
    (constraint-start, f* :: <fragment-list>)
 => (failure, f)
  let input-cursor = pair(constraint-start, f*);
  let nesting-stack = #();
  local method lexer () => (f :: <fragment>)
    if (empty?(input-cursor))
      if (empty?(nesting-stack))
        $end-constraint
      else
        let close/tokens = nesting-stack.head;
        nesting-stack := nesting-stack.tail;
        input-cursor := close/tokens.tail;
        let close = close/tokens.head;
        if (close) close else lexer() end;
      end;
    else
      let next-input = input-cursor.head;
      input-cursor := input-cursor.tail;
      if (instance?(next-input, <template>))
        nesting-stack := pair(pair(#f, input-cursor), nesting-stack);
        input-cursor := template-fragments(next-input);
        lexer();
      elseif (instance?(next-input, <list>))
        nesting-stack := pair(pair(#f, input-cursor), nesting-stack);
        input-cursor := next-input;
        lexer();
      elseif (instance?(next-input, <sequence-fragment>))
        nesting-stack := pair(pair(#f, input-cursor), nesting-stack);
        input-cursor := fragment-fragments(next-input);
        lexer();
      elseif (instance?(next-input, <nested-fragment>))
        nesting-stack
          := pair(pair(fragment-right-delimiter(next-input), input-cursor),
                  nesting-stack);
        input-cursor := fragment-nested-fragments(next-input);
        fragment-left-delimiter(next-input);
      else
        next-input
      end;
    end;
  end method;
  local method error-handler (token-type, fragment, history)
    if (fragment == $end-constraint)
      note(<unexpected-end-of-template-error>,
           source-location: fragment-source-location(f*.last),
           template:        make(<template>, fragments: as(<list>, f*)));
    else
      note(<template-parser-error>,
           source-location: fragment-source-location(fragment),
           token:           fragment,
           template:        make(<template>, fragments: as(<list>, f*)));
    end;
  end method;
  block ()
    values(#f, re-read-fragments(lexer, on-error: error-handler));
  // exception (e :: <reader-error>)
  //  values(e, #f);
  end;
end method;

//// Packaged parsers for optimized direct use.

define method as-expression (t :: <template>)
  let (failure, parsed-f)
    = parse-template-fragments-as
        ($start-expression-constraint, template-fragments(t));
  if (failure)
    error("Template %= wouldn't parse as an expression: %s.",
          parsed-f, failure);
  end;
  parsed-f
end method;

define method as-expression (stuff)
  macro-case (stuff)
    { ?:expression } => expression
  end;
end method;
