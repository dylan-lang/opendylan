Module:    infix-reader
Synopsis:  Constraint parsing
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// I haven't thought of a good way to do this yet so each constraint match
// is going to require at least two parses which is utter crap and must be
// replaced.

define method parse-constraint (type, tokens)
  let all-tokens = 
    pair(parsed-fragment
           (token-class: constraint-class(type),
            token-value: constraint-class(type)),
         tokens);
  let token-stack = all-tokens;
  let input-stack = #();
  let limit = size(token-stack);
  let count = 0;
  local lexer ()
    if (empty?(input-stack))
      if (count = limit)
        count := count + 1;
        token-values(end:, end:);
      else
        let token = token-stack.head;
        token-stack := token-stack.tail;
        input-stack := list(token);
        count := count + 1;
        lexer();
      end
    else
      let token = input-stack.head;
      input-stack := input-stack.tail;
      select (token by instance?)
        <literal-fragment>, <parsed-fragment>
          => token-values(token.token-class, token.token-value);
        <fragment>
          => let expanded = fragment-tokens(token);
             input-stack := concatenate(expanded, input-stack);
             lexer();
        otherwise 
          => let value = input-stack.head;
             input-stack := input-stack.tail;
             token-values(token, value);
      end;
    end;
  end;
  block (return)
    while (#t)
      block ()
        token-stack := all-tokens;
        input-stack := #();
        count := 0;
        let form = run-parser(#f, infix-dylan-parser, 
                              make(<lexer>, function: lexer));
        return(make(<parsed-fragment>, 
                    token-class: parsed-constraint-class(type),
                    token-value: form),
               token-stack);
      exception (c :: <reader-error>)
        *last-error* := c;
        // Warning: Do not optimise to (count > limit) here. Just because
        // all the tokens were consumed does not imply a substring won't
        // parse (e.g. { 1 + }).
        if (limit = 0)
          return(#f);
        else
          // Try one less token than at the failure point.
          limit := min(count, limit) - 1;
        end if;
      end block;
    end while;
  end block;
end method;

// eof
