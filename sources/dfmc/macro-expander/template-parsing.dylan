Module:    dfmc-macro-expander
Synopsis:  Template parsing
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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

define method re-read-template-elements (start-constraint, elements)
  // format(#t, "Parsing - "); force-output();
  let the-tokens = elements;
  let token-stack = #();
  local method lexer () => (fragment)
    case
      the-tokens == #()
        => if (token-stack == #())
             // format(#t, "Done parsing~%"); force-output();
             $end-constraint;
           else
             the-tokens := token-stack.head.tail;
             token-stack := token-stack.tail;
             lexer();
           end;
      instance?(the-tokens.first, <list>)
        => // format(#t, "template ~s~%", the-tokens.first);
           let val = the-tokens.first;
           token-stack := pair(the-tokens, token-stack);
           the-tokens := val;
           lexer();
      otherwise
        => let token = the-tokens.first;
           the-tokens := the-tokens.tail;
           token;
    end case;
  end;
  re-read-fragments(lexer);
end method;
