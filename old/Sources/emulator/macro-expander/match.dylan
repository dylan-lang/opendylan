Module:    infix-reader
Synopsis:  The core matching engine
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*

  The generic fragment grammar generates a fragment list used as input
  to the appropriate expander.

  In order to allow this system to co-exist with prefix fragments, we
  can observe that parsed fragments seen by the expander engine are only
  ever generated under the control of the expander itself through the
  action of parsing as a constraint. This allows us to use the parsed 
  fragment, an s-expression, wrapped with the information we need to 
  successfully re-insert it into the input of the parser. 

  That is, when we parse the constraint ?expr, a wrapped s-expression
  containing the label "expr" is recorded. This is necessary because 
  there is ambiguity in the output of the infix->prefix translation.
  E.g.

    foo :: <integer> -> (foo <integer>)
    foo(<integer>)   -> (foo <integer>)

  Therefore we can't determine the parser class reliably from the
  s-expression itself.

*/

//// Match conditions.

define method match-error (#rest stuff)
  apply(reader-error, #f, stuff);
end method;

define class <match-failure> (<error>)
end class;

define class <pattern-match-failure> (<match-failure>)
  slot fragment,
    init-keyword: fragment:;
  slot pattern,
    init-keyword: pattern:;
  slot cause,
    init-value: #f,
    init-keyword: cause:;
end class;

//// Match protocol.

define generic match
  (pattern, fragment, more-patterns, more-fragments, env, fail);

define generic match-empty
  (pattern, more-patterns, env, fail);

//// The top-level matching algorithm.

// Match-body and match-list decompose their inputs into submatches
// sequences that are then fed to the guts of the matcher.

define constant semicolon? = method (token) token == *semicolon* end;
define constant comma? = method (token) token == *comma* end;

define method match-body (p*, f*, e, fail)
  // format(#t, "Matching - "); force-output();
  let submatches 
    = compute-submatches(p*, f*, fail, semicolon?);
  for (submatch in submatches)
    match-list(submatch.head, submatch.tail, e, fail);
  end;
  // format(#t, "Done matching.~%"); force-output();
end method;

define method match-list (p*, f*, e, fail)
  let submatches 
    = compute-submatches(p*, f*, fail, comma?);
  for (submatch in submatches)
    match-sequence(submatch.head, submatch.tail, e, fail);
  end;
end method;

define method match-sequence (p*, f*, e, fail)
  match-tokens(p*, f*, e, fail);
end method;

// For a given separator, pair-off corresponding sections. 

define method compute-submatches (p*, f*, fail, separator?)
 local compute-submatches (p*, f*, fail, separator?)
  block (return)
    // Case 0: If both the pattern and fragment are empty, there are
    // no submatches to check.
    if (empty?(p*) & empty?(f*)) 
      return(#());
    end;
    let (p*-before, p*-after) = split(p*, separator?);
    // Case 1: The pattern doesn't contain a separator.
    if (~p*-after) 
      // p* = { ?p1 ?p2 }
      // f* = any
      return(list(pair(p*, f*)))
    end;
    // Case 2: The pattern contains a separator but the fragment doesn't.
    let (f*-before, f*-after) = split(f*, separator?);
    if (~f*-after)
      // p* = { ?p1 ; ?p2 }
      // f* = { f1 f2 }
      // This only matches if ?p2 matches the empty fragment.
      if (~empty?(f*-before))
        return(list(pair(p*-before, f*-before), 
                    pair(p*-after.tail, #())));
      else
        fail(p*, f*);
      end
    end;
    // Case 3: Both the pattern and the fragment contain the separator.
    return(pair(pair(p*-before, f*-before), 
                compute-submatches
                  (p*-after.tail, f*-after.tail, fail, separator?)))
  end;
 end;
 compute-submatches(p*, f*, fail, separator?);
end method;

// Match a pattern fragment stream against a program fragment stream.

define method match-tokens (p* :: <list>, f* :: <list>, e, fail)
  case
    empty?(p*) & empty?(f*)
      => #t;
    empty?(p*)
      => fail(p*, f*, cause: "more fragments than expected");
    empty?(f*)
      => match-empty(p*.head, p*.tail, e, fail);
    otherwise
      => match(p*.head, f*.head, p*.tail, f*.tail, e, fail);
  end;
end method;

// Fail by default.
define method match (p, f, p*, f*, e, fail)
  fail(p, f);
end method;

define method match (p :: <literal-fragment>, f :: <parsed-fragment>,
                     p*, f*, e, fail)
  if (p.object ~= f.token-value)
    fail(p, f);
  end;
  match-tokens(p*, f*, e, fail);
end method;

define method match (p :: <literal-fragment>, f :: <literal-fragment>,
                     p*, f*, e, fail)
  if (p.object ~= f.object)
    fail(p, f);
  end;
  if (p.object ~== #"otherwise")
    match-tokens(p*, f*, e, fail);
  else
    match-otherwise(p*, f*, e, fail)
  end;
end method;

// The "otherwise [=>]" special case hack. Cursor is just beyond the
// otherwise in both the pattern and the fragment.

define method match-otherwise (p*, f*, e, fail)
  // Pattern:  { otherwise => }
  // Fragment: { otherwise <not-implies> }
  if (~empty?(p*) & p*.head == *implies* 
        & (empty?(f*) | f*.head ~== *implies*))
    match-tokens(p*.tail, f*, e, fail); // Lose the => in the pattern.
  // Pattern:  { otherwise }
  // Fragment: { otherwise => }
  elseif (~empty?(f*) & f*.head == *implies*
            & (empty?(p*) | p*.head ~== *implies*))
    match-tokens(p*, f*.tail, e, fail); // Lose the => in the fragment.
  else
    match-tokens(p*, f*, e, fail);
  end;
end method;

// Hack!!! Lose this duplication.

define method match (p :: <bracketed-fragment>, f :: <bracketed-fragment>,
                     p*, f*, e, fail)
  match-body(p.fragments, f.fragments, e, fail);
  match-tokens(p*, f*, e, fail);
end method;

define method match (p :: <sbracketed-fragment>, f :: <sbracketed-fragment>,
                     p*, f*, e, fail)
  match-body(p.fragments, f.fragments, e, fail);
  match-tokens(p*, f*, e, fail);
end method;

define method match (p :: <cbracketed-fragment>, f :: <cbracketed-fragment>,
                     p*, f*, e, fail)
  match-body(p.fragments, f.fragments, e, fail);
  match-tokens(p*, f*, e, fail);
end method;

define method match-empty (p, p*, e, fail)
  fail(p, #());
end method;

define method match-empty (p :: <unconstrained-pattern-variable>, p*, e, fail)
  if (empty?(p*))
    bind!(e, p, #());
  else
    next-method();
  end;
end method;

define method match-empty (p :: <constrained-pattern-variable>, p*, e, fail)
  if (empty?(p*))
    let (form, remains) = parse-constraint(p.type, #());
    if (~form)
      fail(p, #(), cause: "doesn't satisfy constraint");
    end;
    bind!(e, p, form);
  else
    next-method();
  end;
end method;

define method match-token (p, f)
  #f
end method;

define method match-token (p :: <literal-fragment>, f :: <literal-fragment>)
  p.object = f.object
end method;

// Hack!!! Lose this duplication.

define method match-token 
    (p :: <bracketed-fragment>, f :: <bracketed-fragment>)
  #t
end method;

define method match-token 
    (p :: <sbracketed-fragment>, f :: <sbracketed-fragment>)
  #t
end method;

define method match-token 
    (p :: <cbracketed-fragment>, f :: <cbracketed-fragment>)
  #t
end method;

// Unconstrained pattern variables match zero or more input tokens,
// smallest match tried first. As an optimisation, we peek at the
// next pattern to see if it's distinctive in any way - if so
// we can save time by searching for it in the input sequence.
define method match (p :: <unconstrained-pattern-variable>, f, 
                     p*, f*, e, fail)
  block (return)
    let all-f = pair(f, f*);
    // If there are no following patterns to consider, just bind the lot.
    if (empty?(p*))
      bind!(e, p, all-f);
      return(#t);
    end;
    // If the following pattern it literal and can easily be scanned for,
    // make a note of it.
    let search-p = 
      if (instance?(p*.head, <literal-fragment>))
        p*.head
      else
        #f
      end;
    let f*-cursor = all-f;
    let consumed = #();
    while (#t)
      if (search-p)
        let (left, right) = split(f*-cursor, curry(match-token, search-p));
        consumed := concatenate(consumed, left);
        if (~right)
          f*-cursor := #();
        else
          f*-cursor := right;
        end;
      end;
      block (retry)
        let fail = method (p, f, #key cause) retry() end;
        match-tokens(p*, f*-cursor, e, fail);
        bind!(e, p, consumed);
        return(#t);
      end;
      if (empty?(f*-cursor))
        fail(pair(p, p*), pair(f, f*));
      end;
      consumed  := concatenate(consumed, list(f*-cursor.head));
      f*-cursor := f*-cursor.tail;
    end;
  end;
end method;

// Like unconstrained pattern variables these match zero or more tokens,
// this time determined by a combination of the parser and delimiting
// intermediate words. Within these constraints, the longest sequence
// matching the declared parser class is consumed. 
//
// If your parser's stupid, a naive way to do this would be to select 
// shrinking subset of tokens and trial-parse them.
define method match (p :: <constrained-pattern-variable>, f, 
                     p*, f*, e, fail)
  let (form, remains) 
    = constrained-match(p, pair(f, f*));
  if (~form)
    fail(p, pair(f, f*), cause: "doesn't satisfy constraint")
  end;
  bind!(e, p, form);
  match-tokens(p*, remains, e, fail);
end method;

define method constrained-match 
    (p :: <unconstrained-pattern-variable>, f*)
  values(f*, #())
end method;

define method constrained-match 
    (p :: <constrained-pattern-variable>, f*)
  let terminators = p.parse-terminators;                     
  let (left, right)
    = if (empty?(terminators))
        values(f*, #())
      else
        split(f*,
              method (token)
                member?(token, terminators, test: match-token)
              end)
      end;
  // HACK!!!
  if (~right) 
    right := #();
  end;
  let (form, remains) = parse-constraint(p.type, left);
  values(form, concatenate(remains, right))
end method;

// Variables

define method match (p :: <variable-pattern>, f, p*, f*, e, fail)
  let (form, remains)
    = parse-constraint(#"variable", pair(f, f*));
  if (~form)
    fail(p, f, cause: "doesn't satisfy constraint");
  end;
  // Verify the submatches. If there's no explicit type component, 
  // we substitute an expression referring to <object> in the Dylan 
  // module for the type.
  match(p.name, form.head, #(), #(), e, fail);
  match(p.type, 
        if (~empty?(form.tail)) 
          form.third
        else
          most-specific-parsed-fragment(#"<object>");
        end,
        #(), #(), e, fail);
  match-tokens(p*, remains, e, fail);
end method;

// Property lists

// Note: The table generated here contains sequences of matching keyword
// values.

define method match-empty (p :: <property-list-pattern>, p*, e, fail)
  match-properties(p, #(), make(<table>), e, fail);
end method;

define method match (p :: <property-list-pattern>, f, p*, f*, e, fail)
  let (table, matched, remains)
    = parse-property-list(pair(f, f*), if (p.rest?) p.rest-pattern end);
  match-properties(p, matched, table, e, fail);
  match-tokens(p*, remains, e, fail);
end method;

// TODO: Apply constraints to sequence pattern variables.

define method match-properties (p, f*, table, e, fail)
  if (~table)
    fail(p, f*, cause: "invalid property list");
  end;
  if (p.rest?)
    bind!(e, p.rest-pattern, f*);
  end;
  if (p.key?)
    for (key in p.key-patterns)
      let var = key.head;
      let var-name = var.name;
      let keyword = as-keyword(var-name);
      let value = element(table, keyword, default: #f);
      // Is there a matching keyword argument?
      if (value)
        remove-key!(table, keyword);
        if (instance?(var, <sequence-pattern-variable>))
          bind-sequence!(e, var, value);
        else
          let (form, remains)
            = constrained-match(var, value.head);
          if (~form | ~empty?(remains))
            fail(p, f*, cause: "not all properties match their constraints");
          end;
          bind!(e, var, form);
        end;
      // HACK!!!
      // If not, is a default specified?
      elseif (~empty?(key.tail))
        let default = key.second;
        let default-substitution 
          = if (instance?(default, <template>))
              template-closure(default, e)
            else
              list(most-specific-parsed-fragment(default))
            end;
        // In the case of a sequence keyword pattern, a single element
        // sequence of the default is bound.
        if (instance?(var, <sequence-pattern-variable>))
          bind-sequence!(e, var, list(default-substitution));
        else
          bind!(e, var, default-substitution);
        end;
      elseif (instance?(var, <sequence-pattern-variable>))
        bind-sequence!(e, var, #());
      else
        fail(p, f*, cause: "missing keyword argument");
      end;
    end;
    if (~p.all-keys? & ~empty?(table))
      fail(p, f*, cause: "extra keys")
    end;
  end;
end method;

// TODO: Construct and return a sequence with the applied constraints to use
// when substituting a rest variable. 

define method parse-property-list (f*, rest-p)
  let (f*, remains) 
    = split(f*, curry(match-token, *semicolon*));
  block (escape)
    let return = method (table) escape(table, f*, remains | #()) end;
    let cursor = f*;
    let table = make(<table>);
    if (empty?(cursor))
      return(table);
    end;
    while (#t)
      let (property, right) = split(cursor, curry(match-token, *comma*));
      if (empty?(property))
        return(#f);
      end;
      let key = property.head;
      if (~(instance?(key, <literal-fragment>) & keyword?(key.object)))
        return(#f);
      end;
      let value-sequence = property.tail;
      // HACK!!! Recognise "key:, val" as well as "key: val"
      if (empty?(value-sequence) & right)
        let (property, new-right) = 
          split(right.tail, curry(match-token, *comma*));
        if (empty?(property))
          return(#f);
        end;
        value-sequence := property;
        right := new-right;
      end;
      // Constrain the value part.
      let (form, remains) 
        = if (rest-p)
            constrained-match(rest-p, value-sequence)
          else
            values(value-sequence, #())
          end;
      if (~form | ~empty?(remains))
        return(#f);
      end;
      let entry = element(table, key.object, default: #());
      entry := concatenate(entry, list(form));
      element(table, key.object) := entry;
      if (~right)
        return(table);
      end;
      cursor := right.tail;
    end;
  end;
end method;

// Spliced names (extension)

define method match (p :: <spliced-pattern-variable>, f, p*, f*, e, fail)
  let (form, remains)
    = parse-constraint(#"name", pair(f, f*));
  if (~form)
    fail(p, f, cause: "doesn't satisfy name constraint for unsplicing");
  end;
  // Check that the name matches the prefix and suffix, if any.
  let name = as(<string>, form.token-value);
  let start-pos 
    = if (p.before)
        if (subsequence-position(name, p.before, test: any-case-equal?) = 0)
          p.before.size
        else
          fail(p, f, cause: "name doesn't have a matching prefix")
        end
      else
        0
      end;
  let end-pos
    = if (p.after)
        if (subsequence-position(name, p.after, test: any-case-equal?)
              = name.size - p.after.size)
          name.size - p.after.size
        else
          fail(p, f, cause: "name doesn't have a matching suffix")
        end
      else
        name.size
      end;
  let core-name = copy-sequence(name, start: start-pos, end: end-pos);
  match(p.pattern, 
        make(<parsed-fragment>, 
             token-class: #"<symbol>",
             token-value: as(<symbol>, core-name)),
        p*, f*, e, fail);
end method;

define method any-case-equal? 
    (c1 :: <character>, c2 :: <character>) => (boolean)
  as-lowercase(c1) == as-lowercase(c2)
end method;

//// Utils

// This is a good example of a silent head & tail of #() is #() victim.
// Try: split(#(1, 2), 5)

define method split (l :: <list>, at :: <function>)
                    => (left :: <list>, right :: <list>)
  local split (l, at)
    case
      l == #()
        => values(#(), #f);
      at(l.head)
        => values(#(), l);
      otherwise
        => let (left, right) = split(l.tail, at);
           values(pair(l.head, left), right);
    end;
  end;
  split(l, at);
end method;

define method split (l :: <list>, at :: <integer>) 
                    => (left :: <list>, right :: <list>)
  if (at = 0)
    values(#(), l);
  else
    let (left, right) = split(l.tail, at - 1);
    values(pair(l.head, left), right);
  end;
end method;

//// Match environments.

define class <env> (<object>)
  slot bindings, init-value: #();
end class;

define method bind! (e :: <env>, var, val)
  trace-binding(var, val);
  e.bindings := pair(pair(var.name, sequence-fragment(fragments: val)), 
                     e.bindings);
end method;

define method bind! (e :: <env>, var, val :: <parsed-fragment>)
  trace-binding(var, val);
  e.bindings := pair(pair(var.name, sequence-fragment(fragments: list(val))), 
                     e.bindings);
end method;

define method lookup (e :: <env>, var, #key default = unsupplied())
  let name = var.name;
  block (return)
    for (pair in e.bindings)
      if (pair.head == name)
        return(binding-value(pair))
      end;
    finally
      if (supplied?(default))
        default
      else
        error("Pattern variable ~s not found in ~s", var, e.bindings);
      end;
    end;
  end;
end method;

define method lookup-raw (e :: <env>, name, #key default = unsupplied())
  block (return)
    for (pair in e.bindings)
      if (pair.head == name)
        return(pair);
      end;
    finally
      if (supplied?(default))
        default
      else
        error("Pattern variable ~s not found in ~s", var, e.bindings);
      end;
    end;
  end;
end method;

define method binding-value (binding :: <pair>)
  if (instance?(binding.tail, <sequence-fragment>))
    binding.tail.fragments
  else
    binding.tail
  end
end method;

// HACK!!!

define method bind-sequence! (e :: <env>, var, val)
  e.bindings := pair(pair(var.name, as(<vector>, val)), e.bindings);
end method;

define method bound-sequence? (val)
  instance?(val, <vector>);
end method;

define method bound-sequence (val)
  as(<list>, val);
end method;

define method match-error (#rest stuff)
  apply(reader-error, #f, stuff);
end method;

define class <match-failure> (<error>)
end class;

define class <pattern-match-failure> (<match-failure>)
  slot fragment,
    init-keyword: fragment:;
  slot pattern,
    init-keyword: pattern:;
  slot cause,
    init-value: #f,
    init-keyword: cause:;
end class;

// eof
