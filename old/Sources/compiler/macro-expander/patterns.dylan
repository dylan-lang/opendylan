Module:    infix-reader
Language:  infix-dylan
Synopsis:  Patterns
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Abstract root of all pattern objects

define abstract class <pattern-object> (<object>)
end class;

//// The empty pattern

define pattern <empty-pattern> (<pattern-object>)
end class;

//// The generic pattern

define pattern <generic-pattern> (<pattern-object>)
  pattern slots patterns;
end pattern;

//// Basic pattern components

define abstract class <simple-pattern> (<pattern-object>)
end class;

define method wildcard? (o)
  #f;
end method;

define abstract class <pattern-variable> (<simple-pattern>)
  slot name,
    required-init-keyword: name:;
  slot wildcard?,
    init-value: #t,
    init-keyword: wildcard?:;
end class;

define class <unconstrained-pattern-variable> (<pattern-variable>)
end class;

define class <constrained-pattern-variable> (<pattern-variable>)
  slot type,
    required-init-keyword: type:;
  slot parse-terminators,
    init-value: #();
end class;

define method register-parse-terminator (v :: <constrained-pattern-variable>, f)
  v.parse-terminators := pair(f, v.parse-terminators);
  f;
end method;

define method pattern-variable-pattern (#key name, substitution? = #f)
  let (name, type) = constrained-name-name-and-constraint(name);
  // The type may be #f, and this is picked up later during pattern
  // compilation if no default constraint is applied.
  if (~type | wildcard-constraint-name?(type))
    make(<unconstrained-pattern-variable>, name: name)
  else
    make(<constrained-pattern-variable>, name: name, type: type);
  end
end method;

define method pattern-variable-substitution (#key name)
  pattern-variable-pattern(name: name, substitution?: #t)
end method;

define method ellipsis-pattern-variable-pattern ()
  make(<unconstrained-pattern-variable>, name: #"...", wildcard: #t);
end method;

define method ellipsis-name? (name)
  name == #"...";
end method;

define method ellipsis-pattern? (var)
  ellipsis-name?(var.name);
end method;

define class <sequence-pattern-variable> (<pattern-object>)
  slot name,
    required-init-keyword: name:;  
  slot separator,
    init-keyword: separator:;  
end pattern;

define method sequence-pattern-variable-pattern (#key name, separator = #f)
  let (name, type)
    = constrained-name-name-and-constraint(name);
  make(<sequence-pattern-variable>, name: name, separator: separator);
end method;

define class <spliced-pattern-variable> (<pattern-object>)
  slot pattern,
    required-init-keyword: pattern:;
  slot before,
    init-value: #f,
    init-keyword: before:;
  slot after,
    init-value: #f,
    init-keyword: after:;
end class;

define method spliced-pattern-variable-pattern (#key pattern, before, after)
  make(<spliced-pattern-variable>,
       pattern: pattern, 
       before: before,
       after: after);
end method;

define pattern <variable-pattern> (<simple-pattern>)
  pattern slots name, type;
end class;

//// Combinations

define pattern <pattern-sequence-pattern> (<pattern-object>)
  pattern slots patterns;
end class;

define method pattern-sequence-pattern (#key patterns)
  // Fold degenerate pattern sequences.
  if (size(patterns) = 1)
    patterns.first;
  else
    make(<pattern-sequence-pattern>, 
         patterns: compute-wildcard(patterns));
  end;
end method;

//// Groups

define pattern <property-list-pattern> (<pattern-object>)
  pattern slots rest-pattern, key-patterns;
  slot rest?,
    init-value: #f;
  slot key?,
    init-value: #f;
  slot all-keys?,
    init-value: #f;
end pattern;

define method initialize (p :: <property-list-pattern>, #rest args)
  next-method();
  if (p.rest-pattern)
    p.rest? := #t;
  end;
  if (p.key-patterns)
    let cursor = p.key-patterns;
    while (~empty?(cursor))
      let token = cursor.first;
      select (token)
        #"&key"
          => p.key? := #t;
             p.key-patterns := #();
             cursor := cursor.tail;
             while (~empty?(cursor) & cursor.first ~== #"&all-keys")
               p.key-patterns := pair(cursor.first, p.key-patterns);
               cursor := cursor.tail;
             end;
        #"&all-keys"
          => p.all-keys? := #t;
             cursor := cursor.tail;
      end;
    end;
  end;
end method;

///// Anti-hygiene

//// Extensions

define class <coercing-substitution> (<pattern-object>)
end class;

define pattern <string-pattern-variable-pattern> (<coercing-substitution>)
  pattern slots name;
end pattern;

define method initialize (p :: <string-pattern-variable-pattern>, #key keys)
  next-method();
  p.name := pattern-variable-pattern(name: as(<symbol>, p.name));
end method;

define pattern <symbol-pattern-variable-pattern> (<coercing-substitution>)
  pattern slots name;
end pattern;

define method initialize (p :: <symbol-pattern-variable-pattern>, #key keys)
  next-method();
  p.name := pattern-variable-pattern(name: p.name);
end method;

//// Special procedural substitutions.

define pattern <expression-substitution> (<pattern-variable>)
  pattern slots expression;
end pattern;

define method expression-substitution (#key expression)
  make(<expression-substitution>, 
       name:       #"anonymous",
       expression: expression);
end method;

define pattern <sequence-expression-substitution> 
    (<sequence-pattern-variable>)
  pattern slots expression;
end pattern;

define method sequence-expression-substitution (#key expression, separator)
  make(<sequence-expression-substitution>, 
       name:       #"anonymous",
       separator:  separator,
       expression: expression);
end method;

//// Constrained name component access.

define method constrained-name-name-and-constraint (name) 
    => (name, constraint)
  let string = as(<string>, name);
  let colon = find-key(string, curry(\==, ':'), failure: #f);
  if (colon)
    let type = as(<symbol>, copy-sequence(string, start: colon + 1));
    if (~constraint-name?(type))
      error("Unknown constraint ~s in pattern variable ~s",
            type, name);
    end;
    let var = if (colon = 0) 
                type 
              else 
                as(<symbol>, copy-sequence(string, end: colon))
              end;
    values(var, type)
  else
    values(name, #f)
  end
end method;

//// Pattern variable iteration.

define generic do-pattern-variables (f, p) => ();

define method do-pattern-variables (f, p :: <pattern-object>) => () end;

define method do-pattern-variables (f, p :: <pattern-variable>) => ()
  f(p);
end method;

define method do-pattern-variables (f, p :: <sequence-pattern-variable>) => ()
  f(p);
end method;

define method do-pattern-variables (f, p :: <bracketed-fragment>) => ()
  for (sub-p in p.fragments) 
    do-pattern-variables(f, sub-p);
  end;
end method;

define method do-pattern-variables (f, p :: <property-list-pattern>) => ()
  if (p.rest?) 
    do-pattern-variables(f, p.rest-pattern);
  end;
  if (p.key?) 
    for (sub-p in p.key-patterns)
      do-pattern-variables(f, sub-p);
    end;
  end;
end method;

define method do-pattern-variables (f, p :: <variable-pattern>) => ()
  do-pattern-variables(f, p.name);
  do-pattern-variables(f, p.type);
end method;

// eof
