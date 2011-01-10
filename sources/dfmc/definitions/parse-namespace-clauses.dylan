Module:   dfmc-definitions
Synopsis: Namespace clause parsers.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Dynamic context.

// In order to allow informative conditions, context such as the current
// module/library definition, current clause, and current option, are made
// available to signaling units through thread variables.

/*
define thread variable *current-definition* = #f;
define thread variable *current-clause* = #f;
define thread variable *current-option* = #f;

define method namespace-context ()
  list(namespace: *current-definition*, 
       clause:    *current-clause*,
       option:    *current-option*)
end method;

define dood-class <parsed-use-clause> (<object>)
  lazy slot clause-namespace;
  lazy slot clause-name;
  lazy slot clause-import-spec, 
    init-keyword: import:;
  lazy slot clause-exclude-spec;
  lazy slot clause-rename-spec;
  lazy slot clause-prefix-spec;
  lazy slot clause-export-spec;
end class;
*/

define serious-program-warning <skipping-namespace-clause>
  slot condition-namespace-name, required-init-keyword: namespace-name:;
  format-string "Skipping malformed clause in %s due to previous syntax error.";
  format-arguments namespace-name;
end serious-program-warning;

define method parse-namespace-clauses (name, clauses)
  collecting (use-clauses :: <vector>,
	      create-clauses :: <vector>,
	      export-clauses :: <vector>)
    macro-case (clauses)
      { ?clauses:* }
        => #f;
    clauses:
      { }
        => #f;
      { ?clause:*; ... }
        => handling-parse-errors
             macro-case (clause)
               { use ?:name, ?options:* }
                 => collect-into(use-clauses, parse-use-clause(clause));
               { create ?names:* }
                 => collect-into(create-clauses, parse-create-clause(clause));
               { export ?names:* }
                 => collect-into(export-clauses, parse-export-clause(clause));
             end macro-case;
           on-error (condition)
             // Pass it on for collection.
             signal(condition); 
             // Warn about what we're skipping.
             note(<skipping-namespace-clause>,
                  source-location: fragment-source-location(clause),
                  namespace-name:  name);
           end;
    end macro-case;
    values(collected(use-clauses), 
           collected(create-clauses),
           collected(export-clauses))
  end collecting;
end method;

define option <use-import-option> => import:   :: rename-set end;
define option <use-rename-option> => rename:   :: rename-set end;
define option <use-prefix-option> => prefix:   :: prefix end;
define option <use-exclude-option> => exclude: :: name-set end;
define option <use-export-option> => export:   :: name-set end;

define method parse-option-value (constraint == #"name-set", fragment)
  macro-case (fragment) 
    { all }          => #"all";
    { { ?names:* } } => parse-names-to-symbols(names);
  end;
end method;

define method parse-option-value (constraint == #"rename-set", fragment)
  macro-case (fragment) 
    { all }          => #"all";
    { { ?renames:* } } => parse-renames-to-symbols(renames);
  end;
end method;

define serious-program-warning <invalid-symbolic-prefix-option>
  slot condition-option,
    required-init-keyword: option:;
  slot condition-using,
    required-init-keyword: using:;
  format-string
    "Non-string prefix option in use clause: %s - using %=.";
  format-arguments
    option, using;
end serious-program-warning;

define serious-program-warning <invalid-prefix-option>
  slot condition-option,
    required-init-keyword: option:;
  format-string
    "Non-string prefix option in use clause: %s - ignoring";
  format-arguments
    option;
end serious-program-warning;

define method parse-option-value (constraint == #"prefix", fragment)
  macro-case (fragment)
    { ?prefix:expression }
      => begin
           select (prefix by instance?)
             <string-fragment>
               => fragment-value(prefix);
             <name-fragment>
               => let use = as(<string>, fragment-name(prefix));
                  note(<invalid-symbolic-prefix-option>,
                       source-location: fragment-source-location(prefix),
                       option: prefix,
                       using:  use);
                  use;
             <symbol-fragment>
               => let use = as(<string>, fragment-value(prefix));
                  note(<invalid-symbolic-prefix-option>,
                       source-location: fragment-source-location(prefix),
                       option: prefix,
                       using:  use);
                  use;
             otherwise
               => note(<invalid-prefix-option>,
                       source-location: fragment-source-location(prefix),
                       option: prefix);
                  "";
           end;
         end;
  end;
end method;

define method use-options ()
  list(<use-import-option>,
       <use-rename-option>,
       <use-prefix-option>,
       <use-exclude-option>,
       <use-export-option>)
end method;

define method parse-use-clause (form)
  macro-case (form)
    { use ?:name, ?options:* }
      => begin
           let initargs = parse-options(use-options(), options, form);
           make(<use-clause>, 
                use: fragment-identifier(name), 
                options: initargs);
         end;
  end macro-case;
end method;

define method parse-export-clause (form)
  macro-case (form)
    { export ?names:* }
      => begin
           make(<export-clause>, 
                names: parse-names-to-symbols(names));
         end;
  end macro-case;
end method;

define method parse-create-clause (form)
  macro-case (form)
    { create ?names:* }
      => begin
           make(<create-clause>, 
                names: parse-names-to-symbols(names));
         end;
  end macro-case;
end method;

//// Merging name set options.

// This code deals with merging multiple occurrences of import: and
// exclude: options in a single use clause.

define abstract program-warning <namespace-clause-warning>
  slot condition-namespace,
    required-init-keyword: namespace:;
  slot condition-clause,
    required-init-keyword: clause:;
  slot condition-option,
    required-init-keyword: option:;
end program-warning;

define program-warning 
    <all-specified-multiple-times> (<namespace-clause-warning>)
  format-string
    "\"All\" specified more than once as an %s option of %s in the "
    "definition of %s.";
  format-arguments
    option, clause, namespace;
end program-warning;

define program-warning 
    <all-specified-with-explicit-names> (<namespace-clause-warning>)
  format-string
    "\"All\" specified as well an explicit name set as an %s option of %= "
    "in the definition of %=.";
  format-arguments
    option, clause, namespace;
end program-warning;

// TODO: CORRECTNESS: Make sure what the following code was intended to
// handle is being handled.

/*
// False indicates that no option has been seen yet. We just take the
// first definition as-is.

define method merge-name-sets (s1 == #f, s2 :: <object>) => (merged-s1-s2)
  s2
end method;

define method merge-name-sets (s1 == #"all", s2 == #"all") => (merged-s1-s2)
  apply(note, <all-specified-multiple-times>, namespace-context());
  s1
end method;

define method merge-name-sets (s1 == #"all", s2 :: <sequence>)
    => (merged-s1-s2)
  apply(note, <all-specified-with-explicit-names>, namespace-context());
  s1
end method;

define method merge-name-sets (s1 :: <sequence>, s2 :: <sequence>) 
    => (merged-s1-s2)
  union(s1, s2);
end method;

// If none of the above match, reverse the order of the sets and
// try again.

define method merge-name-sets (s1 :: <object>, s2 :: <object>) 
    => (merged-s1-s2)
  merge-name-sets(s2, s1)
end method;

*/

/*
// Parse a comma-separated list of names.

define method parse-names (names :: <fragment>) => (name-sequence :: <vector>)
  macro-case (names)
    { ?names:* } => as(<vector>, names);
  names:
    { } 
      => #();
    { ?:name, ... }
      => pair(name, ...);
  end macro-case
end method;
*/

define method parse-names-to-symbols
    (names :: <fragment>) => (name-sequence :: <vector>)
  macro-case (names)
    { ?names:* } => as(<vector>, names);
  names:
    { } 
      => #();
    { ?:name, ... }
      => pair(as(<symbol>, name), ...);
  end macro-case
end method;

define method parse-renames-to-symbols
    (names :: <fragment>) => (name-sequence :: <vector>)
  macro-case (names)
    { ?names:* } => as(<vector>, names);
  names:
    { } 
      => #();
    { ?:name, ... }
      => pair(as(<symbol>, name), ...);
    { ?:name => ?new:name, ... }
      => pair(pair(as(<symbol>, name), as(<symbol>, new)), ...);
  end macro-case
end method;
