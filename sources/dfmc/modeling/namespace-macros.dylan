Module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro &library-definer
  { define &library ?:name ?clauses:* end }
    => { let clauses = parsed-namespace-clauses ?clauses end;
         do-define-core-library
           (?#"name" ## "-library", ?#"name", merge-clauses(clauses)); }
end macro;

// TODO: The clause parsing code is duplicated to work around a macro
// bug to do with escapes being lost if passed through multiple
// macros.

define method merge-clauses (clauses)
  collecting (uses, creates, exports)
    for (clause in clauses)
      select (clause by instance?)
        <use-clause> => collect-into(uses, clause);
        <create-clause> => collect-into(creates, clause);
        <export-clause> => collect-into(exports, clause);
      end;
    end;
    list(use-clauses: collected(uses),
         create-clauses: collected(creates),
         export-clauses: collected(exports));
  end;
end method;

define macro &module-definer
  { define &module ?:name ?clauses:* end }
    => { let clauses = list(?clauses);
                       // parsed-namespace-clauses ?clauses end;
         do-define-core-module
           (?#"name" ## "-module", ?#"name", merge-clauses(clauses)); }
clauses:
  { }
    => { }
  { ?clause:*; ... }
    => { ?clause, ... }
clause:
  { use ?:name, ?options:* }
    => { make(<use-clause>, use: ?#"name", options: list(?options)) }
  { create ?names:* }
    => { make(<create-clause>, names: list(?names)) }
  { export ?names:* }
    => { make(<export-clause>, names: list(?names)) }
options:
  { }
    => { }
  { ?option:*, ... }
    => { ?option, ... }
option:
  { prefix: ?prefix:* }
    => { prefix: ?prefix }
  { import: all }
    => { import: #"all" }
  { import: ?rename-set }
    => { import: ?rename-set }
  { exclude: ?name-set }
    => { exclude: ?name-set }
  { rename: ?rename-set }
    => { rename: ?rename-set }
  { export: all }
    => { export: #"all" }
  { export: ?name-set }
    => { export: ?name-set }
prefix-name:
  { ?prefix:expression }
    => { check-type(?prefix, <string>) }
name-set:
  { { ?names } }
    => { list(?names) }
rename-set:
  { { ?renames } }
    => { list(?renames) }
names:
  { }
    => { }
  { ?:name, ... }
    => { ?#"name", ... }
renames:
  { }
    => { }
  { ?:name, ... }
    => { ?#"name", ... }
  { ?before:name => ?after:name, ... }
    => { pair(?#"before", ?#"after"), ... }
end macro;

// Note: Currently supports just one kind of each clause.

define macro parsed-namespace-clauses
  { parsed-namespace-clauses ?clauses:* end }
    => { list(?clauses) }
clauses:
  { }
    => { }
  { ?clause:*; ... }
    => { ?clause, ... }
clause:
  { use ?:name, ?options:* }
    => { make(<use-clause>, use: ?#"name", options: list(?options)) }
  { create ?names:* }
    => { make(<create-clause>, names: list(?names)) }
  { export ?names:* }
    => { make(<export-clause>, names: list(?names)) }
options:
  { }
    => { }
  { ?option:*, ... }
    => { ?option, ... }
option:
  { prefix: ?prefix:* }
    => { prefix: ?prefix }
  { import: all }
    => { import: #"all" }
  { import: ?rename-set }
    => { import: ?rename-set }
  { exclude: ?name-set }
    => { exclude: ?name-set }
  { rename: ?rename-set }
    => { rename: ?rename-set }
  { export: all }
    => { export: #"all" }
  { export: ?name-set }
    => { export: ?name-set }
prefix-name:
  { ?prefix:expression }
    => { check-type(?prefix, <string>) }
name-set:
  { { ?names } }
    => { list(?names) }
rename-set:
  { { ?renames } }
    => { list(?renames) }
names:
  { }
    => { }
  { ?:name, ... }
    => { ?#"name", ... }
renames:
  { }
    => { }
  { ?:name, ... }
    => { ?#"name", ... }
  { ?before:name => ?after:name, ... }
    => { pair(?#"before", ?#"after"), ... }
end macro;
