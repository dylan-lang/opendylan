Module:       testworks-specs
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A useful macro to define module specs

define macro module-spec-definer
  { define module-spec ?module-name:name (?options:*)
      ?specs:*
    end}
    => { define module-spec-protocol ?module-name ()
           ?specs
         end;
         define module-spec-suite ?module-name ()
           ?specs
         end;
         }
end macro module-spec-definer;

define macro module-spec-protocol-definer
  { define module-spec-protocol ?module-name:name (?options:*)
      ?specs:*
    end }
    => { define protocol-spec ?module-name (?options)
           ?specs
         end }
 specs:
  { } => { }
  { ?spec:*; ... } => { ?spec ... }
 spec:
  { protocol ?protocol-name:name }
    => { }
  { ?definition:* }
    => { ?definition; }
end macro module-spec-protocol-definer;

define macro module-spec-suite-definer
  { define module-spec-suite ?module-name:name (?options:*)
      ?specs:*
    end }
    => { define suite ?module-name ## "-module-test-suite" (?options)
           suite ?module-name ## "-protocol-test-suite";
           ?specs
         end }
 specs:
  { } => { }
  { ?spec:*; ... } => { ?spec ... }
 spec:
  { protocol ?protocol-name:name }
    => { suite ?protocol-name ## "-protocol-test-suite"; }
  { ?definition:* }
    => { }
end macro module-spec-suite-definer;


/// Library specs

// Like the "define suite" macro, but allows clauses like "module foo;"
// in its body which expand to "suite foo-module-test-suite".
define macro library-spec-definer
  { define library-spec ?library-name:name (?options:*)
      ?subsuites:*
    end}
    => { define suite ?library-name ## "-test-suite" (?options)
           ?subsuites
         end
         }
 subsuites:
  { } => { }
  { ?thing; ... } => { ?thing; ... }
 thing:
  { module ?module-name:name }
    => { suite ?module-name ## "-module-test-suite" }
  { ?x:* } => { ?x }
end macro library-spec-definer;

