Module:       warnings-test-suite
Synopsis:     A test suite for compiler warnings
Author:       Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macro name/pattern consistency

define macro statement-macro-name-mismatch
  { statement-macro-name-mismatch-NOT ?stuff:* end } => { }
end macro;

define macro function-macro-name-mismatch
  { function-macro-name-mismatch-NOT(?stuff:*) } => { }
end macro;

define macro local-declaration-macro-name-mismatch
  { local-declaration-macro-name-mismatch-NOT ?stuff:* } => { }
end macro;

define macro body-macro-name-mismatch-definer
  { define ?mods:* body-macro-name-mismatch-NOT ?stuff:* end } => { }
end macro;

define macro list-macro-name-mismatch-definer
  { define ?mods:* list-macro-name-mismatch-NOT ?stuff:* } => { }
end macro;

/// Defining word validity

define macro non-definer-definition-definerz
  { define ?mods:* non-definer-definition ?stuff:* end } => { }
end macro;

/// Macro main pattern consistency

define macro inconsistent-non-definer-rules
  { inconsistent-non-definer-rules ?stuff:* end } => { }
  { inconsistent-non-definer-rules(?stuff:*) }    => { }
  { inconsistent-non-definer-rules ?stuff:* }     => { }
end macro;

define macro inconsistent-definer-rules-definer
  { define ?mods:* inconsistent-definer-rules ?stuff:* end } => { }
  { define ?mods:* inconsistent-definer-rules ?stuff:* }     => { }
end macro;

/// Inconsistent pattern variable use

define macro unconstrained-name-macro
  { unconstrained-name-macro(?stuff) } => { }
end macro;

define macro duplicate-names-macro
  { duplicate-names-macro(?x:expression, ?x:expression) } => { }
end macro;

define macro element-as-sequence-macro
  { element-as-sequence-macro(?stuff:*) } => { ??stuff ... } 
end macro;

define macro sequence-as-element-macro
  { sequence-as-element-macro(#key ??stuff:*) } => { ?stuff }
end macro;

define macro splicer-macro
  { splicer-macro(?x:*) } => { ?x ## "-spliced" }
end macro;

/*---*** keith: currently crashes the compiler
splicer-macro(2);
splicer-macro(a b c);
*/

define macro undefined-element-substitution-macro
  { undefined-element-substitution-macro(?stuff:*) } => { ?not-stuff }
end macro;

undefined-element-substitution-macro(2);

define macro undefined-sequence-substitution-macro
  { undefined-sequence-substitution-macro(?stuff:*) } => { ??not-stuff ... }
end macro;

undefined-sequence-substitution-macro(2);

/// Aux-rules

define macro aux-rule-overflow
  { aux-rule-overflow(?stuff) } => { }
stuff:
  { ?stuff:* } => { ?stuff }
end macro;

aux-rule-overflow(2);
