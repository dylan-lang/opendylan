Module:       Dylan-User
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library jam
  use common-dylan;
  use collections;
  use io;
  use system;
  use parser-run-time;

  export jam;
end library;

define module jam
  // Interpreter state
  create <jam-state>,
         jam-state-copy,
         jam-variable,
         jam-variable-setter;

  // Variable expansion
  create jam-expand-arg,
         jam-expand-list;

  // Rules
  create jam-rule,
         jam-rule-setter,
         jam-invoke-rule;

  // Targets
  create jam-target-variable,
         jam-target-variable-setter,
         jam-target-bind,
         jam-target-build;

  // Reader
  create jam-read-file,
         jam-read;
end;

define module regular-expression
  use common-dylan;
  
  export <regular-expression>,
         copy-regular-expression,
         <epsilon-regular-expression>,
         <symbol-regular-expression>,
         regular-expression-symbol,
         <symbol-set-regular-expression>,
         regular-expression-symbol-set,
         <union-regular-expression>,
         regular-expression-union1,
         regular-expression-union2,
         <concatenation-regular-expression>,
         regular-expression-head,
         regular-expression-tail,
         <closure-regular-expression>,
         regular-expression-enclosed,
         <accept-regular-expression>,
         <regular-expression-dfa-state>,
         regular-expression-dfa-state-transitions,
         regular-expression-dfa,
         do-regular-expression-dfa-state-position;
end module;

define module jam-internals
  use common-dylan;
  use threads;
  use set;
  use bit-set;
  use collectors;
  use streams;
  use file-system;
  use locators;
  use date;
  use machine-words;
  use byte-vector;
  use parser-run-time;
  use regular-expression;
  use operating-system;

  use jam;
end module;


