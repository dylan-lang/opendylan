Module: internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Definitions.

// All definitions are handled directly by the compiler.

// define macro for
//  { for (?header:*) ?fbody:* end } => { #f }
// end;

//// Statements.

define macro unless
  { unless (?test:expression) ?:body end }
   => { if (~?test) ?body else #f end }
end macro unless;

define macro case
  { case ?cases:case-body end }  
    => { ?cases }
cases:
  { } 
    => { #f }
  { otherwise => ; } 
    => { #t }
  { otherwise => ?:body } 
    => { ?body }
  { ?test:expression => ; ... } 
    => { ?test | (...)  }
  { ?test:expression => ?:body; ... } 
    => { if (?test) ?body else ... end if }
end macro case;

define macro select
  { select (?what) ?:case-body end } 
    => { ?what; ?case-body }
what:
  { ?object:expression by ?compare:expression }
    => { let _object = ?object;
         let _compare = ?compare }
  { ?object:expression } 
    => { let _object = ?object;
         let _compare = \== }
case-body:
  { otherwise => ?:body; }   
    => { ?body }
  { ?keys => ?:body; ... }
    => { if (?keys) ?body else ... end if }
  { }
    => { error("Fell through select cases on %=.", _object) }
keys:
  { (?inner-keys) }
    => { ?inner-keys }
  { ?inner-keys }
    => { ?inner-keys }
inner-keys:
  { ?key:expression }
    => { _compare(_object, ?key) }
  { ?key:expression, ... }
    => { _compare(_object, ?key) | ... }
end macro select;

// While and Until are defined as primitive converters.
/*
define macro while
  { while (?test:expression) ?:body end }
    => { local method _while-loop () 
           if (?test) ?body; _while-loop() end; 
         end method _while-loop;
         _while-loop() }
end macro while;

define macro until
  { until (?test:expression) ?:body end }
    => { local method _until-loop () 
           if (~?test) ?body; _until-loop() end; 
         end method _until-loop;
         _until-loop() }
end macro until;
*/

// Begin is a primitive converter.

// Block is build in terms of the primitive converters:
//
//   %with-exit (?name) ?body end
//   %with-cleanup ?body cleanup ?cleanup-body end
//   %with-afterwards ?body afterwards ?afterwards-body end
//   %with-handler ?body handler ?afterwards-body end

/*
define macro block
  { block (?:name) ?ebody end }
     => { %with-exit (?name) ?ebody end }
  { block () ?ebody end }
     => { ?ebody }

// Left-recursive so leftmost clause is innermost
ebody:
/*
  {   ... 
    exception (?excp, 
               #rest ?options:expression,
               #key ?test:expression = always(#t),
                    ?init-arguments:expression = #())
      ?:body }
   => { %with-handler
          ...
        handler
          ?body
        end }
*/
  { ?abody cleanup ?cleanup:body}
    => { %with-cleanup 
           ?abody
         cleanup
           ?cleanup
         end }
  { ?abody }
    => { ?abody }

abody:
  { ?main:body afterwards ?after:body }
    => { %with-afterwards
           ?main
         afterwards 
           ?after
         end }
  { ?main:body }
    => { ?main }

 excp:
  { ?type:expression }           => { ?type }
  { ?:name :: ?type:expression } => { ?type, condition: ?name }
end;
*/

define macro iterate
  { iterate ?:name (?bindings:*) ?:body end }
    => { iterate-aux ?name (?bindings) ?body end }
bindings:
  { ?:variable = ?:expression, ... }
    => { var: ?variable, init: ?expression, ... }
  { }
    => { }
end macro;

define macro iterate-aux
  { iterate-aux ?:name (#key ??var:variable, ??init:expression) ?:body end }
    => { local method ?name (??var, ...) ?body end;
         ?name(??init, ...) }
end macro;

define macro when
  { when (?test:expression) ?action:body end }
    => { if (?test) ?action end }
end macro;
