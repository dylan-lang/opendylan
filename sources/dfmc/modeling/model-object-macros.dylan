Module: dfmc-modeling
Author: Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Define the structure of a run-time class, generating a corresponding
// class for use in the compiler and source code for the run-time version
// for later compilation.

// Support for mixing in extra compile-stage only classes?
// (If you do, use it in place of &virtual-class-definer, below.)

define macro compiler-class-definer
  { define ?mods:* compiler-class ?:name (?supers:*) ?slots:* end }
    => { define ?mods compiler-class-only ?name (?supers) ?slots end;
         define ?mods compiler-class-accessors ?name (?supers) ?slots end; }
end macro;

define macro compiler-model-class-definer
  { define ?mods:* compiler-model-class ?:name (?compiler-supers:*) (?model-supers:*) ?slots:* end }
    => { define ?mods compiler-class ?name (?compiler-supers) ?slots end;
         define ?mods model-class ?name (?model-supers) ?slots end; }
end macro;

define macro &class-definer
  { define ?mods:* &class ?:name (?supers:*) ?slots:* end }
    => { define ?mods compiler-model-class ?name (?supers) (?supers) ?slots end }
end macro;

define macro virtual-compiler-model-class-definer
  { define ?mods:* virtual-compiler-model-class ?:name (?compiler-supers:*) (?model-supers:*) ?slots:* end }
    => { define ?mods compiler-class-only ?name (<virtual-object>, ?compiler-supers)
           ?slots
         end;
         define ?mods compiler-class-accessors ?name (?compiler-supers) ?slots end;
         define ?mods model-class ?name (?model-supers)
           ?slots
         metaclass <virtual-class>;
         end;
         }
end macro;

define macro &virtual-class-definer
  { define ?mods:* &virtual-class ?:name (?supers:*) ?slots:* end }
    => { define ?mods virtual-compiler-model-class ?name (?supers) (?supers) ?slots end }
end macro;

define macro virtual-compiler-model-subclass-definer
  { define ?mods:* virtual-compiler-model-subclass ?:name (?compiler-supers:*) (?model-supers:*) ?slots:* end }
    => { define ?mods compiler-class-only ?name (?compiler-supers)
           ?slots
         end;
         define ?mods compiler-class-accessors ?name (?compiler-supers) ?slots end;
         define ?mods model-class ?name (?model-supers)
           ?slots
         metaclass <virtual-class>;
         end;
         }
end macro;

define macro &virtual-subclass-definer
  { define ?mods:* &virtual-subclass ?:name (?supers:*) ?slots:* end }
    => { define ?mods virtual-compiler-model-subclass ?name (?supers) (?supers) ?slots end }
end macro;

/*
define macro virtual-compiler-model-class-definer
  { define ?mods:* virtual-compiler-model-class ?:name (?compiler-supers:*) (?model-supers:*) ?slots:* end }
    => { define ?mods compiler-class-only ?name (<virtual-object>, ?compiler-supers)
           ?slots
         end;
         // define ?mods compiler-class-accessors ?name (?compiler-supers) ?slots end;
         // define ?mods model-class ?name (?model-supers)
         //   ?slots
         //   metaclass <virtual-class>;
         // end;
         }
end macro;

define macro &virtual-class-definer
  { define ?mods:* &virtual-class ?:name (?supers:*) ?slots:* end }
    => { define ?mods virtual-compiler-model-class ?name (?supers) (?supers) ?slots end }
end macro;

define macro &virtual-subclass-definer
  { define ?mods:* &virtual-subclass ?:name (?supers:*) ?slots:* end }
    => { define ?mods compiler-class-only ?name (?supers)
           ?slots
         end }
end macro;
*/

define constant $&class-names = make(<object-table>); // TODO: <class-keyed-table>

define method install-&class-mapping (class, name)
  $&class-names[class] := name;
end method;

define method &object-class (object) => (res)
  let class = object-class(object);
  let name  = element($&class-names, class, default: #f);
  if (name)
    dylan-value(name)
  else
    let best = <object>;
    let name = #f;
    // find most specific existing supertype
    for (n keyed-by c in $&class-names)
      when (subtype?(class, c) & subtype?(c, best))
        best := c;
        name := n;
      end when;
    finally
      if (name)
              install-&class-mapping(class, name);
        &object-class(object)
      else
        error("Unable to find class mapping for %=", object);
      end if
    end for;
  end if;
end method;

define macro compiler-class-only-definer
  { define ?mods:* compiler-class-only "<" ## ?:name ## ">"
        (?supers:*)
      ?slots:*
    end }
    => { define ?mods dood-class "<&" ## ?name ## ">" (?supers) ?slots end;
         install-&class-mapping("<&" ## ?name ## ">", "<" ## ?#"name" ## ">") }
supers:
  { }
    => { <model-properties> }
  { ?non-empty-supers:* }
    => { ?non-empty-supers }
non-empty-supers:
  { "<" ## ?super:name ## ">", ... }
    => { "<&" ## ?super ## ">", ... }
  { }
    => { }
slots:
  { }
    => { }
  { metaclass ?other:*; ... }
    => { ... }
  { ?slot:*; ... }
    => { ?slot ; ... }
slot:
  // TODO: Handle repeated slots properly.
  { ?mods:* repeated &slot ?:name ?stuff:* }
    => { ?mods slot "^" ## ?name ## "-values" }
  { ?mods:* &slot ?:name, ?props:* }
    => { ?mods slot "^" ## ?name /* :: <&object> */, ?props }
  { ?mods:* &slot ?:name \:: ?type:*, ?props:* }
    => { ?mods slot "^" ## ?name :: ?type, ?props }
  { ?mods:* &runtime-only-computed-slot ?:variable, ?props:* }
    => { virtual slot ?variable }
  { ?mods:* &computed-slot ?:name, ?props:* }
    => { ?mods slot "%" ## ?name /* :: <&object> */, ?props }
  { ?mods:* &computed-slot ?:name \:: ?type:*, ?props:* }
    => { ?mods slot "%" ## ?name :: ?type, ?props }
  { ?mods:* &computed-slot-no-default ?:name, ?props:* }
    => { ?mods slot "%" ## ?name /* :: <&object> */ }
  { ?mods:* &computed-slot-no-default ?:name \:: ?type:*, ?props:* }
    => { ?mods slot "%" ## ?name :: ?type }
  { ?mods:* slot ?any:* }
    => { ?mods slot ?any }
type:
/*  { ?:name ( ?type ) }
    => { "^" ## ?name ( ?type ) }  */
  // TODO: NEED A UNIFYING TYPE or SUMTIN
  { false-at-compile-time-or(?type) }
    => { false-or(?type) }
  { false-or(?type) }
    => { false-or(?type) }
  { <object> }
    => { <model-value> }
  { <simple-object-vector> }
    => { <simple-object-vector> }
  { <integer> }
    => { <integer> }
  { <single-float> }
    => { <single-float> }
  { <byte-string> }
    => { <byte-string> }
  { <byte-character> }
    => { <byte-character> }
  { <boolean> }
    => { <boolean> }
  { <symbol> }
    => { <symbol> }
  { <list> }
    => { <list> }
  { "<" ## ?:name ## ">" }
    => { "<&" ## ?name ## ">" }
props:
  { }
    => { }
  { ?prop:*, ... }
    => { ?prop, ... }
prop:
  { getter-name: ?val:expression }
    => { getter-name: ?val }
  { setter-name: ?val:expression }
    => { setter-name: ?val }
  { init-keyword: ?val:expression }
    => { init-keyword: ?val }
  { required-init-keyword: ?val:expression }
    => { required-init-keyword: ?val }
  { init-value: ?:name }
    => { init-function: method () dylan-value(?#"name") end }
  { init-value: ?val:expression }
    => { init-function: method () /* run-stage */ (?val) end }
  { size-getter: ?val:expression }
    => { size-getter: ?val }
  { ?key:token ?val:expression }
    => { ?key /* run-stage */ (?val) }
mods:
  { } => { }
  { compiler-class-open ... } => { open ... }
  { compiler-open ... } => { open ... }
  { open ... } => { ... }
  { raw ... } => { ... }
  { runtime-constant ... } => { ... }
  { compiler-constant ... } => { constant ... }
  { ?mod:name ... } => { ?mod ... }
end macro compiler-class-only-definer;



define macro compiler-class-accessors-definer
  { define ?mods:* compiler-class-accessors "<" ## ?:name ## ">"
        (?supers:*)
      ?slots:*
    end }
    => { ?slots }
slots:
  { }
    => { }
  { ?slot:*; ... }
    => { ?slot ; ... }
slot:
  { inherited &slot ?:name ?etc:* }
    => { #f };
  { lazy constant ?mods:* &slot ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name) }
  { constant ?mods:* &slot ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name) }
  { lazy compiler-constant ?mods:* &slot ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name) }
  { compiler-constant ?mods:* &slot ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name) }
  { ?mods:* &slot ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name);
         do-define-evaluator-override
           (?#"name" ## "-setter", "^" ## ?name ## "-setter") }
  { ?mods:* &runtime-only-computed-slot ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name);
         do-define-evaluator-override
           (?#"name" ## "-setter", "^" ## ?name ## "-setter") }
  { ?mods:* &computed-slot ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name);
         do-define-evaluator-override
           (?#"name" ## "-setter", "^" ## ?name ## "-setter") }
  { ?mods:* &computed-slot-no-default ?:name ?etc:* }
    => { do-define-evaluator-override
           (?#"name", "^" ## ?name);
         do-define-evaluator-override
           (?#"name" ## "-setter", "^" ## ?name ## "-setter") }
  { ?other:* }
    => { #f }
end macro compiler-class-accessors-definer;

define macro model-class-definer
  { define ?mods:* model-class "<" ## ?:name ## ">" (?supers:*) ?slots:* end }
    => { define function "source-constructor-for-" ## ?name ()
           #{ define ?mods class "<" ## ?name ## ">" (?supers) ?slots end };
         end function;
         do-define-core-unadorned-definition
           ("<" ## ?#"name" ## ">", "source-constructor-for-" ## ?name); }
slots:
  { }
    => { }
  { ?mods:* &slot ?:name \:: false-at-compile-time-or(?type:*),
      ?standard-props; ... }
    => { ?mods slot ?name :: ?type, ?standard-props; ... }
  { ?mods:* &slot ?:variable, ?standard-props; ... }
    => { ?mods slot ?variable, ?standard-props; ... }
  { ?mods:* &runtime-only-computed-slot ?:variable, ?standard-props; ... }
    => { ?mods slot ?variable, ?standard-props; ... }
  { ?mods:* &computed-slot ?:variable, ?standard-props; ... }
    => { ?mods slot ?variable, ?standard-props; ... }
  { ?mods:* &computed-slot-no-default ?:variable, ?standard-props; ... }
    => { ?mods slot ?variable, ?standard-props; ... }
  { ?mods:* slot ?stuff:*; ... }
    => { ... }
  { ?other:*; ... }
    => { ?other; ... }
mods:
  { } => { }
  { compiler-class-open ... } => { ... }
  { lazy ... } => { ... }
  { weak ... } => { ... }
  { disk ... } => { ... }
  { compiler-constant ... } => { ... }
  { runtime-constant ... } => { constant ... }
  { ?mod:name ... } => { ?mod ... }
standard-props:
  { }
    => { }
  { reinit-expression: ?:expression, ... }
    => { ... }
  { ?standard-prop:*, ... }
    => { ?standard-prop, ... }
end macro model-class-definer;

define macro &top-type-definer
  { define ?mods:* &top-type ?:name end }
    => { define ?mods compiler-class ?name () end;
         do-define-top-type(?#"name"); }
end macro;

define macro &bottom-type-definer
  { define &bottom-type ?:name end }
    => { do-define-bottom-type(?#"name"); }
end macro;

/*
define macro compile-stage-accessors-definer
  { define compile-stage-accessors ?:name (?supers:*) ?slots:* end }
    => { ?slots }
slots:
  { ?slot:*; ... }
    => { ?slot; ... }
  { }
    => { }
slot:
  { &slot ?:name :: ?:expression, ?stuff:* }
    => { define method "^" ## ?name (object)
           "&" ## ?name(object).compile-stage
         end method;
         define method "^" ## ?name ## "-setter" (value, object)
           "&" ## ?name(object) := value.run-stage
         end method }
  { ?other:* }
    => { #f }
end macro;

define macro &constant-definer
  { define &constant ?:name of ?run-time:name = ?:expression }
    => { register-boot-thunk
           (*class-boot-world*, <boot-thunk>,
            debug-name: ?#"run-time",
            constructor:
              method () ?name := ?expression end,
            definer:
              method () define-constant!(?#"run-time", ?name) end)
       }
end macro;
*/

define macro ^mapping-definer
  { define ^mapping ?run-stage:name => ?compile-stage:name end }
    => { install-&class-mapping(?compile-stage, ?#"run-stage"); }
  { define ^mapping ?run-stage:name => ?compile-stage:name
      &slot ?run-slot:name => ?compile-slot:name;
      ?more:*
    end }
    => { define ^mapping ?run-stage => ?compile-stage ?more end;
         define method "^" ## ?run-slot (object :: ?compile-stage) => (res)
           ?compile-slot(object)
         end method;
         define method "^" ## ?run-slot ## "-setter"
             (new-value, object :: ?compile-stage)
           ?compile-slot(object) := new-value
         end method; }
  { define ^mapping ?run-stage:name => ?compile-stage:name
      constant &slot ?run-slot:name => ?compile-slot:name;
      ?more:*
    end }
    => { define ^mapping ?run-stage => ?compile-stage ?more end;
         define method "^" ## ?run-slot (object :: ?compile-stage) => (res)
           ?compile-slot(object)
         end method; }
  { define ^mapping ?run-stage:name => ?compile-stage:name
      repeated &slot ?run-slot:name => ?compile-slot:name;
      ?more:*
    end }
    => { define ^mapping ?run-stage => ?compile-stage ?more end;
         define method "^" ## ?run-slot (object :: ?compile-stage, index) => (res)
           ?compile-slot(object, index)
         end method;
         define method "^" ## ?run-slot ## "-setter"
             (new-value, object :: ?compile-stage, index)
           ?compile-slot(object, index) := new-value
         end method; }
  { define ^mapping ?run-stage:name => ?compile-stage:name
      &instance ?run-val:name => ?compile-val:expression;
      ?more:*
    end }
    => { define ^mapping ?run-stage => ?compile-stage ?more end;
         do-define-core-instance(?#"run-val", ?#"run-stage", ?compile-val); }
end macro;

//// Functions.

// Defines a compile-stage version of a function in the run-time that
// can be called by the simple compile-stage evaluators.

define macro &override-function-definer
  { define &override-function "^" ## ?:name end }
    => { do-define-evaluator-override(?#"name", "^" ## ?name); };
  { define &override-function "^" ## ?:name = ?override:name end }
    => { do-define-evaluator-override(?#"name", ?override); };
  { define &override-function "^" ## ?:name ?etc:* end }
    => { define method "^" ## ?name ?etc end;
         do-define-evaluator-override(?#"name", "^" ## ?name); }
end macro;

define macro &override-operator-definer
  { define &override-operator ?:name end }
    => { do-define-evaluator-override(?#"name", "^operator-" ## ?name); };
  { define &override-operator ?:name = ?override:name end }
    => { do-define-evaluator-override(?#"name", ?override); };
  { define &override-operator ?:name ?etc:* end }
    => { define method "^operator-" ## ?name ?etc end;
         do-define-evaluator-override(?#"name", "^operator-" ## ?name); }
end macro;


// Lazy determinants of runtime slot offsets of runtime models

define macro runtime-slot-offset-definer
  { define runtime-slot-offset ?slot:name (?class:name) }
    =>
  {
   define variable "*" ## ?slot ## "-runtime-slot-offset*" = #f;

   define inline function ?slot ## "-runtime-slot-offset" ()
    => (slot-offset :: <integer>)
     "*" ## ?slot ## "-runtime-slot-offset*"
     | (begin
          let class :: <&class> = dylan-value(?#"class");
          let slot-descriptor :: <&slot-descriptor> =
            ^slot-descriptor(class, dylan-value(?#"slot"));
          "*" ## ?slot ## "-runtime-slot-offset*" :=
            ^slot-offset(slot-descriptor, class);
        end);
   end;

  }

end macro;
