module:   sealing-workbench
author:   Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////
//// meta-object definition macro
////

define macro &class-definer
  { define &class ?name ?meta (?supers) ?open }
    => { define constant ?name
           = &make(&<class>, name: ?#"name",
                   ?open, ?meta, superclasses: list(?supers)) }
 open:
  { open } => { open?: #t, abstract?: #t }
  { }      => { }
 meta:
  { = ?class:name } => { meta-class: ?class, library: $dylan-library }
  { }               => { }
end macro;


////
//// booting classes from their real counterparts
////

define macro crossover-class-definer
  { define crossover-class ?base:name (?supers) ?open }
  => { define &class \& ## ?base = ?base (?supers) ?open;
       define method object-&class(&object :: ?base)
         \& ## ?base
       end method object-&class; }
 supers:
  { ?super:name, ... } => { \& ## ?super, ... }
  { }                  => { }
 open:
  { open } => { open }
  { }      => { }
end macro;


////
//// the really funky sealing-world macro
////

define macro sealing-world
  { sealing-world
      ?libraries
    dispatch
      ?calls
    end }
  => { ?libraries;
       ?calls;
       values() }
 libraries:
  { ?library; ... } => { ?library; ... }
  { }               => { }
 library:
  { library ?name ?decls }
  => { let library = make(<library>, name: ?#"name");
       ?decls;
       finalize-definition(library) }
 decls:
  { ?decl, ... } => { ?decl; ... }
  { }            => { }
 decl:
  { sealed-class ?name (?supers) }
  => { let ?name
         = &make(&<class>, name: ?#"name",
                 superclasses: list(?supers), library: library);
       // format-out("CPL(%=) = %=\n", ?name, ?name.&all-superclasses);
       library.definitions[?#"name"] := ?name }
  { open-class ?name (?supers) }
  => { let ?name
         = &make(&<class>, name: ?#"name", open?: #t,
                 superclasses: list(?supers), library: library);
       library.definitions[?#"name"] := ?name }
  { sealed-abstract-class ?name (?supers) }
  => { let ?name
         = &make(&<class>, name: ?#"name", abstract?: #t,
                 superclasses: list(?supers), library: library);
       library.definitions[?#"name"] := ?name }
  { open-abstract-class ?name (?supers) }
  => { let ?name
         = &make(&<class>, name: ?#"name", open?: #t, abstract?: #t,
                 superclasses: list(?supers), library: library);
       library.definitions[?#"name"] := ?name }
  { sealed-generic ?name(?parameters) }
  => { let ?name
         = &make(&<generic>, name: ?#"name",
                 parameters: list(?parameters), library: library);
       library.definitions[?#"name"] := ?name }
  { open-generic ?name(?parameters) }
  => { let ?name
         = &make(&<generic>, name: ?#"name", open?: #t,
                 parameters: list(?parameters), library: library);
       library.definitions[?#"name"] := ?name }
  { add-method ?gf:name ?name (?parameters) ?next }
  => { let ?name
         = &make(&<method>, name: ?#"name", ?next,
                 parameters: list(?parameters), library: library);
       &add-method(?gf, ?name);
       library.definitions[?#"name"] := ?name }
  { seal-generic ?name (?parameters) }
  => { &seal-generic(?name, list(?parameters), library) }
  { object ?name :: ?type:name }
  => { let ?name = &make(?type, abstract-instance?: #t);
       library.definitions[?#"name"] := ?name }
  { object ?name = ?expression }
  => { let ?name = ?expression;
       library.definitions[?#"name"] := ?name }
 next:
  { calls-next-method } => { calls-next-method?: #t }
  { }                   => { }
 calls:
  { ?call; ... } => { ?call; ... }
  { }            => { }
 call:
  { ?name(?args) } => { dispatch(?name, list(?args)) }
end macro;
