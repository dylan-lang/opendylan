module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro coagulate { coagulate(?:name) } => { ?#"name" } end;

define generic raw-representation-name 
  (class-name :: <symbol>) => (res :: false-or(<symbol>));

define method raw-representation-name 
    (name :: <symbol>) => (res :: singleton(#f))
  #f 
end method;

define macro &dylan-raw-type-definer
  { define &dylan-raw-type ?:name ?maybe-superclass:* }
    => { define &raw-type ?name ?maybe-superclass;
         define method raw-representation-name 
             (class-name == coagulate("<" ## ?name ## ">"))
          => (res :: <symbol>)
           coagulate("<raw-" ## ?name ## ">")
         end method }
end macro;

define macro &raw-type-definer
  { define &raw-type ?:name ?superclass:name }
    => { define &raw-type-aux ?name ?superclass }
  { define &raw-type ?:name }
    => { define &raw-type-aux ?name object }
end macro;

define macro &raw-type-and-accessors-definer
  { define &raw-type-and-accessors ?:name ?superclass:name }
    => { define &raw-type-aux ?name ?superclass;
         define &raw-type-accessors ?name object }
  { define &raw-type-and-accessors ?:name }
    => { define &raw-type-aux ?name object;
         define &raw-type-accessors ?name object }
end macro;

define macro &raw-type-aux-definer
  { define &raw-type-aux ?:name ?superclass:name }
    => { define class "<&raw-" ## ?name ## ">" ("<&raw-" ## ?superclass ## ">")
         end class;
         install-&class-mapping("<&raw-" ## ?name ## ">", "<raw-" ## ?#"name" ## ">");
         define compiler-open generic "raw-" ## ?name 
             (back-end /* :: <back-end> */) 
          => (raw-type-decriptor); 
         do-define-raw-type
           (coagulate("<raw-" ## ?name ## ">"),
	    coagulate("<raw-" ## ?superclass ## ">"),
	    "raw-" ## ?name) }
end macro;

define macro &raw-type-accessors-definer
  { define &raw-type-accessors ?:name ?superclass:name }
    => { define &primitive "primitive-" ## ?name ## "-at"
             (address :: <raw-address>, 
              offset :: <raw-integer>, 
              byte-offset :: <raw-integer>)
          => (data :: "<raw-" ## ?name ## ">");
         define side-effecting &primitive "primitive-" ## ?name ## "-at-setter"
             (value :: "<raw-" ## ?name ## ">", 
              address :: <raw-address>, 
              offset :: <raw-integer>, 
              byte-offset :: <raw-integer>)
          => (data :: "<raw-" ## ?name ## ">") }
end macro;

/*
define macro &raw-types-definer 
  { define &raw-types
      export ?type-names; 
    end }
    => { ?type-names } 
type-names:
  { }
    => { }
  { ?name:*, ... }
    => { define &raw-type ?name; ... }
end macro;
*/

define macro &dylan-raw-types-definer 
  { define &dylan-raw-types
      export ?type-names; 
    end }
    => { ?type-names } 
type-names:
  { }
    => { }
  { ?name:*, ... }
    => { define &dylan-raw-type ?name; ... }
end macro;

define macro &raw-types-and-accessors-definer 
  { define &raw-types-and-accessors
      export ?type-names; 
    end }
    => { ?type-names } 
type-names:
  { }
    => { }
  { ?name:*, ... }
    => { define &raw-type-and-accessors ?name; ... }
end macro;

define macro &raw-machine-word-subtypes-definer 
  { define &raw-machine-word-subtypes
      export ?type-names; 
    end }
    => { ?type-names } 
type-names:
  { }
    => { }
  { ?name:*, ... }
    => { define &raw-type ?name machine-word; ... }
end macro;

define macro &dylan-raw-machine-word-subtypes-definer 
  { define &dylan-raw-machine-word-subtypes
      export ?type-names; 
    end }
    => { ?type-names } 
type-names:
  { }
    => { }
  { ?name:*, ... }
    => { define &dylan-raw-type ?name machine-word; ... }
end macro;

define macro &raw-machine-word-subtypes-and-accessors-definer 
  { define &raw-machine-word-subtypes-and-accessors
      export ?type-names; 
    end }
    => { ?type-names } 
type-names:
  { }
    => { }
  { ?name:*, ... }
    => { define &raw-type-and-accessors ?name machine-word; ... }
end macro;



// eof
