module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DEFINING

define class <primitive-descriptor> (<object>)
  constant slot primitive-emitter :: <function>, 
    required-init-keyword: emitter:;
end class;

define macro &primitive-definer 
  { define ?adjectives:* &primitive ?:name
      (?parameters:*) => (?values:*) }
    => { define ?adjectives &primitive-aux (?name, #f)
           (?parameters) => (?values) }
end macro;

define macro &primitive-aux-definer 
  { define ?adjectives &primitive-aux (?:name, ?override:expression)
        (?parameters:*) => (?values:*) }
    => { do-define-core-primitive
           (?#"name", #(?adjectives),
	    ?override,
	    primitive-spec (?parameters) (?values) end) }
adjectives:
    { } => { }
    { ?adjective:name ...} => { ?#"adjective", ... }
end macro;
    
define function make-primitive-specs 
    (x :: <simple-object-vector>) 
 => (reqs :: <simple-object-vector>, rest :: false-or(<rest-variable-spec>))
  let req-size
    = logand(size(x), lognot(1)); // make even
  let reqs 
    = collecting (as <simple-object-vector>)
        for (i from 0 below req-size by 2)
	  let name = x[i];
	  let type = x[i + 1];
	  collect(make(<required-variable-spec>,
		       variable-name:   make-variable-name-fragment(as(<symbol>, name)),
		       type-expression: type & make-variable-name-fragment(as(<symbol>, type))));
	end for;
      end collecting;
  let name
    = odd?(size(x)) & as(<symbol>, x[size(x) - 1]);
  let rest
    = name & make(<rest-variable-spec>, variable-name: make-variable-name-fragment(name));
  values(reqs, rest);
end function;

define function make-primitive-sig-spec 
    (args :: <simple-object-vector>, vals :: <simple-object-vector>)
  let (arg-reqs, arg-rest) = make-primitive-specs(args);
  let (val-reqs, val-rest) = make-primitive-specs(vals);
  make(<primitive-signature-spec>,
       argument-required-variable-specs: arg-reqs,
       argument-rest-variable-spec:      arg-rest,
       value-required-variable-specs:    val-reqs,
       value-rest-variable-spec:         val-rest);
end function;

define macro primitive-specs
  { primitive-specs () end }
    => { #[] }
  { primitive-specs (?specs) end }
    => { concatenate(?specs) }
specs:
  { }               => { }
  { \#rest ?:name } => { vector(?"name") }
  { ?spec, ... }    => { ?spec, ... }
spec:
  { ?:name }
   => { immutable-vector(?"name", #f) }
  { ?:name :: ?type:name }
   => { immutable-vector(?"name", ?"type") }
end macro;

define macro primitive-spec
  { primitive-spec (?req-specs:*) (?val-specs:*) end }
    => { make-primitive-sig-spec
	  (primitive-specs (?req-specs) end, primitive-specs (?val-specs) end) }
end macro;

/*
define macro primitive-req-specs
  { primitive-req-specs (?req-specs) end }
    => { vector(?req-specs) }
req-specs:
  { }                   => { }
  { \#rest ?:variable } => { }
  { ?req-spec, ... }    => { ?req-spec, ... }
req-spec:
  { ?:name }
   => { make(<required-variable-spec>,
	     variable-name: make-variable-name-fragment(?#"name")) }
  { ?:name :: ?type:name }
   => { make(<required-variable-spec>,
	     variable-name:   make-variable-name-fragment(?#"name"),
	     type-expression: make-variable-name-fragment(?#"type")) }
end macro;

define macro primitive-rest-spec
  { primitive-rest-spec (?rest-spec) end }
    => { ?rest-spec }
rest-spec:
  { }                 
    => { #f }
  { ?:variable, ... } 
    => { ... }
  { \#rest ?:name }
    => { make(<rest-variable-spec>,
	      variable-name: make-variable-name-fragment(?#"name")) }
end macro;

define macro primitive-spec
  { primitive-spec 
       (?req-specs:*) (?rest-spec:*) (?val-req-specs:*) (?val-rest-spec:*) 
    end }
    => { make(<primitive-signature-spec>,
	      argument-required-variable-specs: 
		primitive-req-specs (?req-specs) end,
	      argument-rest-variable-spec:      
	        primitive-rest-spec (?rest-spec) end,
	      value-required-variable-specs:    
                primitive-req-specs (?val-req-specs) end,
	      value-rest-variable-spec:         
                primitive-rest-spec (?val-rest-spec) end) }
end macro;
*/

define macro &primitive-and-override-definer 
  { define ?adjectives:* &primitive-and-override ?:name
      (?parameters:*) => (?values:*) ?:body end }
    => { define &primitive-override ?name (?parameters) ?body end; 
         define ?adjectives &primitive-aux (?name, ?name ## "-override")
             (?parameters) => (?values) }
end macro;

define macro &primitive-override-definer 
  { define ?adjectives:* &primitive-override ?:name (?override-parameters)
      ?:body 
    end }
    => { define method ?name ## "-override" (?override-parameters)
           ?body
         end method }
override-parameters:
    { } => { }
    { \#rest ?:name } => { #rest ?name }
    { ?variable-name, ... } => { ?variable-name, ... }
variable-name: 
  { ?:name :: ?:expression ?ignore:* }
    => { ?name }
end macro;
    
define macro &simple-machine-word-primitive-definer
  { define &simple-machine-word-primitive ?:name (?parameters:*) => (?values:*) }
    => { define side-effect-free stateless dynamic-extent &primitive ?name
             (?parameters) => (?values) }
end macro &simple-machine-word-primitive-definer;

define macro &machine-word-primitive-definer
  { define custom &machine-word-primitive ?:name (?parameters:*) => (?values:*)
      ?:body
    end }
    => { define side-effect-free stateless dynamic-extent &primitive-and-override ?name
             (?parameters) => (?values)
           ?body
         end }
  { define &machine-word-primitive ?:name (?parameters:*) => (?values:*)
      ?:body 
    end }
    => { define &machine-word-primitive-1 ?name 
             (extract-mw-operand-unsigned, make-raw-literal) (?parameters) => (?values)
           ?body
         end }
  { define sign-extend &machine-word-primitive ?:name (?parameters:*) => (?values:*)
      ?:body 
    end }
    => { define &machine-word-primitive-1 ?name 
             (extract-mw-operand-signed, make-raw-literal) (?parameters) => (?values)
           ?body
         end }
  { define overflow &machine-word-primitive ?:name (?parameters:*) => (?values:*)
      ?:body 
    end }
    => { define &machine-word-primitive-1 ?name 
             (extract-mw-operand-unsigned, make-raw-literal-with-overflow) (?parameters)
          => (?values)
           ?body
         end }
  { define sign-extend overflow &machine-word-primitive ?:name (?parameters:*) => (?values:*)
      ?:body 
    end }
    => { define &machine-word-primitive-1 ?name 
             (extract-mw-operand-signed, make-raw-literal-with-overflow)
             (?parameters) => (?values)
           ?body
         end }
  { define overflow sign-extend &machine-word-primitive ?:name (?parameters:*) => (?values:*)
      ?:body 
    end }
    => { define &machine-word-primitive-1 ?name 
             (extract-mw-operand-signed, make-raw-literal-with-overflow)
             (?parameters) => (?values)
           ?body
         end }
end macro &machine-word-primitive-definer;

define macro &machine-word-primitive-1-definer
  { define &machine-word-primitive-1 ?:name (?extractor:name, ?generator:name)
        (?parameters:*) => (?values:*)
      ?:body
    end }
    => { define side-effect-free stateless dynamic-extent &primitive-and-override ?name
             (?parameters) => (?values)
           &machine-word-primitive-body (?extractor, ?generator, ?parameters) ?body end
         end }
end macro &machine-word-primitive-1-definer;

define macro &machine-word-primitive-body
  { &machine-word-primitive-body (?extractor:name, ?generator:name) ?:body end }
    => { let result = begin ?body end;
	 ?generator(result) }
  { &machine-word-primitive-body
         (?extractor:name, ?generator:name, ?parameter-name:name :: ?parameter-type:name,
	  ?rest-parameters:*)
      ?:body 
    end }
    => { let ?parameter-name = ?extractor(?parameter-name);
         &machine-word-primitive-body (?extractor, ?generator, ?rest-parameters) ?body end }
end macro &machine-word-primitive-body;

// eof
