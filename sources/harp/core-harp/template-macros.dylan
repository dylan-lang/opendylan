module:    harp-templates
Synopsis:  Macro support for the HARP template pattern matchers
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Macros for handling the backend templates
//// Started by Tony (8 Apr 94)


/// DEFINE TEMPLATE-DEFINER-MACRO defines an infix macro for defining templates for 
/// a given instruction set. The macros so defined adds a function definition
/// into the code-gen-fn slot of the named op(s) for the instruction set. If 
/// the SELF option is used, then the actual op is passed as a parameter.


define macro template-definer-macro-definer
  { define template-definer-macro ?:name (?backend:name, ?instruction-set:name) end }
    => { define macro ?name ## "-definer"
           { define ?name \?body:* end }
             => { define op-template (?backend, ?instruction-set)
                    \?body
                  end }
         end macro }
end macro;



/// DEFINE OP-TEMPLATE defines templates in the given instruction set.
/// It is not normally used directly, but is used by the expansion of
/// DEFINE TEMPLATE-DEFINER-MACRO


define macro op-template-definer
  { define op-template (?backend:name, ?instruction-set:name) ?:name ?body:* end }
    => { define op-template (?backend, ?instruction-set) (?name) ?body end }

  { define op-template (?backend:name, ?instruction-set:name) (?names:*) ?body:* end }
    => { begin
           let template-fn = template-function (?backend, <op>) ?body end;
           let instruction-set = ?instruction-set;
           ?names
         end }
names:
  { } => { }
  { ?:name, ... }
    => { instruction-set . "harp-" ## ?name . op-code-gen-fn := template-fn;
         ... }
end macro;




define macro local-template-definer-macro-definer
  { define local-template-definer-macro ?:name (?backend:name) end }
    => { define macro ?name ## "-definer"
           { define ?name \?body:* end }
             => { define local-template (?backend)
                    \?body
                  end }
         end macro }
end macro;


/// DEFINE LOCAL-TEMPLATE defines a function into a namespace parallel
/// to the module namespace, and accessible via LOCAL-FN. If the SELF 
/// option is used, then the value of the normal named module variable 
/// is passed as a parameter.



define macro local-template-definer
  { define local-template (?backend:name) ?:name ?body:* end }
    => { define local-template (?backend) (?name) ?body end }

  // If we use the "self" option, then the self parameter is passed by
  // currying
  { define local-template (?backend:name) (?name1:name, ?names:*) 
      options (self);
      ?body:* 
    end }
    => { define constant ?name1 ## "-template" 
           = template-function (?backend, <integer>) options (self); ?body end;

         define-curried-template-functions ?name1 ## "-template" 
           (?name1, ?names) 
         end }

  // If we don't use "self", then make a template function which just
  // doesn't accept the self parameter, by pretending that it uses option (self)

  { define local-template (?backend:name) (?name1:name, ?names:*) ?body:* end }
    => { define constant ?name1 ## "-template" 
           = template-function (?backend, <object>) options (self); ?body end;
         define-shared-template-functions ?name1 ## "-template" 
           (?name1, ?names) 
         end }

end macro;


define macro define-shared-template-functions
  { define-shared-template-functions ?extra-arg:name () end }
    => { }

  { define-shared-template-functions ?inner-fn:name (?name1:name, ?more:*) end }
    => { define-shared-template-functions ?inner-fn (?more) end;
         define constant "harp-" ## ?name1 = ?inner-fn; }
end macro;


define macro define-curried-template-functions
  { define-curried-template-functions ?extra-arg:name () end }
    => { }

  { define-curried-template-functions ?inner-fn:name (?name1:name, ?more:*) end }
    => { define-curried-template-functions ?inner-fn (?more) end;
         define constant "harp-" ## ?name1 = template-curry(?inner-fn, ?name1); }
end macro;


define method template-curry (inner-fn :: <function>, op-value)
  method (be :: <harp-back-end>, #rest other-args)
    apply(inner-fn, be, op-value, other-args);
  end method;
end method;


define macro local-fn
  { local-fn(?:name) }
    => { "harp-" ## ?name }
end macro;


// call-local may be used to call local templates

define macro call-local
  { call-local(?:name, ?args:*) }
    => { (local-fn(?name))(?args) }
end macro;



// TEMPLATE-FUNCTION constructs the lambda which performs the pattern match 
//
// The arguments passed to the lambda are (backend, op, ....)
// By default, the op parameter does not appear in the patterns. However, it
// may be bound to a pattern variable if "options (self)" is supplied.

define macro template-function
  { template-function (?backend:name, ?op:name)
      options (self);
      ?self-patterns:*
    end }
    => { template-function-aux (?backend, ?op) (?self-patterns) ()
           ?self-patterns
         end }

  { template-function (?backend:name, ?op:name)
      options ();
      ?patterns:*
    end }
    => { template-function-aux (?backend, ?op) (?patterns) ()
           ?patterns
         end }

  { template-function (?backend:name, ?op:name)
      ?patterns:*
    end }
    => { template-function-aux (?backend, ?op) (?patterns) ()
           ?patterns
         end }

patterns:
  // Insert a dummy parameter into the template for OP
  { } => { }
  { pattern (?be:name, ?rest:*) ?:body ... }
    => { pattern (?be, $op$, ?rest) ?body ... }

end macro;



define macro template-function-aux
  // Here's a dirty way of coming up with "gensyms" for each parameter
  // NB we can't just use name mangling, because of the possibility
  // of duplication of base names in the pattern.
  // Assume templates will never take more than 8 args

  { template-function-aux (?backend:name, ?op:name)
      (pattern ( ?a1:*, ?a2:*, ?a3:*, ?a4:*, ?a5:*, ?a6:*, ?a7:*, ?a8:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend, a2$ :: ?op, a3$, a4$, a5$, a6$, a7$, a8$ )
	   ?patterns
         end
	}

  { template-function-aux (?backend:name, ?op:name) 
      (pattern ( ?a1:*, ?a2:*, ?a3:*, ?a4:*, ?a5:*, ?a6:*, ?a7:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend, a2$ :: ?op, a3$, a4$, a5$, a6$, a7$ )
	   ?patterns
         end
	}

  { template-function-aux (?backend:name, ?op:name) 
      (pattern ( ?a1:*, ?a2:*, ?a3:*, ?a4:*, ?a5:*, ?a6:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend, a2$ :: ?op, a3$, a4$, a5$, a6$ )
	   ?patterns
         end
	}

  { template-function-aux (?backend:name, ?op:name) 
      (pattern ( ?a1:*, ?a2:*, ?a3:*, ?a4:*, ?a5:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend, a2$ :: ?op, a3$, a4$, a5$ )
	   ?patterns
         end
	}

  { template-function-aux (?backend:name, ?op:name) 
      (pattern ( ?a1:*, ?a2:*, ?a3:*, ?a4:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend, a2$ :: ?op, a3$, a4$ )
	   ?patterns
         end
	}

  { template-function-aux (?backend:name, ?op:name) 
      (pattern ( ?a1:*, ?a2:*, ?a3:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend, a2$ :: ?op, a3$ )
	   ?patterns
         end
	}

  { template-function-aux (?backend:name, ?op:name) 
      (pattern ( ?a1:*, ?a2:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend, a2$ :: ?op )
	   ?patterns
         end
	}

  { template-function-aux (?backend:name, ?op:name) 
      (pattern ( ?a1:name ?opts:* ) ?stuff:*) ()
      ?patterns:*
    end }
    => { template-function-aux () () ( a1$ :: ?backend )
	   ?patterns
         end
	}

  { template-function-aux () () (?parameters:*)
      ?patterns:*
    end }
    => { method (?parameters)
           compile-clauses (?parameters) ?patterns end;
         end method }

end macro;


define macro compile-clauses
  { compile-clauses (?parameters:*) end }
    => { pattern-error(?parameters) }

  { compile-clauses (?parameters:*) 
      pattern (?p-params:*) ?:body ?other-patterns:*
    end }
    => { compile-one-clause ((?p-params) (?parameters))
           (?p-params)
           ?body
         fail
           compile-clauses (?parameters) ?other-patterns end 
         end }

other-patterns:
  { } => { }
  { pattern ?stuff:* } => { pattern ?stuff }

parameters:
    { } => { }
    { ?parameter:name, ... } => { ?parameter, ... }
    { ?parameter:name :: ?class:name, ... } => { ?parameter, ... }

end macro;


define macro compile-one-clause

  // First recurse over all the bindings
  { compile-one-clause ((?pvar:name ?opts:*, ?prest:*) (?mvar:name, ?mrest:*) ) (?any-conditions:*) 
      ?match-body:body 
    fail ?fail-body:body
    end }
    => { let ?pvar = ?mvar;
         compile-one-clause ((?prest) (?mrest)) (?any-conditions)
	  ?match-body
	  fail ?fail-body
         end }

  // Finally perform the tests

  { compile-one-clause (() ()) (?conditions:*)
      ?match-body:body 
    fail ?fail-body:body
    end }
    => { let match? = 
	   compile-one-test (?conditions)
	     ?match-body
           end;

         unless (match?)
	   ?fail-body
         end unless
       }

end macro;

define macro compile-one-test

  // Finally perform the tests

  { compile-one-test (?pvar:name by ?fn:name is ?p2var:name, ?conditions:*) ?:body end }
    => { let ?pvar = ?fn(?pvar);
	 if (?pvar == ?p2var)
	   compile-one-test (?conditions) ?body end
         end }

  { compile-one-test (?pvar:name is ?p2var:name, ?conditions:*) ?:body end }
    => { if (?pvar == ?p2var)
	   compile-one-test (?conditions) ?body end
         end }

  { compile-one-test (?pvar:name by ?fn:name, ?conditions:*) ?:body end }
    => { let ?pvar = ?fn(?pvar);
	 if (?pvar)
	   compile-one-test (?conditions) ?body end
         end
        }

  { compile-one-test (?pvar:name :: "<" ## ?class:name ## ">" by ?fn:name, ?conditions:*) ?:body end }
    => { let ?pvar = ?fn(?pvar);
	 if (instance?(?pvar, "<" ## ?class ## ">"))
	   compile-one-test (?conditions) ?body end
         end }

  { compile-one-test (?pvar:name :: any by ?fn:name, ?conditions:*) ?:body end }
    => { let ?pvar = ?fn(?pvar);
	 compile-one-test (?conditions) ?body end
        }

  { compile-one-test (?pvar:name :: ?any:name by ?fn:name, ?conditions:*) ?:body end }
    => { let ?pvar = ?fn(?pvar);
	 if (?pvar)
	   compile-one-test (?conditions) ?body end
         end }

  { compile-one-test (?pvar:name :: "<" ## ?class:name ## ">" of ?fn:name, ?conditions:*) ?:body end }
    => { if (instance?(?pvar, "<" ## ?class ## ">"))
	   if (?fn(?pvar))
	     compile-one-test (?conditions) ?body end
	   end
         end }

  { compile-one-test (?pvar:name, ?conditions:*) ?:body end }
    => { if (?pvar)
	   compile-one-test (?conditions) ?body end
         end }

  { compile-one-test (?pvar:name :: "<" ## ?class:name ## ">", ?conditions:*) ?:body end }
    => { if (instance?(?pvar, "<" ## ?class ## ">"))
	   compile-one-test (?conditions) ?body end
         end }

  { compile-one-test (?pvar:name :: any, ?conditions:*) ?:body end }
    => { compile-one-test (?conditions) ?body end }

  { compile-one-test (?pvar:name :: ?any:name, ?conditions:*) ?:body end }
    => { if (?pvar)
	   compile-one-test (?conditions) ?body end
         end }

  { compile-one-test () ?:body end }  => { ?body; #t }

end macro;


/*

  Just for testing purposes

define local-template-definer-macro local-harp-template
  (<harp-back-end>)
end;

define local-harp-template add-setting-flags
  pattern (be, d, s1, s2)
    three-2(be, local-fn(add2), d, s1, s2);
end local-harp-template;

*/
