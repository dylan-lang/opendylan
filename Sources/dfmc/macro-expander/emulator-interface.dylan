Module: dfmc-macro-expander
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// We actually need an interface whereby, given the name of the module
// binding that currently contains the macro, the appropriate word and
// word-class is returned (although the word-class is binding invariant). 

define method macro-word-in-variable-name
    (compiled-macro, variable-name) => (word, word-class)
  access(infix-reader, macro-word-in-variable-name)
    (compiled-macro, variable-name);
end method;

define method compile-macro-definition 
    (binding-name, main-rules, aux-rule-sets) => (compiled-macro)
  access(infix-reader, process-compiler-macro-definition)
    (binding-name, main-rules: main-rules, aux-rule-sets: aux-rule-sets)
end method;

// TODO: This is the unhygienic definition.

define macro expanding-fragment
  { expanding-fragment (?fragment:expression) ?:body end }
    => { do-expanding-fragment(fragment, method () ?body end); }
end macro;

define method do-expanding-fragment (fragment, f)
  with-fragment-info (fragment)
    let new-fragment
      = call-with-syntax-classifier
          (method ()
             f();
           end,
           classify-word);
    infixify(new-fragment);
  end;
end method;

define method expand-macro-call
    (compiled-macro, fragment) => (expanded-fragment)
  expanding-fragment (fragment)
    access(infix-reader, process-infix-macro-call)
               (compiled-macro, fragment-argument(fragment));
  end;
end method;

define macro handling-parse-errors
  { handling-parse-errors 
      ?:body 
    on-error (?:variable, #rest ?options:*)
      ?fixup:body
    end }
    => { do-handling-parse-errors
           (method () ?body end, method (?variable) ?fixup end, ?options) }
end macro;

define thread variable *parse-handler-depth* = 0;

define method do-handling-parse-errors (body, fixup, #key context = "form")
  let this-depth = *parse-handler-depth* + 1;
  local method closest-handler? (condition)
    this-depth = *parse-handler-depth*
  end method;
  dynamic-bind (*parse-handler-depth* = this-depth)
    block ()
      body();
    exception (e :: <macro-match-error>, test: closest-handler?)
      fixup(e)
    exception (e :: <reader-error>, test: closest-handler?)
      fixup(e)
    end;
  end;
end method;

// eof
