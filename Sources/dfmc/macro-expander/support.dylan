Module: dfmc-macro-expander
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function pattern-variable-name (var)
  let symbol = fragment-name(var);
  /*
  let name = if (symbol == #"...")
               symbol
             else
               as(<symbol>, concatenate(as(<string>, symbol), "__pvar"))
             end;
  */
  // TODO: PERFORMANCE: Avoid copying if it's already a variable name
  make-variable-name-like
    (var, 
     record:          fragment-record(var),
     source-position: fragment-source-position(var),
     name:            symbol);
end function;

/*
define function compile-macro-template (template) => (template)
  // break("Compiling template");
  generate-template-function(template)
end function;

define function generate-expander-function (exp) => (function)
  format-out("Expander generator stub\n");
  compile-rewrite-rule-templates!(exp);
  let f = generate-rewrite-rule-expander-function(exp);
  format-out("Generating expander function\n");
  let module = expander-module(exp);
  method (env, form)
    with-expansion-module (module)
      with-new-hygiene-context (#"unknown")
        format-out("Calling expander function\n");
        f(call-as-fragment-tokens(form));
      end
    end
  end;
end function;
*/

// eof
