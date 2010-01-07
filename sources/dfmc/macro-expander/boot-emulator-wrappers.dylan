Module:    dfmc-macro-expander
Synopsis:  Hacks to convert emulator flavour templates to compiler
           template description objects. Bootstrapping only.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method compile-compiler-template (contents)
  reparse
    (#{ make-template
          (?(compile-template-elements(convert-template(contents)))) });
end method;

/*
define method compile-macro-template (contents)
  #{ make-template
       (?(compile-template-elements(contents))) };
end method;

define method generate-expander-function (exp)
  compile-rewrite-rule-templates!(exp);
  let code = compile-rewrite-rule-expander(exp);
  let prefix = 
    access(infix-reader, reparse)
      (#{ method (env, form)
            let _f*_ = call-as-fragment-tokens(form);
            ?code
          end });
  // format-out("Prefix: %=\n", prefix);
  let raw-expander = compile-prefix-dylan-in(#"dfmc-macro-expander", prefix);
  let module = expander-module(exp);
  method (env, form)
    dynamic-bind (*expansion-module* = module)
      with-new-hygiene-context (#"unknown")
        raw-expander(env, form);
      end;
    end;
  end;
end method;
*/
