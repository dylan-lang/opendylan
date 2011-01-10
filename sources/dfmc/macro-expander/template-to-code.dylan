Module:    dfmc-macro-expander
Synopsis:  Generate code for template substitution operations.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method generate-substitution 
    (subst :: <simple-element-substitution>) => (code)
  let name = element-variable-name(subst);
  #{ import-to-template(?name) }
end method;

define method generate-substitution 
    (subst :: <as-string-substitution>) => (code)
  let name = element-variable-name(subst);
  #{ substitute-as-string(?name) }
end method;

define method generate-substitution 
    (subst :: <as-symbol-substitution>) => (code)
  let name = element-variable-name(subst);
  #{ substitute-as-symbol(?name) }
end method;

define method generate-substitution 
    (subst :: <splicing-substitution>) => (code)
  let inner = element-name-substitution(subst);
  let name = element-variable-name(inner);
  let prefix = element-prefix(subst);
  let suffix = element-suffix(subst);
  let splicer = splicing-function-name(inner);
  #{ ?splicer(?prefix, ?name, ?suffix) }
end method;

define method splicing-function-name (subst :: <as-string-substitution>)
  #{ substitute-spliced-as-string }
end method;

define method splicing-function-name (subst :: <as-symbol-substitution>)
  #{ substitute-spliced-as-symbol }
end method;

define method splicing-function-name (subst :: <simple-element-substitution>)
  #{ substitute-spliced-as-name }
end method;

define method generate-substitution 
    (subst :: <simple-sequence-substitution>) => (code)
  let name = element-variable-name(subst);
  let separator = element-separator(subst);
  if (separator)
    let separator = generate-constructor(separator);
    #{ substitute-sequence-separated(?name, ?separator) }
  else
    #{ substitute-sequence(?name) }
  end;
end method;

define method dude-expander? 
    (exp :: <rewrite-rule-expander>) => (well? :: <boolean>)
  member?(#"dude", expander-adjectives(exp))
end method;

define method generate-dude-expander-function (exp)
  let dudes
    = vector
        ("            The compiler dudes who stayed the course...",
         "           Tony Mann (Dude amongst Dudes)",
         "          Jonathan Bachrach (Dude in Chief)",
         "         Keith Playford (Deputy Dude)",
         "        Nosa Omorogbe (Daddy Dude)",
         "       Gary Palter (Deadline Dude)",
         "      Roman Budzianowski (Dapper Dude)",
         "     Glenn Burke (Dispatch Dude)",
         "    Gail Zacharias (Dependency Dude)",
         "   Paul Howard (Debugger Dude)",
         "  Keith Dennison (Decibel Dude)",
         " Mark Tillotson (Dude-u-Like)",
         "Greg Sullivan (Dynamic Dude)"
        );
  method (tokens)
    let base-name = make-name-fragment(#"dude: ");
    collecting (defs)
      for (dude-name in dudes)
        let full-name = suffix-name-hygienically(base-name, dude-name);
        collect-into(defs, #{ define constant ?full-name = ?dude-name })
      end;
      let defs = collected(defs);
      #{ ??defs; ... }
    end;
  end
end method;
