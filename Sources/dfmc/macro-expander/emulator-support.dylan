Module: dfmc-macro-expander
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function pattern-variable-name (var)
  let symbol = fragment-name(var);
  let name = if (symbol == #"...")
               symbol
             else
               as(<symbol>, concatenate(as(<string>, symbol), "__pvar"))
             end;
  if (*native-template-evaluation?*)
    /*
    make-unhygienic-name-fragment
      (as(<symbol>, as(<string>, symbol))); // unsuffixed
    */
    // TODO: PERFORMANCE: Avoid copying if it's already a variable name
    make-variable-name-like
      (var, 
       record:          fragment-record(var),
       source-position: fragment-source-position(var),
       name:            symbol);
  else
    make(access(infix-reader, <parsed-fragment>),
         token-class: parsed-name:,
         token-value: name)
  end;
end function;

// eof
