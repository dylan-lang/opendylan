Module:    dfmc-macro-expander
Synopsis:  Generate and compose functions to implement a template.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method generate-template-elements-functions (elements :: <list>)
  if (empty?(elements))
    #()
  else
    generate-template-element-function(elements.head, elements.tail);
  end;
end method;

define method generate-template-element-function
    (element :: type-union(<fragment>, <substitution>), rest :: <list>)
  let this = generate-one-template-element-function(element);
  let rest = generate-template-elements-functions(rest);
  pair(this, rest)
end method;

define method generate-template-element-function
    (element :: <separator-fragment>, rest :: <list>)
  if (empty?(rest) | ~instance?(rest.head, <maybe-empty-substitution>))
    next-method();
  else
    let sep = generate-one-template-element-function(element);
    let subst = generate-one-template-element-function(rest.head);
    let rest = generate-template-elements-functions(rest.tail);
    pair(match-method maybe-substitute-separator(sep(), subst()) end, rest);
  end;
end method;

define method generate-one-template-function (element :: <fragment>)
  generate-constructor-function(element);
end method;

define method generate-one-template-function (element :: <parens-fragment>)
  let nested 
    = generate-template-elements-functions
        (fragment-nested-fragments(element));
  match-method make-parens-fragment(call-list(nested)) end
end method;

define method generate-one-template-function (element :: <brackets-fragment>)
  let nested 
    = generate-template-elements-functions
        (fragment-nested-fragments(element));
  match-method make-brackets-fragment(call-list(nested)) end
end method;

define method generate-one-template-function (element :: <braces-fragment>)
  let nested 
    = generate-template-elements-functions
        (fragment-nested-fragments(element));
  match-method make-braces-fragment(call-list(nested)) end
end method;

define method generate-one-template-function (element :: <substitution>)
  generate-substitution-function(element);
end method;

// eof
