Module:    dfmc-macro-expander
Synopsis:  Generate and compose code to implement a template.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method compile-macro-template-to-code (contents)
  let constructor-code = compile-template-elements(contents);
  #{ make-template(?constructor-code) };
end method;

define method compile-template-elements (elements :: <list>)
  if (empty?(elements))
    #{ }
  else
    compile-template-element(elements.head, elements.tail);
  end;
end method;

define method compile-template-element
    (element :: type-union(<fragment>, <substitution>), rest :: <list>)
  let this = compile-one-template-element(element);
  let rest = compile-template-elements(rest);
  #{ ?this, ?rest }
end method;

define method compile-template-element
    (element :: <separator-fragment>, rest :: <list>)
  if (empty?(rest) | ~instance?(rest.head, <maybe-empty-substitution>))
    next-method();
  else
    let sep = compile-one-template-element(element);
    let subst = compile-one-template-element(rest.head);
    let rest = compile-template-elements(rest.tail);
    #{ maybe-substitute-separator(?sep, ?subst), ?rest }
  end;
end method;

define method compile-one-template-element (element :: <fragment>)
  generate-constructor(element);
end method;

define method compile-one-template-element (element :: <parens-fragment>)
  let nested = compile-template-elements(fragment-nested-fragments(element));
  #{ make-parens-fragment(list(?nested)) }
end method;

define method compile-one-template-element (element :: <brackets-fragment>)
  let nested = compile-template-elements(fragment-nested-fragments(element));
  #{ make-brackets-fragment(list(?nested)) }
end method;

define method compile-one-template-element (element :: <braces-fragment>)
  let nested = compile-template-elements(fragment-nested-fragments(element));
  #{ make-braces-fragment(list(?nested)) }
end method;

define method compile-one-template-element (element :: <substitution>)
  generate-substitution(element);
end method;

// eof
