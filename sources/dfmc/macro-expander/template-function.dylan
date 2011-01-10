Module:    dfmc-macro-expander
Synopsis:  Generate and compose functions to implement a template.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <match-environment> = <simple-object-vector>;
define constant <substitution-method> = <method>;

define macro match-method
  { match-method ?:body end } 
    => { method (?=env :: <match-environment>) => (value) 
           ?body
         end }
end macro;

define method generate-template-function 
    (elements :: <list>) => (constructor :: <function>)
  let functions = generate-template-elements-functions(elements);
  match-method
    // TODO: CORRECTNESS: Will fragments always be a list?
    let fragments = call-list-with-collecting(functions, env);
    if (empty?(fragments) | empty-template-elements?(fragments))
      $the-empty-template
    else
      make(<template>, fragments: fragments);
    end;
  end;
end method;

/*
define method generate-template-elements-function 
    (elements :: <list>) => (constructor :: <function>)
  let functions = generate-template-elements-functions(elements);
  match-method
    call-list-with-collecting(functions, env)
  end;
end method;
*/

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
    pair(match-method 
           maybe-substitute-separator(sep(env), subst(env))
         end, rest);
  end;
end method;

define method generate-one-template-element-function (element :: <fragment>)
  generate-constructor-function(element);
end method;

define method generate-one-template-element-function (element :: <parens-fragment>)
  let nested 
    = generate-template-elements-functions
        (fragment-nested-fragments(element));
  match-method 
    make-parens-fragment(call-list-with-collecting(nested, env)) 
  end
end method;

define method generate-one-template-element-function (element :: <brackets-fragment>)
  let nested 
    = generate-template-elements-functions
        (fragment-nested-fragments(element));
  match-method 
    make-brackets-fragment(call-list-with-collecting(nested,env))
  end
end method;

define method generate-one-template-element-function (element :: <braces-fragment>)
  let nested 
    = generate-template-elements-functions
        (fragment-nested-fragments(element));
  match-method 
    make-braces-fragment(call-list-with-collecting(nested, env))
  end
end method;

define method generate-one-template-element-function (element :: <substitution>)
  generate-substitution-function(element);
end method;
