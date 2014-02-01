Module: dfmc-flow-graph
Author: Jonathan Bachrach and Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro graph-class-definer
  { define ?mods:* graph-class ?:name (?supers:*) ?slots:* end }
    => { define ?mods graph-class-aux ?name (?supers) (?slots) end;
         define graph-class-accessors ?name (?slots) end; }
end macro;

define generic used-temporary-accessors
    (c :: <computation>) => (res :: <simple-object-vector>);
define generic class-used-temporary-accessors
    (c :: subclass(<computation>)) => (res :: <simple-object-vector>);

define macro graph-class-aux-definer
  { define ?mods:* graph-class-aux ?:name (?supers:*) (?slots:*) end }
    => { define ?mods class ?name (?supers) ?slots end }
slots:
  { }
    => { }
  { ?slot:*; ... }
    => { ?slot; ... }
slot:
  { virtual temporary slot ?:variable, ?stuff:* }
    => { virtual slot ?variable, ?stuff }
  { temporary slot ?:variable ?init:*, ?stuff:* }
    => { slot ?variable ?init, ?stuff }
  { ?other:* }
    => { ?other }
init:
  { }
    => { }
  { = ?:expression }
    => { = ?expression }
end macro;

define macro graph-class-accessors-definer
  { define graph-class-accessors ?:name (?methods) end }
    => { define graph-class-accessors-aux ?name (?methods) end }
methods:
  { }
    => { }
  { ?modifiers:* temporary slot ?:name :: ?:expression ?init:*, ?stuff:*;
    ... }
    => { make(<temporary-accessors>,
	      getter: ?name, setter: ?name ## "-setter"), ... }
  { ?other:*; ... }
    => { ... }
end macro;

define macro graph-class-accessors-aux-definer
  { define graph-class-accessors-aux ?:name () end }
    => { }
  { define graph-class-accessors-aux ?:name (?methods:*) end }
    => { define constant "$" ## ?name ## "-accessors" :: <simple-object-vector>
           = vector(?methods);
         define method class-used-temporary-accessors
             (c :: subclass(?name), #next next-method)
	  => (res :: <simple-object-vector>)
           concatenate("$" ## ?name ## "-accessors", next-method())
         end method;
         define constant "$" ## ?name ## "-total-temporary-accessors"
           = class-used-temporary-accessors(?name);
         define method used-temporary-accessors
	     (c :: ?name) => (res :: <simple-object-vector>)
           "$" ## ?name ## "-total-temporary-accessors"
         end method }
end macro;

define macro graph-class-tracer
  { graph-class-tracer(?type:name ; "%" ## ?:name ; ?ftype:expression) }
    => { define method ?name (c :: ?type) => (res :: ?ftype)
           c."%" ## ?name
         end;
         define method ?name ## "-setter" (new :: ?ftype, c :: ?type) => (res :: ?ftype)
           trace-dfm-reconnection(as(<symbol>, ?"name" ## "-setter"),
                                  c, "%" ## ?name, new);
           "%" ## ?name ## "-setter"(new, c)
         end; }
end macro;

define macro for-temporary
  { for-temporary (?:variable in ?:expression) ?:body end }
    => { for (?variable in ?expression.temporaries)
           ?body
         end }
end macro;

define macro for-lambda
  { for-lambda (?:variable in ?:expression) ?:body end }
    => { for (sub-e in ?expression.environment.inners)
           let ?variable = sub-e.lambda;
           ?body
         end }
end macro;

define macro for-all-lambdas
  { for-all-lambdas (?:variable in ?:expression) ?:body end }
    => { do-all-lambdas(method (?variable) ?body end, ?expression) }
end macro;

define inline method do-all-lambdas (f, outer-lambda :: <&lambda>)
  for (sub-e in outer-lambda.environment.inners)
    do-all-lambdas(f, sub-e.lambda);
  end;
  f(outer-lambda);
end method;

define macro for-all-used-lambdas
  { for-all-used-lambdas (?:variable in ?:expression) ?:body end }
    => { do-all-lambdas
           (method (?variable) if (lambda-used?(?variable)) ?body end end,
           ?expression) }
end macro;

define macro for-used-lambda
  { for-used-lambda (?:variable in ?:expression) ?:body end }
    => { do-used-lambdas(method (?variable) ?body end, ?expression) }
end macro;

define inline method do-used-lambdas (f, outer-lambda :: <&lambda>)
  for (sub-e in outer-lambda.environment.inners)
    let sub-lambda = sub-e.lambda;
    if (lambda-used?(sub-lambda))
      f(sub-lambda);
    end;
  end;
end method;

// If any aspect of the function is used, it must be preserved since
// the iep and xep will make reference to it.

define method lambda-used? (lambda)
  used?(lambda) | used?(lambda.iep)
end method;

define method lambda-users (lambda)
  concatenate(users(lambda), users(lambda.iep))
end method;

define macro for-computations
  { for-computations (?:variable from ?first:expression before ?last:expression)
      ?:body
    end }
  => { walk-lambda-computations
         (method (previous, ?variable) ignore(previous); ?body end,
	  ?first, before: ?last, previous?: #t) }
  { for-computations (?:variable previous ?previous:variable from ?first:expression)
      ?:body
    end }
  => { walk-lambda-computations
	(method (?previous, ?variable) ?body end, ?first, previous?: #t) }
  { for-computations (?:variable from ?first:expression)
      ?:body
    end }
  => { walk-lambda-computations
	(method (previous, ?variable) ignore(previous); ?body end, ?first, previous?: #t) }
  { for-computations (?:variable previous ?previous:variable in ?:expression)
      ?:body
    end }
  => { for-computations (?variable previous ?previous from ?expression.body)
         ?body
       end }
  { for-computations (?:variable in ?:expression)
      ?:body
    end }
  => { for-computations (?variable from ?expression.body)
         ?body
       end }
end macro;
