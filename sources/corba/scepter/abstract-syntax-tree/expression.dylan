Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <expression-combinator> (<object>)
  constant slot combinator-name :: false-or(<string>) = #f, init-keyword: name:;
  constant slot dylan-function :: false-or(<function>) = #f, init-keyword: function:; 
end class;

define class <binary-expression-combinator> (<expression-combinator>)
end class;

define class <unary-expression-combinator> (<expression-combinator>)
end class;

define class <add-combinator> (<binary-expression-combinator>)
end class;

define constant $add-combinator =
  make(<add-combinator>,
       name: "+",
       function: \+);

define class <minus-combinator> (<binary-expression-combinator>)
end class;

define constant $minus-combinator =
  make(<minus-combinator>,
    name: "-",
    function: \-);

define class <multiply-combinator> (<binary-expression-combinator>)
end class;

define constant $multiply-combinator =
  make(<multiply-combinator>,
    name: "*",
    function: \*);

define class <divide-combinator> (<binary-expression-combinator>)
end class;

define constant $divide-combinator =
  make(<divide-combinator>,
    name: "/",
    function: truncate/);

define class <modulus-combinator> (<binary-expression-combinator>)
end class;

define constant $modulus-combinator =
  make(<modulus-combinator>,
    name: "%",
    function: modulo);

define class <or-combinator> (<binary-expression-combinator>)
end class;

define constant $or-combinator =
  make(<or-combinator>,
    name: "|",
    function: logior);

define class <xor-combinator> (<binary-expression-combinator>)
end class;

define constant $xor-combinator =
  make(<xor-combinator>,
    name: "^",
    function: logxor);

define class <and-combinator> (<binary-expression-combinator>)
end class;

define constant $and-combinator =
  make(<and-combinator>,
    name: "&",
    function: logand);

define class <left-shift-combinator> (<binary-expression-combinator>)
end class;

define constant $left-shift-combinator =
  make(<left-shift-combinator>,
    name: "<<",
    function: ash);

define class <right-shift-combinator> (<binary-expression-combinator>)
end class;

define constant $right-shift-combinator =
  make(<right-shift-combinator>,
    name: ">>",
    function: method (n, s) ash(n, - s) end method);

define class <unary-plus-combinator> (<unary-expression-combinator>)
end class;

define constant $unary-plus-combinator =
  make(<unary-plus-combinator>,
    name: "+",
    function: identity);

define class <unary-minus-combinator> (<unary-expression-combinator>)
end class;

define constant $unary-minus-combinator =
  make(<unary-minus-combinator>,
    name: "-",
    function: negative);

define class <bitwise-negation-combinator> (<unary-expression-combinator>)
end class;

define constant $bitwise-negation-combinator =
  make(<bitwise-negation-combinator>,
    name: "~",
    function: lognot);

define class <no-combinator> (<expression-combinator>)
end class;

define constant $no-combinator =
  make(<no-combinator>);

define class <symbol-combinator> (<expression-combinator>)
end class;

define constant $symbol-combinator =
  make(<symbol-combinator>);

define class <evaluation-kind> (<object>)
end class;

define class <constant-evaluation-kind> (<evaluation-kind>)
end class;

define constant $constant-evaluation-kind =
  make(<constant-evaluation-kind>);

define class <positive-integer-evaluation-kind> (<evaluation-kind>)
end class;

//define constant $positive-integer-evaluation-kind =
//  make(<positive-integer-evaluation-kind>);

// this is trying to do a lot of jobs
// perhaps it should be broken down?
define class <ast-expression> (<object>)
//  slot expression-scope :: <scope>, init-keyword: scope:;
//  slot expression-line :: <integer>, init-keyword: line:;
//  slot expression-filename :: false-or(<string>);
  slot expression-combinator :: <expression-combinator> = $symbol-combinator, init-keyword: combinator:;
  slot expression-value = #"none", init-keyword: value:;
  slot expression-type :: false-or(<idl-type>) = #f, init-keyword: type:;
  constant slot left-subexpression :: false-or(<ast-expression>) = #f, init-keyword: left-subexpression:;
  constant slot right-subexpression :: false-or(<ast-expression>) = #f, init-keyword: right-subexpression:;
  slot expression-scoped-name :: false-or(<ast-scoped-name>) = #f, init-keyword: scoped-name:;
end class;

define method initialize (ast-expression :: <ast-expression>, #key type, expression)
  next-method();
  if (ast-expression.expression-value ~= #"none")
    ast-expression.expression-type :=
      convert-value-to-idl-type(ast-expression.expression-value);
  end if;
  if (type)
    ast-expression.expression-combinator := $no-combinator;
    ast-expression.expression-value := coerce(expression, type);
    ast-expression.expression-type := type;
  end if;
//  ast-expression.expression-scope := *scopes*.first;
//  ast-expression.expression-line := *line-number*;
//  ast-expression.expression-filename := *filename*;
end method;

/* --- Not currently used
define method expression-local-name (expression :: <ast-expression>)
  if (~ expression.expression-scoped-name.empty?)
    expression.expression-scoped-name.last;
  end if;
end method;
*/

define method full-definition (expression :: <ast-expression>)
  expression;
end method;

// CONVERT-VALUE-TO-IDL-TYPE

define method convert-value-to-idl-type (value :: <integer>)
  $long-idl-type;
end method;

define method convert-value-to-idl-type (value :: <single-float>)
  $float-idl-type;
end method;

define method convert-value-to-idl-type (value :: <double-float>)
  $double-idl-type;
end method;

define method convert-value-to-idl-type (value :: <boolean>)
  $boolean-idl-type;
end method;

define method convert-value-to-idl-type (value :: <string>)
  $string-idl-type;
end method;

define method convert-value-to-idl-type (value :: <character>)
  $char-idl-type;
end method;

// COERCE

define method coerce (expression :: <ast-expression>, type :: <idl-type>)
  if (expression.expression-type == type)
    expression.expression-value;
  else
    expression.expression-value := evaluate-internal(expression, $constant-evaluation-kind);
    expression.expression-value := coerce-value(expression, expression.expression-type, type);
  end if;
end method;

// COERCE-VALUE

define method coerce-value (ast-expression :: <ast-expression>,
                            from-type :: <idl-type>,
                            to-type :: <idl-type>)
  case
    (from-type == to-type) =>
      ast-expression.expression-value;
    (coercable-value?(ast-expression, to-type, to-type)) =>
      do-coerce-value(ast-expression, from-type, to-type);
    otherwise =>
      error(make(<idl-coercion-failure>, value: ast-expression, type: to-type));
  end case;
end method;

// COERCABLE-VALUE?

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <idl-type>,
                                to-type :: <idl-type>)
  from-type == to-type;
end method;                                

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <real-idl-type>,
                                to-type :: <real-idl-type>)
  (to-type.range-min <= ast-expression.expression-value) &
  (to-type.range-max >= ast-expression.expression-value)
end method;

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <char-idl-type>,
                                to-type :: <real-idl-type>)
  #t;
end;

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <real-idl-type>,
                                to-type :: <char-idl-type>)
  #t;
end;

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <boolean-idl-type>,
                                to-type :: <real-idl-type>)
  #t;
end;

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <real-idl-type>,
                                to-type :: <boolean-idl-type>)
  #t;
end;

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <char-idl-type>,
                                to-type :: <boolean-idl-type>)
  #t;
end;

define method coercable-value? (ast-expression :: <ast-expression>,
                                from-type :: <boolean-idl-type>,
                                to-type :: <char-idl-type>)
  #t;
end;

// DO-COERCE-VALUE

define method do-coerce-value (ast-expression :: <ast-expression>,
                               from-type :: <idl-type>,
                               to-type :: <idl-type>)
  as(to-type.dylan-type, ast-expression.expression-value);
end method;

define method do-coerce-value (ast-expression :: <ast-expression>,
                              from-type :: <boolean-idl-type>,
                              to-type :: <real-idl-type>)
  select (ast-expression.expression-value)
    #f => 0;
    #t  => 1;
  end select;
end method;
    
define method do-coerce-value (ast-expression :: <ast-expression>,
                                from-type :: <real-idl-type>,
                                to-type :: <boolean-idl-type>)
  select (ast-expression.expression-value)
    0 => #f;
    otherwise => #t;
  end select;
end method;

define method do-coerce-value (ast-expression :: <ast-expression>,
                               from-type :: <char-idl-type>,
                               to-type :: <boolean-idl-type>)
  select (as(<integer>, ast-expression.expression-value))
    0 => #f;
    otherwise => #t;
  end select;
end method;

define method do-coerce-value (ast-expression :: <ast-expression>,
                               from-type :: <boolean-idl-type>,
                               to-type :: <char-idl-type>)
  select (as(<integer>, ast-expression.expression-value))
    #f => as(<character>, 0);
    #t => as(<character>, 1);
  end select;
end method;

define method do-coerce-value (ast-expression :: <ast-expression>,
                               from-type :: <real-idl-type>,
                               to-type :: <char-idl-type>)
  as(<character>, as(<integer>, ast-expression.expression-value));
end method;

// EVAL

define method eval (expression :: <ast-expression>, kind :: <evaluation-kind>)
  evaluate-wrt-kind(expression, evaluate-internal(expression, kind), kind);
end method;

// EVALUATE

define method evaluate (expression :: <ast-expression>, kind :: <evaluation-kind>)
  expression.expression-value := eval(expression, kind);
end method;

// EVALUATE-WRT-KIND

define method evaluate-wrt-kind (expression :: <ast-expression>, value, kind :: <constant-evaluation-kind>)
  value;
end method;
    
define method evaluate-wrt-kind (expression :: <ast-expression>, value, kind :: <positive-integer-evaluation-kind>)
  coerce-value(expression, value, $ulong-idl-type);
end method;

// EVALUATE-COMBINATOR

define method evaluate-combinator (expression :: <ast-expression>,
                                   combinator :: <binary-expression-combinator>,
                                   kind :: <evaluation-kind>)
  expression.left-subexpression.expression-value :=
      evaluate-internal(expression.left-subexpression, kind);
  expression.right-subexpression.expression-value :=
      evaluate-internal(expression.right-subexpression, kind);
  let handler <condition> = method (condition, next-handler) error(make(<idl-evaluation-error>, declarator: expression)) end;
  (combinator.dylan-function)(expression.left-subexpression.expression-value,
                              expression.right-subexpression.expression-value);
end method;

define method evaluate-combinator (expression :: <ast-expression>,
                                   combinator :: <unary-expression-combinator>,
                                   kind :: <evaluation-kind>)

  expression.right-subexpression.expression-value :=
    evaluate-internal(expression.right-subexpression, kind);
  (combinator.dylan-function)(expression.right-subexpression.expression-value);
end method;

define method evaluate-combinator (expression :: <ast-expression>,
                                   combinator :: <symbol-combinator>,
                                   kind :: <evaluation-kind>)
  unless (expression.expression-scoped-name)
    error(make(<idl-evaluation-error>, declarator: expression));
  end unless;

  let declarator = resolve-scoped-name(first(scepter-scopes(get-scepter())), expression.expression-scoped-name, reference?: #t, error?: #t);

  unless (instance?(declarator, <ast-constant>))
    error(make(<idl-constant-expected>, constant: expression.expression-scoped-name, declarator: declarator));
  end unless;

  expression.expression-scoped-name := declarator.declarator-scoped-name;
  evaluate-internal(declarator.constant-value, kind);
end method;

// EVALUATE-INTERNAL

define method evaluate-internal (expression :: <ast-expression>, kind :: <evaluation-kind>)
  let value = expression.expression-value;
  if (value ~= #"none")
    evaluate-wrt-kind(expression, value, kind);
  else
    let value = evaluate-combinator(expression, expression.expression-combinator, kind);
    expression.expression-value := value;
    expression.expression-type := convert-value-to-idl-type(value);
    evaluate-wrt-kind(expression, value, kind);
  end if;
end method;
