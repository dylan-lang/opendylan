Module: java-parser
Author: Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define grammar-sequence <identifiers> (<identifier>);

define class <qualified-name> (<name>)
  constant slot name-identifiers :: <identifiers>,
    required-init-keyword: names:;
end;

define function qualified-name (list :: <pair>) => (name :: <name>)
  if (list.tail == #())
    list.head
  else
    make(<qualified-name>, names: rev-identifiers(list))
  end;
end;

define class <reference-type> (<type-descriptor>)
  constant slot type-name :: type-union(<primitive-type>, <name>),
    required-init-keyword: name:;
  constant slot type-numdims :: <integer>,
    required-init-keyword: numdims:;
end;

define class <binary-expression> (<expression>)
  constant slot expression-operator :: <operator>,
    required-init-keyword: op:;
  constant slot expression-argument-1 :: <expression>,
    required-init-keyword: value1:;
  constant slot expression-argument-2 :: <expression>,
    required-init-keyword: value2:;
end;

define class <assignment> (<binary-expression>, <statement-expression>) end;

define class <unary-expression> (<expression>)
  constant slot expression-operator :: <operator>,
    required-init-keyword: op:;
  constant slot expression-argument :: <expression>,
    required-init-keyword: value:;
end;

define class <post-expression> (<unary-expression>, <statement-expression>) end;
define class <pre-expression> (<unary-expression>, <statement-expression>) end;

define class <cast-expression> (<expression>)
  constant slot cast-type :: <type-descriptor>,
    required-init-keyword: type:;
  constant slot cast-value :: <expression>,
    required-init-keyword: value:;
end;

define class <instanceof-expression> (<expression>)
  constant slot instanceof-type :: <type-descriptor>,
    required-init-keyword: type:;
  constant slot instanceof-value :: <expression>,
    required-init-keyword: value:;
end;

define class <if-expression> (<expression>)
  constant slot if-expression-condition :: <expression>,
    required-init-keyword: condition:;
  constant slot if-expression-true-value :: <expression>,
    required-init-keyword: true:;
  constant slot if-expression-false-value :: <expression>,
    required-init-keyword: false:;
end;


define class <new-array-expression> (<expression>)
  constant slot new-array-type :: <reference-type>,
    required-init-keyword: type:;
  constant slot new-array-dims :: <expressions>,
    required-init-keyword: dims:;
end;

define class <new-class-expression> (<expression>, <statement-expression>)
  constant slot new-class-type :: <name>,
    required-init-keyword: type:;
  constant slot new-class-args :: <expressions>,
    required-init-keyword: args:;
end;

define class <field-access> (<expression>)
  constant slot field-access-value :: <expression>,
    required-init-keyword: value:;
  constant slot field-access-field :: <identifier>,
    required-init-keyword: field:;
end;


define class <array-access> (<expression>)
  constant slot array-access-value :: <expression>,
    required-init-keyword: value:;
  constant slot array-access-index :: <expression>,
    required-init-keyword: index:;
end;

define abstract class <method-call> (<expression>, <statement-expression>)
  constant slot method-call-args :: <expressions>,
    required-init-keyword: args:;
  constant slot method-call-class :: false-or(<expression>),
    required-init-keyword: class:;
end;

define class <named-method-call> (<method-call>)
  constant slot method-call-name :: <identifier>,
    required-init-keyword: name:;
end;

define class <constructor-call> (<method-call>) end;
define method method-call-name (c :: <constructor-call>) #f end;
