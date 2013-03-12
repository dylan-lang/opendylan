Module: java-parser
Author: Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <block> (<statement>)
  constant slot block-statements :: <block-statements>,
    required-init-keyword: statements:;
end;

define method as-block (statement :: <statement>) => (b :: <block>)
  make(<block>, statements: rev-block-statements(list(statement)))
end;

define method as-block (b :: <block>) => (b :: <block>)
  b
end;

define class <empty-statement> (<statement>) end;
define constant $empty-statement = make(<empty-statement>);

define class <for-statement> (<statement>)
  constant slot for-statement-init :: type-union(singleton(#f),
						 <statement-expressions>,
						 <statement-expression>,
						 <local-variable-declaration>),
    required-init-keyword: init:;
  constant slot for-statement-update :: type-union(singleton(#f),
						   <statement-expressions>,
						   <statement-expression>),
    required-init-keyword: update:;
  constant slot for-statement-condition :: false-or(<expression>),
    required-init-keyword: condition:;
  constant slot for-statement-body :: <block>,
    required-init-keyword: body:;
end;


define class <if-statement> (<statement>)
  constant slot if-statement-condition :: <expression>,
    required-init-keyword: condition:;
  constant slot if-statement-true-value :: <statement>,
    required-init-keyword: true:;
  constant slot if-statement-false-value :: false-or(<statement>),
    required-init-keyword: false:;
end;

define class <do-statement> (<statement>)
  constant slot do-statement-condition :: <expression>,
    required-init-keyword: condition:;
  constant slot do-statement-body :: <block>,
    required-init-keyword: body:;
end;

define class <while-statement> (<statement>)
  constant slot while-statement-condition :: <expression>,
    required-init-keyword: condition:;
  constant slot while-statement-body :: <block>,
    required-init-keyword: body:;
end;


define class <break-statement> (<statement>)
  constant slot break-statement-label :: false-or(<identifier>),
    required-init-keyword: label:;
end;

define class <continue-statement> (<statement>)
  constant slot continue-statement-label :: false-or(<identifier>),
    required-init-keyword: label:;
end;

define class <return-statement> (<statement>)
  constant slot return-statement-value :: false-or(<expression>),
    required-init-keyword: value:;
end;

define class <synchronized-statement> (<statement>)
  constant slot synchronized-statement-condition :: <expression>,
    required-init-keyword: condition:;
  constant slot synchronized-statement-body :: <block>,
    required-init-keyword: body:;
end;

define class <throw-statement> (<statement>)
  constant slot throw-statement-value :: <expression>,
    required-init-keyword: value:;
end;

define class <catch> (<grammar-object>)
  constant slot catch-parameter :: <formal-parameter>,
    required-init-keyword: parameter:;
  constant slot catch-body :: <block>,
    required-init-keyword: body:;
end;

define grammar-sequence <catches> (<catch>);

define class <try-statement> (<statement>)
  constant slot try-statement-body :: <block>,
    required-init-keyword: body:;
  constant slot try-statement-catches :: <catches>,
    required-init-keyword: catches:;
  constant slot try-statement-finally :: false-or(<block>),
    required-init-keyword: finally:;
end;  

// False means 'default' case.
//define constant <switch-label> = false-or(<expression>);
define grammar-sequence <switch-labels> (false-or(<expression>));

define class <switch-case> (<grammar-object>)
  constant slot switch-case-labels :: <switch-labels>,
    required-init-keyword: labels:;
  // only last switch case can have empty body...
  constant slot switch-case-body :: false-or(<block>),
    required-init-keyword: body:;
end;

define grammar-sequence <switch-cases> (<switch-case>);

define class <switch-statement> (<statement>)
  constant slot switch-statement-value :: <expression>,
    required-init-keyword: value:;
  constant slot switch-statement-cases :: <switch-cases>,
    required-init-keyword: cases:;
end;

define class <labeled-statement> (<statement>)
  constant slot labeled-statement-label :: <identifier>,
    required-init-keyword: label:;
  constant slot labeled-statement-statement :: <statement>,
    required-init-keyword: statement:;
end;


