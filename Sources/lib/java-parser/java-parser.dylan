Module: java-parser
Author: Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $public-modifier = 1;
define constant $protected-modifier = 2;
define constant $private-modifier = 4;
define constant $static-modifier = 8;
define constant $abstract-modifier = 16;
define constant $final-modifier = 32;
define constant $native-modifier = 64;
define constant $synchronized-modifier = 128;
define constant $transient-modifier = 256;
define constant $volatile-modifier = 512;
define constant <modifiers> = limited(<integer>, min: 0, max: 1023);

define abstract class <import> (<grammar-object>)
  constant slot import-name :: <name>,
    required-init-keyword: name:;
end;

define grammar-sequence <imports> (<import>);

define class <type-import> (<import>) end;
define class <package-import> (<import>) end;


define abstract class <declaration> (<grammar-object>)
  constant slot declaration-modifiers :: <modifiers>,
    required-init-keyword: modifiers:;
end;

define abstract class <body-declaration> (<declaration>) end;

define grammar-sequence <body-declarations> (<body-declaration>);

define generic declaration-body (decl :: <body-declaration>)
 => (body :: false-or(<block>));

define method declaration-body (decl :: <body-declaration>)
 => (body :: singleton(#f))
  #f
end;

define abstract class <type-declaration> (<declaration>)
  constant slot type-declaration-name :: <identifier>,
    required-init-keyword: name:;
  constant slot type-declaration-declarations :: <body-declarations>,
    required-init-keyword: body:;
end;

define grammar-sequence <type-declarations> (<type-declaration>);

define class <formal-parameter> (<grammar-object>)
  constant slot parameter-name :: <identifier>,
    required-init-keyword: name:;
  constant slot parameter-type :: <type-descriptor>,
    required-init-keyword: type:;
end;

define grammar-sequence <formal-parameters> (<formal-parameter>);

define class <variable-declarator> (<grammar-object>)
  constant slot variable-declarator-name :: <identifier>,
    required-init-keyword: name:;
  constant slot variable-declarator-numdims :: <integer>, // number of []'s
    required-init-keyword: numdims:;
  constant slot variable-declarator-init :: false-or(<variable-initializer>),
    required-init-keyword: init:;
end;

define grammar-sequence <variable-declarators> (<variable-declarator>);

define class <local-variable-declaration> (<block-statement>)
  constant slot local-variable-type :: <type-descriptor>,
    required-init-keyword: type:;
  constant slot local-variable-declarators :: <variable-declarators>,
    required-init-keyword: declarators:;
end;

define class <array-initializer> (<variable-initializer>)
  constant slot array-inits :: <variable-initializers>,
    required-init-keyword: inits:;
end;

define class <interface-declaration> (<type-declaration>)
  constant slot interface-extends :: <names>,
    required-init-keyword: extends:;
end;

define class <class-declaration> (<type-declaration>)
  constant slot class-super :: false-or(<name>),
    required-init-keyword: super:;
  constant slot class-interfaces :: <names>,
    required-init-keyword: interfaces:;
end;

define class <field-declaration> (<body-declaration>)
  constant slot field-type :: <type-descriptor>,
    required-init-keyword: type:;
  constant slot field-declarators :: <variable-declarators>,
    required-init-keyword: declarators:;
end;

define class <abstract-method-declaration> (<body-declaration>)
  // only false for <constructor-declaration>
  constant slot method-name :: false-or(<identifier>),
    required-init-keyword: name:;
  constant slot method-parameters :: <formal-parameters>,
    required-init-keyword: parameters:;
  constant slot method-throws :: <names>,
    required-init-keyword: throws:;
  // False means VOID.
  constant slot method-return-type :: false-or(<type-descriptor>),
    required-init-keyword: type:;
end;

define class <method-declaration> (<abstract-method-declaration>)
  constant slot declaration-body :: <block>,
    required-init-keyword: body:;
end;

define class <constructor-declaration> (<method-declaration>)
end;

define class <static-initializer> (<body-declaration>)
  constant slot declaration-body :: <block>,
    required-init-keyword: body:;
end;

define inline method make (c == <static-initializer>, #key body)
    => (object)
  next-method(c, modifiers: $static-modifier, body: body);
end;

define class <compilation-unit> (<grammar-object>)
  constant slot compilation-unit-package :: false-or(<name>),
    required-init-keyword: package:;
  constant slot compilation-unit-imports :: <imports>,
    required-init-keyword: imports:;
  constant slot compilation-unit-types :: <type-declarations>,
    required-init-keyword: types:;
end;

define constant <compilation-units>
    = limited(<vector>, of: <compilation-unit>);

define function modifiers-symbols (mask :: <integer>)
 => (modifiers)
  let modifiers = #();
  when (logand(mask, $public-modifier) ~== 0)
    modifiers := pair(#"public", modifiers);
  end;
  when (logand(mask, $protected-modifier) ~== 0)
    modifiers := pair(#"protected", modifiers);
  end;
  when (logand(mask, $private-modifier) ~== 0)
    modifiers := pair(#"private", modifiers);
  end;
  when (logand(mask, $static-modifier) ~== 0)
    modifiers := pair(#"static", modifiers);
  end;
  when (logand(mask, $abstract-modifier) ~== 0)
    modifiers := pair(#"abstract", modifiers);
  end;
  when (logand(mask, $final-modifier) ~== 0)
    modifiers := pair(#"final", modifiers);
  end;
  when (logand(mask, $native-modifier) ~== 0)
    modifiers := pair(#"native", modifiers);
  end;
  when (logand(mask, $synchronized-modifier) ~== 0)
    modifiers := pair(#"synchronized", modifiers);
  end;
  when (logand(mask, $transient-modifier) ~== 0)
    modifiers := pair(#"transient", modifiers);
  end;
  when (logand(mask, $volatile-modifier) ~== 0)
    modifiers := pair(#"volatile", modifiers);
  end;
  modifiers
end;

define function run-java-parser (source)
  let lexer = make-java-lexer(source);
  run-parser(#f, java-parser, lexer);
end;

