Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// IDENTIFIER

define constant corba/<Identifier> = corba/<string>;

//---*** clashes with one on corba/<string>
//define method class-typecode (class == corba/<Identifier>)
// => (typecode :: <typecode>)
//  corba/$Identifier-typecode
//end method;

/// PROTOCOL

define constant CORBA/<RepositoryId> = CORBA/<string>;

define constant CORBA/<TCKind> = one-of(#"tk-null", #"tk-void", #"tk-short", #"tk-long", #"tk-ushort", #"tk-ulong", #"tk-float", #"tk-double", #"tk-boolean", #"tk-char", #"tk-octet", #"tk-any", #"tk-TypeCode", #"tk-Principal", #"tk-objref", #"tk-struct", #"tk-union", #"tk-enum", #"tk-string", #"tk-sequence", #"tk-array", #"tk-alias", #"tk-except", #"tk-longlong", #"tk-ulonglong", #"tk-longdouble", #"tk-wchar", #"tk-wstring", #"tk-fixed");

define generic CORBA/TCKind/successor (symbol :: CORBA/<TCKind>)
 => (succ :: CORBA/<TCKind>);

define generic CORBA/TCKind/predecessor (symbol :: CORBA/<TCKind>)
 => (pred :: CORBA/<TCKind>);

define generic CORBA/TCKind/< (lesser :: CORBA/<TCKind>, greater :: CORBA/<TCKind>)
 => (lesser? :: <boolean>);

define generic CORBA/TCKind/> (greater :: CORBA/<TCKind>, lesser :: CORBA/<TCKind>)
 => (greater? :: <boolean>);

define method class-typecode (class == CORBA/<TCKind>)
 => (typecode :: <enum-typecode>)
  CORBA/$TCKind-typecode;
end method;

define method object-typecode (object :: CORBA/<TCKind>)
 => (typecode :: <enum-typecode>)
  CORBA/$TCKind-typecode;
end method;

define method class-typecode (class :: subclass(CORBA/<TypeCode>))
 => (typecode :: <typecode-typecode>)
  corba/$TypeCode-typecode;
end method;

define method object-typecode (object :: CORBA/<TypeCode>)
 => (typecode :: <typecode-typecode>)
  corba/$TypeCode-typecode;
end method;

define inline-only function CORBA/TCKind/as-symbol (integer :: <integer>)
 => (s :: <symbol>)
  let typecode = class-typecode(CORBA/<TCKind>);
  typecode-members(typecode)[integer];
end function;

define inline-only function CORBA/TCKind/as-integer (symbol :: <symbol>)
 => (i :: <integer>)
  let typecode = class-typecode(CORBA/<TCKind>);
  typecode-symbol-index(typecode)[symbol];
end function;

define method CORBA/TCKind/successor (symbol :: CORBA/<TCKind>)
 => (succ :: CORBA/<TCKind>)
  let i = CORBA/TCKind/as-integer(symbol);
  CORBA/TCKind/as-symbol(modulo(i + 1, 29));
end method;

define method CORBA/TCKind/predecessor (symbol :: CORBA/<TCKind>)
 => (pred :: CORBA/<TCKind>)
  let i = CORBA/TCKind/as-integer(symbol);
  CORBA/TCKind/as-symbol(modulo(29 + i - 1, 29));
end method;

define method CORBA/TCKind/< (lesser :: CORBA/<TCKind>, greater :: CORBA/<TCKind>)
 => (lesser? :: <boolean>)
  CORBA/TCKind/as-integer(lesser) < CORBA/TCKind/as-integer(greater);
end method;

define method CORBA/TCKind/> (greater :: CORBA/<TCKind>, lesser :: CORBA/<TCKind>)
 => (greater? :: <boolean>)
  CORBA/TCKind/as-integer(greater) > CORBA/TCKind/as-integer(lesser);
end method;

define open abstract class CORBA/<TypeCode> (<object>)
end class;

define sealed class CORBA/TypeCode/<Bounds> (CORBA/<user-exception>)
end class;
define sealed domain make (singleton(CORBA/TypeCode/<Bounds>));
define sealed domain initialize (CORBA/TypeCode/<Bounds>);

define method class-typecode (class == CORBA/TypeCode/<Bounds>)
 => (typecode :: <struct-typecode>)
  CORBA/TypeCode/$Bounds-typecode;
end method;

define method object-typecode (object :: CORBA/TypeCode/<Bounds>)
 => (typecode :: <struct-typecode>)
  CORBA/TypeCode/$Bounds-typecode;
end method;

define sealed class CORBA/TypeCode/<BadKind> (CORBA/<user-exception>)
end class;
define sealed domain make (singleton(CORBA/TypeCode/<BadKind>));
define sealed domain initialize (CORBA/TypeCode/<BadKind>);

define method class-typecode (class == CORBA/TypeCode/<BadKind>)
 => (typecode :: <struct-typecode>)
  CORBA/TypeCode/$BadKind-typecode;
end method;

define method object-typecode (object :: CORBA/TypeCode/<BadKind>)
 => (typecode :: <struct-typecode>)
  CORBA/TypeCode/$BadKind-typecode;
end method;

define open generic CORBA/TypeCode/equal (object :: CORBA/<TypeCode>, tc :: CORBA/<TypeCode>)
 => (result :: CORBA/<boolean>);

define open generic CORBA/TypeCode/kind (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<TCKind>);

define open generic CORBA/TypeCode/id (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<RepositoryId>);

define open generic CORBA/TypeCode/name (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<Identifier>);

define open generic CORBA/TypeCode/member-count (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<unsigned-long>);

define open generic CORBA/TypeCode/member-name (object :: CORBA/<TypeCode>, index :: CORBA/<unsigned-long>)
 => (result :: CORBA/<Identifier>);

define open generic CORBA/TypeCode/member-type (object :: CORBA/<TypeCode>, index :: CORBA/<unsigned-long>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/TypeCode/member-label (object :: CORBA/<TypeCode>, index :: CORBA/<unsigned-long>)
 => (result :: CORBA/<any>);

define open generic CORBA/TypeCode/discriminator-type (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/TypeCode/default-index (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<long>);

define open generic CORBA/TypeCode/length (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<unsigned-long>);

define open generic CORBA/TypeCode/content-type (object :: CORBA/<TypeCode>)
 => (result :: CORBA/<TypeCode>);

/// ---*** could make public functions slots and then just need
/// extra functions to signal right type of errors -- except
/// we still need old function for backward compatibility for a while.

/// TYPECODES

define variable *kinds-to-typecode-classes* :: <table> = make(<table>);

define variable *repository-ids-to-typecodes* :: <table> = make(<table>);

/// ABSTRACT TYPECODES

define abstract class <typecode> (corba/<typecode>)
  constant each-subclass slot typecode-kind :: corba/<TCKind>, init-keyword: kind:;
  constant each-subclass slot typecode-code :: <integer>, init-keyword: code:;
  constant each-subclass slot typecode-alignment :: <integer>, init-keyword: alignment:;
  constant each-subclass slot typecode-anonymous-native-type :: false-or(<type>) = #f, init-keyword: anonymous-type:;
  slot typecode-native-type :: false-or(<type>) = #f, init-keyword: type:;
end class;

define sealed domain make (subclass(<typecode>));
define sealed domain initialize (<typecode>);

define abstract class <empty-typecode> (<typecode>)
end class;

define abstract class <simple-typecode> (<empty-typecode>)
end class;

define abstract class <complex-typecode> (<typecode>)
end class;

define abstract class <indexable-typecode> (<typecode>)
  slot typecode-max-length :: <integer> = 0, init-keyword: max-length:;
end class;

define abstract class <elemental-typecode> (<typecode>)
  slot typecode-element-typecode :: <typecode>, init-keyword: element-typecode:;
end class;

define abstract class <type-typecode> (<complex-typecode>)
  slot typecode-repository-id :: <string> = "", init-keyword: repository-id:;
  slot typecode-name :: <string>, init-keyword: name:;
end class;

define method initialize (typecode :: <type-typecode>, #key)
  next-method();
  intern-typecode(typecode);
end method;

define abstract class <membered-typecode> (<type-typecode>)
  slot typecode-members :: <sequence> = #[], init-keyword: members:; 
end class;

define method typecode-count (typecode :: <membered-typecode>)
  size(typecode-members(typecode));
end method;

/// EQUALITY

define method \= (typecode1 :: <complex-typecode>, typecode2 :: <complex-typecode>)
  => (equal? :: <boolean>)
  object-class(typecode1) = object-class(typecode2)
end method;

define method \= (typecode1 :: <type-typecode>, typecode2 :: <type-typecode>)
  => (equal? :: <boolean>)
  next-method()
    & (typecode-repository-id(typecode1) = typecode-repository-id(typecode2))
    & (typecode-name(typecode1) = typecode-name(typecode2))
end method;

define method \= (typecode1 :: <membered-typecode>, typecode2 :: <membered-typecode>)
  => (equal? :: <boolean>)
  every?(\=, typecode-members(typecode1), typecode-members(typecode2))
end method;

/// SINGLETON MANAGEMENT

define constant $typecode-singletons :: <table> = make(<table>);

define method typecode-singleton (class :: subclass(<typecode>))
 => (typecode :: false-or(<typecode>))
  element($typecode-singletons, class, default: #f)
end method;

define method typecode-singleton-setter (typecode :: <typecode>, class :: subclass(<typecode>))
 => (typecode :: <typecode>)
  element($typecode-singletons, class) := typecode;
end method;

define method make (class :: subclass(<empty-typecode>), #key)
 => (object :: <empty-typecode>)
  typecode-singleton(class)
    | ( typecode-singleton(class) := next-method() )
end method;

/// EMPTY TYPECODES

define class <null-typecode> (<empty-typecode>)
  keyword kind: = #"tk-null";
  keyword code: = 0;
  keyword type: = corba/<null>;
end class;

define constant corba/$null-typecode = make(<null-typecode>);
element(*kinds-to-typecode-classes*, 0) := <null-typecode>;

define class <void-typecode> (<empty-typecode>)
  keyword kind: = #"tk-void";
  keyword code: = 1;
  keyword alignment: = 2;
  keyword type: = corba/<void>;
end class;

define constant corba/$void-typecode = make(<void-typecode>);
element(*kinds-to-typecode-classes*, 1) := <void-typecode>;

define class <short-typecode> (<empty-typecode>)
  keyword kind: = #"tk-short";
  keyword code: = 2;
  keyword alignment: = 2;
  keyword type: = corba/<short>;
end class;

define constant corba/$short-typecode = make(<short-typecode>);
element(*kinds-to-typecode-classes*, 2) := <short-typecode>;

define class <long-typecode> (<empty-typecode>)
  keyword kind: = #"tk-long";
  keyword code: = 3;
  keyword alignment: = 4;
  keyword type: = corba/<long>;
end class;

define constant corba/$long-typecode = make(<long-typecode>);
element(*kinds-to-typecode-classes*, 3) := <long-typecode>;

define class <unsigned-short-typecode> (<empty-typecode>)
  keyword kind: = #"tk-ushort";
  keyword code: = 4;
  keyword alignment: = 2;
  keyword type: = corba/<unsigned-short>;
end class;

define constant corba/$unsigned-short-typecode = make(<unsigned-short-typecode>);
element(*kinds-to-typecode-classes*, 4) := <unsigned-short-typecode>;

define class <unsigned-long-typecode> (<empty-typecode>)
  keyword kind: = #"tk-ulong";
  keyword code: = 5;
  keyword alignment: = 4;
  keyword type: = corba/<unsigned-long>;
end class;

define constant corba/$unsigned-long-typecode = make(<unsigned-long-typecode>);
element(*kinds-to-typecode-classes*, 5) := <unsigned-long-typecode>;

define class <float-typecode> (<empty-typecode>)
  keyword kind: = #"tk-float";
  keyword code: = 6;
  keyword alignment: = 4;
  keyword type: = corba/<float>;
end class;

define constant corba/$float-typecode = make(<float-typecode>);
element(*kinds-to-typecode-classes*, 6) := <float-typecode>;

define class <double-typecode> (<empty-typecode>)
  keyword kind: = #"tk-double";
  keyword code: = 7;
  keyword alignment: = 8;
  keyword type: = corba/<double>;
end class;

define constant corba/$double-typecode = make(<double-typecode>);
element(*kinds-to-typecode-classes*, 7) := <double-typecode>;

define class <boolean-typecode> (<empty-typecode>)
  keyword kind: = #"tk-boolean";
  keyword code: = 8;
  keyword alignment: = 1;
  keyword type: = corba/<boolean>;
end class;

define constant corba/$boolean-typecode = make(<boolean-typecode>);
element(*kinds-to-typecode-classes*, 8) := <boolean-typecode>;

define class <char-typecode> (<empty-typecode>)
  keyword kind: = #"tk-char";
  keyword code: = 9;
  keyword alignment: = 1;
  keyword type: = corba/<char>;
end class;

define constant corba/$char-typecode = make(<char-typecode>);
element(*kinds-to-typecode-classes*, 9) := <char-typecode>;

define class <octet-typecode> (<empty-typecode>)
  keyword kind: = #"tk-octet";
  keyword code: = 10;
  keyword alignment: = 1;
  keyword type: = corba/<octet>;
end class;

define constant corba/$octet-typecode = make(<octet-typecode>);
element(*kinds-to-typecode-classes*, 10) := <octet-typecode>;

define class <any-typecode> (<empty-typecode>)
  keyword kind: = #"tk-any";
  keyword code: = 11;
  keyword alignment: = 1;
  keyword type: = corba/<any>;
end class;

define constant corba/$any-typecode = make(<any-typecode>);
element(*kinds-to-typecode-classes*, 11) := <any-typecode>;

define class <typecode-typecode> (<empty-typecode>)
  keyword kind: = #"tk-typecode";
  keyword code: = 12;
  keyword alignment: = 4;
  keyword type: = <typecode>;
end class;

define constant corba/$typecode-typecode = make(<typecode-typecode>);
element(*kinds-to-typecode-classes*, 12) := <typecode-typecode>;

define class <principal-typecode> (<empty-typecode>)
  keyword kind: = #"tk-principal";
  keyword code: = 13;
end class;

define constant corba/$principal-typecode = make(<principal-typecode>);
element(*kinds-to-typecode-classes*, 13) := <principal-typecode>;

/// COMPLEX TYPECODES

define class <object-reference-typecode> (<type-typecode>)
  keyword kind: = #"tk-objref";
  keyword code: = 14;
  keyword alignment: = 4;
  keyword type: = corba/<object>;
end class;

element(*kinds-to-typecode-classes*, 14) := <object-reference-typecode>;

define class <struct-typecode> (<membered-typecode>)
  keyword kind: = #"tk-struct";
  keyword code: = 15;
  keyword alignment: = 1;
  keyword anonymous-type: = <anonymous-struct>;
end class;

element(*kinds-to-typecode-classes*, 15) := <struct-typecode>;

define class <typecode-member> (<object>)
  slot typecode-member-name :: <string>, init-keyword: name:;
  slot typecode-member-typecode :: <typecode>, init-keyword: typecode:;
  slot typecode-member-native-type :: false-or(<type>), init-keyword: native-type:;
  slot typecode-member-getter :: <function>, init-keyword: getter:;
  slot typecode-member-setter :: <function>, init-keyword: setter:;
  slot typecode-member-init-keyword :: <symbol>, init-keyword: init-keyword:;
end class;

define method \= (member1 :: <typecode-member>, member2 :: <typecode-member>)
  => (equal? :: <boolean>)
  (typecode-member-name(member1) = typecode-member-name(member2))
    & (typecode-member-typecode(member1) = typecode-member-typecode(member2))
end method;

define class <union-typecode> (<membered-typecode>)
  keyword kind: = #"tk-union";
  keyword code: = 16;
  keyword alignment: = 1;
  keyword anonymous-type: = <anonymous-union>;
  slot typecode-discriminator-typecode :: <typecode>, init-keyword: discriminator:;
  slot typecode-default-used :: <integer>, init-keyword: default-used:;
end class;

define method \= (union1 :: <union-typecode>, union2 :: <union-typecode>)
  => (equal? :: <boolean>)
  next-method()
    & (typecode-discriminator-typecode(union1) = typecode-discriminator-typecode(union2))
    & (typecode-default-used(union1) = typecode-default-used(union2))
end method;

element(*kinds-to-typecode-classes*, 16) := <union-typecode>;

define class <typecode-branch> (<typecode-member>)
  slot typecode-label-value, init-keyword: label-value:;
  slot typecode-branch-tags, init-keyword: tags:;
end class;

define method \= (branch1 :: <typecode-branch>, branch2 :: <typecode-branch>)
  => (equal? :: <boolean>)
  next-method()
    & (typecode-label-value(branch1) = typecode-label-value(branch2))
end method;

define class <enum-typecode> (<membered-typecode>)
  keyword kind: = #"tk-enum";
  keyword code: = 17;
  keyword alignment: = 4;
  slot typecode-symbol-index :: <table>;
end class;

define method typecode-generate-symbol-index (typecode :: <enum-typecode>)
  let table = make(<table>);
  let symbols = typecode-members(typecode);
  for (i :: <integer> from 0 below size(symbols))
    table[symbols[i]] := i;
  end for;
  typecode.typecode-symbol-index := table;
end method;

define method typecode-members-setter (members :: <sequence>, typecode :: <enum-typecode>)
 => (members :: <sequence>)
  next-method();
  typecode-generate-symbol-index(typecode);
  members;
end method;

define method typecode-member-name (member :: <symbol>)
 => (name :: <string>)
  as(<string>, member);
end method;

define method initialize (typecode :: <enum-typecode>, #key)
  next-method();
  typecode-generate-symbol-index(typecode);
end method;

element(*kinds-to-typecode-classes*, 17) := <enum-typecode>;

define class <string-typecode> (<simple-typecode>, <indexable-typecode>)
  keyword kind: = #"tk-string";
  keyword code: = 18;
  keyword alignment: = 4;
  keyword type: = corba/<string>;
end class;

define method \= (string1 :: <string-typecode>, string2 :: <string-typecode>)
  => (equal? :: <boolean>)
  next-method()
    & (typecode-max-length(string1) = typecode-max-length(string2))
end method;

element(*kinds-to-typecode-classes*, 18) := <string-typecode>;

define method make (class :: subclass(<string-typecode>), #key max-length = unsupplied())
 => (object :: <string-typecode>)
  if (unsupplied?(max-length))
    typecode-singleton(class)
      | ( typecode-singleton(class) := next-method() )
  else
    next-method()
  end if;
end method;

define constant corba/$string-typecode = make(<string-typecode>);

define class <sequence-typecode> (<complex-typecode>, <indexable-typecode>, <elemental-typecode>)
  keyword kind: = #"tk-sequence";
  keyword code: = 19;
  keyword alignment: = 4;
  keyword type: = corba/<sequence>;
end class;

define method \= (sequence1 :: <sequence-typecode>, sequence2 :: <sequence-typecode>)
  => (equal? :: <boolean>)
  next-method()
    & (typecode-element-typecode(sequence1) = typecode-element-typecode(sequence2))
    & (typecode-max-length(sequence1) = typecode-max-length(sequence2))
end method;

element(*kinds-to-typecode-classes*, 19) := <sequence-typecode>;

define class <array-typecode> (<complex-typecode>, <indexable-typecode>, <elemental-typecode>)
  keyword kind: = #"tk-array";
  keyword code: = 20;
  keyword alignment: = 1;
  keyword type: = corba/<array>;
end class;

// ---*** backward compatibility
define method typecode-length (object :: <array-typecode>)
 => (result :: <integer>)
  typecode-max-length(object)
end method;

// ---*** backward compatibility
define method typecode-length-setter (new-value :: <integer>, object :: <array-typecode>)
 => (result :: <integer>)
  typecode-max-length(object) := new-value
end method;

// ---*** backward compatibility
define method make (class == <array-typecode>, #rest initargs, #key length = unsupplied())
 => (object :: <array-typecode>)
  if (unsupplied?(length))
    next-method()
  else
    apply(next-method, class, max-length: length, initargs)
  end if;
end method;

define method \= (array1 :: <array-typecode>, array2 :: <array-typecode>)
  => (equal? :: <boolean>)
  next-method()
    & (typecode-element-typecode(array1) = typecode-element-typecode(array2))
    & (typecode-max-length(array1) = typecode-max-length(array2))
end method;

element(*kinds-to-typecode-classes*, 20) := <array-typecode>;

define class <alias-typecode> (<type-typecode>)
  keyword kind: = #"tk-alias";
  keyword code: = 21;
  keyword type: = <typecode>;
  slot typecode-aliased :: <typecode>, init-keyword: aliased:;
end class;

define method \= (alias1 :: <alias-typecode>, alias2 :: <alias-typecode>)
  => (equal? :: <boolean>)
  next-method()
    & (typecode-aliased(alias1) = typecode-aliased(alias2))
end method;

element(*kinds-to-typecode-classes*, 21) := <alias-typecode>;

define class <exception-typecode> (<struct-typecode>)
  keyword kind: = #"tk-except";
  keyword code: = 22;
  keyword anonymous-type: = <anonymous-exception>;
end class;

element(*kinds-to-typecode-classes*, 22) := <exception-typecode>;

define class <indirection-typecode> (<simple-typecode>)
  //  keyword kind: = #"tk-ind";
  keyword code: = #xffffffff;
  keyword alignment: = 1;
  keyword type: = <typecode>;
  slot typecode-offset :: <integer>, init-keyword: offset:;
  slot typecode-nesting :: <integer>, init-keyword: nesting:;
end class;

element(*kinds-to-typecode-classes*, #xffffffff) := <indirection-typecode>;

define open generic object-typecode (object :: <object>)
 => (typecode :: <typecode>);

define open generic class-typecode (object :: <type>)
 => (typecode :: <typecode>);

/// INTERNING

define method intern-typecode (typecode :: <typecode>)
  => (typecode :: <typecode>)
  typecode
end method;

define method intern-typecode (typecode :: <type-typecode>)
  => (typecode :: <type-typecode>)
  let repository-id = typecode-repository-id(typecode);
  let existing-typecode = interned-typecode(repository-id);
  if (existing-typecode)
    // ---*** could check and warn about incompatible typecode structures
    existing-typecode
  else
    element(*repository-ids-to-typecodes*, as(<symbol>, typecode-repository-id(typecode))) := typecode;
    typecode
  end if;
end method;

define method interned-typecode (repository-id :: <string>)
  => (typecode :: false-or(<type-typecode>))
  element(*repository-ids-to-typecodes*, as(<symbol>, repository-id), default: #f)
end method;

/// COERCION

define method as (class == <typecode>, kind :: <integer>)
 => (typecode :: <typecode>)
  let typecode-class = element(*kinds-to-typecode-classes*, kind); // NB signal error if not in table match
  make(typecode-class);
end method;

define method as (class == <typecode>, repository-id :: <string>)
  => (typecode :: false-or(<typecode>))
  interned-typecode(repository-id);
end method;

define method as (class == <typecode>, typecode :: <typecode>)
  => (typecode :: <typecode>)
  intern-typecode(typecode)
end method;

/// OFFICIAL OPERATIONS

define method CORBA/TypeCode/equal (object :: <TypeCode>, tc :: <TypeCode>)
 => (result :: CORBA/<boolean>)
  object = tc
end method;

define method CORBA/TypeCode/kind (object :: <typecode>)
 => (result :: CORBA/<TCKind>)
  typecode-kind(object)
end method;

define method CORBA/TypeCode/id (object :: <typecode>)
 => (result :: CORBA/<RepositoryId>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/id (object :: <type-typecode>)
 => (result :: CORBA/<RepositoryId>)
  typecode-repository-id(object)
end method;

define method CORBA/TypeCode/name (object :: <typecode>)
 => (result :: CORBA/<Identifier>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/name (object :: <type-typecode>)
 => (result :: CORBA/<Identifier>)
  typecode-name(object)
end method;

define method CORBA/TypeCode/member-count (object :: <typecode>)
 => (result :: CORBA/<unsigned-long>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/member-count (object :: <membered-typecode>)
 => (result :: CORBA/<unsigned-long>)
  typecode-count(object)
end method;

define method CORBA/TypeCode/member-name (object :: <typecode>, index :: CORBA/<unsigned-long>)
 => (result :: CORBA/<Identifier>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/member-name (object :: <membered-typecode>, index :: CORBA/<unsigned-long>)
 => (result :: CORBA/<Identifier>)
  if ((index < 0) | (index >= typecode-count(object)))
    error(make(corba/typecode/<bounds>))
  end if;
  typecode-member-name(typecode-members(object)[index])
end method;

define method CORBA/TypeCode/member-type (object :: <typecode>, index :: CORBA/<unsigned-long>)
 => (result :: <typecode>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/member-type (object :: <enum-typecode>, index :: CORBA/<unsigned-long>)
 => (result :: <typecode>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/member-type (object :: <membered-typecode>, index :: CORBA/<unsigned-long>)
 => (result :: <typecode>)
  if ((index < 0) | (index >= typecode-count(object)))
    error(make(corba/typecode/<bounds>))
  end if;
  typecode-member-typecode(typecode-members(object)[index])
end method;

define method CORBA/TypeCode/member-label (object :: <typecode>, index :: CORBA/<unsigned-long>)
 => (result :: CORBA/<any>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/member-label (object :: <union-typecode>, index :: CORBA/<unsigned-long>)
 => (result :: CORBA/<any>)
  if ((index < 0) | (index >= typecode-count(object)))
    error(make(corba/typecode/<bounds>))
  end if;
  as(corba/<any>, typecode-label-value(typecode-members(object)[index]))
end method;

define method CORBA/TypeCode/discriminator-type (object :: <typecode>)
 => (result :: <typecode>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/discriminator-type (object :: <union-typecode>)
 => (result :: <typecode>)
  typecode-discriminator-typecode(object)
end method;

define method CORBA/TypeCode/default-index (object :: <typecode>)
 => (result :: CORBA/<long>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/default-index (object :: <union-typecode>)
 => (result :: CORBA/<long>)
  typecode-default-used(object)
end method;

define method CORBA/TypeCode/length (object :: <typecode>)
 => (result :: CORBA/<unsigned-long>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/length (object :: <indexable-typecode>)
 => (result :: CORBA/<unsigned-long>)
  typecode-max-length(object)
end method;

define method CORBA/TypeCode/content-type (object :: <typecode>)
 => (result :: <typecode>)
  error(make(corba/typecode/<badkind>))
end method;

define method CORBA/TypeCode/content-type (object :: <elemental-typecode>)
 => (result :: <typecode>)
  typecode-element-typecode(object)
end method;

define method CORBA/TypeCode/content-type (object :: <alias-typecode>)
 => (result :: <typecode>)
  typecode-aliased(object)
end method;

/// workaround forward reference bug in 2.0a2

define constant CORBA/$TCKind-typecode = make(<enum-typecode>, type: CORBA/<TCKind>, name: "TCKind", repository-id: "IDL:omg.org/CORBA/TCKind:1.0", members: vector(#"tk-null", #"tk-void", #"tk-short", #"tk-long", #"tk-ushort", #"tk-ulong", #"tk-float", #"tk-double", #"tk-boolean", #"tk-char", #"tk-octet", #"tk-any", #"tk-TypeCode", #"tk-Principal", #"tk-objref", #"tk-struct", #"tk-union", #"tk-enum", #"tk-string", #"tk-sequence", #"tk-array", #"tk-alias", #"tk-except", #"tk-longlong", #"tk-ulonglong", #"tk-longdouble", #"tk-wchar", #"tk-wstring", #"tk-fixed"));

define constant CORBA/TypeCode/$Bounds-typecode = make(<exception-typecode>, type: CORBA/TypeCode/<Bounds>, name: "Bounds", repository-id: "IDL:omg.org/CORBA/TypeCode/Bounds:1.0", members: vector());

define constant CORBA/TypeCode/$BadKind-typecode = make(<exception-typecode>, type: CORBA/TypeCode/<BadKind>, name: "BadKind", repository-id: "IDL:omg.org/CORBA/TypeCode/BadKind:1.0", members: vector());

