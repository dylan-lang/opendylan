Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method largest-float (significand-bits :: <integer>, exponent-bits :: <integer>)
  (2.0d0 ^ (significand-bits + 1) - 1) * (2.0d0 ^ ((2 ^ (exponent-bits - 1) - 1) - significand-bits - 1));
end method;

define method largest-integer (number-of-bits :: <integer>, min-or-max, signed-or-unsigned)
  select (signed-or-unsigned)
    signed: =>
      select (min-or-max)
        max: => 2 ^ (number-of-bits - 1) - 1;
        min: => - 2 ^ (number-of-bits - 1);
      end select;
    unsigned: =>  
      select (min-or-max)
        max: => 2 ^ number-of-bits - 1;
        min: => 0;
      end select;
  end select;
end method;

define class <ast-type> (<ast-declarator>)
end class;

define class <ast-concrete-type> (<ast-type>)
end class;

define class <ast-predefined-type> (<ast-concrete-type>)
  constant slot predefined-type :: false-or(<idl-type>) = #f, init-keyword: type:;
end class;

define method can-be-redefined? (predefined-type :: <ast-predefined-type>)
  #f;
end method;

define class <idl-type> (<object>)
  constant slot dylan-type :: false-or(<class>) = #f, init-keyword: dylan-type:;
  constant slot idl-type-name :: false-or(<string>) = #f, init-keyword: name:;
end class;

define class <real-idl-type> (<idl-type>)
  constant slot range-min :: <number>, required-init-keyword: min:;
  constant slot range-max :: <number>, required-init-keyword: max:;
end class;

define class <short-idl-type> (<real-idl-type>)
end class;

define constant $short-idl-type =
  make(<short-idl-type>,
    name: "short",
    dylan-type: <integer>,
    min: largest-integer(16, min:, signed:),
    max: largest-integer(16, max:, signed:));

define class <ushort-idl-type> (<real-idl-type>)
end class;

define constant $ushort-idl-type =
  make(<ushort-idl-type>,
    name: "unsigned short",
    dylan-type: <integer>,
    min: largest-integer(16, min:, unsigned:),
    max: largest-integer(16, max:, unsigned:));

define class <long-idl-type> (<real-idl-type>)
end class;

define constant $long-idl-type =
  make(<long-idl-type>,
    name: "long",
    dylan-type: <integer>,
    min: largest-integer(32, min:, signed:),
    max: largest-integer(32, max:, signed:));

define class <ulong-idl-type> (<real-idl-type>)
end class;

define constant $ulong-idl-type =
  make(<ulong-idl-type>,
    name: "unsigned long",
    dylan-type: <integer>,
    min: largest-integer(32, min:, unsigned:),
    max: largest-integer(32, max:, unsigned:));

define class <longlong-idl-type> (<real-idl-type>)
end class;

define constant $longlong-idl-type =
  make(<longlong-idl-type>,
    name: "long long",
    dylan-type: <integer>,
    min: largest-integer(32, min:, signed:),  // ---*** should be 64
    max: largest-integer(32, max:, signed:)); // ---*** should be 64

define class <ulonglong-idl-type> (<real-idl-type>)
end class;

define constant $ulonglong-idl-type =
  make(<ulonglong-idl-type>,
    name: "unsigned long long",
    dylan-type: <integer>,
    min: largest-integer(32, min:, unsigned:), // should be 64
    max: largest-integer(32, max:, unsigned:)); // should be 64

define class <float-idl-type> (<real-idl-type>)
end class;

define constant $float-idl-type =
  make(<float-idl-type>,
    name: "float",
    dylan-type: <single-float>,
    min: - largest-float(23, 8),
    max: largest-float(23, 8));

define class <double-idl-type> (<real-idl-type>)
end class;

define constant $double-idl-type =
  make(<double-idl-type>,
    name: "double",
    dylan-type: <double-float>,
    min: - largest-float(52, 11),
    max: largest-float(52, 11));

define class <longdouble-idl-type> (<real-idl-type>)
end class;

define constant $longdouble-idl-type =
  make(<longdouble-idl-type>,
    name: "long double",
    dylan-type: <extended-float>,
    min: - largest-float(52, 11), // should be - largest-float(112, 15),
    max: largest-float(52, 11) //  should be largest-float(112, 15)
    );

define class <char-idl-type> (<idl-type>)
end class;

define constant $char-idl-type =
  make(<char-idl-type>,
    name: "char",
    dylan-type: <character>);

define class <wchar-idl-type> (<idl-type>)
end class;

define constant $wchar-idl-type =
  make(<wchar-idl-type>,
    name: "wchar_t",
    dylan-type: <character>);

define class <octet-idl-type> (<real-idl-type>)
end class;

define constant $octet-idl-type =
  make(<octet-idl-type>,
    name: "octet",
    dylan-type: <integer>,
    min: largest-integer(8, min:, unsigned:),
    max: largest-integer(8, max:, unsigned:));

define class <boolean-idl-type> (<idl-type>)
end class;

define constant $boolean-idl-type =
  make(<boolean-idl-type>,
    name: "boolean",
    dylan-type: <boolean>);

define class <string-idl-type> (<idl-type>)
end class;

define constant $string-idl-type =
  make(<string-idl-type>,
    name: "string",
    dylan-type: <string>);

define class <wstring-idl-type> (<idl-type>)
end class;

define constant $wstring-idl-type =
  make(<wstring-idl-type>,
       name: "wstring",
       dylan-type: <unicode-string>);

define class <any-idl-type> (<idl-type>)
end class;

define constant $any-idl-type =
  make(<any-idl-type>,
    name: "any",
    dylan-type: <object>);

define class <void-idl-type> (<idl-type>)
end class;

define constant $void-idl-type =
  make(<void-idl-type>,
    name: "void");

define class <none-idl-type> (<idl-type>)
end class;

//define constant $none-idl-type =
//  make(<none-idl-type>);

define class <pseudo-idl-type> (<idl-type>)
end class;

define constant $pseudo-idl-type =
  make(<pseudo-idl-type>);

define class <Object-idl-type> (<pseudo-idl-type>)
end class;

define constant $Object-idl-type =
  make(<Object-idl-type>,
       name: "Object");

define class <TypeCode-idl-type> (<pseudo-idl-type>)
end class;

define constant $TypeCode-idl-type =
  make(<TypeCode-idl-type>,
       name: "TypeCode");

define class <Principal-idl-type> (<pseudo-idl-type>)
end class;

define constant $Principal-idl-type =
  make(<Principal-idl-type>,
       name: "Principal");

define class <NamedValue-idl-type> (<pseudo-idl-type>)
end class;

//define constant $NamedValue-idl-type =
//  make(<NamedValue-idl-type>,
//       name: "NamedValue");

define constant $predefined-types = vector($long-idl-type,
                                           $ulong-idl-type,
                                           $longlong-idl-type,
					   $ulonglong-idl-type,
                                           $short-idl-type,
                                           $ushort-idl-type,
                                           $float-idl-type,
                                           $double-idl-type,
                                           $longdouble-idl-type,
                                           $char-idl-type,
                                           $wchar-idl-type,
                                           $boolean-idl-type,
                                           $octet-idl-type,
                                           $any-idl-type,
                                           $void-idl-type,
					   $Object-idl-type,
					   $TypeCode-idl-type);
//					   $NamedValue-idl-type,
//					   $Principal-idl-type);

