Module:       warnings-test-suite
Synopsis:     A test suite for compiler warnings
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Primary classes

define primary class <primary-class-1> (<object>)
end class <primary-class-1>;

define primary class <primary-class-2> (<object>)
end class <primary-class-2>;

define class <class-with-two-primary-superclasses>
    (<primary-class-1>, <primary-class-2>)
end class <class-with-two-primary-superclasses>;

ignore(<class-with-two-primary-superclasses>);


/// Inconsistent class precedence list

define class <class-1> (<object>)
end class <class-1>;

define class <class-2> (<object>)
end class <class-2>;

define class <class-1-2> (<class-1>, <class-2>)
end class <class-1-2>;

define class <class-2-1> (<class-2>, <class-1>)
end class <class-2-1>;

define class <inconsistent-cpl> (<class-1-2>, <class-2-1>)
end class <inconsistent-cpl>;

ignore(<inconsistent-cpl>);


/// Abstract classes

define abstract class <abstract-class> (<object>)
end class <abstract-class>;

define method make-of-uninstantiable-class
    () => ()
  make(<abstract-class>)
end method make-of-uninstantiable-class;

define class <concrete-class> (<object>)
end class <concrete-class>;

define abstract class <abstract-with-concrete-superclass> (<concrete-class>)
end class <abstract-with-concrete-superclass>;

ignore(make-of-uninstantiable-class,
       <abstract-with-concrete-superclass>);


/// Keywords

define class <required-keyword-class> (<object>)
  constant slot %a :: <integer>,
    required-init-keyword: a:;
end class <required-keyword-class>;

define function make-missing-required-keyword
    ()
  make(<required-keyword-class>)
end function make-missing-required-keyword;

define function make-wrong-keyword-type
    ()
  make(<required-keyword-class>, a: "Hello")
end function make-wrong-keyword-type;

define function make-wrong-keyword
    ()
  make(<required-keyword-class>,
       a: 10, no-such-keyword: 20)
end function make-wrong-keyword;

ignore(%a,
       make-missing-required-keyword,
       make-wrong-keyword-type,
       make-wrong-keyword);


/// Slots

define abstract class <problem-class> (<object>)
  constant slot uninstantiable-slot;
  constant slot required-keyword-slot-with-default = #f,
    required-init-keyword: foo:;
  constant slot wrong-default-slot :: <integer> = "Hello";
  constant slot duplicate-slot :: <integer>;
  constant slot duplicate-slot :: <string>;
  keyword no-such-keyword: = 10;
end class <problem-class>;

ignore(<problem-class>,
       uninstantiable-slot,
       required-keyword-slot-with-default,
       wrong-default-slot,
       duplicate-slot);


/// Sealing

define class <sealing-violation> (<function>)
end class <sealing-violation>;

ignore(<sealing-violation>);


/// Branch types

define function badly-typed-if
    (x) => (value :: <string>)
  let value :: <string> = if (x == 23) "Hello" end;
  let value :: <string>
    = if (x == 23) "Hello" else 23 end;
  let value :: <string>
    = if (x == 23)
	"Hello"
      elseif (x == 12)
	x
      else
	"default"
      end;
  value
end function badly-typed-if;

define function badly-typed-unless
    (x) => (value :: <string>)
  let value :: <string> = unless (x) "Hello" end;
end function badly-typed-unless;

define function badly-typed-case
    (x) => (value :: <string>)
  let value :: <string>
    = case
	x == 23   => "Hello";
      end;
  let value :: <string>
    = case
	x == 23   => "Hello";
	otherwise => 20;
      end;
  let value :: <string>
    = case
	x == 23   => x;
	otherwise => "default";
      end;
  value
end function badly-typed-case;

define function badly-typed-select
    (x) => (value :: <string>)
  let value :: <string>
    = select (x)
	23        => "Hello";
	otherwise => 20;
      end;
  let value :: <string>
    = select (x)
	23        => 20;
	otherwise => "default";
      end;
  value
end function badly-typed-select;

define function badly-typed-block
    (x :: <integer>) => (value :: <string>)
  block (return)
    if (x == 23) return("Hello!") end;
    if (x == 12) return(10) end;
    x
  end
end function badly-typed-block;

ignore(badly-typed-if,
       badly-typed-unless,
       badly-typed-case,
       badly-typed-select,
       badly-typed-block);


/// False-or typing

define function false-or-type-problem
    (x :: false-or(<string>)) => (y :: false-or(<integer>))
  x
end function false-or-type-problem;

ignore(false-or-type-problem);
