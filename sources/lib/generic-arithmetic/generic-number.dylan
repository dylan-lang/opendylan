Module:    generic-arithmetic-internal
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// See the Integer proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...
/// This file simply defines a set of forwarding methods which may be enhanced
/// by the Big-Integer library and/or other future generic libraries.

///---*** Shouldn't we seal these generics over some domain (i.e., <complex>)?
///---*** However, we can't seal these generics without some mechanism to allow methods
///---*** to be added by other "system" libraries (i.e., Big-Integers, Ratios).

define macro binary-arithmetic-function-definer
  { define binary-arithmetic-function ?:name ?domain:name }
  => { define open generic ?name (x :: <object>, y :: <object>)
	=> (#rest values :: <object>);
       define method ?name (x :: <object>, y :: <object>) => (#rest values :: <object>)
	 "dylan/" ## ?name(x, y)
       end method ?name }
  { define binary-arithmetic-function ?:name }
  => { define binary-arithmetic-function ?name <complex> }
end macro binary-arithmetic-function-definer;

define binary-arithmetic-function \+;
define binary-arithmetic-function \-;
define binary-arithmetic-function \*;
define binary-arithmetic-function \/;
define binary-arithmetic-function \^;

define macro unary-arithmetic-function-definer
  { define unary-arithmetic-function ?:name ?domain:name }
  => { define open generic ?name (x :: <object>) => (#rest values :: <object>);
       define method ?name (x :: <object>) => (#rest values :: <object>)
	 "dylan/" ## ?name(x)
       end method ?name }
  { define unary-arithmetic-function ?:name }
  => { define unary-arithmetic-function ?name <complex> }
end macro unary-arithmetic-function-definer;

define unary-arithmetic-function negative;
define unary-arithmetic-function abs;

define macro unary-division-function-definer
  { define unary-division-function ?:name ?domain:name }
  => { define open generic ?name (x :: <real>) => (result :: <integer>, remainder :: <real>);
       define method ?name (x :: <real>) => (result :: <integer>, remainder :: <real>)
	 "dylan/" ## ?name(x)
       end method ?name }
  { define unary-division-function ?:name }
  => { define unary-division-function ?name <complex> }
end macro unary-division-function-definer;

define unary-division-function floor;
define unary-division-function ceiling;
define unary-division-function round;
define unary-division-function truncate;

define macro binary-division-function-definer
  { define binary-division-function ?:name ?domain:name }
  => { define open generic ?name (x :: <real>, y :: <real>)
	=> (result :: <integer>, remainder :: <real>);
       define method ?name (x :: <real>, y :: <real>)
	=> (result :: <integer>, remainder :: <real>)
	 "dylan/" ## ?name(x, y)
       end method ?name }
  { define binary-division-function ?:name }
  => { define binary-division-function ?name <complex> }
end macro binary-division-function-definer;

define binary-division-function floor/;
define binary-division-function ceiling/;
define binary-division-function round/;
define binary-division-function truncate/;

define macro single-valued-binary-division-function-definer
  { define single-valued-binary-division-function ?:name ?domain:name }
  => { define open generic ?name (x :: <real>, y :: <real>) => (result :: <real>);
       define method ?name (x :: <real>, y :: <real>) => (result :: <real>)
	 "dylan/" ## ?name(x, y)
       end method ?name }
  { define single-valued-binary-division-function ?:name }
  => { define single-valued-binary-division-function ?name <complex> }
end macro single-valued-binary-division-function-definer;

define single-valued-binary-division-function modulo;
define single-valued-binary-division-function remainder;
