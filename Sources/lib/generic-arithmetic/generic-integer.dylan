Module:    generic-arithmetic-internal
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// See the Integer proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...
/// This file simply defines a set of forwarding methods which may be enhanced
/// by the Big-Integer library and/or other future generic libraries.

/// In the Generic-Arithmetic library, <integer> arithmetic is still bounded
/// just as it is in the Dylan library but, as other libraries are loaded,
/// these bounds can change or be eliminated so we can't use constants.
define variable $minimum-integer :: false-or(<integer>) = dylan/$minimum-integer;
define variable $maximum-integer :: false-or(<integer>) = dylan/$maximum-integer;

///---*** Shouldn't we seal these generics over some domain (i.e., <complex>)?
///---*** However, we can't seal these generics without some mechanism to allow methods
///---*** to be added by other "system" libraries (i.e., Big-Integers, Ratios).

define macro nary-logical-function-definer
  { define nary-logical-function ?:name }
  => { define sideways inline method "binary-" ## ?name
	   (integer-1 :: <integer>, integer-2 :: <integer>) => (value :: <integer>)
	 "dylan/" ## ?name(integer-1, integer-2)
       end method "binary-" ## ?name }
end macro nary-logical-function-definer;

define nary-logical-function logior;
define nary-logical-function logxor;
define nary-logical-function logand;

define open generic lognot (x :: <integer>) => (value :: <integer>);
define method lognot (x :: <integer>) => (value :: <integer>)
  dylan/lognot(x)
end method lognot;

define open generic logbit? (p :: dylan/<integer>, x :: <integer>) => (value :: <boolean>);
define method logbit? (p :: dylan/<integer>, x :: <integer>) => (value :: <boolean>)
  dylan/logbit?(p, x)
end method logbit?;

define macro shift-function-definer
  { define shift-function ?:name ?domain:name }
  => { define open generic ?name (x :: <integer>, count :: dylan/<integer>)
	=> (value :: <integer>);
       define inline method ?name (x :: <integer>, count :: dylan/<integer>)
        => (value :: <integer>)
         "dylan/" ## ?name(x, count)
       end method ?name }
  { define shift-function ?:name }
  => { define shift-function ?name <complex> }
end macro shift-function-definer;

define shift-function ash;
define shift-function lsh;

define macro algebraic-function-definer
  { define algebraic-function ?:name ?domain:name }
  => { define open generic ?name (integer-1 :: <integer>, integer-2 :: <integer>)
	=> (value :: <integer>);
       define method ?name (integer-1 :: <integer>, integer-2 :: <integer>)
        => (value :: <integer>)
         "dylan/" ## ?name(integer-1, integer-2)
       end method ?name
       }
  { define algebraic-function ?:name }
  => { define algebraic-function ?name <complex> }
end macro algebraic-function-definer;

define algebraic-function lcm;
define algebraic-function gcd;
