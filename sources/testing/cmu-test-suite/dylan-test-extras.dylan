Module: dylan-test
author: Roger Critchlow (rec@elf.org)
synopsis: A regression test for core Dylan.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================
//
// This program runs a set of simple minded tests past the compiler
// and interpreter.  Lots more tests could be added, obviously, but
// even these few have turned up some problems.  A large block are copied
// from the DIRM examples.
//


define variable *you-dont-know-me-1* = #f;

define variable *you-dont-know-me-2* = #f;

define variable *you-dont-know-me-3* = #f;



define method tautology(arg == #"deques")
  let d = make(<deque>);
  for (i from 5 to 1 by -1)
    let res = push(d, i);
    (res == i)
      | signal("push(d, %=) did not return %=!  Got %=", i, i, res);
  end;
  for (i from 6 to 10)
    let res = push-last(d, i);
    (res == i)
      | signal("push-last(d, %=) did not return %=!  Got %=", i, i, res);
  end;
  let p = pop(d);
  (p = 1)
    | signal("first pop(d) is not 1!  It's %=\n", p);
  let p = pop-last(d);
  (p = 10)
    | signal("first pop-last(d) is not 10!  It's %=\n", p);
  if (buggy?)
    //  this should be the same as push() but is maybe push-last()
    d := add!(d, 1);
    let p = pop(d);
    (p = 1)
      | signal("second pop(d) is not 1!  It's %=\n", p);
    //  this fails with a message about scan!()
    d := remove!(d, 9);
    let p = pop-last(d);
    (p = 8)
      | signal("second pop-last(d) is not 8!  It's %=\n", p);
  end;
end method;

define method tautology(arg == #"tables")
  let a = make(<table>);
end method;

define method tautology(arg == #"arrays")
  let a = make(<array>, dimensions: #(4, 4));
  (dimensions (a) = #(4, 4))
    | signal("dimensions (a) are not #(4, 4)!  They're %=\n", dimensions (a));
  (size(a) = 16)
    | signal("size(a) is not 16!  It's %=\n", size(a));
  for (i from 0 below 4)
    for (j from 0 below 4)
      a[i,j] := i * 4 + j;
    end;
  end;
  (aref(a, 1, 1) = 5)
    | signal("aref(a, 1, 1) is not 5! It's %=\n", aref(a, 1, 1));
  (aref-setter(128, a, 1, 1) = 128)
    | signal("aref-setter(128, a, 1, 1) is not 128! It's %=\n", aref-setter(128, a, 1, 1));
  if (buggy?)
    (rank(a) = 2)
      | signal("rank(a) is not 2!  It's %=\n", rank(a));
    //  Unbound variable: rank
    (row-major-index(a, 1, 1) = 5)
      | signal("row-major-index (a, 1, 1) is not 5!  It's %=\n", row-major-index(a, 1, 1));
    //  Unbound variable: row-major-index
    (dimension(a, 1) = 4)
      | signal("dimension(a, 1) is not 4!  It's %=\n", dimension(a, 1));
    //  Unbound variable: dimension
  end;
end method;

define method tautology(arg == #"symbols")
  instance?(#"foo", <symbol>)		| signal("instance?(#\"foo\", <symbol>) is false!\n");
  instance?(#"foo", <symbol>)		| signal("instance?(foo:, <symbol>) is false!\n");
  (#"foo" = #"FOO")			| signal("#\"foo\" is not FOO:!\n");
  (as(<symbol>, "FOO") = #"foo")	| signal("as(<symbol>, \"FOO\") is not foo:!\n");
  (as(<string>, #"Foo") = "foo")	| signal("as(<string>, Foo:) is not \"foo\"! It's %=\n",
						 as(<string>, Foo:));
end method;

define method tautology(arg == #"floats")
  instance?(1.0, <number>)		| signal("1.0 is not a <number>");
  instance?(1.0, <real>)		| signal("1.0 is not a <real>!\n");
  instance?(1.0, <float>)		| signal("1.0 is not a <float>!\n");
  instance?(1.0s0, <single-float>)	| signal("1.0s0 is not a <single-float>!\n");
  instance?(1.0d0, <double-float>)	| signal("1.0d0 is not a <double-float>!\n");
//  instance?(1.0x0, <extended-float>)	| signal("1.0x0 is not a <extended-float>!\n");
  instance?(1.0, <rational>)		& signal("1.0 is a <rational>!\n");
  instance?(1.0, <integer>)		& signal("1.0 is a <integer>!\n");
  instance?(1.0, <complex>)		| signal("1.0 is not a <complex>!\n");
  (4.0 / 2.0 = 2.0)			| signal("4.0 / 2.0 is not 2.0!\n");
  (floor(3.14) = 3)			| signal("floor(3.14) is not 3\n");
  (ceiling(3.14) = 4)			| signal("ceiling(3.14) is not 4!\n");
  (round(3.14) = 3)			| signal("round(3.14) is not 3!\n");
  (truncate(3.14) = 3)			| signal("truncate(3.14) is not 3!\n");
  (4.0d0 / 2.0d0 = 2.0d0)		| signal("4.0d0 / 2.0d0 is not 2.0d0!\n");
  (floor(3.14d0) = 3)			| signal("floor(3.14d0) is not 3\n");
  (ceiling(3.14d0) = 4)			| signal("ceiling(3.14d0) is not 4!\n");
  (round(3.14d0) = 3)			| signal("round(3.14d0) is not 3!\n");
  (truncate(3.14d0) = 3)		| signal("truncate(3.14d0) is not 3!\n");
//  (4.0x0 / 2.0x0 = 2.0x0)		| signal("4.0x0 / 2.0x0 is not 2.0x0!\n");
//  (floor(3.14x0) = 3)			| signal("floor(3.14x0) is not 3\n");
//  (ceiling(3.14x0) = 4)			| signal("ceiling(3.14x0) is not 4!\n");
//  (round(3.14x0) = 3)			| signal("round(3.14x0) is not 3!\n");
//  (truncate(3.14x0) = 3)		| signal("truncate(3.14x0) is not 3!\n");
  //floor/
  //ceiling/
  //round/
  //truncate/
  //modulo
  //remainder
/*
  if (buggy?)
    // NB - rationals may not be part of the language
    instance?(1, <ratio>)
      | signal("1 is not a <ratio>!\n");
    // Unbound variable: <ratio>
    instance?(1.0, <ratio>)
      & signal("1.0 is a <ratio>!\n");
    // Unbound variable: <ratio>
    (4 / 2 = 2)
      | signal("4 / 2 is not 2!\n");
    // No applicable methods for / with arguments #[4, 2]
//    format-out("rationalize(1,2) is %=\n", rationalize(1,2));
    // Unbound variable: rationalize
//    format-out("numerator(rationalize(1,2)) is %=\n", numerator(rationalize(1,2)));
    // Unbound variable: numerator
//    format-out("denominator(rationalize(1,2)) is %=\n", denominator(rationalize(1,2)));
    // Unbound variable: denominator
  end;
*/
end method;


define open generic test-gf-0 ();
define open generic test-gf-0-opts (#rest r);
define open generic test-gf-1 (a1);
define open generic test-gf-1-opts (a1, #rest r);
define open generic test-gf-2 (a1, a2);
define open generic test-gf-2-opts (a1, a2, #rest r);
define open generic test-gf-3 (a1, a2, a3);
define open generic test-gf-3-opts (a1, a2, a3, #rest r);
define open generic test-gf-4 (a1, a2, a3, a4);
define open generic test-gf-4-opts (a1, a2, a3, a4, #rest r);
define open generic test-gf-5 (a1, a2, a3, a4, a5);
define open generic test-gf-5-opts (a1, a2, a3, a4, a5, #rest r);
define open generic test-gf-6 (a1, a2, a3, a4, a5, a6);
define open generic test-gf-6-opts (a1, a2, a3, a4, a5, a6, #rest r);
define open generic test-gf-7 (a1, a2, a3, a4, a5, a6, a7);
define open generic test-gf-7-opts (a1, a2, a3, a4, a5, a6, a7, #rest r);
define open generic test-gf-8 (a1, a2, a3, a4, a5, a6, a7, a8);
define open generic test-gf-8-opts (a1, a2, a3, a4, a5, a6, a7, a8, #rest r);
define open generic test-gf-9 (a1, a2, a3, a4, a5, a6, a7, a8, a9);
define open generic test-gf-9-opts (a1, a2, a3, a4, a5, a6, a7, a8, a9, #rest r);
define open generic test-gf-10 (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
define open generic test-gf-10-opts (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, #rest r);

define constant nuke-gf = method (gf :: <generic-function>, methods :: <sequence>) => ();
  for (m in copy-sequence(generic-function-methods(gf))) remove-method(gf, m) end;
  for (m in methods) add-method(gf, m) end;
end method;


define constant conses = method (#rest v)
  local method loop (i :: <integer>, ans)
	  if (i == 0)
	    ans
	  else
	    let i :: <integer> = i - 1;
	    loop(i, pair(v[i], ans))
	  end if
	end method;
  let n :: <integer> = size(v) - 1;
  loop(n, v[n])
end method;

define method tautology (x == #"degenerate-dispatch")
  let argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  for (i :: <integer> from 0 below 14)
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i);
    for (j :: <integer> from 0 below i) v[j] := j + 1 end;
    argvecs[i] := v
  end for;
  local method test-one (gf, who, n :: <integer>)
	  let args :: <simple-object-vector> = argvecs[n];
	  let ans = apply(gf, args);
	  if (~instance?(ans, <list>))
	    signal("Degenerate dispatch %s nargs=%d returned garbage: %=", who, n, ans)
	  end;
	  if (ans ~= args)
	    signal("Degenerate dispatch %s nargs=%d returned incorrect answer %=", who, n, ans)
	  end;
	end method;
  local method test-req (gf, who, meth)
	  nuke-gf(gf, list(meth));
	  test-one(gf, who, size(function-specializers(gf)));
	end method;
  local method test-opts (gf, who, meth)
	  nuke-gf(gf, list(meth));
	  let n :: <integer> = size(function-specializers(gf));
	  for (extra from 0 below 4) test-one(gf, who, n + extra) end;
	end method;
  test-req(test-gf-0, "test-gf-0", method() #() end);
  test-opts(test-gf-0-opts, "test-gf-0-opts", method(#rest l) as(<list>, l) end);

  test-req(test-gf-1, "test-gf-1", method(a1) list(a1) end);
  test-opts(test-gf-1-opts, "test-gf-1-opts", method(a1, #rest l) conses(a1, as(<list>, l)) end);

  test-req(test-gf-2, "test-gf-2", method(a1, a2) list(a1, a2) end);
  test-opts(test-gf-2-opts, "test-gf-2-opts", method(a1, a2, #rest l) conses(a1, a2, as(<list>, l)) end);

  test-req(test-gf-3, "test-gf-3", method(a1, a2, a3) list(a1, a2, a3) end);
  test-opts(test-gf-3-opts, "test-gf-3-opts", method(a1, a2, a3, #rest l) conses(a1, a2, a3, as(<list>, l)) end);

  test-req(test-gf-4, "test-gf-4", method(a1, a2, a3, a4) list(a1, a2, a3, a4) end);
  test-opts(test-gf-4-opts, "test-gf-4-opts", method(a1, a2, a3, a4, #rest l) conses(a1, a2, a3, a4, as(<list>, l)) end);

  test-req(test-gf-5, "test-gf-5", method(a1, a2, a3, a4, a5) list(a1, a2, a3, a4, a5) end);
  test-opts(test-gf-5-opts, "test-gf-5-opts", method(a1, a2, a3, a4, a5, #rest l) conses(a1, a2, a3, a4, a5, as(<list>, l)) end);

  test-req(test-gf-6, "test-gf-6", method(a1, a2, a3, a4, a5, a6) list(a1, a2, a3, a4, a5, a6) end);
  test-opts(test-gf-6-opts, "test-gf-6-opts", method(a1, a2, a3, a4, a5, a6, #rest l) conses(a1, a2, a3, a4, a5, a6, as(<list>, l)) end);

  test-req(test-gf-7, "test-gf-7", method(a1, a2, a3, a4, a5, a6, a7) list(a1, a2, a3, a4, a5, a6, a7) end);
  test-opts(test-gf-7-opts, "test-gf-7-opts", method(a1, a2, a3, a4, a5, a6, a7, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l)) end);

  test-req(test-gf-8, "test-gf-8", method(a1, a2, a3, a4, a5, a6, a7, a8) list(a1, a2, a3, a4, a5, a6, a7, a8) end);
  test-opts(test-gf-8-opts, "test-gf-8-opts", method(a1, a2, a3, a4, a5, a6, a7, a8, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l)) end);

  test-req(test-gf-9, "test-gf-9", method(a1, a2, a3, a4, a5, a6, a7, a8, a9) list(a1, a2, a3, a4, a5, a6, a7, a8, a9) end);
  test-opts(test-gf-9-opts, "test-gf-9-opts", method(a1, a2, a3, a4, a5, a6, a7, a8, a9, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l)) end);

  test-req(test-gf-10, "test-gf-10", method(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) end);
  test-opts(test-gf-10-opts, "test-gf-10-opts", method(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l)) end);

end method;

define method tautology (x == #"degenerate-typed-dispatch")
  let argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  for (i :: <integer> from 0 below 14)
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i);
    for (j :: <integer> from 0 below i) v[j] := j + 1 end;
    argvecs[i] := v
  end for;
  local method test-one (gf, who, n :: <integer>)
	  let args :: <simple-object-vector> = argvecs[n];
	  let ans = apply(gf, args);
	  if (~instance?(ans, <list>))
	    signal("Degenerate dispatch %s nargs=%d returned garbage: %=", who, n, ans)
	  end;
	  if (ans ~= args)
	    signal("Degenerate typed dispatch %s nargs=%d returned incorrect answer %=", who, n, ans)
	  end;
	end method;
  local method test-req (gf, who, meth)
	  nuke-gf(gf, list(meth));
	  test-one(gf, who, size(function-specializers(gf)));
	end method;
  local method test-opts (gf, who, meth)
	  nuke-gf(gf, list(meth));
	  let n :: <integer> = size(function-specializers(gf));
	  for (extra from 0 below 4) test-one(gf, who, n + extra) end;
	end method;

  test-req(test-gf-1, "test-gf-1", method(a1 :: <integer>) list(a1) end);
  test-opts(test-gf-1-opts, "test-gf-1-opts", method(a1 :: <integer>, #rest l) conses(a1, as(<list>, l)) end);

  test-req(test-gf-2, "test-gf-2", method(a1 :: <integer>, a2 :: <integer>) list(a1, a2) end);
  test-opts(test-gf-2-opts, "test-gf-2-opts", method(a1 :: <integer>, a2 :: <integer>, #rest l) conses(a1, a2, as(<list>, l)) end);

  test-req(test-gf-3, "test-gf-3", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>) list(a1, a2, a3) end);
  test-opts(test-gf-3-opts, "test-gf-3-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, #rest l) conses(a1, a2, a3, as(<list>, l)) end);

  test-req(test-gf-4, "test-gf-4", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>) list(a1, a2, a3, a4) end);
  test-opts(test-gf-4-opts, "test-gf-4-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, #rest l) conses(a1, a2, a3, a4, as(<list>, l)) end);

  test-req(test-gf-5, "test-gf-5", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>) list(a1, a2, a3, a4, a5) end);
  test-opts(test-gf-5-opts, "test-gf-5-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, #rest l) conses(a1, a2, a3, a4, a5, as(<list>, l)) end);

  test-req(test-gf-6, "test-gf-6", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>) list(a1, a2, a3, a4, a5, a6) end);
  test-opts(test-gf-6-opts, "test-gf-6-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, #rest l) conses(a1, a2, a3, a4, a5, a6, as(<list>, l)) end);

  test-req(test-gf-7, "test-gf-7", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>) list(a1, a2, a3, a4, a5, a6, a7) end);
  test-opts(test-gf-7-opts, "test-gf-7-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l)) end);

  test-req(test-gf-8, "test-gf-8", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>, a8 :: <integer>) list(a1, a2, a3, a4, a5, a6, a7, a8) end);
  test-opts(test-gf-8-opts, "test-gf-8-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l)) end);

  test-req(test-gf-9, "test-gf-9", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>) list(a1, a2, a3, a4, a5, a6, a7, a8, a9) end);
  test-opts(test-gf-9-opts, "test-gf-9-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l)) end);

  test-req(test-gf-10, "test-gf-10", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>) list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) end);
  test-opts(test-gf-10-opts, "test-gf-10-opts", method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, #rest l) conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l)) end);

end method;


define method tautology (x == #"if-discriminating-dispatch")
  let argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  let alpha-argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  let omega-argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  argvecs[0] := #[];
  for (i :: <integer> from 1 below 15)
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i);
    for (j :: <integer> from 0 below i) v[j] := j + 1 end;
    argvecs[i] := v;
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i, fill: #f);
    for (j :: <integer> from 1 below i) v[j] := j + 1 end;
    alpha-argvecs[i] := v;
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i, fill: #f);
    for (j :: <integer> from 0 below i - 1) v[j] := j + 1 end;
    omega-argvecs[i] := v;
  end for;
  local method test-one (gf, who, argvecs :: <simple-object-vector>, n :: <integer>)
	  let args :: <simple-object-vector> = argvecs[n];
	  let ans = apply(gf, args);
	  if (~instance?(ans, <list>))
	    signal("if-discriminating dispatch %s nargs=%d should be %= returned garbage: %=", 
		   who, n, args, ans)
	  end;
	  if (ans ~= args)
	    signal("if-discriminating dispatch %s nargs=%d should be %= returned incorrect answer %=", 
		   who, n, args, ans)
	  end;
	  
	end method;
  local method test-req (gf, who, altargs :: <simple-object-vector>, #rest methods)
	  nuke-gf(gf, methods);
	  test-one(gf, who, argvecs, size(function-specializers(gf)));
	  test-one(gf, who, altargs, size(function-specializers(gf)));
	end method;
  local method test-opts (gf, who, altargs :: <simple-object-vector>, #rest methods)
	  nuke-gf(gf, methods);
	  let n :: <integer> = size(function-specializers(gf));
	  for (extra from 0 below 4)
	    test-one(gf, who, argvecs, n + extra);
	    test-one(gf, who, altargs, n + extra);
	  end for;
	end method;

  test-req(test-gf-1, "test-gf-1", alpha-argvecs,
	   method(a1 :: <integer>)
	       list(a1)
	   end,
	   method(a1 :: <object>)
	       list(a1)
	   end);
  test-opts(test-gf-1-opts, "test-gf-1-opts", alpha-argvecs,
	    method(a1 :: <integer>, 
		   #rest l)
		conses(a1, as(<list>, l))
	    end,
	    method(a1 :: <object>, 
		   #rest l)
		conses(a1, as(<list>, l))
	    end);

  test-req(test-gf-2, "test-gf-2", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>)
	       list(a1, a2)
	   end,
	   method(a1 :: <object>, a2 :: <integer>)
	       list(a1, a2)
	   end);
  test-req(test-gf-2, "test-gf-2-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>)
	       list(a1, a2)
	   end,
	   method(a1 :: <integer>, a2 :: <object>)
	       list(a1, a2)
	   end);
  test-opts(test-gf-2-opts, "test-gf-2-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, 
		   #rest l)
		conses(a1, a2, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, 
		   #rest l)
		conses(a1, a2, as(<list>, l))
	    end);
  test-opts(test-gf-2-opts, "test-gf-2-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, 
		   #rest l)
		conses(a1, a2, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <object>, 
		   #rest l)
		conses(a1, a2, as(<list>, l))
	    end);

  test-req(test-gf-3, "test-gf-3", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>)
	       list(a1, a2, a3)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>)
	       list(a1, a2, a3)
	   end);
  test-req(test-gf-3, "test-gf-3-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>)
	       list(a1, a2, a3)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <object>)
	       list(a1, a2, a3)
	   end);
  test-opts(test-gf-3-opts, "test-gf-3-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, as(<list>, l))
	    end);
  test-opts(test-gf-3-opts, "test-gf-3-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, as(<list>, l))
	    end);

  test-req(test-gf-4, "test-gf-4", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>)
	       list(a1, a2, a3, a4)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>)
	       list(a1, a2, a3, a4)
	   end);
  test-req(test-gf-4, "test-gf-4-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>)
	       list(a1, a2, a3, a4)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <object>)
	       list(a1, a2, a3, a4)
	   end);
  test-opts(test-gf-4-opts, "test-gf-4-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, as(<list>, l))
	    end);
  test-opts(test-gf-4-opts, "test-gf-4-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, a4, as(<list>, l))
	    end);

  test-req(test-gf-5, "test-gf-5", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>)
	       list(a1, a2, a3, a4, a5)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>)
	       list(a1, a2, a3, a4, a5)
	   end);
  test-req(test-gf-5, "test-gf-5-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>)
	       list(a1, a2, a3, a4, a5)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <object>)
	       list(a1, a2, a3, a4, a5)
	   end);
  test-opts(test-gf-5-opts, "test-gf-5-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end);
  test-opts(test-gf-5-opts, "test-gf-5-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end);

  test-req(test-gf-6, "test-gf-6", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6)
	   end);
  test-req(test-gf-6, "test-gf-6-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <object>)
	       list(a1, a2, a3, a4, a5, a6)
	   end);
  test-opts(test-gf-6-opts, "test-gf-6-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end);
  test-opts(test-gf-6-opts, "test-gf-6-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end);

  test-req(test-gf-7, "test-gf-7", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end);
  test-req(test-gf-7, "test-gf-7-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <object>)
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end);
  test-opts(test-gf-7-opts, "test-gf-7-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end);
  test-opts(test-gf-7-opts, "test-gf-7-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end);

  test-req(test-gf-8, "test-gf-8", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end);
  test-req(test-gf-8, "test-gf-8-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <object>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end);
  test-opts(test-gf-8-opts, "test-gf-8-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end);
  test-opts(test-gf-8-opts, "test-gf-8-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end);

  test-req(test-gf-9, "test-gf-9", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end);
  test-req(test-gf-9, "test-gf-9-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <object>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end);
  test-opts(test-gf-9-opts, "test-gf-9-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end);
  test-opts(test-gf-9-opts, "test-gf-9-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end);

  test-req(test-gf-10, "test-gf-10", alpha-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end);
  test-req(test-gf-10, "test-gf-10-omega", omega-argvecs,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <object>)
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end);
  test-opts(test-gf-10-opts, "test-gf-10-opts", alpha-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end);
  test-opts(test-gf-10-opts, "test-gf-10-opts-omega", omega-argvecs,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <object>, 
		   #rest l)
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end);

end method;


define method tautology (x == #"discriminating-dispatch-hairier")
  let argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  let alpha-argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  let omega-argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  argvecs[0] := #[];
  for (i :: <integer> from 1 below 15)
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i);
    for (j :: <integer> from 0 below i) v[j] := j + 1 end;
    argvecs[i] := v;
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i, fill: #f);
    for (j :: <integer> from 1 below i) v[j] := j + 1 end;
    alpha-argvecs[i] := v;
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i, fill: #f);
    for (j :: <integer> from 0 below i - 1) v[j] := j + 1 end;
    omega-argvecs[i] := v;
  end for;
  local method test-one (gf, who, argvecs :: <simple-object-vector>, n :: <integer>)
	  let args :: <simple-object-vector> = argvecs[n];
	  let ans = apply(gf, args);
	  if (~instance?(ans, <list>))
	    signal("if-discriminating dispatch %s nargs=%d should be %= returned garbage: %=", 
		   who, n, args, ans)
	  end;
	  if (ans ~= args)
	    signal("if-discriminating dispatch %s nargs=%d should be %= returned incorrect answer %=", 
		   who, n, args, ans)
	  end;
	  
	end method;
  local method test-req (gf, who, altargs :: <simple-object-vector>, #rest methods)
	  nuke-gf(gf, methods);
	  test-one(gf, who, argvecs, size(function-specializers(gf)));
	  test-one(gf, who, altargs, size(function-specializers(gf)));
	end method;
  local method test-opts (gf, who, altargs :: <simple-object-vector>, #rest methods)
	  nuke-gf(gf, methods);
	  let n :: <integer> = size(function-specializers(gf));
	  for (extra from 0 below 4)
	    test-one(gf, who, argvecs, n + extra);
	    test-one(gf, who, altargs, n + extra);
	  end for;
	end method;

  test-req(test-gf-2, "test-gf-2", alpha-argvecs,
	   method(a1, a2) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2)
	   end);
  test-req(test-gf-2, "test-gf-2-omega", omega-argvecs,
	   method(a1, a2) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2)
	   end,
	   method(a1 :: <integer>, a2 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2)
	   end);
  test-opts(test-gf-2-opts, "test-gf-2-opts", alpha-argvecs,
	    method(a1, a2, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, as(<list>, l))
	    end);
  test-opts(test-gf-2-opts, "test-gf-2-opts-omega", omega-argvecs,
	    method(a1, a2, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, as(<list>, l))
	    end);

  test-req(test-gf-3, "test-gf-3", alpha-argvecs,
	   method(a1, a2, a3) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3)
	   end);
  test-req(test-gf-3, "test-gf-3-omega", omega-argvecs,
	   method(a1, a2, a3) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3)
	   end);
  test-opts(test-gf-3-opts, "test-gf-3-opts", alpha-argvecs,
	    method(a1, a2, a3, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, as(<list>, l))
	    end);
  test-opts(test-gf-3-opts, "test-gf-3-opts-omega", omega-argvecs,
	    method(a1, a2, a3, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, as(<list>, l))
	    end);

  test-req(test-gf-4, "test-gf-4", alpha-argvecs,
	   method(a1, a2, a3, a4) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4)
	   end);
  test-req(test-gf-4, "test-gf-4-omega", omega-argvecs,
	   method(a1, a2, a3, a4) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4)
	   end);
  test-opts(test-gf-4-opts, "test-gf-4-opts", alpha-argvecs,
	    method(a1, a2, a3, a4, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, as(<list>, l))
	    end);
  test-opts(test-gf-4-opts, "test-gf-4-opts-omega", omega-argvecs,
	    method(a1, a2, a3, a4, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, as(<list>, l))
	    end);

  test-req(test-gf-5, "test-gf-5", alpha-argvecs,
	   method(a1, a2, a3, a4, a5) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5)
	   end);
  test-req(test-gf-5, "test-gf-5-omega", omega-argvecs,
	   method(a1, a2, a3, a4, a5) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5)
	   end);
  test-opts(test-gf-5-opts, "test-gf-5-opts", alpha-argvecs,
	    method(a1, a2, a3, a4, a5, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end);
  test-opts(test-gf-5-opts, "test-gf-5-opts-omega", omega-argvecs,
	    method(a1, a2, a3, a4, a5, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, as(<list>, l))
	    end);

  test-req(test-gf-6, "test-gf-6", alpha-argvecs,
	   method(a1, a2, a3, a4, a5, a6) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6)
	   end);
  test-req(test-gf-6, "test-gf-6-omega", omega-argvecs,
	   method(a1, a2, a3, a4, a5, a6) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6)
	   end);
  test-opts(test-gf-6-opts, "test-gf-6-opts", alpha-argvecs,
	    method(a1, a2, a3, a4, a5, a6, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end);
  test-opts(test-gf-6-opts, "test-gf-6-opts-omega", omega-argvecs,
	    method(a1, a2, a3, a4, a5, a6, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
	    end);

  test-req(test-gf-7, "test-gf-7", alpha-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end);
  test-req(test-gf-7, "test-gf-7-omega", omega-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7)
	   end);
  test-opts(test-gf-7-opts, "test-gf-7-opts", alpha-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end);
  test-opts(test-gf-7-opts, "test-gf-7-opts-omega", omega-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
	    end);

  test-req(test-gf-8, "test-gf-8", alpha-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7, a8) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end);
  test-req(test-gf-8, "test-gf-8-omega", omega-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7, a8) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8)
	   end);
  test-opts(test-gf-8-opts, "test-gf-8-opts", alpha-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, a8, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end);
  test-opts(test-gf-8-opts, "test-gf-8-opts-omega", omega-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, a8, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
	    end);

  test-req(test-gf-9, "test-gf-9", alpha-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7, a8, a9) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end);
  test-req(test-gf-9, "test-gf-9-omega", omega-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7, a8, a9) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
	   end);
  test-opts(test-gf-9-opts, "test-gf-9-opts", alpha-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, a8, a9, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end);
  test-opts(test-gf-9-opts, "test-gf-9-opts-omega", omega-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, a8, a9, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
	    end);

  test-req(test-gf-10, "test-gf-10", alpha-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end,
	   method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end);
  test-req(test-gf-10, "test-gf-10-omega", omega-argvecs,
	   method(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) #f end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end,
	   method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		  a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <object>, #next next-method)
	       next-method();
	       list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	   end);
  test-opts(test-gf-10-opts, "test-gf-10-opts", alpha-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end,
	    method(a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end);
  test-opts(test-gf-10-opts, "test-gf-10-opts-omega", omega-argvecs,
	    method(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, #rest l) #f end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <integer>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end,
	    method(a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
		   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>, a9 :: <integer>, a10 :: <object>, 
		   #next next-method, #rest l)
		next-method();
		conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
	    end);

end method;


define open generic test-static-gf-2-alpha (a1, a2);
define open generic test-static-gf-2-opts-alpha (a1, a2, #rest r);
define open generic test-static-gf-3-alpha (a1, a2, a3);
define open generic test-static-gf-3-opts-alpha (a1, a2, a3, #rest r);
define open generic test-static-gf-4-alpha (a1, a2, a3, a4);
define open generic test-static-gf-4-opts-alpha (a1, a2, a3, a4, #rest r);
define open generic test-static-gf-5-alpha (a1, a2, a3, a4, a5);
define open generic test-static-gf-5-opts-alpha (a1, a2, a3, a4, a5, #rest r);
define open generic test-static-gf-6-alpha (a1, a2, a3, a4, a5, a6);
define open generic test-static-gf-6-opts-alpha (a1, a2, a3, a4, a5, a6, #rest r);
define open generic test-static-gf-7-alpha (a1, a2, a3, a4, a5, a6, a7);
define open generic test-static-gf-7-opts-alpha (a1, a2, a3, a4, a5, a6, a7, #rest r);
define open generic test-static-gf-8-alpha (a1, a2, a3, a4, a5, a6, a7, a8);
define open generic test-static-gf-8-opts-alpha (a1, a2, a3, a4, a5, a6, a7, a8, #rest r);
define open generic test-static-gf-9-alpha (a1, a2, a3, a4, a5, a6, a7, a8, a9);
define open generic test-static-gf-9-opts-alpha (a1, a2, a3, a4, a5, a6, a7, a8, a9, #rest r);
define open generic test-static-gf-10-alpha (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
define open generic test-static-gf-10-opts-alpha (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, #rest r);
define open generic test-static-gf-11-alpha (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
define open generic test-static-gf-11-opts-alpha (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, #rest r);


define open generic test-static-gf-2-omega (a1, a2);
define open generic test-static-gf-2-opts-omega (a1, a2, #rest r);
define open generic test-static-gf-3-omega (a1, a2, a3);
define open generic test-static-gf-3-opts-omega (a1, a2, a3, #rest r);
define open generic test-static-gf-4-omega (a1, a2, a3, a4);
define open generic test-static-gf-4-opts-omega (a1, a2, a3, a4, #rest r);
define open generic test-static-gf-5-omega (a1, a2, a3, a4, a5);
define open generic test-static-gf-5-opts-omega (a1, a2, a3, a4, a5, #rest r);
define open generic test-static-gf-6-omega (a1, a2, a3, a4, a5, a6);
define open generic test-static-gf-6-opts-omega (a1, a2, a3, a4, a5, a6, #rest r);
define open generic test-static-gf-7-omega (a1, a2, a3, a4, a5, a6, a7);
define open generic test-static-gf-7-opts-omega (a1, a2, a3, a4, a5, a6, a7, #rest r);
define open generic test-static-gf-8-omega (a1, a2, a3, a4, a5, a6, a7, a8);
define open generic test-static-gf-8-opts-omega (a1, a2, a3, a4, a5, a6, a7, a8, #rest r);
define open generic test-static-gf-9-omega (a1, a2, a3, a4, a5, a6, a7, a8, a9);
define open generic test-static-gf-9-opts-omega (a1, a2, a3, a4, a5, a6, a7, a8, a9, #rest r);
define open generic test-static-gf-10-omega (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
define open generic test-static-gf-10-opts-omega (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, #rest r);
define open generic test-static-gf-11-omega (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
define open generic test-static-gf-11-opts-omega (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, #rest r);



define method test-static-gf-2-alpha (a1 :: <integer>, a2 :: <integer>)
  list(a1, a2)
end method;

define method test-static-gf-2-alpha (a1 :: <object>, a2 :: <integer>)
  list(a1, a2)
end method;

define method test-static-gf-2-omega (a1 :: <integer>, a2 :: <integer>)
  list(a1, a2)
end method;

define method test-static-gf-2-omega (a1 :: <integer>, a2 :: <object>)
  list(a1, a2)
end method;

define method test-static-gf-2-opts-alpha (a1 :: <integer>, a2 :: <integer>,
					   #rest l)
  conses(a1, a2, as(<list>, l))
end method;

define method test-static-gf-2-opts-alpha (a1 :: <object>, a2 :: <integer>,
					   #rest l)
  conses(a1, a2, as(<list>, l))
end method;

define method test-static-gf-2-opts-omega (a1 :: <integer>, a2 :: <integer>,
					   #rest l)
  conses(a1, a2, as(<list>, l))
end method;

define method test-static-gf-2-opts-omega (a1 :: <integer>, a2 :: <object>,
					   #rest l)
  conses(a1, a2, as(<list>, l))
end method;


define method test-static-gf-3-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>)
  list(a1, a2, a3)
end method;

define method test-static-gf-3-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>)
  list(a1, a2, a3)
end method;

define method test-static-gf-3-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>)
  list(a1, a2, a3)
end method;

define method test-static-gf-3-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <object>)
  list(a1, a2, a3)
end method;

define method test-static-gf-3-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, as(<list>, l))
end method;

define method test-static-gf-3-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, as(<list>, l))
end method;

define method test-static-gf-3-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, as(<list>, l))
end method;

define method test-static-gf-3-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <object>,
					   #rest l)
  conses(a1, a2, a3, as(<list>, l))
end method;


define method test-static-gf-4-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>)
  list(a1, a2, a3, a4)
end method;

define method test-static-gf-4-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>)
  list(a1, a2, a3, a4)
end method;

define method test-static-gf-4-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>)
  list(a1, a2, a3, a4)
end method;

define method test-static-gf-4-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <object>)
  list(a1, a2, a3, a4)
end method;

define method test-static-gf-4-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, as(<list>, l))
end method;

define method test-static-gf-4-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, as(<list>, l))
end method;

define method test-static-gf-4-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, as(<list>, l))
end method;

define method test-static-gf-4-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, as(<list>, l))
end method;


define method test-static-gf-5-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>)
  list(a1, a2, a3, a4, a5)
end method;

define method test-static-gf-5-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>)
  list(a1, a2, a3, a4, a5)
end method;

define method test-static-gf-5-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>)
  list(a1, a2, a3, a4, a5)
end method;

define method test-static-gf-5-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <object>)
  list(a1, a2, a3, a4, a5)
end method;

define method test-static-gf-5-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, as(<list>, l))
end method;

define method test-static-gf-5-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <integer>, a5 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, as(<list>, l))
end method;

define method test-static-gf-5-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, as(<list>, l))
end method;

define method test-static-gf-5-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <integer>, a5 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, as(<list>, l))
end method;


define method test-static-gf-6-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>)
  list(a1, a2, a3, a4, a5, a6)
end method;

define method test-static-gf-6-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>)
  list(a1, a2, a3, a4, a5, a6)
end method;

define method test-static-gf-6-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>)
  list(a1, a2, a3, a4, a5, a6)
end method;

define method test-static-gf-6-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <object>)
  list(a1, a2, a3, a4, a5, a6)
end method;

define method test-static-gf-6-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>, a6 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
end method;

define method test-static-gf-6-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <integer>, a5 :: <integer>, a6 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
end method;

define method test-static-gf-6-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>, a6 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
end method;

define method test-static-gf-6-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <integer>, a5 :: <integer>, a6 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, as(<list>, l))
end method;




define method test-static-gf-7-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7)
end method;

define method test-static-gf-7-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7)
end method;

define method test-static-gf-7-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7)
end method;

define method test-static-gf-7-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <object>)
  list(a1, a2, a3, a4, a5, a6, a7)
end method;

define method test-static-gf-7-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>, a6 :: <integer>,
					   a7 :: <integer>, 
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
end method;

define method test-static-gf-7-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <integer>, a5 :: <integer>, a6 :: <integer>,
					   a7 :: <integer>, 
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
end method;

define method test-static-gf-7-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>, a6 :: <integer>,
					   a7 :: <integer>, 
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
end method;

define method test-static-gf-7-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>,
					   a4 :: <integer>, a5 :: <integer>, a6 :: <integer>,
					   a7 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, as(<list>, l))
end method;


define method test-static-gf-8-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8)
end method;

define method test-static-gf-8-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8)
end method;

define method test-static-gf-8-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8)
end method;

define method test-static-gf-8-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <object>)
  list(a1, a2, a3, a4, a5, a6, a7, a8)
end method;



define method test-static-gf-8-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
end method;

define method test-static-gf-8-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
end method;

define method test-static-gf-8-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
end method;

define method test-static-gf-8-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, as(<list>, l))
end method;


define method test-static-gf-9-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
end method;

define method test-static-gf-9-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
end method;

define method test-static-gf-9-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
end method;

define method test-static-gf-9-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <object>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9)
end method;



define method test-static-gf-9-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
end method;

define method test-static-gf-9-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
end method;

define method test-static-gf-9-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
end method;

define method test-static-gf-9-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, as(<list>, l))
end method;


define method test-static-gf-10-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
end method;

define method test-static-gf-10-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
end method;

define method test-static-gf-10-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
end method;

define method test-static-gf-10-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <object>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
end method;



define method test-static-gf-10-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
end method;

define method test-static-gf-10-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
end method;

define method test-static-gf-10-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
end method;

define method test-static-gf-10-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, as(<list>, l))
end method;


define method test-static-gf-11-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <integer>, a11 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
end method;

define method test-static-gf-11-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <integer>, a11 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
end method;

define method test-static-gf-11-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <integer>, a11 :: <integer>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
end method;

define method test-static-gf-11-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
				      a4 :: <integer>, a5 :: <integer>,
				      a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
				      a9 :: <integer>, a10 :: <integer>, a11 :: <object>)
  list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
end method;



define method test-static-gf-11-opts-alpha (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <integer>, a11 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, as(<list>, l))
end method;

define method test-static-gf-11-opts-alpha (a1 :: <object>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <integer>, a11 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, as(<list>, l))
end method;

define method test-static-gf-11-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <integer>, a11 :: <integer>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, as(<list>, l))
end method;

define method test-static-gf-11-opts-omega (a1 :: <integer>, a2 :: <integer>, a3 :: <integer>, 
					   a4 :: <integer>, a5 :: <integer>,
					   a6 :: <integer>, a7 :: <integer>, a8 :: <integer>,
					   a9 :: <integer>, a10 :: <integer>, a11 :: <object>,
					   #rest l)
  conses(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, as(<list>, l))
end method;


define method tautology (x == #"if-discriminating-static-dispatch")
  let argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  let alpha-argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  let omega-argvecs :: <simple-object-vector> = make(<simple-object-vector>, size: 15);
  argvecs[0] := #[];
  for (i :: <integer> from 1 below 15)
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i);
    for (j :: <integer> from 0 below i) v[j] := j + 1 end;
    argvecs[i] := v;
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i, fill: #f);
    for (j :: <integer> from 1 below i) v[j] := j + 1 end;
    alpha-argvecs[i] := v;
    let v :: <simple-object-vector> = make(<simple-object-vector>, size: i, fill: #f);
    for (j :: <integer> from 0 below i - 1) v[j] := j + 1 end;
    omega-argvecs[i] := v;
  end for;
  local method test-one (gf, who, argvecs :: <simple-object-vector>, n :: <integer>)
	  let args :: <simple-object-vector> = argvecs[n];
	  let ans = apply(gf, args);
	  if (~instance?(ans, <list>))
	    signal("if-discriminating dispatch %s nargs=%d should be %= returned garbage: %=", 
		   who, n, args, ans)
	  end;
	  if (ans ~= args)
	    signal("if-discriminating dispatch %s nargs=%d should be %= returned incorrect answer %=", 
		   who, n, args, ans)
	  end;
	  
	end method;
  local method test-req (gf, who, altargs :: <simple-object-vector>)
	  test-one(gf, who, argvecs, size(function-specializers(gf)));
	  test-one(gf, who, altargs, size(function-specializers(gf)));
	end method;
  local method test-opts (gf, who, altargs :: <simple-object-vector>)
	  let n :: <integer> = size(function-specializers(gf));
	  for (extra from 0 below 4)
	    test-one(gf, who, argvecs, n + extra);
	    test-one(gf, who, altargs, n + extra);
	  end for;
	end method;

  test-req(test-static-gf-2-alpha, "test-static-gf-2-alpha", alpha-argvecs);
  test-req(test-static-gf-2-omega, "test-static-gf-2-omega", omega-argvecs);
  test-opts(test-static-gf-2-opts-alpha, "test-static-gf-2-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-2-opts-omega, "test-static-gf-2-opts-omega", omega-argvecs);

  test-req(test-static-gf-3-alpha, "test-static-gf-3-alpha", alpha-argvecs);
  test-req(test-static-gf-3-omega, "test-static-gf-3-omega", omega-argvecs);
  test-opts(test-static-gf-3-opts-alpha, "test-static-gf-3-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-3-opts-omega, "test-static-gf-3-opts-omega", omega-argvecs);

  test-req(test-static-gf-4-alpha, "test-static-gf-4-alpha", alpha-argvecs);
  test-req(test-static-gf-4-omega, "test-static-gf-4-omega", omega-argvecs);
  test-opts(test-static-gf-4-opts-alpha, "test-static-gf-4-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-4-opts-omega, "test-static-gf-4-opts-omega", omega-argvecs);

  test-req(test-static-gf-5-alpha, "test-static-gf-5-alpha", alpha-argvecs);
  test-req(test-static-gf-5-omega, "test-static-gf-5-omega", omega-argvecs);
  test-opts(test-static-gf-5-opts-alpha, "test-static-gf-5-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-5-opts-omega, "test-static-gf-5-opts-omega", omega-argvecs);

  test-req(test-static-gf-6-alpha, "test-static-gf-6-alpha", alpha-argvecs);
  test-req(test-static-gf-6-omega, "test-static-gf-6-omega", omega-argvecs);
  test-opts(test-static-gf-6-opts-alpha, "test-static-gf-6-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-6-opts-omega, "test-static-gf-6-opts-omega", omega-argvecs);

  test-req(test-static-gf-7-alpha, "test-static-gf-7-alpha", alpha-argvecs);
  test-req(test-static-gf-7-omega, "test-static-gf-7-omega", omega-argvecs);
  test-opts(test-static-gf-7-opts-alpha, "test-static-gf-7-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-7-opts-omega, "test-static-gf-7-opts-omega", omega-argvecs);

  test-req(test-static-gf-8-alpha, "test-static-gf-8-alpha", alpha-argvecs);
  test-req(test-static-gf-8-omega, "test-static-gf-8-omega", omega-argvecs);
  test-opts(test-static-gf-8-opts-alpha, "test-static-gf-8-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-8-opts-omega, "test-static-gf-8-opts-omega", omega-argvecs);

  test-req(test-static-gf-9-alpha, "test-static-gf-9-alpha", alpha-argvecs);
  test-req(test-static-gf-9-omega, "test-static-gf-9-omega", omega-argvecs);
  test-opts(test-static-gf-9-opts-alpha, "test-static-gf-9-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-9-opts-omega, "test-static-gf-9-opts-omega", omega-argvecs);

  test-req(test-static-gf-10-alpha, "test-static-gf-10-alpha", alpha-argvecs);
  test-req(test-static-gf-10-omega, "test-static-gf-10-omega", omega-argvecs);
  test-opts(test-static-gf-10-opts-alpha, "test-static-gf-10-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-10-opts-omega, "test-static-gf-10-opts-omega", omega-argvecs);

  test-req(test-static-gf-11-alpha, "test-static-gf-11-alpha", alpha-argvecs);
  test-req(test-static-gf-11-omega, "test-static-gf-11-omega", omega-argvecs);
  test-opts(test-static-gf-11-opts-alpha, "test-static-gf-11-opts-alpha", alpha-argvecs);
  test-opts(test-static-gf-11-opts-omega, "test-static-gf-11-opts-omega", omega-argvecs);

end method;


//// Dispatch Regressions

define method too-smart-about-next-method (a :: <test-class-1>, b :: <test-class-2>)
  if (tc1-islot1(a))
    b
  else
    next-method()
  end if
end method;


define method too-smart-about-next-method (a :: <test-class-2>, b :: <integer>)
  list(a, b)
end method;

define method tautology (arg == #"dispatch-regression-1")
  // reset-gf(too-smart-about-next-method);
  *you-dont-know-me-1* := make(<test-class-1>);
  *you-dont-know-me-2* := make(<test-class-2>);
  local method check (name, fn, expected-val, val)
	  if (~fn(val, expected-val))
	    signal("%s failed - expected %= but got %=", name, expected-val, val)
	  end if;
	end method;
  check("First too-smart test", \==, *you-dont-know-me-2*,
	too-smart-about-next-method(*you-dont-know-me-1*, *you-dont-know-me-2*));
  check("Second too-smart test", \=, list(*you-dont-know-me-2*, 259),
	too-smart-about-next-method(*you-dont-know-me-2*, 259));
end method;


define open generic test-dispatch-regression-2a (a, b, c);

define method test-dispatch-regression-2a (x :: <integer>, y :: subclass(<number>), z :: <symbol>)
  z
end method;

define method test-dispatch-regression-2a (x :: <integer>, y :: <class>, z :: <symbol>)
  x
end method;


define open generic test-dispatch-regression-2b (a, b, c);

define method test-dispatch-regression-2b (x :: <integer>, y :: <class>, z :: <symbol>)
  x
end method;

define method test-dispatch-regression-2b (x :: <integer>, y :: subclass(<number>), z :: <symbol>)
  z
end method;

define open generic test-dispatch-regression-2c (x);
define method test-dispatch-regression-2c (x :: <object>)
  #"foo"
end method;
define method test-dispatch-regression-2c (x :: subclass(<object>))
  #"bar"
end method;


define method tautology (arg == #"dispatch-regression-2")
  let oldv = *gracefully-dispatch-to-ambiguous-methods*;
  block ()
    *gracefully-dispatch-to-ambiguous-methods* := #f;
    local method tryit (part :: <byte-string>, fn) => ()
	    *you-dont-know-me-1* := 259;
	    *you-dont-know-me-2* := <integer>;
	    *you-dont-know-me-3* := #"fubar";
	    let ans = fn(*you-dont-know-me-1*, *you-dont-know-me-2*, *you-dont-know-me-3*);
	    if (ans ~== #"fubar")
	      signal("Dispatch regression 2 %s erroneously returned %=", part, ans)
	    end if;
	    *you-dont-know-me-2* := <collection>;
	    let ans = fn(*you-dont-know-me-1*, *you-dont-know-me-2*, *you-dont-know-me-3*);
	    if (ans ~== 259)
	      signal("Dispatch regression 2 %s erroneously returned %=", part, ans)
	    end if;
	  end method;
    tryit("part 1", test-dispatch-regression-2a);
    tryit("part 2", test-dispatch-regression-2b);
  cleanup
    *gracefully-dispatch-to-ambiguous-methods* := oldv;
  end block
end method;



define open generic test-dispatch-regression-3a (x);
define method test-dispatch-regression-3a (x :: <object>)
  #"foo"
end method;
define method test-dispatch-regression-3a (x :: subclass(<object>))
  #"bar"
end method;

define open generic test-dispatch-regression-3b (x);
define method test-dispatch-regression-3b (x :: subclass(<object>))
  #"bar"
end method;
define method test-dispatch-regression-3b (x :: <object>)
  #"foo"
end method;

define method tautology (arg == #"dispatch-regression-3")
  let oldv = *gracefully-dispatch-to-ambiguous-methods*;
  block ()
    *gracefully-dispatch-to-ambiguous-methods* := #f;
    local method tryit (part :: <byte-string>, fn) => ()
	    *you-dont-know-me-1* := <simple-object-vector>;
	    let ans = fn(*you-dont-know-me-1*);
	    if (ans ~== #"bar")
	      signal("Dispatch regression 3 %s erroneously returned %=", part, ans)
	    end if;
	    *you-dont-know-me-1* := #"yow";
	    let ans = fn(*you-dont-know-me-1*);
	    if (ans ~== #"foo")
	      signal("Dispatch regression 3 %s erroneously returned %=", part, ans)
	    end if;
	  end method;
    tryit("part 1", test-dispatch-regression-3a);
    tryit("part 2", test-dispatch-regression-3b);
  cleanup
    *gracefully-dispatch-to-ambiguous-methods* := oldv;
  end block
end method;


define open class <test-slot-nm-handling> (<object>)
  slot test-slot-nm-handling-1 :: <integer>, init-keyword: one:;
  slot test-slot-nm-handling-2, init-keyword: two:, init-value: #"two";
  class slot test-slot-nm-handling-3;
  each-subclass slot test-slot-nm-handling-4;
end class;

define open class <test-slot-nm-handling-sub> (<test-slot-nm-handling>)
  slot test-slot-nm-handling-gratuitous-1;
  class slot test-slot-nm-handling-gratuitous-2;
end class;


define method test-slot-nm-handling-1 (x :: <test-slot-nm-handling-sub>)
  - next-method()
end method;

define method test-slot-nm-handling-2 (x :: <test-slot-nm-handling-sub>)
  list(next-method())
end method;

define method test-slot-nm-handling-3 (x :: <test-slot-nm-handling-sub>)
  - next-method()
end method;

define method test-slot-nm-handling-4 (x :: <test-slot-nm-handling-sub>)
  list(next-method())
end method;


// @@@@ setters

define method tautology (arg == #"dispatch-regression-4")
  local method test-getter(str, getter, inst, val)
	  let v = getter(inst);
	  if (v ~= val)
	    signal("Dispatch regression 4 %s returned %=, should have been %=", str, v, val)
	  end if
	end method;

  let i1a = make(<test-slot-nm-handling>, one: 1, two: #"two-1a");
  let i1b = make(<test-slot-nm-handling>, one: 11, two: #"two-2a");
  test-slot-nm-handling-3(i1a) := 3;
  test-slot-nm-handling-4(i1a) := #"four-1a";

  test-getter("i1a-1", test-slot-nm-handling-1, i1a, 1);
  test-getter("i1a-2", test-slot-nm-handling-2, i1a, #"two-1a");
  test-getter("i1a-3", test-slot-nm-handling-3, i1a, 3);
  test-getter("i1a-4", test-slot-nm-handling-4, i1a, #"four-1a");
  test-getter("i1b-1", test-slot-nm-handling-1, i1b, 11);
  test-getter("i1b-2", test-slot-nm-handling-2, i1b, #"two-2a");
  test-getter("i1b-3", test-slot-nm-handling-3, i1b, 3);
  test-getter("i1b-4", test-slot-nm-handling-4, i1b, #"four-1a");


  let i2a = make(<test-slot-nm-handling-sub>, one: 33, two: #"tutu");
  let i2b = make(<test-slot-nm-handling-sub>, one: 333, two: #"tutu-too");
  test-slot-nm-handling-3(i2a) := 259;
  test-slot-nm-handling-4(i2a) := #"xxx";

  test-getter("i2a-1", test-slot-nm-handling-1, i2a, -33);
  test-getter("i2a-2", test-slot-nm-handling-2, i2a, #(#"tutu"));
  test-getter("i2a-3", test-slot-nm-handling-3, i2a, -259);
  test-getter("i2a-4", test-slot-nm-handling-4, i2a, #(#"xxx"));
  test-getter("i2b-1", test-slot-nm-handling-1, i2b, -333);
  test-getter("i2b-2", test-slot-nm-handling-2, i2b, #(#"tutu-too"));
  test-getter("i2b-3", test-slot-nm-handling-3, i2b, -259);
  test-getter("i2b-4", test-slot-nm-handling-4, i2b, #(#"xxx"));

  test-getter("i1a-3a", test-slot-nm-handling-3, i1a, 259);
  test-getter("i1a-4a", test-slot-nm-handling-4, i1a, #"four-1a");
  test-getter("i1b-3a", test-slot-nm-handling-3, i1b, 259);
  test-getter("i1b-4a", test-slot-nm-handling-4, i1b, #"four-1a");

end method;


define sealed generic dispatch-regression-5a (arg1, arg2, arg3);

define method dispatch-regression-5a (x :: one-of(#"foo", #"bar", #"baz"), y :: <integer>, z)
  list(x, y, #"foo")
end method;

define method dispatch-regression-5a (x :: one-of(#"foo", #"baz", #"bar"), y :: <integer>, z :: <integer>)
  list(x, y, #"bar")
end method;

define method dispatch-regression-5a (x :: one-of(#"bar", #"foo", #"baz"), y :: <integer>, z :: <symbol>)
  list(x, y, #"baz")
end method;


define function test-dispatch-regression-5a-xxx (x, y, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-xxi (x, y, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-xxs (x, y, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-xix (x, y :: <integer>, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-xii (x, y :: <integer>, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-xis (x, y :: <integer>, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;


define function test-dispatch-regression-5a-1xx (x == #"foo", y, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-1xi (x == #"foo", y, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-1xs (x == #"foo", y, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-1ix (x == #"foo", y :: <integer>, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-1ii (x == #"foo", y :: <integer>, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-1is (x == #"foo", y :: <integer>, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;


define function test-dispatch-regression-5a-2xx (x == #"bar", y, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-2xi (x == #"bar", y, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-2xs (x == #"bar", y, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-2ix (x == #"bar", y :: <integer>, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-2ii (x == #"bar", y :: <integer>, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-2is (x == #"bar", y :: <integer>, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;


define function test-dispatch-regression-5a-3xx (x == #"baz", y, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-3xi (x == #"baz", y, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-3xs (x == #"baz", y, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-3ix (x == #"baz", y :: <integer>, z)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-3ii (x == #"baz", y :: <integer>, z :: <integer>)
  dispatch-regression-5a(x, y, z)
end function;

define function test-dispatch-regression-5a-3is (x == #"baz", y :: <integer>, z :: <symbol>)
  dispatch-regression-5a(x, y, z)
end function;




define method tautology (arg == #"dispatch-regression-5a")
  local method tryit-i (f, x, y, z)
	  let v = if (instance?(z, <integer>)) #"bar" elseif(instance?(z, <symbol>)) #"baz" else #"foo" end;
	  if (f(x, y, z) ~= list(x, y, v))
	    signal("%= on %=, %=, %= was wrong.", f, x, y, z)
	  end if
	end method;
  local method tryit-n (f1, f2, f3, y, z)
	  tryit-i(f1, #"foo", y, z);
	  tryit-i(f2, #"bar", y, z);
	  tryit-i(f3, #"baz", y, z);
	end method;
  tryit-n(test-dispatch-regression-5a-1xx, 
	  test-dispatch-regression-5a-2xx,
	  test-dispatch-regression-5a-3xx, 
	  259, <object>);
  tryit-i(test-dispatch-regression-5a-1xx, #"foo", 259, <object>);
  tryit-i(test-dispatch-regression-5a-2xx, #"bar", 259, <object>);
  tryit-i(test-dispatch-regression-5a-3xx, #"baz", 259, <object>);
  tryit-n(test-dispatch-regression-5a-1xx, 
	  test-dispatch-regression-5a-2xx,
	  test-dispatch-regression-5a-3xx, 
	  259, #"frob");
  tryit-i(test-dispatch-regression-5a-1xx, #"foo", 259, #"frob");
  tryit-i(test-dispatch-regression-5a-2xx, #"bar", 259, #"frob");
  tryit-i(test-dispatch-regression-5a-3xx, #"baz", 259, #"frob");
  tryit-n(test-dispatch-regression-5a-1xx, 
	  test-dispatch-regression-5a-2xx,
	  test-dispatch-regression-5a-3xx, 
	  259, -1);
  tryit-i(test-dispatch-regression-5a-1xx, #"foo", 259, -1);
  tryit-i(test-dispatch-regression-5a-2xx, #"bar", 259, -1);
  tryit-i(test-dispatch-regression-5a-3xx, #"baz", 259, -1);
  tryit-n(test-dispatch-regression-5a-1ix, 
	  test-dispatch-regression-5a-2ix,
	  test-dispatch-regression-5a-3ix, 
	  259, <object>);
  tryit-i(test-dispatch-regression-5a-1ix, #"foo", 259, <object>);
  tryit-i(test-dispatch-regression-5a-2ix, #"bar", 259, <object>);
  tryit-i(test-dispatch-regression-5a-3ix, #"baz", 259, <object>);
  tryit-n(test-dispatch-regression-5a-1ix, 
	  test-dispatch-regression-5a-2ix,
	  test-dispatch-regression-5a-3ix, 
	  259, #"frob");
  tryit-i(test-dispatch-regression-5a-1ix, #"foo", 259, #"frob");
  tryit-i(test-dispatch-regression-5a-2ix, #"bar", 259, #"frob");
  tryit-i(test-dispatch-regression-5a-3ix, #"baz", 259, #"frob");
  tryit-n(test-dispatch-regression-5a-1ix, 
	  test-dispatch-regression-5a-2ix,
	  test-dispatch-regression-5a-3ix, 
	  259, -1);
  tryit-i(test-dispatch-regression-5a-1ix, #"foo", 259, -1);
  tryit-i(test-dispatch-regression-5a-2ix, #"bar", 259, -1);
  tryit-i(test-dispatch-regression-5a-3ix, #"baz", 259, -1);
  tryit-n(test-dispatch-regression-5a-1is, 
	  test-dispatch-regression-5a-2is,
	  test-dispatch-regression-5a-3is, 
	  259, #"qwerty");
  tryit-i(test-dispatch-regression-5a-1is, #"foo", 259, #"qwerty");
  tryit-i(test-dispatch-regression-5a-2is, #"bar", 259, #"qwerty");
  tryit-i(test-dispatch-regression-5a-3is, #"baz", 259, #"qwerty");
  tryit-n(test-dispatch-regression-5a-1ii, 
	  test-dispatch-regression-5a-2ii,
	  test-dispatch-regression-5a-3ii, 
	  259, -1);
  tryit-i(test-dispatch-regression-5a-1ii, #"foo", 259, -1);
  tryit-i(test-dispatch-regression-5a-2ii, #"bar", 259, -1);
  tryit-i(test-dispatch-regression-5a-3ii, #"baz", 259, -1);


end method;







// This definition is itself a poor test of class RCPL maintenance.
define class <test-class-5> (<vector>, <test-class-3>)
end class;

define variable *test-class-5* = make(<test-class-5>);

define method tautology (arg == #"more-instance?")
  instance?(*test-class-5*, <vector>) | signal("*test-class-5* isn't <vector>!");
  instance?(*test-class-5*, <collection>) | signal("*test-class-5* isn't <collection>!");
  instance?(*test-class-5*, <sequence>) | signal("*test-class-5* isn't <sequence>!");
  instance?(*test-class-5*, <test-class-3>) | signal("*test-class-5* isn't <test-class-3>!");
  instance?(*test-class-5*, <test-class-5>) | signal("*test-class-5* isn't <test-class-5>!");
  instance?(*test-class-5*, <test-class-2>) | signal("*test-class-5* isn't <test-class-2>!");
  instance?(*test-class-5*, <test-class-1>) | signal("*test-class-5* isn't <test-class-1>!");
end method;



define variable *dynamic-test-class-1* = #f;
define variable *dynamic-test-class-1a* = #f;
define variable *dynamic-test-class-1b* = #f;
define variable *dynamic-test-class-1c* = #f;
define variable *dynamic-test-class-1d* = #f;
define variable *dynamic-test-class-2* = #f;
define variable *dynamic-test-class-3* = #f;

define variable *dynamic-test-class-5* = #f;

/*
define open class <test-class-1> (<object>)
  open slot tc1-islot1, init-value: #"tc1-islot1";
  open slot tc1-islot2, init-value: #"tc1-islot2";
  open slot tc1-islot3, init-value: #"tc1-islot3";
  open class slot tc1-cslot4, init-value: #"tc1-cslot4";
  open class slot tc1-cslot5, init-value: #"tc1-cslot5";
  open each-subclass slot tc1-eslot6, init-value: #"tc1-eslot6";
end class;

define open class <test-class-2> (<object>)
  open slot tc2-islot1, init-value: #"tc2-islot1";
  open slot tc2-islot2, init-value: #"tc2-islot2";
  open each-subclass slot tc2-eslot3, init-value: #"tc2-eslot3";
  open class slot tc2-cslot4, init-value: #"tc2-cslot4";
end class;

define open class <test-class-3> (<test-class-1>, <test-class-2>)
  open slot tc3-islot1, init-value: #"tc3-islot1";
  open each-subclass slot tc3-eslot2, init-value: #"tc3-eslot2";
  open class slot tc3-cslot3, init-value: #"tc3-cslot3";
end class;
*/


define method tautology (arg == #"dynamic-classes")
  // This tests somd dynamic class creation.  It also exercises the dynamic rcpl
  // maintenance code.  The slot stuff and class relationships are identical to those
  // used in the slot tests for statically defined classes;  indeed, we use those same
  // generic functions for simplicity.
  // There are some extra tests to check against bugs in RCPL maintenance, which are
  // easier to do with dynamicly created classes by virtue of not being done at
  // compile time.
  
  *dynamic-test-class-1* := make(<class>,
				 superclasses: vector(<object>),
				 debug-name: "<dynamic-test-class-1>",
				 slots: list(list(getter: tc1-islot1, setter: tc1-islot1-setter,
						  init-value: #"tc1-islot1"),
					     list(getter: tc1-islot2, setter: tc1-islot2-setter,
						  init-value: #"tc1-islot2"),
					     list(getter: tc1-islot3, setter: tc1-islot3-setter,
						  init-value: #"tc1-islot3"),
					     list(getter: tc1-cslot4, setter: tc1-cslot4-setter,
						  allocation: #"class",
						  init-value: #"tc1-cslot4"),
					     list(getter: tc1-cslot5, setter: tc1-cslot5-setter,
						  allocation: #"class",
						  init-value: #"tc1-cslot5"),
					     list(getter: tc1-eslot6, setter: tc1-eslot6-setter,
						  allocation: #"each-subclass",
						  init-value: #"tc1-eslot6")));
  
  // The next four classes (1a, 1b, 1c, 1d) are to do a regression on a problem with
  // inserting an rcpl position in dynamically.  Defining 1d causes the positions of 1 to
  // be [1, 4] - this we have to insert 2 when we define class 3.
  *dynamic-test-class-1a* := make(<class>,
				  superclasses: vector(<object>),
				  debug-name: "<dynamic-test-class-1a>",
				  slots: #());
  *dynamic-test-class-1b* := make(<class>,
				  superclasses: vector(<object>),
				  debug-name: "<dynamic-test-class-1b>",
				  slots: #());
  *dynamic-test-class-1c* := make(<class>,
				  superclasses: vector(<object>),
				  debug-name: "<dynamic-test-class-1c>",
				  slots: #());
  *dynamic-test-class-1d* := make(<class>,
				  superclasses: vector(*dynamic-test-class-1*,
						       *dynamic-test-class-1a*,
						       *dynamic-test-class-1b*,
						       *dynamic-test-class-1c*),
				  debug-name: "<dynamic-test-class-1d>",
				  slots: #());
  *dynamic-test-class-2* := make(<class>,
				 superclasses: vector(<object>),
				 debug-name: "<dynamic-test-class-2>",
				 slots: list(list(getter: tc2-islot1, setter: tc2-islot1-setter,
						  init-value: #"tc2-islot1"),
					     list(getter: tc2-islot2, setter: tc2-islot2-setter,
						  init-value: #"tc2-islot2"),
					     list(getter: tc2-eslot3, setter: tc2-eslot3-setter,
						  allocation: #"each-subclass",
						  init-value: #"tc2-eslot3"),
					     list(getter: tc2-cslot4, setter: tc2-cslot4-setter,
						  allocation: #"class",
						  init-value: #"tc2-cslot4")));
  *dynamic-test-class-3* := make(<class>,
				 debug-name: "<dynamic-test-class-3>",
				 superclasses: vector(*dynamic-test-class-1*, *dynamic-test-class-2*),
				 slots: list(list(getter: tc3-islot1, setter: tc3-islot1-setter,
						  init-value: #"tc3-islot1"),
					     list(getter: tc3-eslot2, setter: tc3-eslot2-setter,
						  allocation: #"each-subclass",
						  init-value: #"tc3-eslot2"),
					     list(getter: tc3-cslot3, setter: tc3-cslot3-setter,
						  allocation: #"class",
						  init-value: #"tc3-cslot3")));
  // 1's CPL   = [1, <object>]
  // 2's CPL   = [2, <object>]
  // 3's CPL   = [3, 1, 2, <object>].
  // -- Now, 1's other-positions should be [2].

  subtype?(*dynamic-test-class-3*, *dynamic-test-class-2*) | signal("Dynamic 3 not subtype of 2");
  subtype?(*dynamic-test-class-3*, *dynamic-test-class-1*) | signal("Dynamic 3 not subtype of 1");
  ~subtype?(*dynamic-test-class-2*, *dynamic-test-class-1*) | signal("Dynamic 2 is subtype of 1!");
  ~subtype?(*dynamic-test-class-1*, *dynamic-test-class-2*) | signal("Dynamic 1 is subtype of 2!");
  ~subtype?(*dynamic-test-class-2*, *dynamic-test-class-3*) | signal("Dynamic 2 is subtype of 3!");
  ~subtype?(*dynamic-test-class-1*, *dynamic-test-class-3*) | signal("Dynamic 1 is subtype of 3!");
  subtype?(*dynamic-test-class-1d*, *dynamic-test-class-1*) | signal("Dynamic 1d not subtype of 1!");
  

  let tc1-1 = make(*dynamic-test-class-1*);
  let tc1-2 = make(*dynamic-test-class-1*);
  let tc2-1 = make(*dynamic-test-class-2*);
  let tc2-2 = make(*dynamic-test-class-2*);
  let tc3-1 = make(*dynamic-test-class-3*);
  let tc3-2 = make(*dynamic-test-class-3*);

  instance?(tc1-1, *dynamic-test-class-1*) | signal("dynamic tc1-1 not instance? of 1!");
  ~instance?(tc1-1, *dynamic-test-class-2*) | signal("dynamic tc1-1 is instance? of 2!");
  ~instance?(tc1-1, *dynamic-test-class-3*) | signal("dynamic tc1-1 is instance? of 3!");

  instance?(tc2-1, *dynamic-test-class-2*) | signal("dynamic tc2-1 not instance? of 2!");
  ~instance?(tc2-1, *dynamic-test-class-1*) | signal("dynamic tc2-1 is instance? of 1!");
  ~instance?(tc2-1, *dynamic-test-class-3*) | signal("dynamic tc2-1 is instance? of 3!");

  instance?(tc3-1, *dynamic-test-class-1*) | signal("dynamic tc3-1 not instance? of 1!");
  instance?(tc3-1, *dynamic-test-class-2*) | signal("dynamic tc3-1 not instance? of 2!");
  instance?(tc3-1, *dynamic-test-class-3*) | signal("dynamic tc3-1 not instance? of 3!");

  let slottest = method (slotgf, inst, str, sym)
		   // do (method (i)
			 let val = slotgf(inst);
			 if (val ~== sym)
			   signal("%= of %= was %=", sym, str, val)
			 end if
		  //     end method,
		  //     range(from: 0, below: 10000))
		 end method;

  slottest(tc1-islot1, tc1-1, "initial tc1-islot1(tc1-1)", #"tc1-islot1");
  slottest(tc1-islot2, tc1-1, "initial tc1-islot2(tc1-1)", #"tc1-islot2");
  slottest(tc1-islot3, tc1-1, "initial tc1-islot3(tc1-1)", #"tc1-islot3");

  slottest(tc2-islot1, tc2-1, "initial tc2-islot1(tc2-1)", #"tc2-islot1");
  slottest(tc2-islot2, tc2-1, "initial tc2-islot2(tc2-1)", #"tc2-islot2");

  slottest(tc1-islot1, tc3-1, "initial tc1-islot1(tc3-1)", #"tc1-islot1");
  slottest(tc1-islot2, tc3-1, "initial tc1-islot2(tc3-1)", #"tc1-islot2");
  slottest(tc1-islot3, tc3-1, "initial tc1-islot3(tc3-1)", #"tc1-islot3");
  slottest(tc2-islot1, tc3-1, "initial tc2-islot1(tc3-1)", #"tc2-islot1");
  slottest(tc2-islot2, tc3-1, "initial tc2-islot2(tc3-1)", #"tc2-islot2");
  slottest(tc3-islot1, tc3-1, "initial tc3-islot1(tc3-1)", #"tc3-islot1");

  slottest(tc1-islot1, tc1-2, "initial tc1-islot1(tc1-2)", #"tc1-islot1");
  slottest(tc1-islot2, tc1-2, "initial tc1-islot2(tc1-2)", #"tc1-islot2");
  slottest(tc1-islot3, tc1-2, "initial tc1-islot3(tc1-2)", #"tc1-islot3");

  slottest(tc2-islot1, tc2-2, "initial tc2-islot1(tc2-2)", #"tc2-islot1");
  slottest(tc2-islot2, tc2-2, "initial tc2-islot2(tc2-2)", #"tc2-islot2");

  slottest(tc1-islot1, tc3-2, "initial tc1-islot1(tc3-2)", #"tc1-islot1");
  slottest(tc1-islot2, tc3-2, "initial tc1-islot2(tc3-2)", #"tc1-islot2");
  slottest(tc1-islot3, tc3-2, "initial tc1-islot3(tc3-2)", #"tc1-islot3");
  slottest(tc2-islot1, tc3-2, "initial tc2-islot1(tc3-2)", #"tc2-islot1");
  slottest(tc2-islot2, tc3-2, "initial tc2-islot2(tc3-2)", #"tc2-islot2");
  slottest(tc3-islot1, tc3-2, "initial tc3-islot1(tc3-2)", #"tc3-islot1");

///

  slottest(tc1-cslot4, tc1-1, "initial tc1-cslot4(tc1-1)", #"tc1-cslot4");
  slottest(tc1-cslot5, tc1-1, "initial tc1-cslot5(tc1-1)", #"tc1-cslot5");
  slottest(tc1-eslot6, tc1-1, "initial tc1-eslot6(tc1-1)", #"tc1-eslot6");

  slottest(tc2-eslot3, tc2-1, "initial tc2-eslot3(tc2-1)", #"tc2-eslot3");
  slottest(tc2-cslot4, tc2-1, "initial tc2-cslot4(tc2-1)", #"tc2-cslot4");

  slottest(tc1-cslot4, tc3-1, "initial tc1-cslot4(tc3-1)", #"tc1-cslot4");
  slottest(tc1-cslot5, tc3-1, "initial tc1-cslot5(tc3-1)", #"tc1-cslot5");
  slottest(tc1-eslot6, tc3-1, "initial tc1-eslot6(tc3-1)", #"tc1-eslot6");
  slottest(tc2-eslot3, tc3-1, "initial tc2-eslot3(tc3-1)", #"tc2-eslot3");
  slottest(tc2-cslot4, tc3-1, "initial tc2-cslot4(tc3-1)", #"tc2-cslot4");
  slottest(tc3-eslot2, tc3-1, "initial tc3-eslot2(tc3-1)", #"tc3-eslot2");
  slottest(tc3-cslot3, tc3-1, "initial tc3-cslot3(tc3-1)", #"tc3-cslot3");

  slottest(tc1-cslot4, tc1-2, "initial tc1-cslot4(tc1-2)", #"tc1-cslot4");
  slottest(tc1-cslot5, tc1-2, "initial tc1-cslot5(tc1-2)", #"tc1-cslot5");
  slottest(tc1-eslot6, tc1-2, "initial tc1-eslot6(tc1-2)", #"tc1-eslot6");

  slottest(tc2-eslot3, tc2-2, "initial tc2-eslot3(tc2-2)", #"tc2-eslot3");
  slottest(tc2-cslot4, tc2-2, "initial tc2-cslot4(tc2-2)", #"tc2-cslot4");

  slottest(tc1-cslot4, tc3-2, "initial tc1-cslot4(tc3-2)", #"tc1-cslot4");
  slottest(tc1-cslot5, tc3-2, "initial tc1-cslot5(tc3-2)", #"tc1-cslot5");
  slottest(tc1-eslot6, tc3-2, "initial tc1-eslot6(tc3-2)", #"tc1-eslot6");
  slottest(tc2-eslot3, tc3-2, "initial tc2-eslot3(tc3-2)", #"tc2-eslot3");
  slottest(tc2-cslot4, tc3-2, "initial tc2-cslot4(tc3-2)", #"tc2-cslot4");
  slottest(tc3-eslot2, tc3-2, "initial tc3-eslot2(tc3-2)", #"tc3-eslot2");
  slottest(tc3-cslot3, tc3-2, "initial tc3-cslot3(tc3-2)", #"tc3-cslot3");

  // *** Propagation of class slot values (tc1-4, tc1-5, tc2-4, tc3-3).
  tc1-cslot4(tc3-1) := #"tc1-cslot4a";
  slottest(tc1-cslot4, tc3-1, "second tc1-cslot4(tc3-1)", #"tc1-cslot4a");
  slottest(tc1-cslot4, tc3-2, "second tc1-cslot4(tc3-2)", #"tc1-cslot4a");
  slottest(tc1-cslot4, tc1-1, "second tc1-cslot4(tc1-1)", #"tc1-cslot4a");
  slottest(tc1-cslot4, tc1-2, "second tc1-cslot4(tc1-2)", #"tc1-cslot4a");

  tc2-cslot4(tc2-2) := #"tc2-cslot4a";
  slottest(tc2-cslot4, tc2-1, "third tc2-cslot4(tc2-1)", #"tc2-cslot4a");
  slottest(tc2-cslot4, tc2-2, "third tc2-cslot4(tc2-2)", #"tc2-cslot4a");
  slottest(tc2-cslot4, tc3-1, "third tc2-cslot4(tc3-1)", #"tc2-cslot4a");
  slottest(tc2-cslot4, tc3-2, "third tc2-cslot4(tc3-2)", #"tc2-cslot4a");

  // . . .

  // *** Non-propagation of each-subclass slot values (tc1-6 tc2-3 tc3-2)
  tc1-eslot6(tc1-1) := #"tc1-eslot6a";
  slottest(tc1-eslot6, tc1-1, "fourth tc1-eslot6(tc1-1)", #"tc1-eslot6a");
  slottest(tc1-eslot6, tc1-2, "fourth tc1-eslot6(tc1-2)", #"tc1-eslot6a");
  slottest(tc1-eslot6, tc3-1, "fourth tc1-eslot6(tc3-1)", #"tc1-eslot6");
  slottest(tc1-eslot6, tc3-2, "fourth tc1-eslot6(tc3-2)", #"tc1-eslot6");

  tc2-eslot3(tc3-1) := #"tc2-eslot3a";
  slottest(tc2-eslot3, tc2-1, "fifth tc2-eslot3(tc2-1)", #"tc2-eslot3");
  slottest(tc2-eslot3, tc2-1, "fifth tc2-eslot3(tc2-1)", #"tc2-eslot3");
  slottest(tc2-eslot3, tc3-1, "fifth tc2-eslot3(tc3-1)", #"tc2-eslot3a");
  slottest(tc2-eslot3, tc3-1, "fifth tc2-eslot3(tc3-1)", #"tc2-eslot3a");


end method;

//// Tests for a single keyword being associated with more than one
//// #key variables.

// These are variables to defeat compile-time keyword processing.

define variable duplicated-keyword-processing-stooge-1
  = method (#key a: a1 = 1, a: a2 = 2, a: a3 = 3)
      values(a1, a2, a3);
    end;

define variable duplicated-keyword-processing-stooge-2
  = method (#key a: a1 = 1, a: a2 = a1 + 1, a: a3 = a2 + 1)
      values(a1, a2, a3);
    end;

define method tautology (arg == #"duplicated-keyword-processing")
  let (a1, a2, a3) = duplicated-keyword-processing-stooge-1();
  a1 == 1 | signal("simple defaulted a: in a1 is not 1! It's %=\n", a1);
  a2 == 2 | signal("simple defaulted a: in a2 is not 2! It's %=\n", a2);
  a3 == 3 | signal("simple defaulted a: in a3 is not 3! It's %=\n", a3);
  let (a1, a2, a3) = duplicated-keyword-processing-stooge-1(a: 10);
  a1 == 10 | signal("simple supplied a: in a1 is not 10! It's %=\n", a1);
  a2 == 10 | signal("simple supplied a: in a2 is not 10! It's %=\n", a2);
  a3 == 10 | signal("simple supplied a: in a3 is not 10! It's %=\n", a3);
  let (a1, a2, a3) = duplicated-keyword-processing-stooge-2();
  a1 == 1 | signal("complex defaulted a: in a1 is not 1! It's %=\n", a1);
  a2 == 2 | signal("complex defaulted a: in a2 is not 2! It's %=\n", a2);
  a3 == 3 | signal("complex defaulted a: in a3 is not 3! It's %=\n", a3);
  let (a1, a2, a3) = duplicated-keyword-processing-stooge-2(a: 10);
  a1 == 10 | signal("complex supplied a: in a1 is not 10! It's %=\n", a1);
  a2 == 10 | signal("complex supplied a: in a2 is not 10! It's %=\n", a2);
  a3 == 10 | signal("complex supplied a: in a3 is not 10! It's %=\n", a3);
end method;



tautologies := concatenate(tautologies, #(#"floats",
					  #"symbols",
					  #"arrays",
					  #"deques",
					  #"tables",
					  #"degenerate-dispatch",
					  #"degenerate-typed-dispatch",
					  #"if-discriminating-dispatch",
					  #"discriminating-dispatch-hairier",
					  #"if-discriminating-static-dispatch",
					  #"dispatch-regression-1",
					  #"dispatch-regression-2",
					  #"dispatch-regression-3",
					  #"dispatch-regression-4",
					  #"dispatch-regression-5a",
					  #"more-instance?",
					  #"dynamic-classes",
					  #"duplicated-keyword-processing"));

// eof
