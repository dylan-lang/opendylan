Module:    c-ffi-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define c-function increment-unsigned-char
  parameter x :: <c-unsigned-char>;
  result r :: <c-unsigned-char>;
  c-name: "incr_unsigned_char";
end;

define c-function increment-signed-char
  parameter x :: <c-signed-char>;
  result r :: <c-signed-char>;
  c-name: "incr_signed_char";
end;

define c-function increment-unsigned-short
  parameter x :: <c-unsigned-short>;
  result r :: <c-unsigned-short>;
  c-name: "incr_unsigned_short";
end;

define c-function increment-signed-short
  parameter x :: <c-signed-short>;
  result r :: <c-signed-short>;
  c-name: "incr_signed_short";
end;

define c-function increment-size-t
  parameter x :: <c-size-t>;
  result r :: <c-size-t>;
  c-name: "incr_size_t";
end;

define c-function increment-ssize-t
  parameter x :: <c-ssize-t>;
  result r :: <c-ssize-t>;
  c-name: "incr_ssize_t";
end;

/// Try some C functions in some different ways.
/// this could always use more tests.
define test c-function-test ()
  for (i from 0 below (ash(1, 8 * size-of(<C-unsigned-char>)) - 2) by 17)
    check-equal("c-function unsigned char increment",
                increment-unsigned-char(i), i + 1);
  end;

  for (i from 0 below (ash(1, (8 * size-of(<C-signed-char>)) - 1) - 2) by 17)
    check-equal("c-function signed char increment",
                increment-signed-char(i), i + 1);
    check-equal("c-function signed char negative increment",
                increment-signed-char(- i), (- i) + 1);
  end;
  for (i from 0 below (ash(1, 8 * size-of(<C-unsigned-char>)) - 2) by 17)
    check-equal("c-function unsigned short increment",
                increment-unsigned-short(i), i + 1);
  end;
  for (i from 0 below (ash(1, (8 * size-of(<C-signed-char>)) - 1) - 2) by 17)
    check-equal("c-function signed short increment",
                increment-signed-short(i), i + 1);
    check-equal("c-function signed short negative increment",
                increment-signed-short(- i), (- i) + 1);
  end;
  for (i from 0 below (ash(1, 8 * size-of(<C-unsigned-char>)) - 2) by 17)
    check-equal("c-function size_t increment",
                increment-size-t(i), i + 1);
  end;
  for (i from 0 below (ash(1, (8 * size-of(<C-signed-char>)) - 1) - 2) by 17)
    check-equal("c-function ssize_t increment",
                increment-ssize-t(i), i + 1);
    check-equal("c-function ssize_t negative increment",
                increment-ssize-t(- i), (- i) + 1);
  end;
  // !@#$ do more here
end;

//
// -------------
// tests for indirect: option to define c-function

define c-function call-indirect-0
  indirect: #t;
  result val :: <C-int>;
end;

define c-function gimme-a-function-0
  result fun :: <C-function-pointer>;
  c-name: "gimme_a_function_0";
end;

define c-function call-indirect-1
  indirect: #t;
  parameter param1 :: <C-int>;
  result val :: <C-int>;
end;

define c-function gimme-a-function-1
  result fun :: <C-function-pointer>;
  c-name: "gimme_a_function_1";
end;

define c-function call-indirect-2
  indirect: #t;
  parameter param1 :: <C-int>;
  parameter param2 :: <C-int>;
  result val :: <C-int>;
end;

define c-function gimme-a-function-2
  result fun :: <C-function-pointer>;
  c-name: "gimme_a_function_2";
end;


define test c-function-indirect ()
  let fun-0 = gimme-a-function-0();
  let fun-1 = gimme-a-function-1();
  let fun-2 = gimme-a-function-2();
  check-equal("c-function indirect option, 0 args", call-indirect-0(fun-0), 5);
  check-equal("c-function indirect option, 1 arg",  call-indirect-1(fun-1, 7), 7);
  check-equal("c-function indirect option, 2 args", call-indirect-2(fun-2, 4, 5), 9);
end;

//-------
// for bug 217

define constant <SQLRETURN> = <C-short>;
define constant <SQLHENV> = <C-void*>;
define C-pointer-type <LPSQLHENV> => <SQLHENV>;

define C-function SQLAllocEnv
  output parameter EnvironmentHandle :: <LPSQLHENV>;
  result value :: <SQLRETURN>;
  c-name: "SQLAllocEnv", c-modifiers: "__stdcall";
end;

define C-function SQLFreeEnv
  parameter EnvironmentHandle :: <SQLHENV>;
  result value :: <SQLRETURN>;
  c-name: "SQLFreeEnv", c-modifiers: "__stdcall";
end;

define test bug-217 ()
  let (success :: <integer>, henv :: <SQLHENV>) = SQLAllocEnv();
  check-equal("bug 217 got correct success code", 37, success);
  check-instance?("bug 217 henv should be a <SQLHENV>", <SQLHENV>, henv);
  SQLFreeEnv(henv);
end;

//
// -------------
// tests for bug 321

define c-union <union-test>
  slot union-1 :: <c-unsigned-short>;
  slot union-2 :: <C-signed-short>;
  slot union-3 :: <C-unsigned-char>;
  slot union-4 :: <C-signed-char>;
end;

define c-struct <union-test-2>
  slot struct-pad-1 :: <C-char>, getter: #f, setter: #f;
  slot union-struct :: <union-test>;
  slot struct-pad-2 :: <C-char>, getter: #f, setter: #f;
  pointer-type-name: <union-test-2*>;
end;
ignorable(union-struct, union-struct-setter);

define c-function union-tester-1
  parameter arg :: <union-test-2*>;
  result r :: <C-unsigned-short>;
  c-name: "union_tester_1";
end;

define c-function union-tester-1-setter
  parameter new :: <C-unsigned-short>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_1_setter"
end;

define c-function union-tester-2
  parameter arg :: <union-test-2*>;
  result r :: <C-signed-short>;
  c-name: "union_tester_2";
end;

define c-function union-tester-2-setter
  parameter new :: <C-signed-short>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_2_setter"
end;


define c-function union-tester-3
  parameter arg :: <union-test-2*>;
  result r :: <C-unsigned-char>;
  c-name: "union_tester_3";
end;

define c-function union-tester-3-setter
  parameter new :: <C-unsigned-char>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_3_setter"
end;



define c-function union-tester-4
  parameter arg :: <union-test-2*>;
  result r :: <C-signed-char>;
  c-name: "union_tester_4";
end;

define c-function union-tester-4-setter
  parameter new :: <C-signed-char>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_4_setter"
end;


define test bug-321 ()
  let struct = make(<union-test-2*>);
  union-struct(struct).union-1 := #xffff;
  check-equal("bug 321 accessor value 1, #xffff",
              #xffff,
              union-struct(struct).union-1);
  check-equal("bug 321 c-func value 1, #xffff",
              #xffff,
              union-tester-1(struct));
  union-tester-1(struct) := #xffff;
  check-equal("bug 321 accessor second value 1, #xffff",
              #xffff,
              union-struct(struct).union-1);
  check-equal("bug 321 c-func second value 1, #xffff",
              union-tester-1(struct),
              #xffff);
  union-struct(struct).union-1 := 0;
  check-equal("bug 321 accessor value 1, 0",
              0,
              union-struct(struct).union-1);
  check-equal("bug 321 c-func value 1, 0",
              0,
              union-tester-1(struct));
  union-tester-1(struct) := 0;
  check-equal("bug 321 accessor second value 1, 0",
              0,
              union-struct(struct).union-1);
  check-equal("bug 321 c-func second value 1, 0",
              0,
              union-tester-1(struct));

  union-struct(struct).union-2 := -1;
  check-equal("bug 321 accessor value 2, -1",
              -1,
              union-struct(struct).union-2);
  check-equal("bug 321 c-func value 2, -1",
              -1,
              union-tester-2(struct));
  union-tester-2(struct) := -1;
  check-equal("bug 321 accessor second value 2, -1",
              -1,
              union-struct(struct).union-2);
  check-equal("bug 321 c-func second value 2, -1",
              -1,
              union-tester-2(struct));
  union-struct(struct).union-2 := 0;
  check-equal("bug 321 accessor value 2, 0",
              0,
              union-struct(struct).union-2);
  check-equal("bug 321 c-func value 2, 0",
              union-tester-2(struct),
              0);
  union-tester-2(struct) := 0;
  check-equal("bug 321 accessor second value 2, 0",
              0,
              union-struct(struct).union-2);
  check-equal("bug 321 c-func second value 2, 0",
              0,
              union-tester-2(struct));

  union-struct(struct).union-3 := #xff;
  check-equal("bug 321 accessor value 3, #xff",
              #xff,
              union-struct(struct).union-3);
  check-equal("bug 321 c-func value 3, #xff",
              #xff,
              union-tester-3(struct));
  union-tester-3(struct) := #xff;
  check-equal("bug 321 accessor second value 3, #xff",
              #xff,
              union-struct(struct).union-3);
  check-equal("bug 321 c-func second value 3, #xff",
              #xff,
              union-tester-3(struct));
  union-struct(struct).union-3 := 0;
  check-equal("bug 321 accessor value 3, 0",
              0,
              union-struct(struct).union-3);
  check-equal("bug 321 c-func value 3, 0",
              0,
              union-tester-3(struct));
  union-tester-3(struct) := 0;
  check-equal("bug 321 accessor second value 3, 0",
              0,
              union-struct(struct).union-3);
  check-equal("bug 321 c-func second value 3, 0",
              0,
              union-tester-3(struct));

  union-struct(struct).union-4 := -1;
  check-equal("bug 321 accessor value 4, -1",
              -1,
              union-struct(struct).union-4);
  check-equal("bug 321 c-func value 4, -1",
              -1,
              union-tester-4(struct));
  union-tester-4(struct) := -1;
  check-equal("bug 321 accessor second value 4, -1",
              -1,
              union-struct(struct).union-4);
  check-equal("bug 321 c-func second value 4, -1",
              -1,
              union-tester-4(struct));
  union-struct(struct).union-4 := 0;
  check-equal("bug 321 accessor value 4, 0",
              union-struct(struct).union-4,
              0);
  check-equal("bug 321 c-func value 4, 0",
              0,
              union-tester-4(struct));
  union-tester-4(struct) := 0;
  check-equal("bug 321 accessor second value 4, 0",
              0,
              union-struct(struct).union-4);
  check-equal("bug 321 c-func second value 4, 0",
              0,
              union-tester-4(struct));
end;

//
// -------------
// tests for bug 313

define c-function mix-it-up
  input output parameter inout :: <C-int*>;
  result value :: <C-int>;
  c-name: "mix_it_up";
end;

define test bug-313 ()
  let (a :: <integer>, b :: <integer>) = mix-it-up(7);
  check-equal("bug 313  mix it up value 1", a, 7 + 1);
  check-equal("bug 313  mix it up value 2", b, 7 + 7);
end;

