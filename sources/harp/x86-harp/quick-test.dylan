module: x86-harp-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Some additional tests in addition to those in dummy-tests.
// Each test ends up in test3.obj.

define method quick-test 
     (internal-test :: <function>, 
      #rest all-keys, 
      #key type, 
           print-harp? = #t,
           data-function = output-first-literal) => ()
  do-file-test(make(<harp-x86-back-end>),
               "test3",
               apply(vector, print-harp?: print-harp?, all-keys),
               data-function,
               return-first-literal(static: #t),
               call-internal-test(),
               internal-test(static: #t));
end method;

/* Example call:-

quick-test(qtest1);


*/


define variable *test-name* = "the_internal_test_iep";

define variable first-lit = ins--constant-ref(pb, "first_lit");
define variable second-lit = ins--constant-ref(pb, "second_lit");


define method output-first-literal (be :: <harp-x86-back-end>, outputter)
  output-definition(be, outputter, first-lit);
  output-data-byte(be, outputter, 1);
  output-data-byte(be, outputter, 2);
  output-data-byte(be, outputter, 3);
  output-data-byte(be, outputter, 4);
  output-data-byte(be, outputter, 5);
  output-data-byte(be, outputter, 6);
  output-data-byte(be, outputter, 7);
  output-data-byte(be, outputter, 8);
  output-data-byte(be, outputter, 9);
  output-data-byte(be, outputter, 10);
  output-data-byte(be, outputter, 11);
  output-data-byte(be, outputter, 12);
  output-data-byte(be, outputter, 13);
  output-data-byte(be, outputter, 14);
end method;

define method output-second-literal (be :: <harp-x86-back-end>, outputter)
  output-definition(be, outputter, second-lit);
  output-data-byte(be, outputter, 1);
  output-data-byte(be, outputter, 2);
  output-data-byte(be, outputter, 3);
  output-data-byte(be, outputter, 4);
  output-data-byte(be, outputter, 5);
  output-data-byte(be, outputter, 6);
  output-data-byte(be, outputter, 7);
  output-data-byte(be, outputter, 8);
  output-data-byte(be, outputter, 9);
  output-data-byte(be, outputter, 10);
  output-data-byte(be, outputter, 99);  // gratuitous diference
  output-data-byte(be, outputter, 12);
  output-data-byte(be, outputter, 13);
  output-data-byte(be, outputter, 14);
end method;



define method output-two-literals (be :: <harp-x86-back-end>, outputter)
  output-first-literal(be, outputter);
  output-second-literal(be, outputter);
end method;


define lambda-test call-internal-test (be :: <harp-x86-back-end>)
  name "lit1_caller_iep"; 
  start-line 0;
  end-line 0;

  ins--call(be, ins--constant-ref(be, *test-name*), 0);
  ins--rts(be);
end;

define lambda-test return-first-literal (be :: <harp-x86-back-end>)
  name "first_lit_iep"; 
  start-line 5; 
  end-line 10;
  let regs = be.registers;
  ins--scl(be, scl(1));
  ins--move(be, regs.reg-result, first-lit);
  ins--scl(be, scl(3));
  ins--rts-and-drop(be, 0);
end;
  






define lambda-test qtest0 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    result result;
    named reg v0, v1, v2, v3, v4, v5;
    tag loop;

    ins--tag(be, loop);
    ins--move(be, v0, 0);
    ins--move(be, v1, 1);
    ins--move(be, v2, 2);
    ins--move(be, v3, 3);
    ins--move(be, v4, 4);
    ins--move(be, v5, 5);
    ins--scl(be, scl(2), v0, v1, v2, v3, v4, v5);
    ins--move(be, v0, -1);
    ins--move(be, v1, -1);
    ins--move(be, v2, -1);
    ins--move(be, v3, -1);
    ins--move(be, v4, -1);
    ins--move(be, v5, -1);
    ins--scl(be, scl(3), v0, v1, v2, v3, v4, v5);
    ins--bra(be, loop);

    ins--rts-and-drop(be, 0);
  end with-harp;
end;


define lambda-test qtest1 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    named reg zero, the-result;
    named reg eight, low, high;
    named reg l1, l2, l3, cc1, cc2;
    result result;
    tag ov1, ov2, done1, done2;

    ins--move(be, zero, 0);
    ins--scl(be, scl(2), zero);
    ins--call(be, ins--constant-ref(be, "first_lit_iep"), 0);
    ins--move(be, the-result, result);
    ins--scl(be, scl(3), zero, the-result);

    ins--move(be, the-result, #x0fffffff);
    ins--move(be, eight, 8);
    ins--aslx(be, low, high, the-result, eight);
    ins--scl(be, scl(4), the-result, eight, low, high);

    ins--move(be, the-result, #xffffffff);
    ins--aslx(be, low, high, the-result, 8);
    ins--scl(be, scl(4), the-result, eight, low, high);

    ins--move(be, l1, #x00ffffff);
    ins--aslxv(be, ov1, l2, #f, l1, 7);
    ins--move(be, cc1, 1);
    ins--bra(be, done1);
    ins--tag(be, ov1);
    ins--move(be, cc1, 0);
    ins--tag(be, done1);
    ins--scl(be, scl(6), l2, cc1);

    ins--aslxv(be, ov2, l3, #f, l2, cc1);
    ins--move(be, cc2, 1);
    ins--bra(be, done2);
    ins--tag(be, ov2);
    ins--move(be, cc2, 0);
    ins--tag(be, done2);
    ins--scl(be, scl(7), l3, cc2);

    ins--move(be, eax, #x00ffff00);
    ins--move(be, eax, eax);
    ins--move(be, eax, eax);
    ins--move(be, eax, eax);
    ins--asl-trap(be, l2, eax, 7);
    ins--move(be, cc1, 1);
    ins--scl(be, scl(8), l2, cc1);

    ins--asl-trap(be, l3, l2, cc1);
    ins--move(be, cc2, 1);
    ins--scl(be, scl(9), l3, cc2);

    ins--move(be, result, the-result);
    ins--scl(be, scl(10), the-result);
    ins--rts-and-drop(be, 0);
  end with-harp;
end;



define lambda-test qtest2 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    named reg sum, carry, x, y, dummy1, dummy2;

    ins--move(be, dummy1, 0); // use up some regs to force a frame
    ins--move(be, dummy2, 0); // use up some regs to force a frame

    ins--move(be, x, 1);
    ins--move(be, y, -1);
    ins--addcx(be, sum, carry, x, y);
    ins--scl(be, scl(2), sum, carry, x, y);

    ins--move(be, x, 1);
    ins--move(be, y, -1);
    ins--subcx(be, sum, carry, x, y);
    ins--scl(be, scl(3), sum, carry, x, y);

    ins--move(be, x, 1);
    ins--move(be, y, 0);
    ins--addcx(be, sum, carry, x, y);
    ins--scl(be, scl(4), sum, carry, x, y);

    ins--move(be, x, 1);
    ins--move(be, y, 0);
    ins--subcx(be, sum, carry, x, y);
    ins--scl(be, scl(5), sum, carry, x, y, dummy1, dummy2);

    ins--rts-and-drop(be, 0);
  end with-harp;
end;



define lambda-test qtest3 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    named reg dividend, divisor, quot, rem;

    ins--move(be, dividend, 5);
    ins--move(be, divisor, 2);
    ins--roundx(be, quot, #f, dividend, divisor);
    ins--scl(be, scl(2), dividend, divisor, quot);

    ins--move(be, dividend, 5);
    ins--move(be, divisor, 2);
    ins--roundx(be, #f, rem, dividend, divisor);
    ins--scl(be, scl(3), dividend, divisor, rem);

    ins--move(be, dividend, 5);
    ins--move(be, divisor, 2);
    ins--roundx(be, quot, rem, dividend, divisor);
    ins--scl(be, scl(4), dividend, divisor, quot, rem);

    ins--move(be, dividend, -5);
    ins--move(be, divisor, 2);
    ins--roundx(be, quot, rem, dividend, divisor);
    ins--scl(be, scl(5), dividend, divisor, quot, rem);

    ins--move(be, dividend, 5);
    ins--move(be, divisor, -2);
    ins--roundx(be, quot, rem, dividend, divisor);
    ins--scl(be, scl(6), dividend, divisor, quot, rem);

    ins--move(be, dividend, -5);
    ins--move(be, divisor, -2);
    ins--roundx(be, quot, rem, dividend, divisor);
    ins--scl(be, scl(7), dividend, divisor, quot, rem);

    ins--move(be, dividend, 0);
    ins--move(be, divisor, 2);
    ins--roundx(be, quot, rem, dividend, divisor);
    ins--scl(be, scl(8), dividend, divisor, quot, rem);

    ins--move(be, dividend, 0);
    ins--move(be, divisor, -2);
    ins--roundx(be, quot, rem, dividend, divisor);
    ins--scl(be, scl(9), dividend, divisor, quot, rem);

    ins--rts-and-drop(be, 0);
  end with-harp;
end;





define lambda-test qtest4 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    named reg base, index, data;

    ins--move(be, base, first-lit);
    ins--scl(be, scl(2), base);

    ins--move(be, index, 1);
    ins--ldh-index-scaled-signed(be, data, base, index, 4);
    ins--scl(be, scl(3), base, index, data);

    ins--move(be, index, 2);
    ins--ldh-index-scaled-signed(be, data, base, 2, 4);
    ins--scl(be, scl(4), base, index, data);

    ins--move(be, index, 1);
    ins--ldh-index-scaled-signed(be, data, first-lit, index, 4);
    ins--scl(be, scl(5), base, index, data);

    ins--move(be, data, #xffffffff);
    ins--move(be, index, 0);
    ins--st-index-scaled(be, data, base, index, 0);
    ins--scl(be, scl(6), base, index, data);

    ins--move(be, data, #x0f0f0f0f);
    ins--move(be, index, 2);
    ins--st-index-scaled(be, data, base, 2, 0);
    ins--scl(be, scl(7), base, index, data);

    ins--move(be, data, #x0);
    ins--move(be, index, 1);
    ins--st-index-scaled(be, data, base, index, 4);
    ins--scl(be, scl(8), base, index, data);

    ins--move(be, data, first-lit);
    ins--move(be, index, 1);
    ins--st-index-scaled(be, first-lit, first-lit, index, 4);
    ins--scl(be, scl(9), base, index, data);

    ins--rts-and-drop(be, 0);
  end with-harp;
end;



define lambda-test qtest5 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    named reg base, index;
    named dfreg data;

    ins--move(be, base, first-lit);
    ins--scl(be, scl(2), base);

    ins--move(be, index, 1);
    ins--dld-index-scaled(be, data, base, index, 4);
    ins--scl(be, scl(3), base, index, data);

    ins--move(be, index, 2);
    ins--dld-index-scaled(be, data, base, 2, 4);
    ins--scl(be, scl(4), base, index, data);

    ins--move(be, index, 1);
    ins--dld-index-scaled(be, data, first-lit, index, 4);
    ins--scl(be, scl(5), base, index, data);

    ins--move(be, index, 0);
    ins--dst-index-scaled(be, data, base, index, 0);
    ins--scl(be, scl(6), base, index, data);

    ins--move(be, index, 2);
    ins--dst-index-scaled(be, data, base, 2, 0);
    ins--scl(be, scl(7), base, index, data);

    ins--move(be, index, 1);
    ins--dst-index-scaled(be, data, base, index, 4);
    ins--scl(be, scl(8), base, index, data);

    ins--move(be, index, 1);
    ins--dst-index-scaled(be, data, first-lit, index, 4);
    ins--scl(be, scl(9), base, index, data);

    ins--rts-and-drop(be, 0);
  end with-harp;
end;



define lambda-test qtest6 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    named nreg r1, r2;

    ins--ld-teb-address(be, r1, 0);
    ins--ld-teb-address(be, r2, 16);
    ins--scl(be, scl(2), r1, r2);

    ins--ld-teb(be, r1, 0);
    ins--ld-teb(be, r2, 16);
    ins--scl(be, scl(3), r1, r2);

    ins--st-teb(be, r1, 0);
    ins--st-teb(be, r2, 16);
    ins--scl(be, scl(3), r1, r2);

    ins--rts-and-drop(be, 0);
  end with-harp;
end;



define lambda-test qtest7 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    tag t1, t2, t3, done;
    named nreg res, val3, val4, val22, val-big;

    ins--move(be, val-big, #x12345678);
    ins--st(be, val-big, first-lit, 0);
    ins--scl(be, scl(2), val-big);

    ins--move(be, val3, 3);
    ins--conditional-move(be, t1, first-lit, val3, val-big);
    ins--move(be, res, #xc00);
    ins--scl(be, scl(3), val3, val-big, res);

    ins--move(be, val4, 4);
    ins--conditional-move(be, t2, first-lit, val4, val3);
    ins--scl(be, scl(4), val4, val3, val-big, res);

    ins--move(be, val22, 22);
    ins--conditional-move(be, t3, first-lit, val22, val-big);
    ins--move(be, res, #x0bad0bad);
    ins--scl(be, scl(5), val4, val3, val22, val-big, res);
    ins--bra(be, done);

    ins--tag(be, t1);
    ins--move(be, res, #x1bad0bad);
    ins--scl(be, scl(6), res);
    ins--bra(be, done);

    ins--tag(be, t2);
    ins--move(be, res, #x2bad0bad);
    ins--scl(be, scl(7), res);
    ins--bra(be, done);

    ins--tag(be, t3);
    ins--ld(be, res, first-lit, 0);
    ins--scl(be, scl(8), res, val4);

    ins--tag(be, done);
    ins--rts-and-drop(be, 0);
  end with-harp;
end;


define lambda-test qtest8 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 20;

  with-harp (be)
    greg g0, g4, g6;
    named greg parameter1, parameter2, parameter3;
    arg0 arg0;
    tag t55;

    ins--load-count-adjusting-stack(be, g0, 0, 2);
    ins--move(be, parameter1, arg0);
    ins--load-stack-arg-n(be, parameter2, 0);
    ins--load-stack-arg-n(be, parameter3, 1);
    ins--scl(be, scl(1), parameter1, parameter2, parameter3);
    ins--move(be, g4, first-lit);
    ins--beq(be, t55, parameter3, g4);

    ins--scl(be, scl(8), parameter1, parameter2, parameter3);
    ins--move(be, g6, first-lit);
    ins--call-indirect(be, g6, 12, 1);
    ins--scl(be, scl(8), parameter1, parameter2, parameter3);


    ins--tag(be, t55);
    ins--scl(be, scl(8), parameter1, parameter2, parameter3);
    ins--rts-and-drop(be, 0);
  end with-harp;
end;


define method run-qtest-9 ()
  quick-test(qtest9, data-function: output-two-literals);
end method;


define lambda-test qtest9 (be :: <harp-x86-back-end>)
  name *test-name*;
  start-line 13;
  end-line 27;

  with-harp (be)
    named nreg mem1, mem2, count, position;
    tag fail, done, ok1, ok2, ok3, ok4;

    let test = 0;

    // First test while byte-aligned
    test := test + 1;
    ins--move(be, mem1, first-lit);
    ins--move(be, mem2, second-lit);
    ins--move(be, count, 2);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--bne-words(be, fail, mem1, mem2, count);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--move(be, count, 10);
    ins--bne-bytes(be, fail, mem1, mem2, count);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--move(be, count, 3);
    ins--bne-words(be, ok1, mem1, mem2, count);
    ins--bra(be, fail);
    ins--tag(be, ok1);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--move(be, count, 11);
    ins--bne-bytes(be, ok2, mem1, mem2, count);
    ins--bra(be, fail);
    ins--tag(be, ok2);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);


    // Now unalign and test again
    test := test + 1;                   // test 6
    ins--add(be, mem1, mem1, 3);
    ins--add(be, mem2, mem2, 3);
    ins--move(be, count, 1);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--bne-words(be, fail, mem1, mem2, count);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--move(be, count, 7);
    ins--bne-bytes(be, fail, mem1, mem2, count);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--move(be, count, 3);
    ins--bne-words(be, ok3, mem1, mem2, count);
    ins--bra(be, fail);
    ins--tag(be, ok3);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);

    test := test + 1;
    ins--move(be, count, #x7fffffff);
    ins--bne-bytes(be, ok4, mem1, mem2, count);
    ins--bra(be, fail);
    ins--tag(be, ok4);
    ins--move(be, position, test);
    ins--scl(be, scl(test), mem1, mem2, count, position);


    ins--tag(be, done);
    ins--move(be, position, #xd00dd00d);
    ins--scl(be, scl(11), mem1, mem2, count, position);
    ins--rts-and-drop(be, 0);

    ins--tag(be, fail);
    ins--move(be, position, #x05ad05ad);
    ins--scl(be, scl(12), mem1, mem2, count, position);
    ins--rts-and-drop(be, 0);
  end with-harp;
end;




