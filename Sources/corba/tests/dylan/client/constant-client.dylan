Module:    constant-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro check-close-enough
  { check-close-enough (?check-name:expression, ?check-args:*) }
    => { check(?check-name, close-enough?, ?check-args) }
end macro;

define constant $epsilon :: <double-float> = 1.0d-5; // ie 1/1000 th of a %

define method close-enough? (x, y)
  (abs(x - y) / abs(x)) < $epsilon
end method;

define test constant-integer-test (description: "Test integer constants")
  check-equal("Value of MIN_SHORT", $MIN-SHORT, -32768);
  check-equal("Value of MAX_SHORT", $MAX-SHORT, 32767);
  check-equal("Value of MIN_USHORT", $MIN-USHORT, 0);
  check-equal("Value of MAX_USHORT", $MAX-USHORT, 65535);
  check-equal("Value of MIN_LONG", $MIN-LONG, -536870911);   // -2147483648
  check-equal("Value of MAX_LONG", $MAX-LONG, 536870911);    // 2147483647
  check-equal("Value of ULONG_MIN", $MIN-ULONG, 0);
  check-equal("Value of ULONG_MAX", $MAX-ULONG, 536870911);  // 4294967295
  check-equal("Value of OCTAL_INTEGER", $OCTAL-INTEGER, #o01234567);
  check-equal("Value of HEXADECIMAL_INTEGER", $HEXADECIMAL-INTEGER-1, #xABCDEF);
  check-equal("Value of HEXADECIMAL_INTEGER", $HEXADECIMAL-INTEGER-2, #xABCDEF);
end test;

define test constant-character-test (description: "Test character constants")
  check-equal("Value of SPACE", $SPACE, ' ');
  check-equal("Value of UNDERSCORE", $UNDERSCORE, '_');
  check-equal("Value of DIGIT0", $DIGIT0, '0');
  check-equal("Value of LETTERA", $LETTERA, 'A');
  check-equal("Value of NEWLINE", $NEWLINE, '\n');
  check-equal("Value of HORIZONTAL_TAB", $HORIZONTAL-TAB, '\t');
  check-equal("Value of VERTICAL_TAB", $VERTICAL-TAB, '\<0B>');
  check-equal("Value of BACKSPACE", $BACKSPACE, '\b');
  check-equal("Value of CARRIAGE_RETURN", $CARRIAGE-RETURN, '\r');
  check-equal("Value of FORM_FEED", $FORM-FEED, '\f');
  check-equal("Value of ALERT", $ALERT, '\a');
  check-equal("Value of BACKSLASH", $BACKSLASH, '\\');
  check-equal("Value of QUESTION_MARK", $QUESTION-MARK, '?');
  check-equal("Value of SINGLE_QUOTE", $SINGLE-QUOTE, '\'');
  check-equal("Value of DOUBLE_QUOTE", $DOUBLE-QUOTE, '"');
  check-equal("Value of OCTAL_NUMBER_1", $OCTAL-NUMBER-1, '\<1>');
  check-equal("Value of OCTAL_NUMBER_2", $OCTAL-NUMBER-2, '\<A>');
  check-equal("Value of OCTAL_NUMBER_3", $OCTAL-NUMBER-3, '\<53>');
  check-equal("Value of HEXADECIMAL_NUMBER_1", $HEXADECIMAL-NUMBER-1, '\<1>');
  check-equal("Value of HEXADECIMAL_NUMBER_2", $HEXADECIMAL-NUMBER-2, '\<12>');
end test;

define test constant-boolean-test (description: "Test boolean constants")
  check-equal("Value of MY_TRUE", $MY-TRUE, #t);
  check-equal("Value of MY_FALSE", $MY-FALSE, #f);
end test;

define test constant-string-test (description: "Test string constants")
  check-equal("Value of EMPTY_STRING", $EMPTY-STRING, "");
  check-equal("Value of NAME", $NAME, "Keith");
  check-equal("Value of CONCATENATED_STRINGS", $CONCATENATED-STRINGS, "First part+Second part");
  check-equal("Value of ESCAPE_SEQUENCES", $ESCAPE-SEQUENCES, "\n \t \<0B> \b \r \f \a \\ ? \' \" \<1> \<A> \<53> \<1> \<12>");
// TODO: Add check for: const string CONCATENATED_STRINGS_2 = "\xA" "B";
end test;

define test constant-float-test (description: "Test float and double constants")
  check-equal("Value of ZERO_FLOAT", $ZERO-FLOAT, 0.0);
  check-equal("Value of WEE_FLOAT", $WEE-FLOAT, 1.175494s-30);
  check-equal("Value of BIG_FLOAT", $BIG-FLOAT, 3.4028236s+30);
  check-close-enough("Value of NEGATIVE_FLOAT1", $NEGATIVE-FLOAT1, -123.45e10);
  check-close-enough("Value of NEGATIVE_FLOAT2", $NEGATIVE-FLOAT2, -123.45e-10);
  check-equal("Value of ZERO_DOUBLE", $ZERO-DOUBLE, 0.0d0);
  check-equal("Value of WEE_DOUBLE", $WEE-DOUBLE, 2.225d-300);
  check-equal("Value of BIG_DOUBLE", $BIG-DOUBLE, 1.7976931d+300);
  check-close-enough("Value of NEGATIVE_DOUBLE1", $NEGATIVE-DOUBLE1, -678.9d100);
  check-close-enough("Value of NEGATIVE_DOUBLE2", $NEGATIVE-DOUBLE2, -678.9d-100);
  check-equal("Value of FLOAT_EXPR1", $FLOAT-EXPR1, 0.0);
  check-equal("Value of FLOAT_EXPR2", $FLOAT-EXPR2, 0.0);
  check-equal("Value of DOUBLE_EXPR1", $DOUBLE-EXPR1, 0.0d0);
  check-equal("Value of DOUBLE_EXPR2", $DOUBLE-EXPR2, 0.0d0);
end test;

define test constant-expression-test (description: "Test constant expressions")
  check-equal("Value of UNARY_PLUS", $UNARY-PLUS, 2);
  check-equal("Value of UNARY_MINUS", $UNARY-MINUS, -3);
  check-equal("Value of BINARY_MINUS", $BINARY-MINUS, -1);
  check-equal("Value of BINARY_DIVISION", $BINARY-DIVISION, 14);
  check-equal("Value of BINARY_MOD", $BINARY-MOD, 2);

  check-equal("Value of BINARY_XOR", $BINARY-XOR, 1);
  check-equal("Value of BINARY_OR", $BINARY-OR, 3);
  check-equal("Valur of BINARY_AND", $BINARY-AND, 2);
  check-equal("Value of BINARY_LSHIFT", $BINARY-LSHIFT, 40);
  check-equal("Valur of BINARY_RSHIFT", $BINARY-RSHIFT, 25);

  check-equal("Value of MINUS_ONE", $MINUS-ONE, -1);
  check-equal("Value of ICE_CREAM", $ICE-CREAM, 99);
  check-equal("Value of SECS_IN_1_DAY", $SECS-IN-1-DAY, 86400);
  check-equal("Value of FOO", $FOO, 48);
  check-equal("Value of PRICE", $PRICE, 5 * 100.0);
  check-equal("Value of OVERDRAFT", $OVERDRAFT, 1234567.8d0);

  check-equal("Value of A_PRIME", $A-PRIME, $A);

  check-equal("Value of DISTRIB_1", $DISTRIB-1, logxor(logior($A, $B, $C), $D));
  check-equal("Value of DISTRIB_2", $DISTRIB-2, logior(logxor($A, $D), logxor($B, $D), logxor($C, $D)));

  check-equal("Value of DISTRIB_3", $DISTRIB-3, logand(logxor($A, $B, $C), $D));
  check-equal("Value of DISTRIB_4", $DISTRIB-4, logxor(logand($A, $D), logand($B, $D), logand($C, $D)));

  check-equal("Value of DISTRIB_5", $DISTRIB-5, ash(logand($A, $B, $C), -$D));
  check-equal("Value of DISTRIB_6", $DISTRIB-6, logand(ash($A, -$D), ash($B, -$D), ash($C, -$D)));
  check-equal("Value of DISTRIB_7", $DISTRIB-7, ash(logand($A, $B, $C), $D));
  check-equal("Value of DISTRIB_8", $DISTRIB-8, logand(ash($A, $D), ash($B, $D), ash($C, $D)));

  check-equal("Value of DISTRIB_9", $DISTRIB-9, ash(ash($A, -$B), -$C) + $D);
  check-equal("Value of DISTRIB_10", $DISTRIB-10, ash(ash($A + $D, -($B + $D)), -($C + $D)));
  check-equal("Value of DISTRIB_11", $DISTRIB-11, ash(ash($A, $B), $C) - $D);
  check-equal("Value of DISTRIB_12", $DISTRIB-12, ash(ash($A - $D, $B - $D), $C - $D));

  check-equal("Value of DISTRIB_13", $DISTRIB-13, truncate/($A + $B + $C, $D));
  check-equal("Value of DISTRIB_14", $DISTRIB-14, truncate/($A, $D) + truncate/($B, $D) + truncate/($C, $D));
  check-equal("Value of DISTRIB_15", $DISTRIB-15, ($A - $B - $C) * $D);
  check-equal("Value of DISTRIB_16", $DISTRIB-16, $A * $D - $B * $D - $C * $D);
  check-equal("Value of DISTRIB_17", $DISTRIB-17, modulo($A + $B + $C, $D));
  check-equal("Value of DISTRIB_18", $DISTRIB-18, modulo($A, $D) + modulo($B, $D) + modulo($C, $D));

  check-equal("Value of UNARY_1", $UNARY-1, $A - $B + lognot($C));
  check-equal("Value of UNARY_2", $UNARY-2, $A - $B + lognot($C));
end test;

define suite constant-test-suite ()
  test constant-integer-test;
  test constant-character-test;
  test constant-boolean-test;
  test constant-string-test;
  test constant-float-test;
  test constant-expression-test;
end suite;

