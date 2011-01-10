Module:       io-test-suite
Synopsis:     IO library test suite
Author:       James Kirsch, Shri Amit
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test basic-format ()
  check-equal("format with no operators", format-to-string("Testing"), "Testing");
  check-equal("format with %%", format-to-string("%%%%"), "%%");
  check-condition("Missing format operator", <error>, format-to-string("%"));
  check-condition("Invalid format operator", <error>, format-to-string("%q"));
end test basic-format;

define test decimal-control-strings ()
  check-equal("format %d with a positive integer",
	      format-to-string("-%d-", 12), "-12-");
  check-equal("format %d with a negative integer",
	      format-to-string("'%D'", -12), "'-12'");
end test decimal-control-strings;

define test binary-control-strings ()
  check-equal("format %b with a positive integer",
	      format-to-string("%B", 43), "101011");
  check-equal("format %b with a negative integer",
	      format-to-string("%b", -37), "-100101");
end test binary-control-strings; 

define test octal-control-strings ()
  check-equal("format %o with a positive integer", 
	      format-to-string("%O", 28), "34");
  check-equal("format %o with a negative integer",
	      format-to-string("%o", -9483672),"-44132630");
end test octal-control-strings;

define test hex-control-strings ()
  check-equal("format %x with a positive integer", 
	      format-to-string("%X", 117), "75");
  check-equal("format %x with a negative integer",
	      format-to-string("%x", -91827364), "-5792CA4");
end test hex-control-strings;

define test multiple-basic-control-strings ()
  check-equal("multiple control strings 1", 
	      format-to-string("Hex: %x, Dec: %d", 234, 567), "Hex: EA, Dec: 567");
  check-equal("multiple control strings 2",
	      format-to-string("Bin: %b, Oct: %o", 12, 34), "Bin: 1100, Oct: 42");
  check-equal("multiple control strings 3",
	      format-to-string("%d %% %b %% %o %% %x", 42, 42, 42, 42), "42 % 101010 % 52 % 2A");
end test multiple-basic-control-strings;
 
define test existing-string-messages ()
  check-equal("format %s with a string", 
	      format-to-string("Byte string: %s.", "random byte string"),
	      "Byte string: random byte string.");
  check-equal("format %s with a symbol",
	      as-lowercase(format-to-string("Symbol: %s.", #"ranDOM syMBol")),
	      "symbol: random symbol.");
  check-equal("format %s with a character",
	      format-to-string("Character: %s.", 'c'), "Character: c.");
end test existing-string-messages;
  
define suite format-test-suite ()
  test basic-format;
  test decimal-control-strings;
  test binary-control-strings;
  test octal-control-strings;
  test hex-control-strings;
  test multiple-basic-control-strings;
  test existing-string-messages;
end suite format-test-suite;
