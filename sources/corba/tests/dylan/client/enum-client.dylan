Module:    enum-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant *enumtest-ior-file* = "c:\\temp\\enumtest.ior";

define constant $test-enum-symbols
  = #[#"Mercury",
      #"Venus",
      #"Earth",
      #"Mars",
      #"Jupiter",
      #"Saturn",
      #"Uranus",
      #"Neptune",
      #"Pluto"];

define test enum-successor-test ()
  for (i from 0 below size($test-enum-symbols) - 1)
    let symbol = $test-enum-symbols[i];
    let successor = $test-enum-symbols[i + 1];
    let result = planet/successor(symbol);
    check-equal(format-to-string("%= is successor to %=", successor, symbol),
                result, successor);
  end for;
end test;

define test enum-successor-wrap-test ()
  let symbol = last($test-enum-symbols);
  let successor = first($test-enum-symbols);
  check-equal("Successor wrap check", planet/successor(symbol), successor)
end test;

define test enum-predecessor-test ()
  for (i from 1 below size($test-enum-symbols))
    let symbol = $test-enum-symbols[i];
    let predecessor = $test-enum-symbols[i - 1];
    let result = planet/predecessor(symbol);
    check-equal(format-to-string("%= is predecessor to %=", predecessor, symbol),
                result, predecessor);
  end for;
end test;

define test enum-predecessor-wrap-test ()
  let symbol = first($test-enum-symbols);
  let predecessor = last($test-enum-symbols);
  check-equal("Predecessor wrap check", planet/predecessor(symbol), predecessor)
end test;

define test enum-comparison-test ()
  for (i from 0 below size($test-enum-symbols))
  let symbol-i = $test-enum-symbols[i];
    for (j from 0 below size($test-enum-symbols))
      let symbol-j = $test-enum-symbols[j];
      if (i = j)
        check-false(format-to-string("%= < %= is false", symbol-i, symbol-j),
                    planet/<(symbol-i, symbol-j));
        check-false(format-to-string("%= > %= is false", symbol-i, symbol-j),
                    planet/>(symbol-i, symbol-j));
      elseif (i < j)
        check-true(format-to-string("%= < %= is true", symbol-i, symbol-j),
                   planet/<(symbol-i, symbol-j));
        check-false(format-to-string("%= > %= is false", symbol-i, symbol-j),
                    planet/>(symbol-i, symbol-j));
      else
        check-false(format-to-string("%= < %= is false", symbol-i, symbol-j),
                    planet/<(symbol-i, symbol-j));
        check-true(format-to-string("%= > %= is true", symbol-i, symbol-j),
                   planet/>(symbol-i, symbol-j));
      end if;
    end for;
  end for;
end test;

define test enum-any-test ()
  for (symbol in $test-enum-symbols)
    let any = as(CORBA/<any>, symbol);
    check-true("any is an instance of CORBA/<any>", instance?(any, CORBA/<any>));
    check-equal("any has correct typecode", CORBA/any/type(any), class-typecode(<planet>));
    check-equal("any has correct internal value", CORBA/any/value(any), symbol);
    check-equal("Coerce any back to symbol", as(<planet>, any), symbol);
  end for;
end test;

define test enum-in-parameter-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let context = corba/orb/get-default-context(orb);
  let enum = as(<enumtest>, corba/orb/file-to-object(orb, *enumtest-ior-file*));
  enumtest/reset-in-parameter(enum);
  for (symbol in $test-enum-symbols)
    check-true(format-to-string("%= in parameter", symbol), enumtest/in-parameter(enum, symbol));
  end for;
end test;

define test enum-result-test ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let context = corba/orb/get-default-context(orb);
  let enum = as(<enumtest>, corba/orb/file-to-object(orb, *enumtest-ior-file*));
  enumtest/reset-result(enum);
  for (symbol in $test-enum-symbols)
    check-equal(format-to-string("Result %=", symbol), enumtest/result(enum), symbol);
  end for;
end test;

define suite enum-test-suite ()
  test enum-successor-test;
  test enum-successor-wrap-test;
  test enum-predecessor-test;
  test enum-predecessor-wrap-test;
  test enum-comparison-test;
  test enum-any-test;
  test enum-in-parameter-test;
  test enum-result-test;
end suite;
