Module:    array-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $ArrayTest-ior-file = "c:\\temp\\ArrayTest.ior";

define method get-ArrayTest-reference ()
 => (reference :: <ArrayTest>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  as(<ArrayTest>, corba/orb/file-to-object(orb, $ArrayTest-ior-file));
end method;

define constant $messages = #["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

define test string-array-test ()
  let ArrayTest = get-ArrayTest-reference();
  let array = make(<StringArray>, dimensions: #[10], fill: "");
  for (i from 0 below 10)
    array[i] := $messages[i];
  end for;
  check("String array attribute setter", ArrayTest/string-array-attribute-setter, array, ArrayTest);
  check-false("Check string array attribute", ArrayTest/check-string-array-attribute(ArrayTest));
  check-equal("String array attribute getter", ArrayTest/string-array-attribute(ArrayTest), array);
  let (i, ii, iii) = ArrayTest/string-array-operation(ArrayTest, array, array);
  check-equal("String array operation first result", i, array);
  check-equal("String array operation second result", ii, array);
  check-equal("String array operation third result", iii, array);
  check-true("Coerce string array to Any", instance?(as(CORBA/<any>, array), CORBA/<any>));
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let any = make(CORBA/<any>,
                 value: array,
		 type: corba/orb/create-array-tc(orb,
                                                 10,
                                                 corba/orb/create-string-tc(orb, 0)));
  check-equal("Coerce Any to string array", as(<StringArray>, any), array);
end test;

define test short-array-test ()
  let ArrayTest = get-ArrayTest-reference();
  let array = make(<ShortArray>, dimensions: #[4, 4], fill: 0);
  let counter = 0;
  for (i from 0 below 4)
    for (j from 0 below 4)
      array[i,j] := counter;
      counter := counter + 1;
    end for;
  end for;
  check("Short array attribute setter", ArrayTest/short-array-attribute-setter, array, ArrayTest);
  check-false("Check short array attribute", ArrayTest/check-short-array-attribute(ArrayTest));
  check-equal("Short array attribute getter", ArrayTest/short-array-attribute(ArrayTest), array);
  let (i, ii, iii) = ArrayTest/short-array-operation(ArrayTest, array, array);
  check-equal("Short array operation first result", i, array);
  check-equal("Short array operation second result", ii, array);
  check-equal("Short array operation third result", iii, array);
  check-true("Coerce short array to Any", instance?(as(CORBA/<any>, array), CORBA/<any>));
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let any = make(CORBA/<any>,
                 value: array,
		 type:  corba/orb/create-array-tc(orb,
                                                  4,
                                                  corba/orb/create-array-tc(orb,
                                                                  4,
                                                                  corba/$short-typecode)));
  check-equal("Coerce Any to short array", as(<ShortArray>, any), array);
end test;

define test float-array-test ()
  let ArrayTest = get-ArrayTest-reference();
  let array = make(<FloatArray>, dimensions: #[3, 3, 3], fill: 0.0);
  let number = 0.0;
  for (i from 0 below 3)
    for (j from 0 below 3)
      for (k from 0 below 3)
	array[i, j, k] := number;
	number := number + 0.7;
      end for;
    end for;
  end for;
  check("Float array attribute setter", ArrayTest/float-array-attribute-setter, array, ArrayTest);
  check-false("Check Float array attribute", ArrayTest/check-float-array-attribute(ArrayTest));
  check-equal("Float array attribute getter", ArrayTest/float-array-attribute(ArrayTest), array);
  let (i, ii, iii) = ArrayTest/float-array-operation(ArrayTest, array, array);
  check-equal("Float array operation first result", i, array);
  check-equal("Float array operation second result", ii, array);
  check-equal("Float array operation third result", iii, array);
  check-true("Coerce float array to Any", instance?(as(CORBA/<any>, array), CORBA/<any>));
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let any = make(CORBA/<any>,
                 value: array,
		 type: corba/orb/create-array-tc(orb,
                                                 3,
                                                 corba/orb/create-array-tc(orb,
                                                                 3,
                                                                 corba/orb/create-array-tc(orb,
                                                                                 3,
                                                                                 corba/$float-typecode))));
  check-equal("Coerce Any to float array", as(<FloatArray>, any), array);
end test;

define suite array-test-suite ()
  test string-array-test;
  test short-array-test;
  test float-array-test;
end suite;

