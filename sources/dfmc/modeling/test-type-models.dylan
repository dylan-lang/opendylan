Module:   dfmc-modeling
Synopsis: Testing of type models.
Author:   Keith Playford and Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// mini testing harness

define macro type-test-definer
  { define type-test ?:name ?decls:* checks ?checks end }
    => { define constant ?name ## "-type-test"
           = method () => (passed, failed, flamed?)
               access(dfmc-management, ensure-booted)();
	       block ()
		 ?decls
		 let passed = 0;
		 let failed = 0;
		 local method record-check (result)
			 if (result)
			   passed := passed + 1;
			 else
			   failed := failed + 1;
			 end if;
		       end method record-check;
		 ?checks;
		 values(passed, failed, #f)
	       exception (<error>)
		 values(0, 0, #t)
	       end block
             end method;
         $type-tests[?#"name"] := ?name ## "-type-test" }
 checks:
  { }
    => { }
  { ?:expression; ... }
    => { record-check(?expression); ... }
end macro type-test-definer;

define constant $type-tests = make(<table>);

define method run-type-tests () => ()
  let total-passed = 0;
  let total-failed = 0;
  let total-flamed = 0;
  for (test keyed-by name in $type-tests)
    format-out("// %s:", name);
    let (passed, failed, flamed?) = test();
    if (flamed?)
      total-flamed := total-flamed + 1;
      format-out(" flamed");
    else
      if (passed ~= 0)
        format-out(" %d passed", passed);
        total-passed := total-passed + passed;
      end if;
      if (failed ~= 0)
        format-out(" %d failed", failed);
        total-failed := total-failed + failed;
      end if;
    end if;
    format-out("\n");
  end for;
  format-out("Passed %d checks", total-passed);
  if (total-failed ~= 0)
    format-out(", failed %d", total-failed);
  end if;
  if (total-flamed ~= 0)
    format-out(", %d tests flamed", total-flamed);
  end if;
  format-out(".\n");
  values();
end method run-type-tests;


// obvious tests

// is this generally useful enough to live somewhere else?
define constant map-values
  = method (func :: <function>, #rest objects) => (#rest objects)
      apply(values, map(func, objects))
    end method;

define type-test limited-integers
  let (zero, one) = map-values(run-stage, 0, 1);
  let <&nat> = &limited-integer(0, #f);
  let <&byte> = &limited-integer(0, 255);
  let <&pos> = &limited-integer(1, #f);
  let <&neg> = &limited-integer(#f, -1);
checks
  <&nat>.^limited-integer-min = 0;
  <&nat>.^limited-integer-max = #f;
  <&nat>.&limited-integer-max = &false;
  ~^subtype?(<&nat>, <&byte>);
  ^subtype?(<&byte>, <&nat>);
  ~^disjoint?(<&byte>, <&nat>);
  ^disjoint?(<&pos>, <&neg>);
  ~^subtype?(<&nat>, <&pos>);
  ^subtype?(<&pos>, <&nat>);
  ^instance?(zero, <&nat>);
  ~^instance?(zero, <&pos>);
  ~^instance?(zero, <&neg>);
  ^instance?(one, <&nat>);
  ^instance?(one, <&pos>);
end type-test limited-integers;
