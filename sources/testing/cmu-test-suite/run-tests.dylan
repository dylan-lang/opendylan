Module: dylan-test
author: Roger Critchlow (rec@elf.org)
synopsis: A regression test for core Dylan.

// gts,6jul98 -- specializing tautology on <sequence> as a special case is needlessly 
// confusing.
// was: define method tautology(arg :: <sequence>) => (result :: <integer>);

define method do-tautologies(tests :: <sequence>) => (result :: <integer>);
  let (warnings, errors) = values(0, 0);

  let handler <warning>     // <warning>'s return to signalling context
    = method(condition, next)
        format-out("Warning in tautologies: %=. Continuing...\n", condition);
        warnings := warnings + 1;
      end method;

  let (elapsed-seconds, elapsed-microseconds) =
    timing ()
      for (test in tests)
	block ()
	  if (test)
	    format-out("Tautologies on %S\n", as(<string>, test));
	    tautology(test);
	  end if;

	exception (the-error :: <error>)  // <error>'s skip to next test
	  format-out("Error in tautologies: %=.  Skipping to %=\n",
	    the-error, head(tests));
	  errors := errors + 1;
	end;
      end for;
    end timing;

  format-out("Tautology completed with %D warnings and %D fatal errors\n", warnings, errors);
  if (elapsed-seconds ~= 0 | elapsed-microseconds ~= 0)
    format-out("Tautology tests completed in %D.%S seconds\n",
	       elapsed-seconds, integer-to-string(elapsed-microseconds, size: 6))
  end if;
  warnings + errors;
end method;

/*
if (empty?(args))
  exit(exit-code: tautology(tautologies));
else
  let args = map(curry(as, <symbol>), args);
  if (every?(rcurry(member?, tautologies), args))
    exit(exit-code: tautology(args));
  else
    format-out("usage: tautologies [package ...]\n");
    for (arg in tautologies)
      format-out("\t%s\n", as(<string>, arg));
    end for;
    exit(exit-code: -1);
  end if;
end if;
*/


do-tautologies(tautologies);
