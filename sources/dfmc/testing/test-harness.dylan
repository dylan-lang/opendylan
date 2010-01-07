Module:    DFMC-Testing
Author:    Jonathan Bachrach and Steve Rowley
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *tests*          = make(<table>);           // Test repository
define variable *test-successes* = make(<stretchy-vector>); // Result repository
define variable *test-failures*  = make(<stretchy-vector>); // Result repository
define variable *test-flamers*   = make(<stretchy-vector>); // Result repository

define function clear-tests() => ()
  // Clear away old test results, if any.
  size(*test-successes*) := 0;
  size(*test-failures*)  := 0;
  size(*test-flamers*)   := 0
end;

// Function for use in tests...
define function compile-library-until-optimized (lib)
  block()
    compile-library-from-definitions(lib, force?: #t, skip-link?: #t,
                                     compile-if-built?: #t, skip-heaping?: #t);
  exception (e :: <abort-compilation>)
  end
end function;

define function print-test-report 
       (#key stream         = *standard-output*, 
             title          = "DFMC Test Suite Report:",
             test-successes = *test-successes*, 
             test-failures  = *test-failures*,
             test-flamers   = *test-flamers*,
             lift-rocks?    = #f,
             cursor-x-max   = 80, 
             cursor-x-min   = size("SUCCEEDING: ")) => ()
  // Print test report in reasonably readable format.
  let cursor-x      = 0;
  let indent-string = make(<string>, size: cursor-x-min, fill: ' ');
  local method key< (k1 :: <symbol>, k2 :: <symbol>) => (lt :: <boolean>)
          // Sort for keywords
          as(<string>, k1) < as(<string>, k2)
        end,
        method print-callout(stream, title)
          // Print the bars above & below the title.
          newline(stream);
          for (x in title) write-element(stream, '=') end
        end,
        method newline (stream, #key indent?)
          // Newline & maybe indent, updating x cursorpos.
          write-element(stream, '\n');
          if (indent?)
            write(stream, indent-string);
            cursor-x := cursor-x-min
          else
            cursor-x := 0
          end
        end,
        method print-item(item, stream)
          // Printer who breaks lines & indents when necessary.
          let item-string  = as-lowercase(as(<string>, item));
          let item-length  = size(item-string);
          let new-cursor-x = cursor-x + item-length + 2; // Include comma & space
          if (new-cursor-x <= cursor-x-max)
            cursor-x := new-cursor-x
          else
            newline(stream, indent?: #t);
            cursor-x := cursor-x + item-length + 2 // off by one on last item
          end;
          write(stream, item-string)
        end,
        method print-test-list (tests)
          // Print a list of comma-separated test names or "(None)"
          cursor-x := cursor-x-min;
          if (empty?(tests))
            write(stream, "(None)")
          else
            print-separated-collection(
               sort(tests, test: key<),
               stream: stream, conjunction: "&", printer: print-item)
          end
        end;
  let n-successes = size(test-successes);
  let n-failures  = size(test-failures);
  let n-flames    = size(test-flamers);
  // Title & summary
  print-callout(stream, title);
  format(stream, "\n%s %d successes + %d failures + %d flames = %d total.",
         title, n-successes, n-failures, n-flames, 
         n-successes + n-failures + n-flames);
  print-callout(stream, title);
  // Details
  format(stream, "\n\nSUCCEEDING: "); print-test-list(test-successes);
  format(stream, "\n\nFAILING:    "); print-test-list(test-failures);
  format(stream, "\n\nFLAMING:    "); print-test-list(test-flamers);
  format(stream, "\n\n");
  when (lift-rocks? & size(*test-flamers*) ~= 0)
    for (rock in sort!(copy-sequence(*test-flamers*), test: key<))
      block ()                             // Lift suspicious rocks & see what
        run-tests(tests:       list(rock), //    kinds of slimy bugs crawl out
                  safely?:     #f,         // So errors not caught higher up
                  progress?:   #f,         // Gag progress
                  report?:     #f,         // Gag reporting
                  lift-rocks?: #f)         // Don't recurse
      exception (e :: <error>)             // Talk about the error
        format(stream, "Error in test %=: %s\n\n", rock, e)
      end
    end
  end;
  values()
end;

define function do-with-testing-context (code :: <function>, library-or-false)
  with-library-context (library-or-false | dylan-library-compilation-context())
    without-dependency-tracking
      code()
    end;
  end;
end function;

define macro with-testing-context
  { with-testing-context (?:expression) ?:body end }
    => { do-with-testing-context(method () ?body end, ?expression) }
end macro;

define function compiler-test-internal (name, test) => (result :: <boolean>)
  // Compile the template
  let lib-desc = 
    dynamic-bind (*progress-stream*           = #f,  // with-compiler-muzzled
                  *demand-load-library-only?* = #f)
      compile-template(test, compiler: compile-library-until-optimized)
    end;
  // Run the initializations; the last one is the test value.
  let init-method-value = #f;
  with-testing-context (lib-desc)
    for (cr in library-description-compilation-records(lib-desc))
      for (form in compilation-record-top-level-forms(cr))
	let init-method = form-init-method(form);
	when (init-method) 
	  init-method-value := eval(init-method);
	end
      end
    end
  end;
  init-method-value
end;

define macro compiler-test-definer
  // Compile forms, run initializations, last init method must return #t.
  { define compiler-test ?test-name:name = ?template:* }
    => { *tests*[?#"test-name"] := 
           method () 
             compiler-test-internal
               (?#"test-name", method () ?template end) end }
end;

define macro manual-compiler-test-definer
  // A pure code test: run something, get a boolean back.
  { define manual-compiler-test ?test-name:name = ?code:* }
    => { *tests*[?#"test-name"] := method () ?code end }
end;

// *** Will also need a native-compiler-test-definer.

define function run-test (name, #key safely?   = #t, 
                                     progress? = *standard-output*)
 => (result)
  // If safely?: is given, then catch all errors & mark that test as a flamer.
  // Otherwise, let the signal happen so you can figure out why it's flaming.
  when (progress?)                   // Progress note, for ssslooowww tests
    format(progress?, "// Testing %s\n", as-lowercase(as(<string>, name)))
  end;
  let test = *tests*[name];          // Get the test out of test registry
  let result =
    if (~instance?(test, <function>))  // No such test, warn if permitted to talk.
      when (progress?)
	format(progress?, "Ignoring test %= = %=, because it's not a <function>.",
	       name, test)
      end;
      #"ignored test"
    elseif (safely?)                   // Don kludge-proof goggles & rubber gloves
      block ()
	test() ~== #f
      exception (e :: <error>)
        e
      end
    // Accept the risk of signals for benefit of debugging information in them.
    else
      test() ~== #f
    end;
  select (result by instance?)
    singleton(#t) =>
      add!(*test-successes*, name);     // Test succeeds
      #"succeeds";
    singleton(#f) =>
      add!(*test-failures*, name);      // Test fails
      #"fails";
    <error> =>
      add!(*test-flamers*, name);       // Test flames
      result;
    otherwise => result;
  end;
end;

define function run-tests (#key tests       = key-sequence(*tests*),
                                safely?     = #t,
                                progress?   = *standard-output*,
                                report?     = *standard-output*, 
                                lift-rocks? = #f)
    => ()
  // Run some tests, maybe printing a report.
  clear-tests();                         // Throw out old results.
  for (test in tests)                    // Get new results.
    run-test(test, safely?: safely?, progress?: progress?)
  end;
  when (report?)                         // Report if requested to given stream.
    print-test-report(stream: report?, lift-rocks?: lift-rocks?)
  end;
  values()
end;
