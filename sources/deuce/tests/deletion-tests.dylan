Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -=- Auxiliary definitions -=-

define constant $interval-descriptions :: <simple-object-vector>
  = vector("empty", "end-of-line", "single-line", "part-lines",
           "section", "part-sections", "buffer");

define constant $deletion-results :: <simple-object-vector>
  = vector
      ("Line 1\nLine 2\nLine 3\nLine 4\nLine 5",
       "Line 1Line 2\nLine 3\nLine 4\nLine 5",
       "\nLine 2\nLine 3\nLine 4\nLine 5",
       "Lne 3\nLine 4\nLine 5",
       "\nLine 4\nLine 5",
       "Line 1\nLine 2\nLie 5",
       "");

define function test-delete! (index :: <integer>) => ()
  reset-testing-buffer-state();
  dynamic-bind(*buffer* = *testing-buffer*)
    delete!((testing-intervals())[index]);
    // Check contents.
    check-equal
      (format-to-string("Deleted %s interval", $interval-descriptions[index]),
       as(<string>, *buffer*),
       $deletion-results[index]);
    // Check buffer structure.
    let start-line = bp-line(interval-start-bp(buffer-start-node(*buffer*)));
    let end-line = bp-line(interval-end-bp(buffer-end-node(*buffer*)));
    // If the following fails, you can add ", verbose?: #t" to get
    // some useful(?) diagnostics.
    check-true
      ("Buffer line link structure okay after deletion",
       last-line-from(start-line, in-buffer?: #t) == end-line);
    // If the following fails, you can add ", verbose?: #t" to get
    // some useful(?) diagnostics.
    check-true
      ("Buffer BPs okay after deletion",
       check-buffer-bps(*buffer*));
  end dynamic-bind;
end function;

define function test-invalid-delete!
    (single-line? :: <boolean>,
     invalid-start? :: <boolean>, invalid-end? :: <boolean>)
 => ()
  reset-testing-buffer-state();
  dynamic-bind(*buffer* = *testing-buffer*)
    let start-index = if (invalid-start?) -1 else 0 end;
    let start-bp = make(<bp>, line: *line1-1*, index: start-index);
    let end-line = if (single-line?) *line1-1* else *line1-3* end;
    let end-index = if (invalid-end?) 20 else 4 end;
    let end-bp = make(<bp>, line: end-line, index: end-index);
    check-condition
      (format-to-string
         ("Can't delete %s-line interval with invalid %s",
          if (single-line?) "single" else "multi" end,
          if (invalid-start?)
            if (invalid-end?) "start and end" else "start" end
          else "end" end),
       <error>,
       delete!(make-interval(start-bp, end-bp, in-order?: #t)));
  end dynamic-bind;
end function;



// -=- Tests and Suites -=-

/*
// For the moment, we'll take the test for "delete!" as a sufficient
// test for "cleanup-empty-nodes", as delete! is the only place which
// calls it.
define test deletion-cleanup-test ()
  // ---*** TO DO
end test;
*/


define test deletion-delete!-test ()
  // Check for a representative sample of valid deletions.
  for (interval-index from 0 below size($interval-descriptions))
    test-delete!(interval-index);
  end;

  // Now check for deleting intervals bounded by bps with invalid indices.
  // [(*, #f, #f) are the only combinations not to be used here]
  test-invalid-delete!(#t, #t, #f);
  test-invalid-delete!(#t, #f, #t);
  test-invalid-delete!(#t, #t, #t);
  test-invalid-delete!(#f, #t, #f);
  test-invalid-delete!(#f, #f, #t);
  test-invalid-delete!(#f, #t, #t);
end test;


define test deletion-kill!-test ()
  // ---*** TO DO
  // I won't add tests for this until I add tests for the history
  // mechanism in general.
end test;


define suite deletion-suite ()
//  test deletion-cleanup-test;
  test deletion-delete!-test;
  test deletion-kill!-test;
end suite;

/*

Things to test ...

kill!
  Check killing empty, within-a-line and cross-line intervals.
  Check killing empty lines, and killing across them.
  Check effects of "merge?:" and "reverse?:" [whatever they're
    supposed to be!]
  Check effect of clipboard-policy.
  Check state of history and clipboard before and after.

delete!
  Check deleting empty and {part,whole,cross}-{line,section} intervals.
  Check no empty lines (and sections/nodes?) are left behind?
  Check deleting intervals bounded bby bps with invalid indices.

[For now, I'll take the following two as being sufficiently tested by
the tests on delete!.]
merge-sections
  Check that content of resulting section is as expected.
remove-empty-nodes
  Check with start, end or both nodes having empty sections.

*/
