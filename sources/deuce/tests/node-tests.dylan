Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -=- Intervals -=-

define test interval-creation-test ()
  // -=- make -=-
  // Check that the "in-order?" keyword is obeyed.
  let line = make(<text-line>, contents: "A line of text");
  let start-bp = line-start(line);
  let end-bp = line-end(line);

  let interval = make-interval(start-bp, end-bp);
  check-true
    ("BPs given in right order come out ordered okay",
     bp-less?(interval-start-bp(interval), interval-end-bp(interval)));
  let interval = make-interval(end-bp, start-bp);
  check-true
    ("BPs given in wrong order come out ordered okay",
     bp-less?(interval-start-bp(interval), interval-end-bp(interval)));
  let interval = make-interval(end-bp, start-bp, in-order?: #t);
  check-false
    ("BPs given in wrong order come out ordered wrong with \"in-order: #t\"",
     bp-less?(interval-start-bp(interval), interval-end-bp(interval)));
end test;



define test interval-characters-test ()
  // -=- Setup -=-

  let accumulator :: <string> = "";
  let skip-line-index :: <integer> = 0;
  local
    method accumulate-line-contents
        (line :: <line>, si :: <integer>, ei :: <integer>,
         last-line? :: <boolean>)
      accumulator
        := concatenate
             (accumulator,
              if ( last-line? ) "*" else "" end,
              copy-sequence(line-contents(line), start: si, end: ei));
    end method,
    method nth-skipper (n :: <integer>)
      method (line :: <line>)
        skip-line-index := skip-line-index + 1;
        (skip-line-index == n)
      end
    end method,
    method reset-state () => ()
      accumulator := "";
      skip-line-index := 0
    end method;


  // -=- do-lines -=-

  let line1 = make(<text-line>, contents: "Line 1");
  let line2 = make(<text-line>, contents: "Line 2", previous: line1);
  let line3 = make(<text-line>, contents: "Line 3", previous: line2);
  let line4 = make(<text-line>, contents: "Line 4", previous: line3);
  line1.line-next := line2;
  line2.line-next := line3;
  line3.line-next := line4;
  let line1-start-bp = line-start(line1);
  let line4-end-bp = line-end(line4);
  let interval = make-interval(line1-start-bp, line4-end-bp);

  let small-start-bp = make-bp(line1, 2);
  let small-end-bp = make-bp(line4, line-length(line4) - 2);
  let small-interval = make-interval(small-start-bp, small-end-bp);

  let line1-end-bp = line-end(line1);
  let line-interval = make-interval(line1-start-bp, line1-end-bp);
  let empty-interval = make-interval(line1-start-bp, line1-start-bp);

  let line2-start-bp = line-start(line2);
  let eol-interval = make-interval(line1-end-bp, line2-start-bp);

  // ... default skip test

  reset-state();
  do-lines(accumulate-line-contents, empty-interval);
  check-equal
    ("do-lines forward, default skip-test, empty interval",
     accumulator, "*");

  reset-state();
  do-lines(accumulate-line-contents, line-interval);
  check-equal
    ("do-lines forward, default skip-test, single line",
     accumulator, "*Line 1");

  reset-state();
  do-lines(accumulate-line-contents, eol-interval);
  check-equal
    ("do-lines forward, default skip-test, interval over just EOL",
     accumulator, "*");

  reset-state();
  do-lines(accumulate-line-contents, interval);
  check-equal
    ("do-lines forward, default skip-test, whole lines",
     accumulator, "Line 1Line 2Line 3*Line 4");

  reset-state();
  do-lines(accumulate-line-contents, small-interval);
  check-equal
    ("do-lines forward, default skip-test, partial lines",
     accumulator, "ne 1Line 2Line 3*Line");

  reset-state();
  do-lines(accumulate-line-contents, interval, from-end?: #t);
  check-equal
    ("do-lines from-end, default skip-test, whole lines",
     accumulator, "Line 4Line 3Line 2*Line 1");

  reset-state();
  do-lines(accumulate-line-contents, small-interval, from-end?: #t);
  check-equal
    ("do-lines from-end, default skip-test, partial lines",
     accumulator, "LineLine 3Line 2*ne 1");

  // ... skip second line
  let skip-second = nth-skipper(2);

  reset-state();
  do-lines
    (accumulate-line-contents, small-interval, skip-test: skip-second);
  check-equal
    ("do-lines forward, skip second line, partial lines",
     accumulator, "ne 1Line 3*Line");

  reset-state();
  do-lines
    (accumulate-line-contents, small-interval, from-end?: #t,
     skip-test: skip-second);
  check-equal
    ("do-lines from-end, skip second line, partial lines",
     accumulator, "LineLine 2*ne 1");

  // ... skip first line
  let skip-first = nth-skipper(1);

  reset-state();
  do-lines
    (accumulate-line-contents, empty-interval, skip-test: skip-first);
  check-equal
    ("do-lines forward, skip first line, empty interval",
     accumulator, "");

  reset-state();
  do-lines
    (accumulate-line-contents, line-interval, skip-test: skip-first);
  check-equal
    ("do-lines forward, skip first line, single line",
     accumulator, "");

  reset-state();
  do-lines
    (accumulate-line-contents, eol-interval, skip-test: skip-first);
  check-equal
    ("do-lines forward, skip first line, interval over just EOL",
     accumulator, "*");

  reset-state();
  do-lines
    (accumulate-line-contents, small-interval, skip-test: skip-first);
  check-equal
    ("do-lines forward, skip first line, partial lines",
     accumulator, "Line 2Line 3*Line");

  reset-state();
  do-lines
    (accumulate-line-contents, small-interval, from-end?: #t,
     skip-test: skip-first);
  check-equal
    ("do-lines from-end, skip first line, partial lines",
     accumulator, "Line 3Line 2*ne 1");

  // ... skip all lines
  let skip-all = always(#t);

  reset-state();
  do-lines
    (accumulate-line-contents, small-interval, skip-test: skip-all);
  check-equal
    ("do-lines forward, skip all lines, partial lines",
     accumulator, "");

  reset-state();
  do-lines
    (accumulate-line-contents, small-interval, from-end?: #t,
     skip-test: skip-all);
  check-equal
    ("do-lines from-end, skip all lines, partial lines",
     accumulator, "");

  // ... for lines from different buffers.
  let section = make(<section>, start-line: #f, end-line: #f);
  let two-buffer-line = copy-line(line1);
  add-line!(section, two-buffer-line);
  let two-buffer-line-start-bp = line-start(two-buffer-line);
  let two-buffer-line-end-bp = line-end(two-buffer-line);
  let node
    = make(<section-node>, section: section,
           start-bp: two-buffer-line-start-bp,
           end-bp: two-buffer-line-end-bp);
  section-nodes(section) := list(node);
  let two-buffer
    = make(<testing-special-purpose-buffer>,
           major-mode: find-mode(<testing-mode>));
  add-node!(two-buffer, node);

  let permanent-two-buffer-line-start-bp
    = make(<bp>, line: two-buffer-line, index: 0, buffer: two-buffer);
  check-condition
    ("creating an interval that spans two buffers errors",
     <error>,
     make-interval
       (permanent-two-buffer-line-start-bp, two-buffer-line-end-bp));

  reset-state();

  // -=- count-lines -=-

  check-equal
    ("Count of lines in empty interval is 1",
     count-lines(empty-interval), 1);
  check-equal
    ("Count of lines in single-line interval is 1",
     count-lines(line-interval), 1);
  check-equal
    ("Count of lines in interval just over EOL is 2",
     count-lines(eol-interval), 2);
  check-equal
    ("Count of lines in 4-line interval with partial lines is 4",
     count-lines(small-interval), 4);
  check-equal
    ("Count of lines in 4-line interval with whole lines is 4",
     count-lines(interval), 4);


  // -=- do-characters -=-

  let accum :: <list> = #();
  let count :: <integer> = 0;
  local
    method accumulate-even-characters
	(char :: <character>, line :: <line>, index :: <integer>)
      even?(count)
	& (accum := pair(char, accum));
      count := count + 1;
    end method,
    method reset-state () => ()
      count := 0;
      accum := #();
    end method,
    method accumulator-getter () => (str :: <string>)
      let local-accum = reverse(accum);
      let result = make(<string>, size: local-accum.size);
      map-into(result, identity, local-accum)
    end method;

  // ... default skip test

  reset-state();
  do-characters(accumulate-even-characters, empty-interval);
  check-equal
    ("do-characters forward, default skip-test, empty interval",
     accumulator-getter(), "");

  reset-state();
  do-characters(accumulate-even-characters, line-interval);
  check-equal
    ("do-characters forward, default skip-test, single line",
     accumulator-getter(), "Ln ");

  reset-state();
  do-characters(accumulate-even-characters, eol-interval);
  check-equal
    ("do-characters forward, default skip-test, interval over just EOL",
     accumulator-getter(), "\n");

  reset-state();
  do-characters(accumulate-even-characters, interval);
  check-equal
    ("do-characters forward, default skip-test, whole lines",
     accumulator-getter(), "Ln \nie2Ln \nie4");

  reset-state();
  do-characters(accumulate-even-characters, small-interval);
  check-equal
    ("do-characters forward, default skip-test, partial lines",
     accumulator-getter(), "n \nie2Ln \nie");

  reset-state();
  do-characters(accumulate-even-characters, interval, from-end?: #t);
  check-equal
    ("do-characters from-end, default skip-test, whole lines",
     accumulator-getter(), "4ei\n nL2ei\n nL");

  reset-state();
  do-characters(accumulate-even-characters, small-interval, from-end?: #t);
  check-equal
    ("do-characters from-end, default skip-test, partial lines",
     accumulator-getter(), "ei\n nL2ei\n n");


  // -=- count-characters -=-
  check-equal
    ("Count of characters in empty interval is 0",
     count-characters(empty-interval), 0);
  check-equal
    ("Count of characters in single-line interval is 6",
     count-characters(line-interval), 6);
  check-equal
    ("Count of characters in interval just over EOL is 1",
     count-characters(eol-interval), 1);
  check-equal
    ("Count of characters in 4-line interval with partial lines is 23",
     count-characters(small-interval), 23);
  check-equal
    ("Count of characters in 4-line interval with whole lines is 27",
     count-characters(interval), 27);


  // -=- as(<string>, ...) -=-
  let interval = make-interval(line1-start-bp, line4-end-bp);
  let small-interval = make-interval(small-start-bp, small-end-bp);
  let line-interval = make-interval(line1-start-bp, line1-end-bp);
  let empty-interval = make-interval(line1-start-bp, line1-start-bp);
  let eol-interval = make-interval(line1-end-bp, line2-start-bp);

  check-equal
    ("empty interval as <string>",
     as(<string>, empty-interval), "");
  check-equal
    ("EOL interval as <string>",
     as(<string>, eol-interval), "\n");
  check-equal
    ("line interval as <string>",
     as(<string>, line-interval), "Line 1");
  check-equal
    ("small interval as <string>",
     as(<string>, small-interval), "ne 1\nLine 2\nLine 3\nLine");
  check-equal
    ("larger interval as <string>",
     as(<string>, interval), "Line 1\nLine 2\nLine 3\nLine 4");
end test;



define test interval-contents-test ()
  // -=- Setup -=-

  reset-testing-buffer-state();

  dynamic-bind(*buffer* = *testing-buffer*)
    // -=- copy-interval -=-
    // Only need to test this on one representative complex interval,
    // because that guarantees it's right for smaller intervals, since
    // we've already tested copy-line and do-lines.
    begin
/*
      local
        method last-line-from (line :: <line>) => (last-line :: <line>)
          // Follow the line-next chain from line, until we hit #f,
          // returning the last line we saw.
          for (this = line then line-next(this), until: ~line-next(this))
          finally this
          end for;
        end method;
*/
      let part-sections-interval-copy
        = copy-interval(*part-sections-interval*);
      let start-bp = interval-start-bp(part-sections-interval-copy);
      let end-bp = interval-end-bp(part-sections-interval-copy);
      check-true
	("Copied part-sections interval",
         last-line-from(bp-line(start-bp)) == bp-line(end-bp));
      check-equal
	("Contents equal after copying",
         as(<string>, part-sections-interval-copy),
         as(<string>, *part-sections-interval*));
    end;
  

    // -=- bp-within-interval? -=-
    check-true
      ("BP within empty interval",
       bp-within-interval?(*start1-1-bp*, *empty-interval*));
    check-false
      ("BP NOT within empty interval",
       bp-within-interval?(*middle1-1-bp*, *empty-interval*));

    check-true
      ("BP within EOL interval (1)",
       bp-within-interval?(*end1-1-bp*, *eol-interval*));
    check-true
      ("BP within EOL interval (2)",
       bp-within-interval?(*start1-2-bp*, *eol-interval*));
    check-false
      ("BP NOT within EOL interval",
       bp-within-interval?(*middle1-1-bp*, *eol-interval*));

    check-true
      ("BP within line interval",
       bp-within-interval?(*middle1-1-bp*, *line-interval*));
    // --- [Check start and end as well?  Doesn't seem worth it.]
    check-false
      ("BP NOT within line interval",
       bp-within-interval?(*start1-2-bp*, *line-interval*));

    check-true
      ("BP within part-lines interval",
       bp-within-interval?(*start1-2-bp*, *part-lines-interval*));
    // --- [Check start and end as well?  Doesn't seem worth it.]
    check-false
      ("BP NOT within part-lines interval",
       bp-within-interval?(*end1-3-bp*, *part-lines-interval*));

    check-true
      ("BP within section interval",
       bp-within-interval?(*start1-2-bp*, *section-interval*));
    // --- [Check start and end as well?  Doesn't seem worth it.]
    check-false
      ("BP NOT within section interval",
       bp-within-interval?(*start2-1-bp*, *section-interval*));

    check-true
      ("BP within part-sections interval",
       bp-within-interval?(*start2-1-bp*, *part-sections-interval*));
    // --- [Check start and end as well?  Doesn't seem worth it.]
    check-false
      ("BP NOT within part-sections interval",
       bp-within-interval?(*end3-1-bp*, *part-sections-interval*));

    check-true
      ("BP within buffer interval (1)",
       bp-within-interval?(*start1-1-bp*, *buffer-interval*));
    check-true
      ("BP within buffer interval (2)",
       bp-within-interval?(*start2-1-bp*, *buffer-interval*));
    check-true
      ("BP within buffer interval (3)",
       bp-within-interval?(*end3-1-bp*, *buffer-interval*));


    // -=- interval-read-only?[-setter] -=-
    // NOTE: If any subclasses of <interval> define their own methods,
    // we should test their behaviour in addition; but none do, yet. 

    check-false
      ("buffer-interval is NOT read-only (1)",
       interval-read-only?(*buffer-interval*));

    interval-read-only?(*empty-interval*) := #t;
    check-true
      ("empty-interval is read-only (1)",
       interval-read-only?(*empty-interval*));
    check-true
      ("EOL-interval is read-only (1)",
       interval-read-only?(*eol-interval*));
    check-true
      ("line-interval is read-only (1)",
       interval-read-only?(*line-interval*));
    check-true
      ("part-lines-interval is read-only (1)",
       interval-read-only?(*part-lines-interval*));
    check-true
      ("section-interval is read-only (1)",
       interval-read-only?(*section-interval*));
    check-false
      ("part-sections-interval is NOT read-only (1)",
       interval-read-only?(*part-sections-interval*));
    check-true
      ("buffer-interval is read-only (2)",
       interval-read-only?(*buffer-interval*));

    interval-read-only?(*empty-interval*) := #f;
    interval-read-only?(*part-sections-interval*) := #t;
    check-false
      ("empty-interval is NOT read-only (2)",
       interval-read-only?(*empty-interval*));
    check-false
      ("EOL-interval is NOT read-only (2)",
       interval-read-only?(*eol-interval*));
    check-false
      ("line-interval is NOT read-only (2)",
       interval-read-only?(*line-interval*));
    check-true
      ("part-lines-interval is read-only (2)",
       interval-read-only?(*part-lines-interval*));
    check-true
      ("section-interval is read-only (2)",
       interval-read-only?(*section-interval*));
    check-true
      ("part-sections-interval is read-only (2)",
       interval-read-only?(*part-sections-interval*));
  end dynamic-bind;
end test;

define suite interval-suite ()
  test interval-creation-test;
  test interval-characters-test;
  test interval-contents-test;
end suite;

/*

Things to test for intervals ...

interval-{start,end}-bp[-setter]
  Nothing to test for <{basic,simple}-interval> -- they're just slots.

method on make for <interval>
  Check that in-order? is obeyed (even if <bp>s aren't in order).

make-interval
  [Not worth testing for now.]

copy-interval (for <basic-interval>)
  [Check the use of skip-test?  Hardly seems worth it.]
  Manually check the line linking structure after copying.

bp-within-interval?
  Check for empty, eol, {part,whole}-{line,section} and whole-buffer
    intervals.

do-lines
  Check with some "accumulator" function.
  Check the use of skip-test; and of from-end?.
  Check for empty, eol, single-line and multi-line intervals.
  Check for the start, end, middle or all lines satisfying the
    skip-test.
  Check that the start and end indices are correctly used for the
    first and last lines.
  Check for lines with different buffers (should error).

count-lines
  [Check the use of skip-test?  Hardly seems worth it.]
  Check for empty, eol, single-line and multi-line intervals.  [MS
    Word gives '1' for eol interval.]

do-characters
  Check with some "accumulator" function.
  Check the use of from-end?.
  Check that the start and end indices are correctly used for the
    first and last lines.
  Check for empty, eol, single-line and multi-line intervals.
  Check that the right number of '\n's are included.
  [I think the "ei >= length" test in the "~from-end?" case should
    really be "ei > length", since the end index is exclusive (as
    usual in Dylan); but this should be impossible anyway, except
    maybe for non-moving BPs, if text gets edited under them.]

count-characters
  Check the use of skip-test.
  Check for empty, eol, single-line and multi-line intervals.  [MS
    Word gives '0' for eol interval this time.]

method on as(<string>, ...) for <interval>
  Compare result to existing <string> with \=.  Check with empty and
    non-empty strings.
  Check for empty, eol, single-line and multi-line intervals.

interval-read-only?[-setter]
  Check setting and getting, for empty, eol,
    {part,whole}-{line,section} and whole-buffer intervals.
  Check for lines in read-only buffers.
  Check for intervals with read-only lines in read-write buffers.
  Check for intervals with all read-write lines in read-write
    buffers.

*/

/* ---*** Low priority for testing, for now.

// -=- Nodes -=-

define test node-creation-test ()
  // node-section for different classes
  // node-definition-{name,type}
end test;

define test node-contents-test ()
  // do-lines
end test;

define suite node-suite ()
  test node-creation-test;
  test node-contents-test;
end suite;

*/

/*

Things to test for nodes ...

node-{next,previous,parent,children,buffer}[-setter]
  [Why only one parent?  We could allow a graph.]
  Nothing to test (for <basic-node>) -- they're just slots.

note-node-changed
  Nothing to test -- this does nothing, for now.

methods on do-lines, count-lines
  for <basic-node>
    For now, this just errors; check that.
  for <section-node>
    Hardly worth testing -- just delegates on the node-section.

node-section[-setter]
  Check #f for non-section nodes, and the correct <section> for
    <section-node>s.  

node-definition-{name,type}
  Check that you get #f for non-definition nodes, and the appropriate
    values from the <section> for definition nodes.

*/
