Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test bp-creation-test ()
  let buffer = make(<non-file-buffer>,
		    major-mode: find-mode(<fundamental-mode>));
  let line = make(<text-line>, contents: "A line of text");
  dynamic-bind (*buffer* = buffer)
    let sbp = make(<bp>, line: line, index: 0);
    let pbp = make(<bp>, line: line, index: 0, buffer: buffer);
    let mbp = make(<bp>, line: line, index: 0, buffer: buffer, moving?: #t);
    // Check range-checking in make-bp
    check-condition
      ("make-bp with index below range",
       <error>,
       make-bp(line, -1));
    check-true
      ("make-bp with index in range",
       make-bp(line, 1));
    check-condition
      ("make-bp with index above range",
       <error>,
       make-bp(line, 100));
    // Correct classes?
    check-true
      ("BP is simple",
       instance?(sbp, <simple-bp>));
    check-true
      ("BP is permanent",
       instance?(pbp, <permanent-bp>));
    // 'simple-bp?' correct?
    check-true
      ("'simple-bp?' works on a simple BP",
       simple-bp?(sbp));
    check-false
      ("'simple-bp?' works on a permanent BP",
       simple-bp?(pbp));
    check-false
      ("'simple-bp?' works on a moving BP",
       simple-bp?(mbp));
    // 'moving-bp?' correct?
    check-false
      ("'moving-bp?' works on a simple BP",
       moving-bp?(sbp));
    check-false
      ("'moving-bp?' works on a permanent BP",
       moving-bp?(pbp));
    check-true
      ("'moving-bp?' works on a moving BP",
       moving-bp?(mbp));
    // This tests both 'copy-bp' and \= on BPs
    let (sbp-copy, pbp-copy, mbp-copy) = values(#f, #f, #f);
    check-equal
      ("Simple BPs copy and compare",
       sbp, (sbp-copy := copy-bp(sbp)));
    check-equal
      ("Permanent BPs copy and compare",
       pbp, (pbp-copy := copy-bp(pbp)));
    check-equal
      ("Moving BPs copy and compare",
       mbp, (mbp-copy := copy-bp(mbp)));
    check-equal
      ("Simple BP and moving BP at same place are \\=",
       sbp, mbp);
    check-equal
      ("Simple BP and permanent BP at same place are \\=",
       sbp, pbp);
    check-equal
      ("Permanent BP and moving BP at same place are \\=",
       pbp, mbp);
  end
end test bp-creation-test;

define test bp-contents-test ()
  let buffer = make(<non-file-buffer>,
		    major-mode: find-mode(<fundamental-mode>));
  // We'll modify the contents of line1, so we use copy-sequence.
  let line1 = make(<text-line>, contents: copy-sequence("A line of text"));
  let line2 = make(<text-line>, contents: "Another line of text");
  let non-text-line = make(<testing-diagram-line>);
  dynamic-bind (*buffer* = buffer)
    let bp = make(<bp>, line: line1, index: 0);

    // We're testing both movement and contents
    // (bp-character[-{before,setter}], looking-at?) here.
    check-condition
      ("bp-character-setter cannot set to '\\n'",
       <error>,
       bp-character(bp) := '\n');
    check-equal
      ("Line 1, 1st character set to 'O'",
       (bp-character(bp) := 'O'), 'O');
    check-equal
      ("Line 1, 1st character correct",
       bp-character(bp), 'O');
    // ---*** Does the next test make sense?!
    check-equal
      ("Character before 1st character correct",
       bp-character-before(bp), '\n');
    move-bp!(bp, line1, line-length(line1) - 1);
    check-equal
      ("Line 1, last character set to '!'",
       (bp-character(bp) := '!'), '!');
    check-equal
      ("Line 1, last character correct",
       bp-character(bp), '!');
    check-equal
      ("Character before line 1, last character correct",
       bp-character-before(bp), 'x');
    move-bp!(bp, line1, 3);
    check-equal
      ("BP moved to 4th character",
       bp-index(bp), 3);
    check-equal
      ("BP still on 1st line",
       bp-line(bp), line1);
    check-equal
      ("4th character set to 'o'",
       (bp-character(bp) := 'o'), 'o');
    check-equal
      ("4th character correct",
       bp-character(bp), 'o');
    check-equal
      ("Character before fourth character correct",
       bp-character-before(bp), 'l');
    check-true
      ("BP is looking at \"one of\"",
       bp-looking-at?(bp, "one of"));
    move-bp!(bp, line1, line-length(line1) - 1);
    check-true
      ("BP is looking at \"!\"",
       bp-looking-at?(bp, "!"));
    move-bp!(bp, line2, 0);
    check-equal
      ("BP now on second line",
       bp-line(bp), line2);
    check-equal
      ("BP moved to first character",
       bp-index(bp), 0);
    check-true
      ("BP is looking at \"Another\"",
       bp-looking-at?(bp, "Another"));

    // Try to look at contents of non-text-line.
    move-bp!(bp, non-text-line, 0);
    check-condition
      ("Set first character of non-text-line (should error)",
       <error>, // No applicable methods.
       (bp-character(bp) := 'X'));
    check-equal
      ("Get first character of non-text-line",
       bp-character(bp), '\n');
    check-equal
      ("Get character before first character of line with no previous",
       bp-character-before(bp), '\n');
    check-false
      ("BP is not looking at anything on a non-text line",
       bp-looking-at?(bp, ""));
  end
end test bp-contents-test;

define test moving-bp-test ()
  let buffer = make(<non-file-buffer>,
		    major-mode: find-mode(<fundamental-mode>));
  let line1 = make(<text-line>, contents: "A line of text");
  let line2 = make(<text-line>, contents: "A line of text");
  dynamic-bind (*buffer* = buffer)
    let bp = make(<bp>, line: line1, index: 0, moving?: #t);
    let non-moving-bp = make(<bp>, line: line1, index: 0);
    check-equal
      ("Moving and non-moving bp at same place are \\=",
       bp, non-moving-bp);
    check-equal
      ("Line 1 has a moving BP in it",
       line-bps(line1), list(bp));
    check-true
      ("Line 2 has no BPs in it",
       empty?(line-bps(line2)));
    move-bp!(bp, line1, 2);
    check-true
      ("Line 1 still has a moving BP in it",
       member?(bp, line-bps(line1)));
    move-bp!(bp, line2, 0);
    check-equal
      ("Line 1 no longer has a moving BP in it",
       line-bps(line1), #());
    check-true
      ("Line 2 now has a moving BP in it",
       member?(bp, line-bps(line2)));
    let bp2 = make(<bp>, line: line2, index: 0, moving?: #t);
    check-equal
      ("Moving BPs at same place (after move) are \\=",
       bp, bp2);
    kill-bp!(bp);
    kill-bp!(bp2);
    // We DON'T kill the non-moving-bp, as we're testing to make sure
    // that it doesn't appear in the line-bps lists.
    check-equal
      ("Line 1 no longer has any BPs in it after kill",
       line-bps(line1), #());
    check-equal
      ("Line 2 no longer has any BPs in it after kill",
       line-bps(line2), #());
  end
end test moving-bp-test;

define test bp-less-test ()
  let buffer
    = make(<non-file-buffer>,
           major-mode: find-mode(<fundamental-mode>));
  let line1 = make(<text-line>, contents: "Line1", previous: #f);
  let line2 = make(<text-line>, contents: "Line2", previous: line1);
  let line3 = make(<text-line>, contents: "Line3", previous: line2, next: #f);
  line-next(line1) := line2;
  line-next(line2) := line3;
  dynamic-bind (*buffer* = buffer)
    let bp1 = make(<bp>, line: line2, index: 2, buffer: buffer);
    let bp2 = make(<bp>, line: line1, index: 0, buffer: buffer);

    // Check all 8 possibilities: {less,equal,greater} x {line,index}.
    check-false
      ("BP at line 2 index 2 NOT less than BP at line 1 index 0",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line1, 2);
    check-false
      ("BP at line 2 index 2 NOT less than BP at line 1 index 2",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line1, 4);
    check-false
      ("BP at line 2 index 2 NOT less than BP at line 1 index 4",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line2, 0);
    check-false
      ("BP at line 2 index 2 NOT less than BP at line 2 index 0",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line2, 2);
    check-false
      ("BP at line 2 index 2 NOT less than BP at line 2 index 2",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line2, 4);
    check-true
      ("BP at line 2 index 2 less than BP at line 2 index 4",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line3, 0);
    check-true
      ("BP at line 2 index 2 less than BP at line 3 index 0",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line3, 2);
    check-true
      ("BP at line 2 index 2 less than BP at line 3 index 2",
       bp-less?(bp1, bp2));
    move-bp!(bp2, line3, 4);
    check-true
      ("BP at line 2 index 2 less than BP at line 3 index 4",
       bp-less?(bp1, bp2));

    // Try comparing bps from different buffers -- should error.
    let other-buffer
      = make(<non-file-buffer>,
             major-mode: find-mode(<fundamental-mode>));
    let other-buffer-bp
      = make(<bp>, line: line2, index: 2, buffer: other-buffer);
    check-condition
      ("Comparing BPs from different buffers signals error",
       <error>,
       bp-less?(bp1, other-buffer-bp));

    // Check the results of order-bps.
  /* ---*** order-bps isn't exported from "deuce"; should we export it
  // elsewhere for testing?
    move-bp!(bp1, line2, 2);
    local
      method order-bps-into-sequence (bp1 :: <bp>, bp2 :: <bp>)
        let (#rest bps) = order-bps(bp1, bp2);
        bps
      end method;
    move-bp!(bp2, line2, 2);
    check-true
      ("Ordered bps which were \\= are still \\=",
       apply(\=, order-bps-into-sequence(bp1, bp2)));
    move-bp!(bp2, line2, 4);
    check-true
      ("Ordered bps which were bp-less? are still bp-less?",
       apply(bp-less?, order-bps-into-sequence(bp1, bp2)));
    move-bp!(bp2, line2, 0);
    check-true
      ("Ordered bps which were not bp-less? now are",
       apply(bp-less?, order-bps-into-sequence(bp1, bp2)));
  */

  end;
end test bp-less-test;

define suite bp-suite ()
  test bp-creation-test;
  test bp-contents-test;
  test moving-bp-test;
  test bp-less-test;
end suite bp-suite;

/*

Things to test ...

Different subclasses of <bp>
  Use instance? to test result of make.
  Check result of moving-bp?, simple-bp?

make-bp
  Check for index in- and out-of-range.

\=
  Check for copied bps.
  Check for bps which have been moved around.
  Check for different subclasses of bp.
  Check for bps from different buffers (should give #f).

bp-{line,index,character}[-setter]
  Check making and getting these values.
  Check moving with move-bp! and getting these values.

bp-less?
  Check for bps which are less, equal and greater in line, index or
    both.
  Check for different subclasses of bp.
  Check for bps from different buffers (should error).

order-bps
  Check this on bps which are \= and (others) which are bp-less?

initialize for <moving-bp-mixin>
  Check that created bps which are NOT <moving-bp>s are NOT present in
    the line-bps list.

copy-bp
  Check that copied bps of various subclasses are \= but not \==

kill-bp!
  Check that the bp-line has no reference to the bp afterwards.

bp-character
  Check at start, middle and end of line.
  Check on a <line> which is not a <text-line> (should error).

bp-character-before
  Check at start, middle and end of line.
  Check on a <line> (or preceding line) which is not a <text-line>
    (should error).
  [What if the previous line is not a <text-line>, or there is no
    previous line?  Does it make sense always to return '\n'?]

bp-character-setter
  Check at start, middle and end of line.
  Check on a <line> which is not a <text-line> (should error).
  Check that it can't insert a '\n' (should error).
  [Like with bp-character, maybe use "line-character-setter"?]

bp-looking-at?
  Check at start, middle and end of line.
  Check on a <line> which is not a <text-line> (should return #f).

*/
