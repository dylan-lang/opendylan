Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -=- Auxilliary definitions -=-

define constant $insertion-line-indices :: <simple-object-vector>
  = vector(0, 1, 2);

define constant $insertion-indices :: <simple-object-vector>
  = vector
      (pair(-1, "before start"),
       pair(0, "at start"),
       pair(3, "in middle"),
       pair(6, "at end"),
       pair(7, "after end"));

define constant $insert-results-format
  = vector
      ("%sLine 1\nLine 2\nLine 3\nLine 4\nLine 5",
       "Lin%se 1\nLine 2\nLine 3\nLine 4\nLine 5",
       "Line 1%s\nLine 2\nLine 3\nLine 4\nLine 5",
       "Line 1\n%sLine 2\nLine 3\nLine 4\nLine 5",
       "Line 1\nLin%se 2\nLine 3\nLine 4\nLine 5",
       "Line 1\nLine 2%s\nLine 3\nLine 4\nLine 5",
       "Line 1\nLine 2\n%sLine 3\nLine 4\nLine 5",
       "Line 1\nLine 2\nLin%se 3\nLine 4\nLine 5",
       "Line 1\nLine 2\nLine 3%s\nLine 4\nLine 5");

define function make-result-vector (str :: <string>) => (results :: <vector>)
  map(rcurry(format-to-string, str), $insert-results-format)
end function;

define constant $insert-into-line-characters
  = vector("\n", "X");

define constant $insert-into-line-strings
  = vector
      ("", "\n", "ABC", "\nABC", "A\nBC", "ABC\n", "A\nB\nC\n", "A\n\n\nBC");


// -=- Tests and Suites -=-

// ---*** This _doesn't_ test that moving BPs get moved correctly :-(
// I'm not sure that we can easily check they get moved to the right
// place, but we _could_ at least check that they're on a line in the
// buffer and at a valid index for that line.  Could also check that
// node-{start,end}-bps are indeed at the start/end of lines which
// have no line-{previous,next}.

define function test-insert!
    (thing :: type-union(<character>, <string>, <interval>),
     results :: <vector>)
 => ()
  let result-index :: <integer> = 0;
  let thing-without-newlines
    = select ( thing.object-class )
        <character> => if ( thing == '\n' ) 'N' else thing end;
        <string> =>
          map(method (c) if ( c == '\n' ) 'N' else c end; end, thing);
        otherwise => thing
      end;
  let check-message
    = format-to-string("Insert %=", thing);
  for (line-index :: <integer> in $insertion-line-indices)
    let check-message
      = format-to-string
	  ("%s at line %=", check-message, line-index);
    for (idx :: <pair> in $insertion-indices)
      let (index, index-description) = values(head(idx), tail(idx));
      let check-message
	= format-to-string
	    ("%s, %s", check-message, index-description);
      reset-testing-buffer-state();
      dynamic-bind(*buffer* = *testing-buffer*)
        // Get the "line-index"th line in the buffer.  We can't just
        // iterate over a collection of *line1-{1,2,3}*, as these
        // global variables get rebound every time the buffer state is
        // reset.
        let buffer-lines :: <vector> = testing-lines();
        let insertion-lines :: <vector>
          = vector(buffer-lines[0], buffer-lines[1], buffer-lines[2]);
        let line :: <text-line> = insertion-lines[line-index];
        // Check contents after insertion (or check for <error>).
        if (index = -1 | index = 7)
          check-condition
            (check-message,
             <error>,
             insert!(make-bp(line, index), thing));
        else
          insert!(make-bp(line, index), thing);
          check-equal
            (check-message,
             as(<string>, *buffer*),
             results[result-index]);
          result-index := result-index + 1;
        end if;
        // Check buffer structure.
        check-true
          ("Buffer line link structure okay after insertion",
           last-line-from
             (first(buffer-lines), in-buffer?: #t) == last(buffer-lines));
        // If the following fails, you can add ", verbose?: #t" after
        // "*buffer*" below to get some useful(?) diagnostics.
        check-true
          ("Buffer BPs okay after insertion",
           check-buffer-bps(*buffer*));
      end dynamic-bind;
    end for;
  end for;
end function;

define test insert-character-into-line-test ()
  for (str-index from 0 below size($insert-into-line-characters))
    let char-str :: <string> = $insert-into-line-characters[str-index];
    let results = make-result-vector(char-str);
    // Note the extra "[0]" here, as compared with the next test.
    // It extracts the first <character> from a <string>.
    test-insert!(char-str[0], results);
  end for;
end test;

define test insert-string-into-line-test ()
  for (str-index from 0 below size($insert-into-line-strings))
    let string :: <string> = $insert-into-line-strings[str-index];
    let results :: <vector> = make-result-vector(string);
    test-insert!(string, results);
  end for;
end test;


define test insert-interval-test ()
  let intervals :: <vector> = vector();

  reset-testing-buffer-state();
  dynamic-bind(*buffer* = *testing-buffer*)
    intervals := map(copy-interval, testing-intervals());
  end dynamic-bind;

  for (interval in intervals)
    test-insert!(interval, make-result-vector(as(<string>, interval)));
  end for;
end test;

define suite insertion-suite ()
  test insert-character-into-line-test;
  test insert-string-into-line-test;
  test insert-interval-test;
end suite;

/*

Things to test ...

insert! for <character>
insert! for <string>
  See insert-into-line.

insert! for <basic-interval>
  Check inserting at {start,middle,end}-of-{line,section}.
  Check for empty, single-line and multi-line intervals.
  [At the moment, we seem to be assuming that to-be-inserted intervals
    contain no <section-node>s, just <basic-node>s.  If/when we do,
    check for inserting intervals spanning 2 and 3 sections.]
  [I assume that later we may want to be able to insert "section
    breaks", if the interval spans sections.  I expect we'd have to
    create fresh sections and add the lines to them.]
  [What happens if we insert an interval into the middle of itself?!?
    If it's something bad, maybe we'd better catch that!  A: We don't
    expect to, since we normally only insert from the clipboard or
    kill-ring.  We could check for this but it would be expensive.]

insert-into-line for <text-line>, <character>
  Check inserting at -1, 0, middle-of-line, end-of-line, past-end.
  Check for '\n' and non-'\n' characters.
  Check lines with and without moving-bps.
  [Why doesn't the bp-relocating code need to check that the bps are
    <moving-bp>s?  Are all bps inserted into <text-line>s guaranteed
    to be moving?]

insert-into-line for <text-line>, <string>
  [What is the "insert-moving!(nbp, '\n')" in the ">1-'\n's" clause
    for?]
  Check inserting at -1, 0, middle-of-line, end-of-line, past-end.
  Check for strings containing 0, 1 and n '\n's; the string "\n"; and
    a string with 2/3 consecutive '\n's.
  Check lines with and without moving-bps.
  [Why doesn't the bp-relocating code need to check that the bps are
    <moving-bp>s?  Are all bps inserted into <text-line>s guaranteed
    to be moving?]

insert-into-line for <diagram-line>, XXX
  [Won't bother testing these for now.]
  [What if you insert a <string> which contains (begins/ends with?) a
    '\n'?  Will you get the same effect as if you had inserted each
    character separately, given that the <character> method treats
    '\n' specially?]

split-for-insertion
  [This is tested within the insert-into-line tests.]
  Check inserting at -1, 0, middle-of-line, end-of-line, past-end.
  Check splitting first, middle and last line in a section.

insert-moving!
  [Hardly worth testing.]
  Check that the bp points to the inserted object (and that it's == to
    the original argument)?

*/
