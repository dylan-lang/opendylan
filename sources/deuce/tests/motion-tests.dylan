Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -=- Auxiliary Definitions -=-

define function check-bp-position
    (bp :: <basic-bp>, predicate :: <function>, target :: <basic-bp>) => ()
  check-true
    ("bp position-predicate okay",
     predicate(bp));
  check-equal
    ("bp is at expected position",
     bp, target);
end function;

define function test-moving-out-of-interval
    (interval :: <interval>)
 => ()
  // Test moving past ends of nodes, with and without fixup.
  let start-bp = copy-bp(interval-start-bp(interval));
  let end-bp = copy-bp(interval-end-bp(interval));
  let start-bp-copy = copy-bp(start-bp);
  let end-bp-copy = copy-bp(end-bp);
  // ... with interval and fixup
  decrement-bp!(start-bp, interval: interval);
  check-equal
    ("start bp in same place after fixed-up move out of interval",
     start-bp, start-bp-copy);
  increment-bp!(end-bp, interval: interval);
  check-equal
    ("end bp in same place after fixed-up move out of interval",
     end-bp, end-bp-copy);

  // ... with interval but no fixup
  check-false
    ("start moved outside interval (no fixup) returns #f",
     decrement-bp!(start-bp, fixup?: #f, interval: interval));
  check-equal
    ("start bp in same place after move (no fixup) out of interval",
     start-bp, start-bp-copy);
  check-false
    ("end moved outside interval (no fixup) returns #f",
     increment-bp!(end-bp, fixup?: #f, interval: interval));
  check-equal
    ("end bp in same place after move (no fixup) out of interval",
     end-bp, end-bp-copy);

  // It wouldn't make sense to check "with no interval", since we're
  // not allowed to supply "interval: #f".  If the interval is
  // unspecified, we get "the whole buffer", which we'll test elsewhere.
end function;


// -=- Tests and Suite -=-

define test motion-basic-test ()
  reset-testing-buffer-state();
  dynamic-bind(*buffer* = *testing-buffer*)
    let len = line-length(*line1-2*); // All test lines are same length.
    let not-start-of-line? = complement(start-of-line?);
    let not-end-of-line? = complement(end-of-line?);

    // -=- {start,end}-of-line? predicates and {in,de}crement-bp! -=-

    // Test moving within lines, and past ends of lines.
    let line1-2-start = line-start(*line1-2*);
    let line1-2-end = line-end(*line1-2*);
    // ... check initial position
    test-output("Checking initial position\n");
    check-bp-position
      (line1-2-start, start-of-line?, make-bp(*line1-2*, 0));
    check-bp-position
      (line1-2-end, end-of-line?, make-bp(*line1-2*, len));

    // ... move within line
    test-output("Moving within line\n");
    increment-bp!(line1-2-start);
    check-bp-position
      (line1-2-start, not-start-of-line?, make-bp(*line1-2*, 1));
    decrement-bp!(line1-2-end);
    check-bp-position
      (line1-2-end, not-end-of-line?, make-bp(*line1-2*, len - 1));

    // ... move past line ends
    test-output("Moving past line ends\n");
    decrement-bp!(line1-2-start);
    decrement-bp!(line1-2-start);
    check-bp-position
      (line1-2-start, end-of-line?, make-bp(*line1-1*, len));
    increment-bp!(line1-2-end);
    increment-bp!(line1-2-end);
    check-bp-position
      (line1-2-end, start-of-line?, make-bp(*line1-3*, 0));

    // Test moving past ends of nodes, with and without fixup.
    test-output("Moving out of node/section interval\n");
    test-moving-out-of-interval(*node2*);

    // Test moving past ends of buffer, with and without fixup.
    test-output("Moving out of buffer interval\n");
    test-moving-out-of-interval(*buffer*);

    // Test moving past ends of arbitrary interval.
    test-output("Moving out of arbitrary interval\n");
    test-moving-out-of-interval(*part-sections-interval*);


    // -=- move-forward-or-backward! -=-

    local
      method matcher (char :: <character>, exclude? :: <boolean>)
        method (c :: <character>) values(c == char, exclude?) end
      end method,
      method check-move
          (char :: <character>, exclude? :: <boolean>, reverse? :: <boolean>,
           target-bp :: <simple-bp>,
           #key interval)
        let bp :: <basic-bp> = make-bp(*line1-3*, 2);
        let buff-ivl = bp-buffer(bp);
        let ivl-supplied? :: <boolean> = #t;
        unless (interval)
          interval := buff-ivl;
          ivl-supplied? := #f;
        end;
        move-forward-or-backward!
          (bp, matcher(char, exclude?), reverse?, interval: interval);
        check-equal
          (format-to-string
             ("Move %sward from 'n' to %=, %sclusive%s",
              if (reverse?) "back" else "for" end, char,
              if (exclude?) "ex" else "in" end,
              if (ivl-supplied?) ", bounded by interval" else "" end),
           bp, target-bp);
      end method;

    // ... moving to a different character
    check-move(' ', #f, #f, make-bp(*line1-3*, 5));
    check-move(' ', #t, #f, make-bp(*line1-3*, 4));
    check-move('L', #f, #t, make-bp(*line1-3*, 0));
    check-move('L', #t, #t, make-bp(*line1-3*, 1));

    // ... moving to same character as start
    check-move('n', #f, #f, make-bp(*line1-3*, 3));
    check-move('n', #t, #f, make-bp(*line1-3*, 2));
    check-move('n', #f, #t, make-bp(*line1-2*, 2));
    check-move('n', #t, #t, make-bp(*line1-2*, 3));

    let line1-3-ivl
      = make-interval(line-start(*line1-3*), line-end(*line1-3*));

    // ... moving to character at interval bound
    check-move('\n', #f, #f, line-end(*line1-3*), interval: line1-3-ivl);
    check-move('\n', #t, #f, line-end(*line1-3*), interval: line1-3-ivl);
    check-move('L', #f, #t, line-start(*line1-3*), interval: line1-3-ivl);
    check-move('L', #t, #t, make-bp(*line1-3*, 1), interval: line1-3-ivl);

    // ... moving to character past interval bound
    check-move('i', #t, #f, line-end(*line1-3*), interval: line1-3-ivl);
    check-move('e', #t, #t, line-start(*line1-3*), interval: line1-3-ivl);
  end dynamic-bind;
end test;

define test motion-over-test ()
  reset-testing-buffer-state();
  dynamic-bind(*buffer* = *testing-buffer*)
    let len = line-length(*line1-2*); // All test lines are same length.
    let start-bp = make-bp(*line1-3*, 2);
    local
      method check-move-over
          (move-over :: <function>, n :: <integer>, target-bp :: <simple-bp>,
           #rest args)
        let bp :: <basic-bp> = start-bp;
        let result-bp :: <basic-bp> = apply(move-over, bp, n, args);
        check-equal
          (format-to-string
             ("Move %d from %= to %=",
              n, bp-character(bp), bp-character(target-bp)),
           result-bp, target-bp);
      end method;

    // -=- move-over-characters -=-
    check-move-over(move-over-characters, 0, start-bp);
    check-move-over(move-over-characters, 1, make-bp(*line1-3*, 3));
    check-move-over(move-over-characters, -1, make-bp(*line1-3*, 1));
    check-move-over(move-over-characters, 2, make-bp(*line1-3*, 4));
    check-move-over(move-over-characters, -2, make-bp(*line1-3*, 0));
    check-move-over(move-over-characters, 100, make-bp(*line3-1*, len));
    check-move-over(move-over-characters, -100, make-bp(*line1-1*, 0));

    // -=- move-over-words -=-
    check-move-over(move-over-words, 0, start-bp);
    check-move-over(move-over-words, 1, make-bp(*line1-3*, 4));
    check-move-over(move-over-words, -1, make-bp(*line1-3*, 0));
    check-move-over(move-over-words, 2, make-bp(*line1-3*, 6));
    check-move-over(move-over-words, -2, make-bp(*line1-2*, 5));
    check-move-over(move-over-words, 100, make-bp(*line3-1*, len));
    check-move-over(move-over-words, -100, make-bp(*line1-1*, 0));

    // -=- move-over-lines -=-
    let diag-line2 = make(<testing-structural-diagram-line>);
    let diag-line3 = make(<testing-structural-diagram-line>);
    add-line!(*section2*, diag-line2, after: #"start");
    add-line!(*section3*, diag-line3, after: #"start");
    check-move-over(move-over-lines, 0, start-bp);
    check-move-over(move-over-lines, 1, line-start(*line2-1*));
    check-move-over(move-over-lines, 1, line-start(diag-line2), skip-test: #f);
    check-move-over(move-over-lines, -1, line-start(*line1-2*));
    check-move-over(move-over-lines, 2, line-start(*line3-1*));
    check-move-over(move-over-lines, 2, line-start(*line2-1*), skip-test: #f);
    check-move-over(move-over-lines, -2, line-start(*line1-1*));
    check-move-over(move-over-lines, 100, line-start(*line3-1*));
    check-move-over(move-over-lines, -100, line-start(*line1-1*));
    remove-line!(*section2*, diag-line2);
    remove-line!(*section3*, diag-line3);

/* ---*** Won't bother testing list traversal just now, because we
// know it's buggy, but it's not urgent.
    // -=- move-over-balanced-thing! -=-
    // ---***

    // -=- move-over-matched-thing! -=-
    // ---***

    // -=- move-over-lists -=-
    let list-line :: <text-line>
      = make(<text-line>,
        contents: "\\(foo (bar '\\)\\'' \\'baz '\\\\( (zom)' zarg)\\)");
    add-line!(*section1*, list-line);

    // ---*** What about $list-single-quote?  What does it mean?

    // ... starting from a $list-escape at the start of the line
    start-bp := make-bp(list-line, 0);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, 5));
    check-move-over(move-over-lists, -1, make-bp(*line1-3*, 5));
    check-move-over(move-over-lists, 2, make-bp(list-line, 41));
    check-move-over(move-over-lists, -2, make-bp(*line1-3*, 0));
    check-move-over(move-over-lists, 100, make-bp(*line3-1*, len));
    check-move-over(move-over-lists, -100, make-bp(*line1-1*, 0));

    // ... starting from an escaped $list-open near the start of the line
    start-bp := make-bp(list-line, 1);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, 5));
    check-move-over(move-over-lists, -1, make-bp(*line1-3*, 5));
    check-move-over(move-over-lists, 2, make-bp(list-line, 41));
    check-move-over(move-over-lists, -2, make-bp(*line1-3*, 0));

    // ... starting from a $word-alphabetic near the start of the line
    start-bp := make-bp(list-line, 2);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, 5));
    check-move-over(move-over-lists, -1, make-bp(*line1-3*, 5));
    check-move-over(move-over-lists, 2, make-bp(list-line, 41));
    check-move-over(move-over-lists, -2, make-bp(*line1-3*, 0));

    // ... starting from the first unescaped $list-open in the line
    start-bp := make-bp(list-line, 6);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, 41));
    check-move-over(move-over-lists, -1, make-bp(list-line, 2));
    check-move-over(move-over-lists, 2, make-bp(*line2-1*, 4));
    check-move-over(move-over-lists, -2, make-bp(*line1-3*, 5));

    // ... starting from the first unescaped $list-double-quote in the line
    start-bp := make-bp(list-line, 11);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, 17));
    check-move-over(move-over-lists, -1, make-bp(list-line, 6));
    check-move-over(move-over-lists, 2, make-bp(list-line, 35));
    check-move-over(move-over-lists, -2, make-bp(list-line, 0));

    // ... starting from a $list-escape of a $list-double-quote, just
    // inside the end of a matched pair is $list-double-quotes
    start-bp := make-bp(list-line, 14);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, 16));
    check-move-over(move-over-lists, -1, make-bp(list-line, 12));
    check-move-over(move-over-lists, 2, make-bp(list-line, 23));
    check-move-over(move-over-lists, -2, make-bp(list-line, 7));

    // ... starting from just after the second unescaped
    // $list-double-quote in the line
    start-bp := make-bp(list-line, 17);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, 23));
    check-move-over(move-over-lists, -1, make-bp(list-line, 11));
    check-move-over(move-over-lists, 2, make-bp(list-line, 35));
    check-move-over(move-over-lists, -2, make-bp(list-line, 7));

    // ... starting from a $list-escape followed by another
    // $list-escape, within a matched pair of $list-double-quotes
    start-bp := make-bp(list-line, 25);
    check-move-over(move-over-lists, 0, start-bp);
    check-move-over(move-over-lists, 1, make-bp(list-line, ));
    check-move-over(move-over-lists, -1, make-bp(list-line, ));
    check-move-over(move-over-lists, 2, make-bp(list-line, ));
    check-move-over(move-over-lists, -2, make-bp(list-line, ));

    // -=- move-up-or-down-lists -=- ---***?
*/
  end dynamic-bind;
end test;

/*
define test motion-over-or-until-test ()
  // -=- {for,back}ward-{over,until} -=-
  // ---*** Not too complicated, so not urgent to test.
end test;
*/

define test motion-bp-info-test ()
  reset-testing-buffer-state();
  dynamic-bind(*buffer* = *testing-buffer*)
    // -=- [relevant-]definition-interval -=-
    // --- Mode-specific, so won't test here.

    // -=- atom-under-bp -=-
    // [Not worth testing select-atom-under-bp on top of this.]
    // --- We don't test the syntax classes for atoms here.
    local
      method check-atom-under-bp
               (line :: <text-line>, index :: <integer>, target :: <string>)
        let bp :: <basic-bp> = make-bp(line, index);
        let (start-bp, end-bp) = atom-under-bp(bp);
        check-equal
          (format-to-string("Looking for atom at %=", bp),
           as(<string>, make-interval(start-bp, end-bp, in-order?: #t)),
           target);
      end method;

    // ... check at start/middle of line
    check-atom-under-bp(*line1-2*, 0, "Line"); // start of atom
    check-atom-under-bp(*line1-2*, 2, "Line"); // middle of atom
    check-atom-under-bp(*line1-2*, 3, "Line"); // end of atom
    check-atom-under-bp(*line1-2*, 4, "Line"); // after end of atom
    // ... check at end of line
    check-atom-under-bp(*line1-2*, 5, "2"); // within atom
    check-atom-under-bp(*line1-2*, 6, "2"); // after end of atom

    // ... check at start of node
    check-atom-under-bp(*line2-1*, 0, "Line"); // start of atom
    // ... check at end of node
    check-atom-under-bp(*line2-1*, 5, "4"); // within atom
    check-atom-under-bp(*line2-1*, 6, "4"); // after end of atom

    // ... check at start of buffer
    check-atom-under-bp(*line1-1*, 0, "Line"); // start of atom
    // ... check at end of buffer
    check-atom-under-bp(*line3-1*, 5, "5"); // within atom
    check-atom-under-bp(*line3-1*, 6, "5"); // after end of atom

    // -=- {char,line}-index->bp (and vice versa) -=-
    local
      method index->bp-checker
                 (converter :: <function>, description :: <string>)
              => (checker :: <method>)
               let check-message :: <string>
                 = format-to-string
                     ("Converting %s-index %%d to bp", description);
               let converter :: <function> = curry(converter, *buffer*);
               method (index :: <integer>, target :: false-or(<basic-bp>))
                 let check-message :: <string>
                   = format-to-string(check-message, index);
                 if (index >= 0)
                   check-equal(check-message, converter(index), target);
                 else
                   check-condition(check-message, <error>, converter(index));
                 end if;
               end method
      end method;
    let check-char-index->bp = index->bp-checker(char-index->bp, "char");
    let check-line-index->bp = index->bp-checker(line-index->bp, "line");

    check-char-index->bp(-1, #f);
    check-char-index->bp(0, make-bp(*line1-1*, 0));
    check-char-index->bp(1, make-bp(*line1-1*, 1));
    check-char-index->bp(25, make-bp(*line2-1*, 4));
    check-char-index->bp(34, make-bp(*line3-1*, 6));
    check-char-index->bp(35, #f);

    check-line-index->bp(-1, #f);
    check-line-index->bp(0, line-start(*line1-1*));
    check-line-index->bp(1, line-start(*line1-2*));
    check-line-index->bp(3, line-start(*line2-1*));
    check-line-index->bp(4, line-start(*line3-1*));
    check-line-index->bp(5, #f);

    local
      method bp->index-checker
                 (converter :: <function>, description :: <string>)
              => (checker :: <method>)
               let check-message :: <string>
                 = format-to-string
                     ("Converting bp %%= to %s-index", description);
               method (bp :: <basic-bp>, target :: <integer>)
                 check-equal
                   (format-to-string(check-message, bp),
                    converter(bp), target);
               end method;
      end method;
    let check-bp->char-index = bp->index-checker(bp->char-index, "char");
    let check-bp->line-index = bp->index-checker(bp->line-index, "line");

    check-bp->char-index(make-bp(*line1-1*, 0), 0);
    check-bp->char-index(make-bp(*line1-1*, 1), 1);
    check-bp->char-index(make-bp(*line2-1*, 4), 25);
    check-bp->char-index(make-bp(*line3-1*, 6), 34);

    check-bp->line-index(make-bp(*line1-1*, 0), 0);
    check-bp->line-index(make-bp(*line1-2*, 1), 1);
    check-bp->line-index(make-bp(*line2-1*, 4), 3);
    check-bp->line-index(make-bp(*line3-1*, 6), 4);

    check-true
      ("char index <-> bp conversion is self-consistent over whole buffer",
       block(return)
         let char-index :: <integer> = 0;
         do-characters
           (method (char :: <character>, line :: <line>, index :: <integer>)
              let bp :: <basic-bp> = make-bp(line, index);
              let consistent1 :: <boolean>
                = (char-index->bp(*buffer*, bp->char-index(bp)) = bp);
              let consistent2 :: <boolean>
                = (bp->char-index(char-index->bp(*buffer*, char-index))
                   == char-index);
              unless (consistent1 & consistent2)
                test-output
                  ("char index <-> bp conversion self-consistency failed"
                   "  at char %=, line %=, index %d"
                   "  bp is %=, char-index is %=\n",
                   char, line, index, bp, char-index);
                return(#f);
              end;
              inc!(char-index);
            end method, *buffer-interval*);
         #t // If we get this far, it's worked.
       end block);

    check-true
      ("line index <-> bp conversion is self-consistent over whole buffer",
       block(return)
         let line-index :: <integer> = 0;
         do-lines
           (method (line :: <line>, si, ei, last?)
              ignore(si, ei, last?);
              let bp :: <basic-bp> = line-start(line);
              let consistent1 :: <boolean>
                = (line-index->bp(*buffer*, bp->line-index(bp)) = bp);
              let consistent2 :: <boolean>
                = (bp->line-index(line-index->bp(*buffer*, line-index))
                   == line-index);
              unless (consistent1 & consistent2)
                test-output
                  ("line index <-> bp conversion self-consistency failed"
                   "  at line %=,"
                   "  bp is %=, line-index is %=\n",
                   line, bp, line-index);
                return(#f);
              end;
              inc!(line-index);
            end method, *buffer-interval*);
         #t // If we get this far, it's worked.
       end block);

  end dynamic-bind;
end test;

define suite motion-suite ()
  test motion-basic-test;
  test motion-over-test;
//  test motion-over-or-until-test;
  test motion-bp-info-test;
end suite;

/*

Things to test ...

line-{start,end}
  [Not worth testing.]

{start,end}-of-line?
  Move to start/end from elsewhere and check for #t from pred.
  Move elsewhere from start/end and check for #f.
  Move off start/end of line and check for #t from opposite pred on
    prev/next line.

{in,de}crement-bp!
  Check that bounding intervals are respected (with fixup) or that #f
    is returned (without), if we try to move outside the interval.
  [What if bp is outside interval to start with?  A: Tough; this has to
    be very fast.]
  [What if extreme line in interval is a structural-diagram-line?  A:
    If you're using fixup, you'll get moved to the extremum, even if
    it _is_ on a <structural-diagram-line>.]
  Check {in,de}crementing at start/end [sic] of line, node and
    buffer.

move-forward-or-backward!
  Check inclusive and exclusive, forward and backward.
  Check with a predicate which matches the character at the starting
    bp; check with one matching the character before, at and after the
    limit.

move-over-characters
  Check forward and backward (0, 1 and n)[; check interval is
    respected?].

move-over-words (and move-over-atoms)
  [Why are these written as separate functions?  Does it really give
    efficiency?]
  Check forward and backward (0, 1 and n)[; check interval is
    respected?].
  [Don't bother checking atom boundaries are found correctly, as the
    only difference from the word case is a slightly different syntax
    table.]

move-over-lists
  [Why does this use two calls to "move-forward-or-backward!" in the
    middle -- why not "increment-bp!"?  Is it because of structural
    diagram lines?]
  Check forward and backward (0, 1, and n)[; check interval is
    respected?].
  Check behaviour when there's an escape character near/at the extreme
    of the interval.
  [It looks as if this will just move over atoms if you aren't next to
    (ignoring whitespace) a list: is this intentional?  Can I get a
    spec of the behaviour?]
  [What is $list-single-quote about?  It's not the same as
    $list-escape, is it?  Is this for character literals?  They may
    have more than one character between the ''s, in the case of
    escape-characters.]
  Check behaviour when there's no lists to move over, and when there
    are unbalanced delimiters.

move-over-matching-thing!
  Check forward and backward[; check interval is respected?].
  Check matches right next to the start (e.g., "''"), including in the
    opposite direction.
  Check moves where there's no match in the direction within the
    interval.

move-over-balanced-thing!
  Check forward and backward[; check interval is respected?].
  Check matches right next to the start (e.g., "''"), including in the
    opposite direction.
  Check moves where there's no match in the direction within the
    interval; check move when "things" are unbalanced.

move-over-atom!
  [---*** Not exported?]
  Much as for move-over-atoms.
  [Could this be used as a sub-method of move-over-{atom,word}s?]

move-over-expressions
move-up-or-down-expressions
  Has to be tested separately for each major mode.
  Check forward and backward (0, 1, and n)[; check interval is
    respected?].
  Check that the mode-specific notion of an "expression" is
    understood, despite surrounding text.  [Hard to test well.]

move-up-or-down-lists
  [Could we use a variation on move-over-balanced-thing!, whose local
    "matches-char?" method succeeds when count = n (the number of
    levels up or down to move)?  It would probably be "n, and include
    the match" for forward, and "n + 1 and exclude the match" for
    backward, or something.  I don't know if this would make it
    faster.]
  Check forward and backward (0, 1, and n)[; check interval is
    respected?].
  Check behaviour when there's nowhere further up or down to go.
  Check behaviour when you have to move over intervening lists to go
    up or down.

move-over-lines
  Check forward and backward (0, 1 and n)[; check interval is
    respected?].
  Check moving over section/node boundaries.

{for,back}ward-{over,until}
  Check forward and backward[; check interval is respected?].
  Check behaviour on empty sequence of chars.
  Check behaviour when {only, none of} the chars occur in the text
    (respectively for {over,until}).

[relevant-]definition-interval
  Has to be tested separately for each major mode.  Nothing general to
    test.

[select-]atom-under-bp
  Check behaviour for bps at the {start,middle,end} (and just
    before/after start/end) of atoms at the {start,middle,end} of a
    {line,node,buffer}.
  [Don't bother checking "select-" version.]

{char,line}-index->bp
bp->{char,line}-index
  Check behaviour for out-of-range, 0 or negative indices.
  [Fix dissimilarity between methods in char- and line-index->bp.]

*/
