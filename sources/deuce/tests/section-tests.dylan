Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test section-lines-test ()
  // -=- Setup -=-

  // -=- {add,remove}-line! -=-
  let section = make(<section>, start-line: #f, end-line: #f);
  let line1 = make(<text-line>, contents: "Line 1");
  let line2 = make(<text-line>, contents: "Line 2");
  let line3 = make(<text-line>, contents: "Line 3");
  let line4 = make(<text-line>, contents: "Line 4");
  local
    method check-lines-in-section
        (lines :: <vector> /*of: <line>*/) => (okay? :: <boolean>)
      // Check that the lines in the sequence have their line-next and
      // line-previous pointers correct, given that they're supposed
      // to be connected in sequence order.  Check the cached line
      // count.  Check the section-{start,end}-line.
      let start-line = section.section-start-line;
      let end-line = section.section-end-line;
      if ( empty?(lines) )
        ~(start-line | end-line)
          // We should test this, but %n-lines isn't exported.
          /* & (section.%n-lines = 0) */
      else
        let previous-line :: false-or(<line>) = #f;
        start-line = first(lines)
          & every?
              (method (line)
                 line.line-previous = previous-line
                   & (~previous-line | previous-line.line-next = line)
                   & (previous-line := line) // for side-effect!
               end, lines)
          & previous-line.line-next = #f
          & end-line = previous-line
          // We should test this, but %n-lines isn't exported.
          /* & section.%n-lines = size(lines) */
      end if
    end method;

  add-line!(section, line2);
  check-true
    ("Added line2 of 4 to empty section",
     line2.line-section = section
       & check-lines-in-section(vector(line2)));

  add-line!(section, line1, after: #"start");
  check-true
    ("Added line1 of 4 to section at start",
     check-lines-in-section(vector(line1, line2)));

  add-line!(section, line4, after: #"end");
  check-true
    ("Added line4 of 4 to section at end",
     check-lines-in-section(vector(line1, line2, line4)));

  remove-line!(section, line2);
  line2.line-section := #f; // ---*** TEMP until swm changes Deuce
  check-true
    ("Removed line2 of 4 from (middle of) section",
     check-lines-in-section(vector(line1, line4)));

  check-condition
    ("Tried to add line3 of 4 to section after line2 (error)",
     <error>,
     add-line!(section, line3, after: line2));

  check-condition
    ("Tried to add line1 of 4 to section (error)",
     <error>,
     add-line!(section, line1));

  check-condition
    ("Tried to remove line2 of 4 from section (error)",
     <error>,
     remove-line!(section, line2));

  add-line!(section, line3, after: line1);
  check-true
    ("Added line3 of 4 to section after line1",
     line3.line-section = section
       & check-lines-in-section(vector(line1, line3, line4)));

  remove-line!(section, line4);
  line4.line-section := #f; // ---*** TEMP until swm changes Deuce
  check-true
    ("Removed line4 of 4 from (end of) section",
     check-lines-in-section(vector(line1, line3)));

  remove-line!(section, line1);
  line1.line-section := #f; // ---*** TEMP until swm changes Deuce
  check-true
    ("Removed line1 of 4 from (start of) section",
     check-lines-in-section(vector(line3)));


  // -=- do-lines -=-
  // Fix up the lines.
  add-line!(section, line2, after: #"start");
  add-line!(section, line1, after: #"start");
  add-line!(section, line4, after: #"end");

  // ---*** These local methods and their state are exactly the same
  // as in "interval-characters-test" in node-tests.dylan, so maybe we
  // should re-use?

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

  // ... default skip test.
  reset-state();
  do-lines(accumulate-line-contents, section);
  check-equal
    ("do-lines forward, default skip-test, section",
     accumulator, "Line 1Line 2Line 3*Line 4");

  reset-state();
  do-lines(accumulate-line-contents, section, from-end?: #t);
  check-equal
    ("do-lines forward, default skip-test, section",
     accumulator, "Line 4Line 3Line 2*Line 1");

  // ... skip second line.
  let skip-second = nth-skipper(2);

  reset-state();
  do-lines(accumulate-line-contents, section, skip-test: skip-second);
  check-equal
    ("do-lines forward, skip second line, section",
     accumulator, "Line 1Line 3*Line 4");

  reset-state();
  do-lines
    (accumulate-line-contents, section, from-end?: #t,
     skip-test: skip-second);
  check-equal
    ("do-lines forward, skip second line, section",
     accumulator, "Line 4Line 2*Line 1");


  // -=- count-lines -=-
  check-equal
    ("count-lines default skip-test, section",
     count-lines(section), 4);

  reset-state();
  check-equal
    ("count-lines skip second line, section",
     count-lines(section, skip-test: skip-second), 3);
end test;

define test section-contents-test ()
  // -=- Setup -=-
  // --- Could share some of this with line-tests?

  let line
    = make(<text-line>, contents: "A line of text");
  let start-bp = line-start(line);
  let end-bp = line-end(line);
  let section = make(<section>, start-line: #f, end-line: #f);
  add-line!(section, line);
  let container
    = make(<flat-file-source-container>, pathname: "file.text");
      // We don't try to read from this pathname!
  add-section!(container, section);
  let node
    = make(<section-node>, section: section,
           start-bp: start-bp, end-bp: end-bp);
  section-nodes(section) := list(node);
  let buffer
    = make(<testing-special-purpose-buffer>,
           major-mode: find-mode(<text-mode>));
  add-node!(buffer, node);
  push!(container-buffers(container), buffer);
  dynamic-bind (*buffer* = buffer)

    // -=- as(<string>, ...) -=-
    // [Not worth testing for now, as the interval test covers this.]


    // -=- note-section-{changed,compiled} -=-
    let mod-tick = section.section-modification-tick;
    // I'd prefer to set the container-modified? state to #f explicitly,
    // but the setter isn't exported, so I'll have to rely on it being
    // initialized to #f.
    // container-modified?(container) := #f;
    buffer-modified?(buffer) := #f;
    note-section-changed(section);
    // ---*** Should check propagation of modification to nodes, but
    // that does nothing for now, so we can't.
    check-true
      ("Note section modified: section tick updated",
       section.section-modification-tick > mod-tick);
    check-true
      ("Note section modified: container marked modified",
       container-modified?(container));
    check-true
      ("Note section modified: buffer marked modified",
       buffer-modified?(buffer));

    let mod-tick = section.section-modification-tick;
    let comp-tick = section.section-compilation-tick;
    note-section-compiled(section);
    let new-comp-tick = section.section-compilation-tick;
    // We're checking that the compilation tick is equal to what the
    // modification tick was, and the modification tick hasn't changed.
    check-true
      ("Note section compiled",
       new-comp-tick > comp-tick & new-comp-tick = mod-tick);


    // -=- resectionize-section -=-
    *did-resectionize* := #f;
    note-section-changed(section);
    resectionize-section(section);
    check-true
      ("Resectionize section after modification",
       *did-resectionize*);

    *did-resectionize* := #f;
    resectionize-section(section);
    check-false
      ("Resectionize section does nothing if section unmodified",
       *did-resectionize*);
  end
end test;

define suite section-suite ()
  test section-lines-test;
  test section-contents-test;
end suite;

/*

Things to test ...

section-source-container
section-{start,end}-line[-setter]
section-nodes[-setter]
section-lock
  Nothing to test (for <basic-section>) -- they're just slots.

section-defining-{line,name,type}
  [Hardly worth testing, I think, at least for <basic-section>.]
  Check that these give #f for non-definition sections.
  Check they give the right results on definition sections.
    [Can we construct these without using the sectionizer?]

{add,remove}-line!
  Add and remove, and check for presence/absence.
  Check in combination with section-{start,end}-line-setter.

do-lines
  Check forward and backward; check for non-trivial skip-test.

count-lines
  [Could do with a local method.]
  Check we get the right number; check for non-trivial skip-test.

as(c == <string>, s :: <section>)
  [Not worth testing for now, as the interval test covers this.]
  Check \=, including '\n's (especially terminating one).

note-section-changed
  Call it.  Check whether it correctly notifies "owners".
  Check that the modification tick increased.
  Check with container.

note-section-compiled
  Call it.  Check that compilation tick is now \= to modification
    tick, when it wasn't before.

resctionize-section
  Check that it does nothing for hard-sectioned containers.
  Check it does nothing for sections not modified since last
    sectionization.
  (Check that redisplay is queued when needed?)
  [Can find-mode-from-pathname return anything but a valid <mode>,
    e.g., #f?]

*/
