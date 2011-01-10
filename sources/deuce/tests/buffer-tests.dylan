Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test buffer-creation-test ()
  // -=- make-empty-buffer -=-
  let new-buffer = make-empty-buffer(<non-file-buffer>);
  dynamic-bind(*buffer* = new-buffer)
    check-equal
      ("New buffer is in fundamental mode",
       buffer-major-mode(new-buffer), find-mode(<fundamental-mode>));
    let node = buffer-start-node(new-buffer);
    let end-node = buffer-end-node(new-buffer);
    check-true
      ("New buffer contains exactly 1 node",
       node == end-node & node ~== #f);
    check-equal
      ("Node believes it is contained in new buffer",
       node-buffer(node), new-buffer);
    let section = node-section(node);
    check-true
      ("Node contains section",
       section & section-nodes(section) = list(node));
    let line = section-start-line(section);
    let end-line = section-end-line(section);
    check-true
      ("Section contains exactly 1 empty line",
       line & line == end-line & line-length(line) == 0);
    let start-bp = interval-start-bp(new-buffer);
    let end-bp = interval-end-bp(new-buffer);
    check-true
      ("Buffer interval BPs point to that line",
       bp-line(start-bp) == line & line == bp-line(end-bp));
    let start-bp-buffer = bp-buffer(start-bp);
    let end-bp-buffer = bp-buffer(end-bp);
    check-true
      ("Buffer interval BPs are in correct buffer",
       start-bp-buffer == new-buffer & new-buffer == end-bp-buffer);
    check-true
      ("end-bp is moving and start-bp is not",
       moving-bp?(end-bp) & ~moving-bp?(start-bp));
    check-true
      ("end-bp does not precede start-bp",
       start-bp = end-bp | bp-less?(start-bp, end-bp));
  end;
end test;

define test buffer-change-test ()
  // -=- buffer-modified? -=-
  // --- Test for <non-file-buffer>, <file-buffer> and
  // <special-purpose-buffer>

  // -=- note-buffer-changed -=-
  // --- Test for <file-buffer> (for now).

  // -=- revert-buffer -=-
  // -=- sectionize-buffer -=-
  // -=- save-buffer -=-
  // -=- save-buffer-as (on <basic-buffer>, <pathname>) -=-
  // -=- kill-buffer -=-
end test;


define test buffer-structure-test ()
  reset-testing-buffer-state();
  let buffer = make-empty-buffer(<non-file-buffer>);
  let section = node-section(buffer-start-node(buffer));
  dynamic-bind(*buffer* = buffer)

    // -=- buffer-contains-section?
    // ... on <non-file-buffer> (effectively on <basic-buffer>)
    check-true
      ("Non-file buffer contains own section",
       buffer-contains-section?(buffer, section));
    check-false
      ("Non-file buffer does NOT contain file buffer section",
       buffer-contains-section?(buffer, *section2*));

    // ... on <file-buffer>
    check-true
      ("File buffer contains own section",
       buffer-contains-section?(*testing-buffer*, *section2*));
    check-false
      ("File buffer does NOT contain non-file buffer section",
       buffer-contains-section?(*testing-buffer*, section));

    // ... on <testing-special-purpose-buffer>?  [No applicable method.]


/*
[---*** Not exported from deuce-internals!]
    // -=- section-less? -=-
    check-true
      ("section 1 is \"less than\" section 2",
       section-less?(*section1*, *section2*));
    check-true
      ("section 2 is \"less than\" section 2",
       section-less?(*section2*, *section2*));
    check-false
      ("section 3 is NOT \"less than\" section 2",
       section-less?(*section3*, *section2*));
*/

/*
[---*** Not exported from deuce-internals!]
    // -=- line-less? -=-
    // ... within one section
    check-true
      ("line 1, section 1 is \"less than\" line 2, section 1",
       line-less?(*line1-1*, *line1-2*));
    check-true
      ("line 2, section 1 is \"less than\" line 2, section 1",
       line-less?(*line1-2*, *line1-2*));
    check-false
      ("line 3, section 1 is NOT \"less than\" line 2, section 1",
       line-less?(*line1-3*, *line1-2*));

    // ... across sections
    check-true
      ("line 1, section 1 is \"less than\" line 1, section 2",
       section-less?(*line1-1*, *line2-1*));
    check-false
      ("line 1, section 3 is NOT \"less than\" line 1, section 2",
       section-less?(*line3-1*, *line2-1*));

    // -=- line-{next,previous}-in-buffer -=-
    // ... at extremes of buffer
    check-false
      ("No line-previous-in-buffer at start of buffer",
       line-previous-in-buffer(*line1-1*, *testing-buffer*));
    check-equal
      ("line-next-in-buffer at start of buffer",
       line-next-in-buffer(*line1-1*, *testing-buffer*), *line1-2*);
    check-equal
      ("line-previous-in-buffer at end of buffer (also start of section)",
       line-previous-in-buffer(*line3-1*, *testing-buffer*), *line2-1*);
    check-false
      ("No line-next-in-buffer at end of buffer",
       line-next-in-buffer(*line3-1*, *testing-buffer*));

    // ... at extremes of section (already done start of section)
    check-equal
      ("line-next-in-buffer at end of section",
       line-next-in-buffer(*line1-3*, *testing-buffer*), *line2-1*);

    // ... within section
    check-equal
      ("line-previous-in-buffer within section",
       line-previous-in-buffer(*line1-2*, *testing-buffer*), *line1-1*);
    check-equal
      ("line-next-in-buffer within section",
       line-next-in-buffer(*line1-2*, *testing-buffer*), *line1-3*);

    // .. with non-standard skip-test
    local
      method skip-lines-in-section2 (line :: <line>)
        line-section(line) ~== *section2*
      end;
    check-equal
      ("line-previous-in-buffer with non-standard skip-test",
       line-previous-in-buffer
         (*line3-1*, *testing-buffer*, skip-test: skip-lines-in-section2),
       *line1-3*);
    check-equal
      ("line-next-in-buffer with non-standard skip-test",
       line-next-in-buffer
         (*line1-3*, *testing-buffer*, skip-test: skip-lines-in-section2),
       *line3-1*);

    // .. for lines in NO buffer and/or node and/or section?
*/

/*
[---*** Not exported from deuce-internals!]
    // -=- section-node -=-
    check-equal
      ("section-node consistent for correct buffer",
       node-section(section-node(*section2*)), *section2*);
    check-equal
      ("section-node #f for section not in buffer",
       section-node(section), #f);
*/

    // -=- {add-remove}-node!
    local
      method check-nodes-in-buffer
          (nodes :: <vector> /*of: <node>*/) => (okay? :: <boolean>)
        // Check that the nodes in the sequence have their node-next and
        // node-previous pointers correct, given that they're supposed
        // to be connected in sequence order.  Check the
        // buffer-{start,end}-node.
        let start-node = *testing-buffer*.buffer-start-node;
        let end-node = *testing-buffer*.buffer-end-node;
        if ( empty?(nodes) )
          ~(start-node | end-node)
        else
          let previous-node :: false-or(<node>) = #f;
          start-node = first(nodes)
            & every?
                (method (node)
                   node.node-previous = previous-node
                     & (~previous-node | previous-node.node-next = node)
                     & (previous-node := node) // for side-effect!
                 end, nodes)
            & previous-node.node-next = #f
            & end-node = previous-node
        end if
      end method;

      remove-node!(*testing-buffer*, *node3*);
      check-true
        ("Removed node 3 of 3 (end) from buffer",
         node-buffer(*node3*) == #f
           & check-nodes-in-buffer(vector(*node1*, *node2*)));

      check-condition
        ("Trying to remove node 3 of 3 again (crashes)",
         <error>,
         remove-node!(*testing-buffer*, *node3*));

      remove-node!(*testing-buffer*, *node1*);
      check-true
        ("Removed node 1 of 3 (start) from buffer",
         node-buffer(*node1*) == #f
           & check-nodes-in-buffer(vector(*node2*)));

      add-node!(*testing-buffer*, *node1*, after: #"start");
      check-true
        ("Added node 1 of 3 to start of buffer",
         node-buffer(*node1*) == *testing-buffer*
           & check-nodes-in-buffer(vector(*node1*, *node2*)));

      check-condition
        ("Trying to add node 1 of 3 again (crashes)",
         <error>,
         add-node!(*testing-buffer*, *node1*));

      add-node!(*testing-buffer*, *node3*, after: #"end");
      check-true
        ("Added node 3 of 3 to end of buffer",
         node-buffer(*node3*) == *testing-buffer*
           & check-nodes-in-buffer(vector(*node1*, *node2*, *node3*)));

      remove-node!(*testing-buffer*, *node2*);
      check-true
        ("Removed node 2 of 3 (middle) from buffer",
         node-buffer(*node2*) == #f
           & check-nodes-in-buffer(vector(*node1*, *node3*)));

      add-node!(*testing-buffer*, *node2*, after: *node1*);
      check-true
        ("Added node 2 of 3 to middle of buffer",
         node-buffer(*node2*) == *testing-buffer*
           & check-nodes-in-buffer(vector(*node1*, *node2*, *node3*)));
  end dynamic-bind;
end test;

define test buffer-characters-test ()
  // -=- do-lines -=-
  // [Not complicated, so not urgent to test.]

  // -=- as(<string>, ...) -=-
  reset-testing-buffer-state();
  dynamic-bind(*buffer* = *testing-buffer*)
    check-equal
      ("(file) buffer as <string>",
       as(<string>, *testing-buffer*),
       "Line 1\nLine 2\nLine 3\nLine 4\nLine 5");
  end;

  // -=- count-lines -=-
  // ... over nodes with sections and nodes without.
end test;


define suite buffer-suite ()
  test buffer-creation-test;
  test buffer-change-test;
  test buffer-structure-test;
  test buffer-characters-test;
end suite;

/*

Things to test ...

buffer-name[-setter]
buffer-{start,end}-node[-setter]
buffer-read-only?[-setter]
  Nothing to test -- they're just slots.

method on initialize for <basic-buffer>
  [Not too urgent for testing: Check buffers are always in
    title-string order (case insensitive?) for an editor, when they're
    newly created.]
method on initialize for <file-buffer-mixin>
  [Not important for testing: Check name is correct.]
  [What about names for container-less buffers?]

[Check handling of *buffer* -- make sure it _is_ always in sync as
  claimed.]

methods on interval-{start,end}-bp for <basic-buffer>
  Check on buffers with 0, 1, 2, and n nodes.
  [Not worth testing this.  If anything is wrong, it's probably in
    "node surgery" elsewhere, or in interval-{start-end}-bp on
    <node>.]

buffer-major-mode[-setter]
buffer-minor-modes[-setter]
  Nothing to test, they're just slots.
  [But we'll need these to check results after "mode surgery" (yes,
    that's 'M' for "Mouse")].

buffer-modified?[-setter]
note-buffer-changed
  for <basic-buffer>
    Noting buffers as changed has no effect -- can't check.
  for <non-file-buffer-mixin>
    [Not worth checking: that buffers are seen as modified iff
      non-empty?]
  for <file-buffer-mixin>
    Check that buffers are seen as modified iff changed since last save.
    Check that buffers noted as changed are seen as modified, when
      they weren't before the noting.
  for <special-purpose-buffer-mixin>
    Nothing to test, it's just a slot, and note-... just assigns it #t.

revert-buffer
  for <basic-buffer>
    should just error.
  for <file-buffer-mixin>
    ---*** TO DO
    Check return value.

sectionize-buffer
  Check that hard-sectioned buffers aren't modified.
  Nothing else worth testing -- depends on major mode's method.

save-buffer
  for <basic-buffer>
    does nothing.
  for <file-buffer-mixin>
    ---*** TO DO
    Check return value.

save-buffer-as (on <basic-buffer>, <pathname>)
  [Maybe needs to handle different file types/formats?]
  Save file to a (randomly-generated?) <pathname>, then read it back
    in to a <string> and compare that with "as(<string>, ...)" on the
    buffer.
  Check behaviour for existing files (writable and read-only) and for
    <pathnames> pointing to non-existent directories.
  Check return value.

kill-buffer
  for <basic-buffer>
  for <file-buffer-mixin>
  ---*** TO DO

buffer-has-hard-sections? (on <[non-]file-buffer-mixin>)
  Check for each class of buffer.

buffer-contains-section? (on <basic-buffer> and <file-buffer-mixin>)
  Check for each class of buffer, for contained and not.

make-empty-buffer [default name should be "Unnamed[ /n/]"?]
  [Check that this sets everything up right.  If the node/section/line
    structure is broken here, we're really in trouble!]
  [What is the intended use of the "with-keywords-removed"?]
  Check that node's start-bp is non-moving and end-bp is moving.
  Just use "as(<string>, ...)" on buffer as a quick structure check?

pathname->buffer-name
file-buffer? (Check for each class of buffer.)
buffer-pathname[-setter]
  [<file-buffer-mixin> has a getter but no setter!  There are no other
    methods.  A: What would the setter do?  Rename source container?
    Add a new one?  Maybe add one when we decide.]
  ---*** TO DO

special-purpose-buffer?
  Check for each class of buffer.

method on do-lines for <basic-buffer>
  [Not complicated, so not urgent to test.]
  Check with some "accumulator" function.
  Check the use of skip-test; and of from-end?.
  Check for the start, end, middle or all nodes having no/some/all
    lines satisfying the skip-test.
  Check for empty, single-node and multi-node buffers.

method on as(<string>, ...) for <basic-buffer>
  Just test that contents are as expected for one example.

method on count-lines for <basic-buffer>
  Hardly worth testing.
  [What about nodes with lines but no section?]

line-{next,previous}-in-buffer
  Check at {end,middle,start} of {buffer,section}.
  Check use of skip-test.
  Check for real <buffer> and for "buffer == #f".
  Check for lines which are in a section in a node in a buffer,
    lines which are in a section in a node in NO buffer,
    lines which are in a section in NO node,
    lines which are in NO section.

line-less?
  [---*** Not exported from deuce-internals!]
  [This gives "<=" behaviour, not "<"; is this intended?  A: It's
    intended but it may not be right.]
  Check for same line, different (<,>) lines in same section, and
    different (<,>) lines in different sections.

section-less?
  [---*** Not exported from deuce-internals!]
  [This gives "<=" behaviour, not "<"; is this intended?  A: It's
    intended but it may not be right.]
  Check for same section, and different (<,>) sections.

section-node [Use "any?"?  Maybe less efficient?]
  [---*** Not exported from deuce-internals!]
  Check result using node-section for consistency.
  Check for sections in and not in the supplied buffer.

line-node
bp-node
  Hardly worth testing.

add-node! [this is a single method with an implicit GF]
  Check for adding "after:" {#f, #start, first node}, {#end,
    last-node} and some intermediate node.  (Maybe use "as(<string>,
    ...)" to check results?  Or walk node list directly?)
  Check buffer-{start,end}-node before and after.

remove-node! [this is a single method with an implicit GF]
  Check for removing first, last, and intermediate nodes.  (Maybe use
    "as(<string>, ...)" to check results?  Or walk node list directly?)
  Check buffer-{start,end}-node before and after.

*/
