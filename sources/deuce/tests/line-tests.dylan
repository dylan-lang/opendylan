Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -=- The tests and suites -=-

define test line-change-test ()
  let line
    = make(<text-line>, contents: "A line of text",
           properties: list(#"foo"));
  let start-bp = line-start(line);
  let end-bp = line-end(line);
  let section = make(<section>, start-line: #f, end-line: #f);
  add-line!(section, line);
  let node
    = make(<section-node>, section: section,
           start-bp: start-bp, end-bp: end-bp);
  section-nodes(section) := list(node);
  let buffer
    = make(<testing-special-purpose-buffer>,
           major-mode: find-mode(<testing-mode>));
  add-node!(buffer, node);

  dynamic-bind (*buffer* = buffer)
    // Nodes currently have no modification tick but we check those for
    // lines and sections.  We also check the mode-specific line-changed
    // function is called.
    let line-mod-tick-1 = line-modification-tick(line);
    let section-mod-tick-1 = section-modification-tick(section);
    *testing-mode-line* := #f;

    note-line-changed(line);

    let line-mod-tick-2 = line-modification-tick(line);
    let section-mod-tick-2 = section-modification-tick(section);
    check-true
      ("note-line-changed increased line-modification-tick",
       line-mod-tick-2 > line-mod-tick-1);
    check-true
      ("note-line-changed flushed line-contents-properties",
       empty?(line-contents-properties(line)));
    check-true
      ("note-line-changed increased section-modification-tick",
       section-mod-tick-2 > section-mod-tick-1);
    check-true
      ("note-line-changed called do-note-line-changed on <testing-mode>",
       *testing-mode-line* = line);

    // ---*** Could also check with a <file-buffer>s, to check its
    // source container got the notification.  But I'm just checking the
    // propagation of the change message here, not its effects, and that
    // propagation could be viewed as an effect specific to
    // <file-buffers>.  Or maybe not.
  end
end test;

define test line-contents-test-1 ()
  let line = make(<text-line>, contents: "A line of text");

  // Check read-only? on lines.
  // NOTE: The read-only? status of a line is only heeded at the level
  // of user commands (in commands.dylan), not by things like
  // line-{contents,length}-setter.
  check-false
    ("Line is not initially read-only",
     line-read-only?(line));
  check-true
    ("Line set to read-only",
     line-read-only?(line) := #t);
  check-true
    ("Line is now read-only",
     line-read-only?(line));

  // Check contents and length.
  check-false
    ("Line containing \"A line of text\" is not empty",
     line-empty?(line));
  check-equal
    ("Line containing \"A line of text\" has length 14",
     line-length(line), 14);
  check-equal
    ("Line length set to 6",
     (line-length(line) := 6), 6);
  check-equal
    ("Line length is 6",
     line-length(line), 6);
  check-equal
    ("Line contents are still \"A line of text\"",
     line-contents(line), "A line of text");
  check-equal
    ("Line length set to 8",
     (line-length(line) := 8), 8);
  check-equal
    ("Line length is 8",
     line-length(line), 8);
  check-equal
    ("Line length set to 0",
     (line-length(line) := 0), 0);
  check-equal
    ("Line length is 0",
     line-length(line), 0);

  // Check empty? on lines.
  check-true
    ("Line is now empty",
     line-empty?(line));
  check-equal
    ("Line contents set to \"\\t   \"",
     (line-contents(line) := "\t   "), "\t   ");
  check-equal
    ("Line contents are \"\\t   \"",
     line-contents(line), "\t   ");
  check-true
    ("Line containing \"\\t   \" is empty",
     line-empty?(line));
end test;

define test line-copy-test ()
  let section = make(<section>, start-line: #f, end-line: #f);
  let line = make(<text-line>, contents: "A line of text");
  add-line!(section, line);
  line.line-next := line;
  line.line-previous := line;
  line.line-contents-properties := list(#"foo");

  // Check copy-line.
  // ---*** Should check separately for <rich-text-line> and any other
  // classes of line which have other stuff to copy.
  let line2 = copy-line(line);
  check-true
    ("Copied line is not identical",
     line ~== line2);
  check-equal
    ("Copied line has contents \\= to original",
     line.line-contents, line2.line-contents);
  // ---*** Check that contents "shrink" when copied, if length is less
  // than real length of contents.
  check-equal
    ("Copied line has length \\= to original",
     line.line-length, line2.line-length);
  check-false
    ("Copied line has no next line",
     line2.line-next);
  check-false
    ("Copied line has no previous line",
     line2.line-previous);
  check-false
    ("Copied line has no section",
     line2.line-section);
  check-true
    ("Copied line has no line-contents-properties",
     empty?(line2.line-contents-properties));

  // Check copying parts of the line.
  check-equal
    ("Copying initial subrange of line works",
     (copy-line(line, end: 4)).line-contents, "A li");
  check-equal
    ("Copying final subrange of line works",
     (copy-line(line, start: 4)).line-contents, "ne of text");
  check-equal
    ("Copying middle subrange of line works",
     (copy-line(line, start: 4, end: 6)).line-contents, "ne");
end test;

define test line-dump-test ()
  // Check do-characters for all subclasses of <line> which have
  // special methods.  Currently, these are: <fixed-text-line>,
  // <rich-text-line>, <diagram-line>, <structural-diagram-line>.

  // Check for <fixed-text-line>.
  let stream = make(<string-stream>, direction: #"output");
  let contents = "A line of text";
  let line
    = make(<fixed-text-line>, contents: contents, length: size(contents));
  dump-line(line, stream);
  check-equal
    ("<fixed-text-line> dumps to <string-stream> okay",
     concatenate(line.line-contents, "\n"), stream.stream-contents);

  // Check for <rich-text-line>.
/* ---*** dump-line not yet implemented for <rich-text-line>.
  let stream = make(<string-stream>, direction: #"output");
  let line = make(<rich-text-line>, contents: "A line of text");
  dump-line(line, stream);
  check-equal
    ("<rich-text-line> dumps to <string-stream> okay",
     line.line-contents, stream.stream-contents);
*/

  // Check for (a concrete subclass of) <diagram-line>.
  let stream = make(<string-stream>, direction: #"output");
  let line = make(<testing-diagram-line>);
  check-condition
    ("<testing-diagram-line> errors when dumping to <string-stream>",
     <error>,
     dump-line(line, stream));

  // Check for <structural-diagram-line>.
  let stream = make(<string-stream>, direction: #"output");
  let line = make(<testing-structural-diagram-line>);
  dump-line(line, stream);
  check-equal
    ("<structural-diagram-line> dumps to <string-stream> okay",
     "", stream.stream-contents);
end test;

define test line-do-characters-test ()
  // Check do-characters for all subclasses of <line> which have
  // special methods.  Currently, these are: <text-line>,
  // <diagram-line>, <structural-diagram-line>.

  // Check for <text-line>.
  let line = make(<text-line>, contents: copy-sequence("A line of text!"));
  local
    method uppercase-if-even-index
        (char :: <character>, line :: <line>, index :: <integer>)
      when ( even?(index) )
        // I'd use "as-uppercase!" but that's broken in the emu.
        line-contents(line)[index] := as-uppercase(char);
      end;
    end;

  do-characters(uppercase-if-even-index, line, start: 2, end: 8);
  check-equal
    ("do-characters (on subrange)",
     line.line-contents, "A LiNe of text!");

  do-characters
    (uppercase-if-even-index, line, start: 7, end: 14, from-end?: #t);
  check-equal
    ("do-characters 'from-end' (on subrange)",
     line.line-contents, "A LiNe oF TeXt!");

  // Check for (a concrete subclass of) <diagram-line>.
  check-condition
    ("do-characters on <testing-diagram-line> errors",
     <error>,
     do-characters(identity, make(<testing-diagram-line>)));

  // [Not really worth testing for <structural-diagram-line>s, since
  // the method does nothing.
end test;

define test line-as-string-test ()
  // Check as(<string>, ...) for all subclasses of <line> which have
  // special methods.  Currently, these are: <text-line>,
  // <diagram-line>.

  // Check for <text-line>.
  let my-string :: <string> = "A line of text";
  let line = make(<text-line>, contents: my-string);
  check-equal
    ("<text-line> coerced to <string>",
     my-string, as(<string>, line));

  // Check for (a concrete subclass of) <diagram-line>.
  let line = make(<testing-diagram-line>);
  check-equal
    ("<testing-diagram-line> coerced to <string>",
     "", as(<string>, line));
end test;

define suite line-suite ()
  test line-change-test;
  test line-contents-test-1;
  test line-copy-test;
  test line-dump-test;
  test line-do-characters-test;
  test line-as-string-test;
end suite line-suite;

/*

Things to test ...

line-{next,previous}[-setter]
line-section[-setter]
line-bps[-setter]
line[-contents]-properties[-setter]
line-modification-tick[-setter]
  [Not worth testing for <basic-line> -- they're just slots.]

note-line-changed
  Check propagation of change information.

line-read-only?[-setter]
  Check setting and getting, for different classes of lines.
  Check that modification tick is -1 for read-only lines.

line-length[-setter]
  [Note the use of copy-bytes -- depends on byte-sized characters.]
  Check setting and getting, for increasing and decreasing size;
    include decreasing to 0.

line-contents[-setter]
  Check setting and getting; check contents and length are \= to those
    for the value it was set to.

line-empty?
  Check for different classes of <line>.
  Check for newly-created lines and for lines which have had contents
    added and removed.

copy-line
  Check for each class of line.
  ---*** Check that contents "shrink" when copied, if length is less
    than real length of contents.

methods on dump-line
    (for <[rich-]text-line>, <[structural-]diagram-line>)
  Dump to a <string-stream> [can we?] and test for \= to an example.
  Method for <diagram-line> should error.
  [Method for <rich-text-line> does nothing for now.]

methods on do-characters
    (for <text-line>, <[structural-]diagram-line>)
  [For <text-line> could use "for (i from _start to _end by _step)"?]
  Check using function with observable effects.
  Check with empty and non-empty strings.

methods on as(<string>, ...)
    (for <[rich-]text-line>, <diagram-line>)
  [No real method for <rich-text-line> yet.]
  Compare result to existing <string> with \=.  Check with empty and
    non-empty strings.

*/
