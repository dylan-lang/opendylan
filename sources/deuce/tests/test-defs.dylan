Module:       deuce-test-suite
Synopsis:     Test suite for the Deuce editor
Author:       Hugh Greene, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -=- Auxiliary classes and related methods -=-

// Editor and frame classes.

define sealed class <testing-editor> (<basic-editor>)
end class <testing-editor>;

define class <testing-frame> (<basic-editor-frame>)
end class <testing-frame>;

// Window class and some dummy functions, mostly used by commands.dylan.

define class <testing-window> (<basic-window>)
  constant slot command-enabled?-table :: <table> = make(<table>);
end class <testing-window>;

define sealed method display-buffer-name
    (window :: <testing-window>, buffer :: false-or(<buffer>))
 => ()
  // Do nothing.
end method;

define sealed method command-enabled?
    (window :: <testing-window>, command :: <function>)
 => (enabled? :: <boolean>)
  element(window.command-enabled?-table, command, default: #t)
end method;

define sealed method command-enabled?-setter
    (enabled? :: <boolean>, window :: <testing-window>, command :: <function>)
 => (enabled? :: <boolean>);
  window.command-enabled?-table[command] := enabled?
end method;



define variable *testing-editor* :: false-or(<editor>) = #f;

define function run-testing-deuce (#key reset?)
  when (~*testing-editor* | reset?)
    *testing-editor* := make(<testing-editor>);
  end;
  when (~*editor-frame* | reset?)
    let frame = make(<testing-frame>, editor: *testing-editor*);
    frame-window(frame) := make(<testing-window>, frame: frame);
    //---*** Globally set these because CAPI back-end never calls 'frame-top-level'
    *editor-frame* := frame;
    let buffer = make-empty-buffer(<non-file-buffer>);
    *buffer* := buffer;
  end;
end function run-testing-deuce;

run-testing-deuce(reset?: #t);


// Modes.

define sealed class <testing-mode> (<fundamental-mode>)
end class;

/* We need to do this to test sectionization, but we can't because
// these mapping tables aren't exported :-(
begin
  gethash(*keyword->major-mode*,   #"testing") := <testing-mode>;
  gethash(*file-type->major-mode*, #"testing") := <testing-mode>
end;
*/

define variable *testing-mode-line* = #f;

define method do-note-line-changed
    (mode :: <testing-mode>, line :: <basic-line>)
 => (line :: <basic-line>)
  *testing-mode-line* := line;
end method;

define variable *did-resectionize* :: <boolean> = #f;

/* We're defining a method on a class we don't own here: <text-mode>.
// Ideally it'd be defined on <testing-mode> but we can't associate
// any file type with <testing-mode>, as mentioned above, so
// resectionize-section would never get here.
*/
define sideways method do-resectionize-section
    (mode :: <fundamental-mode>, section :: <basic-section>)
 => (resectionized? :: <boolean>)
  *did-resectionize* := #t;
end method;


// Buffers.

define sealed class <testing-special-purpose-buffer>
    (<basic-special-purpose-buffer>)
end class <testing-special-purpose-buffer>;


// Diagram lines.

define sealed class <testing-diagram-line>
    (<diagram-line>)
end class;

define sealed class <testing-structural-diagram-line>
    (<structural-diagram-line>)
end class;



// -=- Data structures for testing -=-

// lines, sections, nodes, container and buffer
define variable (*line1-1*, *line1-2*, *line1-3*, *line2-1*, *line3-1*)
  = values(#f, #f, #f, #f, #f);
define variable (*section1*, *section2*, *section3*)
  = values(#f, #f, #f);
define variable (*node1*, *node2*, *node3*)
  = values(#f, #f, #f);
// define variable *testing-buffer* = #f;
define variable *container* = #f;
define variable *testing-buffer* = #f;

// BPs
define variable
    (*start1-1-bp*, *middle1-1-bp*, *end1-1-bp*,
     *start1-2-bp*, *middle1-3-bp*, *end1-3-bp*,
     *start2-1-bp*, *middle3-1-bp*, *end3-1-bp*)
  = values(#f, #f, #f, #f, #f, #f, #f, #f, #f);

// intervals
define variable
   (*empty-interval*, *eol-interval*, *line-interval*,
    *part-lines-interval*, *section-interval*,
    *part-sections-interval*, *buffer-interval*)
  = values(#f, #f, #f, #f, #f, #f, #f);

define function reset-testing-buffer-state () => ()
  // Create the buffer, and its container.
  when (*testing-buffer*)
    kill-buffer(*testing-buffer*);
  end when;
  *container*
    := make(<flat-file-source-container>, pathname: "********.text");
       // We don't try to read from this pathname!
  *testing-buffer*
    := make
         (<file-buffer>,
          major-mode: find-mode(<testing-mode>),
          container: *container*,
          name: "*testing*");
  push!(container-buffers(*container*), *testing-buffer*);

  local
    method make-testing-node
        (section :: <section>, start-line :: <line>, end-line :: <line>)
     => (node :: <section-node>)
      let node :: <section-node>
	= make(<section-node>, section: section,
	       start-bp:
		 make(<bp>, line: start-line, index: 0,
                      buffer: *testing-buffer*),
	       end-bp:
		 make(<bp>, line: end-line, index: line-length(end-line),
		      buffer: *testing-buffer*, moving?: #t));
      section-nodes(section) := list(node);
      node
    end method;

  // ... 1st node & section, 3 lines
  *line1-1* := make(<text-line>, contents: copy-sequence("Line 1"));
  *line1-2* := make(<text-line>, contents: copy-sequence("Line 2"));
  *line1-3* := make(<text-line>, contents: copy-sequence("Line 3"));
  *section1* := make(<section>, start-line: #f, end-line: #f);
  add-line!(*section1*, *line1-1*);
  add-line!(*section1*, *line1-2*);
  add-line!(*section1*, *line1-3*);
  *node1* := make-testing-node(*section1*, *line1-1*, *line1-3*);

  // ... 2nd node & section, 1 line
  *line2-1* := make(<text-line>, contents: copy-sequence("Line 4"));
  *section2* := make(<section>, start-line: #f, end-line: #f);
  add-line!(*section2*, *line2-1*);
  *node2* := make-testing-node(*section2*, *line2-1*, *line2-1*);

  // ... 3rd node & section, 1 line
  *line3-1* := make(<text-line>, contents: copy-sequence("Line 5"));
  *section3* := make(<section>, start-line: #f, end-line: #f);
  add-line!(*section3*, *line3-1*);
  *node3* := make-testing-node(*section3*, *line3-1*, *line3-1*);

  // ... put the sections into the container
  add-section!(*container*, *section1*);
  add-section!(*container*, *section2*);
  add-section!(*container*, *section3*);

  // ... put the nodes into the buffer
  add-node!(*testing-buffer*, *node1*);
  add-node!(*testing-buffer*, *node2*);
  add-node!(*testing-buffer*, *node3*);

  // ... some BPs
  *start1-1-bp* := line-start(*line1-1*);
  *middle1-1-bp* := make-bp(*line1-1*, 1);
  *end1-1-bp* := line-end(*line1-1*);
  *start1-2-bp* := line-start(*line1-2*);
  *middle1-3-bp* := make-bp(*line1-3*, 2);
  *end1-3-bp* := line-end(*line1-3*);
  *start2-1-bp* := line-start(*line2-1*);
  *middle3-1-bp* := make-bp(*line3-1*, 3);
  *end3-1-bp* := line-end(*line3-1*);

  // ... some intervals
  dynamic-bind(*buffer* = *testing-buffer*)
    // We need the dynamic-bind, or buffer-bp will be wrong, and so
    // order-bps, called within make on <interval>, will get the
    // wrong answer (but not fail).
    *empty-interval* := make-interval(*start1-1-bp*, *start1-1-bp*);
    *eol-interval* := make-interval(*end1-1-bp*, *start1-2-bp*);
    *line-interval* := make-interval(*start1-1-bp*, *end1-1-bp*);
    *part-lines-interval* := make-interval(*middle1-1-bp*, *middle1-3-bp*);
    *section-interval* := make-interval(*start1-1-bp*, *end1-3-bp*);
    *part-sections-interval* := make-interval(*middle1-3-bp*, *middle3-1-bp*);
    *buffer-interval* := make-interval(*start1-1-bp*, *end3-1-bp*);
  end;
end function;

begin
  reset-testing-buffer-state();
end;

// -=- Dynamic access to these module variables -=-

// The values of these change with each call to
// reset-testing-buffer-state, so we have to evaluate them dynamically
// to get the right value.

define function testing-lines () => (lines :: <vector> /*of: <line>*/)
  vector(*line1-1*, *line1-2*, *line1-3*, *line2-1*, *line3-1*)
end function;

define function testing-intervals
    () => (intervals :: <vector> /*of: <interval>*/)
  vector
    (*empty-interval*, *eol-interval*, *line-interval*,
     *part-lines-interval*, *section-interval*, *part-sections-interval*,
     *buffer-interval*)
end function;



// -=- Common test functions -=-

define function last-line-from
    (line :: <line>,
     #key in-buffer? :: <boolean> = #f, verbose? :: <boolean> = #f)
 => (last-line :: false-or(<line>))
  // Follow the line-next chain from line, until we hit #f,
  // returning the last line we saw.  Also check that
  // previous(next(line)) == line (when next(line) ~== #f), i.e. that
  // next and previous pointers match up.
  let (next, previous)
    = if (in-buffer?)
        values(rcurry(line-next-in-buffer, *buffer*),
               rcurry(line-previous-in-buffer, *buffer*))
      else
        values(line-next, line-previous)
      end;
  block(return)
    for (this = line then next(this), until: ~next(this))
      when (verbose?) *format-function*("this = %=", this); end;
      unless(previous(next(this)) == this)
        when (verbose?)
          *format-function*
            ("this.next = %=, this.next.previous = %=",
             this.next, this.next.previous);
        end when;
        return(#f);
      end;
    finally this
    end for
  end
end function;

define function check-buffer-bps
    (buffer :: <basic-buffer>, #key verbose? :: <boolean> = #f)
 => (okay? :: <boolean>)
  block(return)
    // Check that the node-{start,end}-bps are indeed at the start/end
    // of lines which have no line-{previous,next}.
    for (node = buffer-start-node(buffer)
           then node-next(node),
         until: ~node)
      when (verbose?)
        *format-function*("node: %=", node);
      end when;
      let start-bp = interval-start-bp(node);
      let start-line = bp-line(start-bp);
      let start-index = bp-index(start-bp);
      let end-bp = interval-end-bp(node);
      let end-line = bp-line(end-bp);
      let end-index = bp-index(end-bp);
      unless
          ( ~line-previous(start-line) & start-index == 0
          & ~line-next(end-line) & end-index == line-length(end-line)
          & ~moving-bp?(start-bp) & moving-bp?(end-bp)
          & ~simple-bp?(start-bp) & ~simple-bp?(end-bp))
        when (verbose?)
          ~line-previous(start-line)
            | *format-function*("start line has a previous line\n");
          start-index == 0
            | *format-function*("start index not == 0\n");
          ~line-next(end-line)
            | *format-function*("end line has a next line\n");
          end-index == line-length(end-line)
            | *format-function*
                ("end index %= not == line length %=", end-index,
                 line-length(end-line));
          ~moving-bp?(start-bp)
            | *format-function*("start BP is a <moving-bp>\n");
          moving-bp?(end-bp)
            | *format-function*("end BP is not a <moving-bp>\n");
          ~simple-bp?(start-bp)
            | *format-function*("start BP is not a <permanent-bp>\n");
          ~simple-bp?(end-bp)
            | *format-function*("end BP is not a <permanent-bp>\n");
        end when;
        return(#f);
      end unless;
    end for;

    // Check that all line-bps are moving-bps, that they're on the
    // right line and that their indices are in range for the line
    // they're on.
    do-lines
      (method (line, si, ei, last?)
         ignore(si, ei, last?);
         for (bp in line-bps(line))
           let _line = bp-line(bp);
           let _index = bp-index(bp);
           unless (moving-bp?(bp) & _line == line
                     & 0 <= _index & _index <= line-length(line))
             when (verbose?)
               moving-bp?(bp)
                 | *format-function*("line BP is not a <moving-bp>\n");
               _line == line
                 | *format-function*("line BP is on wrong line\n");
               0 <= _index
                 | *format-function*("line BP has index < 0\n");
               _index <= line-length(line)
                 | *format-function*("line BP has index > line length\n");
             end when;
             return(#f);
           end unless;
         end for;
       end method, buffer);

    // If we got this far, everything's okay.
    #t
  end;
end function;
