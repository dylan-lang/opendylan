Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Language-specific major modes, e.g., Dylan or C++

define constant <breakpoint-state>
    = one-of(#f, #"none",
             #"enabled-break", #"disabled-break", #"test-break", #"step",
             #"enabled-trace", #"disabled-trace", #"profile", #"current",
             #"warning", #"serious-warning");

// Language-specific operations follow...
define protocol <<language-major-mode>> (<<major-mode>>)
  function do-sectionize-buffer
    (mode :: <major-mode>, buffer :: <buffer>) => (sectionized? :: <boolean>);
  function do-sectionize-container
    (mode :: <major-mode>, container :: <source-container>) => (sectionized? :: <boolean>);
  function do-resectionize-section
    (mode :: <major-mode>, section :: <section>) => (resectionized? :: <boolean>);
  function do-note-line-changed
    (mode :: <major-mode>, line :: <line>) => (line :: <line>);
  // Returns an interval containing the entire definition around the BP,
  // which is often simply the node containing the current section
  function do-definition-interval
    (mode :: <major-mode>, bp :: <bp>) => (interval :: false-or(<interval>));
  // Returns an interval containing a "relevant" definition name near the BP
  function do-relevant-function-interval
    (mode :: <major-mode>, bp :: <bp>) => (interval :: false-or(<interval>));
  // Motion
  function do-move-over-expressions
    (mode :: <major-mode>, bp :: <bp>, n :: <integer>, #key fixup?, interval)
 => (bp :: false-or(<bp>));
  function do-move-up-or-down-expressions
    (mode :: <major-mode>, bp :: <bp>, n :: <integer>, #key fixup?, interval)
 => (bp :: false-or(<bp>));
  function do-insert-comment
    (mode :: <major-mode>, line :: <line>, #key column)
 => (bp :: false-or(<bp>));
  // Indentation
  // 'nchars' is the change in the number of characters of indentation
  // 'dx' is the change of the indentation width, measured in characters
  function do-indent-line
    (mode :: <major-mode>, line :: <line>)
 => (bp :: false-or(<bp>), nchars :: <integer>, dx :: <integer>);
  function do-indent-region
    (mode :: <major-mode>, region :: <interval>) => ();
  function do-comment-region
    (mode :: <major-mode>, region :: <interval>, #key comment?) => ();
  // Environment query
  // The interval argument contains the "object" to query about
  function do-edit-definition
    (mode :: <major-mode>, interval :: <interval>, window :: <window>, #key name)
 => (success? :: <boolean>);
  function do-complete-name
    (mode :: <major-mode>, interval :: <interval>, window :: <window>,
     #key menu?)
 => (completion :: type-union(<string>, <boolean>), ambiguous? :: <boolean>);
  function do-complete-dynamically
    (mode :: <major-mode>, interval :: <interval>, window :: <window>,
     #key menu?, completion-state)
 => (completion :: type-union(<string>, <boolean>), ambiguous? :: <boolean>);
  function do-describe-object
    (mode :: <major-mode>, interval :: <interval>, window :: <window>, #key name)
 => (success? :: <boolean>);
  function do-browse-object
    (mode :: <major-mode>, interval :: <interval>, window :: <window>, #key name)
 => (success? :: <boolean>);
  function do-browse-class
    (mode :: <major-mode>, interval :: <interval>, window :: <window>, #key name)
 => (success? :: <boolean>);
  function do-browse-function
    (mode :: <major-mode>, interval :: <interval>, window :: <window>, #key name)
 => (success? :: <boolean>);
  function do-show-arglist
    (mode :: <major-mode>, interval :: <interval>, window :: <window>, #key name)
 => (success? :: <boolean>);
  function do-show-documentation
    (mode :: <major-mode>, interval :: <interval>, window :: <window>, #key name)
 => (success? :: <boolean>);
  function definition-browser-parameters
    (mode :: <major-mode>, interval :: <interval>, what)
 => (definition, name-key :: <function>, generator :: <function>,
     major-mode :: <major-mode>, node-class :: subclass(<definition-node>));
  // Compilation
  function compilation-supported?
    (mode :: <major-mode>) => (compilation-supported? :: <boolean>);
  function do-compile-to-core
    (mode :: <major-mode>, interval :: <interval>) => ();
  function do-compile-file
    (mode :: <major-mode>, pathname :: <pathname>) => ();
  function do-load-file
    (mode :: <major-mode>, pathname :: <pathname>) => ();
  function do-build-project
    (mode :: <major-mode>, buffer :: <buffer>, scope) => ();
  function do-macroexpand
    (mode :: <major-mode>, interval :: <interval>, stream :: <stream>) => ();
  // Breakpoints -- 'state' is #f, #"none", #"enabled-break", #"disabled-break", etc
  function line-breakpoint?
    (mode :: <major-mode>, line :: <line>) => (state :: <breakpoint-state>);
  function line-breakpoint?-setter
    (state :: <breakpoint-state>, mode :: <major-mode>, line :: <line>)
 => (state :: <breakpoint-state>);
  function line-breakpoint-icon
    (mode :: <major-mode>, line :: <line>, window :: <window>)
 => (image :: false-or(<object>));
end protocol <<language-major-mode>>;

define open abstract class <language-mode> (<major-mode>)
end class <language-mode>;


/// Purely syntactic language-mode operations

// The default method doesn't need to do anything, because the act of
// creating a buffer already ensures a single node with a section in it
define method do-sectionize-buffer
    (mode :: <major-mode>, buffer :: <buffer>) => (sectionized? :: <boolean>)
  #f
end method do-sectionize-buffer;

// Default method does nothing
define method do-sectionize-container
    (mode :: <major-mode>, container :: <source-container>) => (sectionized? :: <boolean>)
  #f
end method do-sectionize-container;

// Default method does nothing
define method do-resectionize-section
    (mode :: <major-mode>, section :: <section>) => (resectionized? :: <boolean>)
  #f
end method do-resectionize-section;

// Default method does nothing
define method do-note-line-changed
    (mode :: <major-mode>, line :: <basic-line>) => (line :: <basic-line>)
  line
end method do-note-line-changed;


define method do-definition-interval
    (mode :: <major-mode>, bp :: <basic-bp>)
 => (interval :: false-or(<basic-interval>))
  //--- This presumes that the sectionization is up to date...
  let node = bp-node(bp);
  node & make-interval(interval-start-bp(node), interval-end-bp(node), in-order?: #t)
end method do-definition-interval;

define method do-relevant-function-interval
    (mode :: <major-mode>, bp :: <basic-bp>)
 => (interval :: false-or(<basic-interval>))
  let node = bp-node(bp) | bp-buffer(bp);
  // Use the atom right under the point
  let sbp = if (atom-syntax(bp-character-before(bp)) == $atom-delimiter)
              forward-over(bp, #[' ', '\t', '\f'], interval: node)
            else
              move-over-atoms(bp, -1, interval: node)
            end;
  let ebp = move-over-atoms(sbp, 1, interval: node);
  make-interval(sbp, ebp, in-order?: #t)
end method do-relevant-function-interval;


define method do-move-over-expressions
    (mode :: <major-mode>, bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  move-over-lists(bp, n, fixup?: fixup?, interval: interval)
end method do-move-over-expressions;

define method do-move-up-or-down-expressions
    (mode :: <major-mode>, bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  move-up-or-down-lists(bp, n, fixup?: fixup?, interval: interval)
end method do-move-up-or-down-expressions;


// This should be run inside a 'with-change-recording' of a paste change record
define method do-insert-comment
    (mode :: <major-mode>, line :: <basic-line>, #key column)
 => (bp :: false-or(<basic-bp>))
  ignore(column);
  #f
end method do-insert-comment;


define method do-indent-line
    (mode :: <major-mode>, line :: <basic-line>)
 => (bp :: false-or(<basic-bp>), nchars :: <integer>, dx :: <integer>)
  values(#f, 0, 0)
end method do-indent-line;

define method do-indent-region
    (mode :: <major-mode>, region :: <basic-interval>) => ()
  local method indent (line :: <basic-line>, si, ei, last?)
          ignore(si, ei, last?);
          do-indent-line(mode, line)
        end method;
  do-lines(indent, region)
end method do-indent-region;

// This should be run inside a 'with-change-recording' of a replace change record
define method do-comment-region
    (mode :: <major-mode>, region :: <basic-interval>, #key comment?) => ()
  ignore(comment?);
  #f
end method do-comment-region;


/// Stubs for operations that require querying an environment

define method do-edit-definition
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key name)
 => (success? :: <boolean>)
  ignore(name);
  #f
end method do-edit-definition;


define method do-complete-name
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key menu? = #f)
 => (completion :: type-union(<string>, <boolean>), ambiguous? :: <boolean>)
  values(#f, #f)
end method do-complete-name;

define method do-complete-dynamically
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key menu? = #f, completion-state: state = #f)
 => (completion :: type-union(<string>, <boolean>), ambiguous? :: <boolean>)
  let (word, word-bp, search-bp, reverse?, completion)
    = if (state[0])
        values(state[0], state[1], state[2], state[3], state[4])
      else
        let (bp1, bp2) = atom-under-bp(point());
        let word = (atom-syntax(bp-character(bp1)) == $atom-alphabetic
                    & as(<byte-string>, make-interval(bp1, bp2, in-order?: #t)));
        values(word, bp1, bp1, #t, word)
      end;
  state[0] := #f;                // assume failure
  when (word)
    let bp = search-bp;
    block (break)
      local method maybe-break ()
              when (bp)
                let is-atom?   = atom-syntax(bp-character-before(bp)) ~== $atom-alphabetic;
                let (bp1, bp2) = atom-under-bp(bp);
                bp := (if (reverse?) bp1 else bp2 end);
                when (is-atom?)
                  let string = as(<byte-string>, make-interval(bp1, bp2, in-order?: #t));
                  when (~string-equal?(string, completion) & ~string-equal?(string, word))
                    completion := string;
                    break()
                  end
                end
              end
            end method;
      // 'reverse?' could be either #t or #f here
      while (bp)
        bp := search(bp, word, reverse?: reverse?);
        maybe-break()
      end;
      // Switch to searching forward when reverse search fails
      when (~bp & reverse?)
        bp       := move-over-atoms(word-bp, 1);
        reverse? := #f;
        while (bp)
          bp := search(bp, word, reverse?: reverse?);
          maybe-break()
        end
      end
    end block;
    when (bp & completion)
      state[0] := word;
      state[1] := word-bp;
      state[2] := bp;
      state[3] := reverse?;
      state[4] := completion;
      values(completion, #f)
    end
  end
end method do-complete-dynamically;

define function reset-dynamic-completion-state
    (frame :: <editor-state-mixin>) => (state :: <simple-object-vector>)
  let state :: <simple-object-vector>
    = frame-dynamic-completion-state(frame)
      | (frame-dynamic-completion-state(frame) := vector(#f, #f, #f, #f, #f));
  unless (frame-last-command-type(frame) == #"dynamic-complete")
    state[0] := #f
  end;
  state
end function reset-dynamic-completion-state;


define method do-describe-object
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key name)
 => (success? :: <boolean>)
  ignore(name);
  #f
end method do-describe-object;

define method do-browse-object
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key name)
 => (success? :: <boolean>)
  ignore(name);
  #f
end method do-browse-object;

define method do-browse-class
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key name)
 => (success? :: <boolean>)
  ignore(name);
  #f
end method do-browse-class;

define method do-browse-function
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key name)
 => (success? :: <boolean>)
  ignore(name);
  #f
end method do-browse-function;


define method do-show-arglist
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key name)
 => (success? :: <boolean>)
  ignore(name);
  #f
end method do-show-arglist;

define method do-show-documentation
    (mode :: <major-mode>, interval :: <basic-interval>, window :: <basic-window>,
     #key name)
 => (success? :: <boolean>)
  ignore(name);
  #f
end method do-show-documentation;


define method definition-browser-parameters
    (mode :: <major-mode>, interval :: <basic-interval>, what)
 => (definition, name-key :: <function>, generator :: <function>,
     major-mode :: <major-mode>, node-class :: subclass(<definition-node>))
  values(#f, always(""), always(#[]), mode, <definition-node>)
end method definition-browser-parameters;


define method compilation-supported?
    (mode :: <major-mode>) => (compilation-supported? :: <boolean>)
  #f
end method compilation-supported?;

define method do-compile-to-core
    (mode :: <major-mode>, interval :: <basic-interval>) => ()
  #f
end method do-compile-to-core;

define method do-compile-file
    (mode :: <major-mode>, pathname :: <pathname>) => ()
  #f
end method do-compile-file;

define method do-load-file
    (mode :: <major-mode>, pathname :: <pathname>) => ()
  #f
end method do-load-file;

define method do-build-project
    (mode :: <major-mode>, buffer :: <basic-buffer>, scope :: <symbol>) => ()
  #f
end method do-build-project;

define method do-macroexpand
    (mode :: <major-mode>, interval :: <basic-interval>, stream :: <stream>) => ()
  #f
end method do-macroexpand;


/// Breakpoints

define method line-breakpoint?
    (mode :: <major-mode>, line :: <basic-line>)
 => (state :: <breakpoint-state>)
  #f
end method line-breakpoint?;

define method line-breakpoint?-setter
    (state :: <breakpoint-state>, mode :: <major-mode>, line :: <basic-line>)
 => (state :: <breakpoint-state>)
  state
end method line-breakpoint?-setter;

define method line-breakpoint-icon
    (mode :: <major-mode>, line :: <basic-line>, window :: <basic-window>)
 => (image :: false-or(<object>))
  ignore(window);
  let state = line-breakpoint?(mode, line);
  state & select (state)
            #"none"            => $potential-breakpoint;
            #"enabled-break"   => $enabled-breakpoint;
            #"disabled-break"  => $disabled-breakpoint;
            #"step"            => $step-breakpoint;
            #"test-break"      => $test-breakpoint;
            #"enabled-trace"   => $enabled-tracepoint;
            #"disabled-trace"  => $disabled-tracepoint;
            #"profile"         => $profile-point;
            #"current"         => $current-location;
            #"warning"         => $warning;
            #"serious-warning" => $serious-warning;
          end
end method line-breakpoint-icon;


/// Line display with breakpoint icons

//--- Need a more modular way to do this
define constant $breakpoint-image-width  :: <integer> = 20;
define constant $breakpoint-image-height :: <integer> = 16;
define constant $breakpoint-image-offset :: <integer> =  2;

define method display-line
    (line :: <text-line>, mode :: <language-mode>, window :: <basic-window>,
     x :: <integer>, y :: <integer>,
     #key start: _start = 0, end: _end = line-length(line), align-y = #"top") => ()
  let image = line-breakpoint-icon(mode, line, window);
  when (image & _start = 0)        // no icon on continuation lines
    let image-y = if (align-y == #"top") y else y - $breakpoint-image-height + 2 end;
    draw-image(window, standard-images(window, image), x, image-y + $breakpoint-image-offset)
  end;
  next-method(line, mode, window, x + $breakpoint-image-width, y,
              start: _start, end: _end, align-y: align-y)
end method display-line;

define method line-size
    (line :: <text-line>, mode :: <language-mode>, window :: <basic-window>,
     #key start: _start, end: _end)
 => (width :: <integer>, height :: <integer>, baseline :: <integer>)
  ignore(_start, _end);
  let (width, height, baseline) = next-method();
  // The width of a breakpoint line has to include the icons on the left...
  values(width + $breakpoint-image-width, height, baseline)
end method line-size;

define method position->index
    (line :: <text-line>, mode :: <language-mode>, window :: <basic-window>,
     x :: <integer>)
 => (index :: <integer>)
  let x = x - $breakpoint-image-width;
  if (x < 0) 0 else next-method(line, mode, window, x) end
end method position->index;

define method index->position
    (line :: <text-line>, mode :: <language-mode>, window :: <basic-window>,
     index :: <integer>)
 => (x :: <integer>)
  next-method(line, mode, window, index)
end method index->position;

define method line-margin
    (line :: <text-line>, mode :: <language-mode>, window :: <basic-window>)
 => (margin :: <integer>)
  $breakpoint-image-width
end method line-margin;
