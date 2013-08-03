Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Shell mode, for building shells and interactors

define protocol <<shell-mode>> (<<major-mode>>)
  getter shell-section?
    (section :: <section>) => (shell-section? :: <boolean>);
  function shell-input-complete?
    (mode :: <shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>)
 => (complete? :: <boolean>, message :: false-or(<string>));
  function process-shell-input
    (mode :: <shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>, #key window) => ();
  function do-process-shell-input
    (mode :: <shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>, #key window) => ();
  function terminate-shell
    (mode :: <shell-mode>, buffer :: <basic-shell-buffer>) => ();
end protocol <<shell-mode>>;

// Intended for subclassing...
define open abstract class <shell-mode> (<major-mode>)
end class <shell-mode>;

define method mode-name
    (mode :: <shell-mode>) => (name :: <byte-string>)
  "Shell"
end method mode-name;

define method initialize-major-mode
    (mode :: <shell-mode>, #key command-set = mode-command-set(mode)) => ()
  next-method();
  let nothing = 0;
  let shift   = $shift-key;
  let control = $control-key;
  let meta    = $meta-key;
  let command-set = copy-command-set(command-set);
  mode-command-set(mode) := command-set;
  select (command-set-name(command-set))
    #"emacs" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
                    vector('p', meta,    previous-shell-input),
                    vector('n', meta,    next-shell-input),
                    vector(#"up",        control, previous-shell-input),
                    vector(#"down",      control, next-shell-input),
                    vector(#"return",    nothing, activate-shell-input-newline),
                    vector(#"return",    control, activate-shell-input),
                    vector('w', control, trim-shell-output-history),
                    vector('g', control, cancel-shell-command));
      let command-table = escape-command-table(command-set);
      add-commands!(command-table,
                    vector('p', nothing, previous-shell-input),
                    vector('n', nothing, next-shell-input));
    #"windows" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
                    vector(#"up",        control, previous-shell-input),
                    vector(#"down",      control, next-shell-input),
                    vector(#"return",    nothing, activate-shell-input-newline),
                    vector(#"return",    control, activate-shell-input),
                    vector('x', control, trim-shell-output-history),
                    vector(#"escape", nothing, cancel-shell-command));
    otherwise =>
      #[];
  end
end method initialize-major-mode;


/// Shell mode buffers

// Shell buffers consist of a set of (hard) sections, where each section
// represents one input/output interaction with the user
define open abstract class <basic-shell-buffer>
    (<basic-special-purpose-buffer>)
  // State for 'next/previous-shell-input'
  slot %current-input :: false-or(<section-node>) = #f;
  slot shell-buffer-section-class :: <class> = <simple-shell-section>,
    init-keyword: section-class:;
end class <basic-shell-buffer>;

// Want to be able to trim the output history across sections...
define method buffer-has-hard-sections?
    (buffer :: <basic-shell-buffer>) => (hard-sections? :: <boolean>)
  #f
end method buffer-has-hard-sections?;

// The idea is to initially position the point in the latest shell section
// into which the user can type some input, or at the end of the "header"
// section if there is one
define method buffer-initial-point
    (buffer :: <basic-shell-buffer>, #key point :: false-or(<basic-bp>) = #f)
 => (bp :: false-or(<basic-bp>))
  block (return)
    for (node = buffer-end-node(buffer) then node-previous(node),
         until: ~node)
      let section = node-section(node);
      when (shell-section?(section))
        return(line-start(section-start-line(section)))
      end
    end;
    let node    = buffer-start-node(buffer);
    let section = node & node-section(node);
    when (section)
      return(line-start(section-end-line(section)))
    end;
    next-method()
  end
end method buffer-initial-point;

define method revert-buffer
    (buffer :: <basic-shell-buffer>,
     #key buffer-filler :: false-or(<function>) = #f, major-mode)
 => (reverted? :: <boolean>)
  ignore(buffer-filler, major-mode);
  #f
end method revert-buffer;

define method kill-buffer
    (buffer :: <basic-shell-buffer>,
     #key frame = *editor-frame*, editor, no-exit-frame) => ()
  ignore(frame, editor, no-exit-frame);
  terminate-shell(buffer-major-mode(buffer), buffer);
  next-method()
end method kill-buffer;

define sealed class <simple-shell-buffer> (<basic-shell-buffer>)
end class <simple-shell-buffer>;

define sealed domain make (singleton(<simple-shell-buffer>));
define sealed domain initialize (<simple-shell-buffer>);


/// Shell mode sections

define open abstract class <basic-shell-section> (<basic-section>)
  // The interval from the 'section-start-line' up to, but not including,
  // 'section-output-line' is the input part of the section.  The interval
  // from 'section-output-line' through 'section-end-line' is the output
  // part of the section.  If 'section-output-line' is #f, then no output
  // has occurred in the section yet.
  slot section-output-line :: false-or(<basic-line>) = #f,
    init-keyword: output-line:;
end class <basic-shell-section>;

define method shell-section?
    (section :: <section>) => (shell-section? :: <boolean>)
  #f
end method shell-section?;

define method shell-section?
    (section :: <basic-shell-section>) => (shell-section? :: <boolean>)
  #t
end method shell-section?;

define sealed class <simple-shell-section> (<basic-shell-section>)
end class <simple-shell-section>;

define sealed domain make (singleton(<simple-shell-section>));
define sealed domain initialize (<simple-shell-section>);


/// Shell input, for presentations

define open class <shell-input> (<basic-interval>)
end class <shell-input>;

define sealed class <simple-shell-input> (<shell-input>)
end class <simple-shell-input>;

define sealed inline method make
    (class == <shell-input>, #rest initargs, #key, #all-keys)
 => (input :: <simple-shell-input>)
  apply(make, <simple-shell-input>, initargs)
end method make;

define sealed domain make (singleton(<simple-shell-input>));
define sealed domain initialize (<simple-shell-input>);


/// Shell mode methods

// Methods should return #t when the current input section is complete,
// otherwise they should return #f and (optionally) an error message
define method shell-input-complete?
    (mode :: <shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>)
 => (complete? :: <boolean>, message :: false-or(<string>))
  values(#t, #f)
end method shell-input-complete?;

define method process-shell-input
    (mode :: <shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>,
     #key window = frame-window(*editor-frame*),
          processor = do-process-shell-input) => ()
  // Set up 'section-output-line', opening a line for the new output if necessary
  let start-line = section-start-line(section);
  let start-bp   = line-start(start-line);
  let end-line   = section-end-line(section);
  let end-bp     = line-end(end-line);
  insert-moving!(end-bp, '\n');
  section-output-line(section) := bp-line(end-bp);        // first line of output
  let interval = make-interval(start-bp, line-end(line-previous(bp-line(end-bp))),
                               in-order?: #t);
  block ()
    // Ask the concrete mode implementation to process the input
    // This will presumably generate some output in the process
    processor(mode, buffer, section, window: window);
    // Mark the input section with a "presentation"
    let property = list(#"input", interval);
    local method set-property (line :: <basic-line>, si, ei, last?);
            ignore(si, ei, last?);
            line-properties(line)
              := concatenate!(line-properties(line), property)
          end method;
    do-lines(set-property, interval);
  cleanup
    create-next-shell-section(buffer, section, window: window)
  end
end method process-shell-input;

define method create-next-shell-section
    (buffer :: <basic-shell-buffer>, section :: <basic-section>,
     #key window = frame-window(*editor-frame*)) => ()
  // Protect this section against further modifications
  let old-node = section-node(section, buffer: buffer);
  interval-read-only?(old-node) := #t;
  // Now create the next shell section
  let new-node
    = make-empty-section-node(buffer,
                              section-class: shell-buffer-section-class(buffer));
  add-node!(buffer, new-node);
  move-point!(interval-end-bp(new-node), window: window);
  queue-redisplay(window, $display-text, centering: 1)
end method create-next-shell-section;

// Methods should process the input and insert the output at the
// end of the section, setting the appropriate redisplay degree
define method do-process-shell-input
    (mode :: <shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>,
     #key window = frame-window(*editor-frame*)) => ()
  ignore(window);
  #f
end method do-process-shell-input;

define method terminate-shell
    (mode :: <shell-mode>, buffer :: <basic-shell-buffer>) => ()
  #f
end method terminate-shell;


/// Shell mode commands

define command activate-shell-input (frame)
    "Activate the current shell input line.\n"
    "If the current position is not in the active input section,\n"
    "this will copy the section into the active input section."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-shell-buffer> = frame-buffer(frame);
  let bp   = point();
  let line = bp-line(bp);
  let last-node = buffer-end-node(buffer);
  let section = node-section(last-node);
  when (shell-section?(section))
    if (line-section(line) == section)
      // Try to activate the input
      let mode :: <shell-mode> = buffer-major-mode(buffer);
      let (complete?, message) = shell-input-complete?(mode, buffer, section);
      if (complete?)
        process-shell-input(mode, buffer, section, window: window);
        buffer.%current-input := #f;
        frame-last-command-type(frame) := #"shell"
      else
        if (message)
          command-error(message)
        else
          do-insert-newline(frame);
          frame-last-command-type(frame) := #"insert"
        end
      end;
    else
      copy-previous-section(frame, line-section(line))
    end
  end
end command activate-shell-input;

define command activate-shell-input-newline (frame)
    "Insert a newline, then activate the current shell input line.\n"
    "If the current position is not in the active input section,\n"
    "this will copy the section into the active input section."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-shell-buffer> = frame-buffer(frame);
  let bp   = point();
  let line = bp-line(bp);
  let last-node = buffer-end-node(buffer);
  let section = node-section(last-node);
  when (shell-section?(section))
    if (line-section(line) == section)
      // Try to activate the input iff the newline was at the end of the input,
      // or there is only trailing whitespace
      let end-bp = interval-end-bp(buffer);
      if (bp = end-bp
          | forward-over(bp, #[' ', '\t', '\n', '\f']) = end-bp)
        // Ensure redisplay has already happened in case the input line gets
        // a parse error
        redisplay-window(window);
        activate-shell-input(frame)
      else
        do-insert-newline(frame);
        frame-last-command-type(frame) := #"insert"
      end
    else
      copy-previous-section(frame, line-section(line))
    end
  end
end command activate-shell-input-newline;

define method copy-previous-section
    (frame :: <editor-state-mixin>, section :: <basic-section>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-shell-buffer> = frame-buffer(frame);
  let last-node = buffer-end-node(buffer);
  let line      = section & section-start-line(section);
  let input     = line & get-property(line-properties(line), #"input");
  when (input)
    let interval = make-interval(interval-start-bp(last-node), interval-end-bp(last-node),
                                 in-order?: #t);
    let bp = interval-end-bp(last-node);
    move-point!(bp);
    delete!(interval);
    insert-moving!(bp, input);
    move-point!(bp);
    queue-redisplay(window, $display-text, centering: 1);
  end;
  frame-last-command-type(frame) := #"insert"
end method copy-previous-section;


define command previous-shell-input (frame)
    "Yank the previous shell input line."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-shell-buffer> = frame-buffer(frame);
  let last-node = buffer-end-node(buffer);
  let node      = buffer.%current-input | last-node;
  let wrapped?  = #f;
  block (break)
    // Loop until we find a node that's got an #"input" property,
    // but don't wrap around more than once
    while (#t)
      node := case
                node-previous(node) =>
                  node-previous(node);
                wrapped? =>
                  command-error("The input history is empty");
                otherwise =>
                  wrapped? := #t;
                  node-previous(last-node);
              end;
      if (node)
        buffer.%current-input := node;
        let section = node-section(node);
        when (shell-section?(section))
          let line  = section & section-start-line(section);
          let input = line & get-property(line-properties(line), #"input");
          when (input)
            let interval = make-interval(interval-start-bp(last-node), interval-end-bp(last-node),
                                         in-order?: #t);
            let bp = interval-end-bp(last-node);
            move-point!(bp);
            delete!(interval);
            insert-moving!(bp, input);
            move-point!(bp);
            queue-redisplay(window, $display-text, centering: 1);
            break()
          end
        end
      else
        command-error("The input history is empty")
      end
    end
  end;
  frame-last-command-type(frame) := #"shell"
end command previous-shell-input;

define command next-shell-input (frame)
    "Yank the next shell input line."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-shell-buffer> = frame-buffer(frame);
  let first-node = buffer-start-node(buffer);
  let last-node  = buffer-end-node(buffer);
  let node       = buffer.%current-input | last-node;
  let wrapped?   = #f;
  block (break)
    while (#t)
      node := case
                node-next(node) =>
                  node-next(node);
                wrapped? =>
                  command-error("The input history is empty");
                otherwise =>
                  wrapped? := #t;
                  node-next(first-node);
              end;
      if (node)
        buffer.%current-input := node;
        let section = node-section(node);
        when (shell-section?(section))
          let line  = section & section-start-line(section);
          let input = line & get-property(line-properties(line), #"input");
          when (input)
            let interval = make-interval(interval-start-bp(last-node), interval-end-bp(last-node),
                                         in-order?: #t);
            let bp = interval-end-bp(last-node);
            move-point!(bp);
            delete!(interval);
            insert-moving!(bp, input);
            move-point!(bp);
            queue-redisplay(window, $display-text, centering: 1);
            break()
          end
        end
      else
        command-error("The input history is empty")
      end
    end
  end;
  frame-last-command-type(frame) := #"shell"
end command next-shell-input;


define command trim-shell-output-history (frame)
    "Remove the marked sections from the shell's output history."
  block (return)
    let bp1 = point();
    let bp2 = mark();
    if (bp2)
      let window :: <basic-window> = frame-window(frame);
      let buffer :: <basic-buffer> = window-buffer(window);
      let (bp1, bp2) = order-bps(bp1, bp2);
      let start-node = bp-node(bp1);
      let end-node   = bp-node(bp2);
      let last-node  = buffer-end-node(buffer);
      // If both BPs are in the last node, it's just a simple cut operation
      when (start-node == last-node & end-node == last-node)
        return(cut-region(frame))
      end;
      // Otherwise, "trim" the output history
      when (end-node == last-node)
        end-node := node-previous(last-node);
        when (~end-node)
          command-error("You can't delete the only remaining input section")
        end
      end;
      move-point!(section-start-line(node-section(node-next(end-node))));
      let interval = make-interval(interval-start-bp(start-node), interval-end-bp(end-node),
                                   in-order?: #t);
      kill!(interval);
      queue-redisplay(window, $display-all)
    else
      command-error("There is no selected region")
    end
  end
end command trim-shell-output-history;

define command cancel-shell-command (frame)
    "Cancel the current command, then start a new input section."
  let reset? = (frame-last-command-type(frame) == #"cancel");
  cancel-command(frame);
  when (reset?)
    let window :: <basic-window> = frame-window(frame);
    let buffer :: <basic-shell-buffer> = frame-buffer(frame);
    let last-node = buffer-end-node(buffer);
    let section   = node-section(last-node);
    create-next-shell-section(buffer, section, window: window);
    // Next control-G shouldn't cancel the input...
    frame-last-command-type(frame) := #f
  end
end command cancel-shell-command;
