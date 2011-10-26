Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Deuce-based Dylan interactor controls

define open class <dylan-interactor> (<environment-shell-window>)
  slot interactor-last-input  :: false-or(<string>) = #f;
  slot interactor-next-subset :: false-or(<result-subset>) = #f;
  slot interactor-command-line-server :: false-or(<command-line-server>) = #f;
end class <dylan-interactor>;

ignorable(interactor-last-input, interactor-last-input-setter);

define class <interactor-context> (<environment-context>)
  constant slot interactor :: <dylan-interactor>,
    required-init-keyword: interactor:;
end;

define method context-project (context :: <interactor-context>)
 => (project :: <project-object>)
//  let project =
 frame-current-project(sheet-frame(context.interactor));
/*  let project-context
    = context.context-project-context
        | begin
            let library = project.project-library;
            let module = library & library-default-module(project, library);
            let project-context
              = make(<project-context>,
                     project: project,
                     module: module);
            context.context-project-context := project-context;
          end;
  project; */
end;

define method do-destroy-sheet
    (sheet :: <dylan-interactor>) => ()
  next-method();
  let buffer = window-buffer(sheet);
  when (instance?(buffer, <basic-shell-buffer>))
    // Lose any Dylan objects we might be hanging on to...
    do-lines(method (line, si, ei, last?)
	       ignore(si, ei, last?);
	       remove-property!(line-properties(line), #"object");
	     end method, buffer)
  end
end method do-destroy-sheet;

define open generic interactor-remote-thread
    (interactor :: <dylan-interactor>) => (thread :: <thread-object>);

define open generic interactor-stack-frame-context
    (pane :: <dylan-interactor>)
 => (maybe-frame :: false-or(<stack-frame-object>));

define method make-interactor-command-line-server
    (#key banner :: false-or(<string>) = #f,
          interactor :: <dylan-interactor>,
          input-stream :: <stream>,
          output-stream :: <stream>,
          echo-input? :: <boolean> = #f,
          profile-commands? :: <boolean> = #f)
 => (server :: <command-line-server>)
  let context = make(<interactor-context>, banner: banner, interactor: interactor);
  make(<command-line-server>,
       context:           context,
       input-stream:      input-stream,
       output-stream:     output-stream,
       echo-input?:       echo-input?,
       profile-commands?: profile-commands?)
end method;

define function make-dylan-interactor
    (#rest initargs,
     #key class = <dylan-interactor>,
          project, frame, remote-thread, buffer, documentation, #all-keys)
 => (window :: <dylan-interactor>)
  ignore(frame, remote-thread);
  let window = apply(make, class, documentation: documentation, initargs);
  dynamic-bind (*editor-frame* = window)
    let buffer = buffer | make-interactor-buffer();
    let stream
      = make(<interval-stream>,
             interval: buffer,
             direction: #"output");
    let server
      = make-interactor-command-line-server
          (input-stream: stream,
           output-stream: stream,
           interactor: window);
    window.interactor-command-line-server := server;
    dynamic-bind (*buffer* = buffer)
      select-buffer(window, buffer)
    end
  end;
  window
end function make-dylan-interactor;

define method make-interactor-buffer
    (#key editor = $environment-editor) => (buffer :: <buffer>)
  let buffer = make-dylan-shell(anonymous?: #t,
				section-class: <dylanworks-shell-section>,
				major-mode: find-mode(<dylanworks-shell-mode>),
				editor: editor);
  let header = make-empty-section-node(buffer);
  add-node!(buffer, header, after: #"start");
  buffer
end method make-interactor-buffer;

define method environment-empty-buffer
    (#key editor = $environment-editor)
 => (buffer :: <buffer>)
  let name = "Empty buffer";
  let buffer
    = find-buffer(editor, name)
        | make-empty-buffer(<non-file-buffer>,
			    anonymous?: #t,
			    name: name,
			    editor: editor,
			    read-only?: #t);
  buffer
end method environment-empty-buffer;

define method interactor-stack-frame-context
    (pane :: <dylan-interactor>)
 => (maybe-frame :: false-or(<stack-frame-object>))
  // The interactor, by itself, really knows nothing about stack
  // frame selection.
  #f
end method interactor-stack-frame-context;

define method interactor-last-values
    (pane :: <dylan-interactor>)
 => (values :: false-or(<sequence>))
  //---*** Should return the values from the last evaluation
  #f
end method interactor-last-values;

define method shell-parse-input
    (pane :: <dylan-interactor>, text :: <string>)
 => (complete? :: <boolean>, message :: false-or(<string>))
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  with-busy-cursor (frame)
    case
      interactor-empty-input?(pane, text) =>
	values(#t, #f);
      interactor-valid-command?(pane, text) =>
	values(#t, #f);
      //---*** Should be removed when the compiler gets its act together
      ~interactor-terminated-code?(pane, text) =>
	values(#f, #f);
      otherwise =>
	let module  = frame-ensure-project-database(frame) & frame-current-module(frame);
	when (module)
	  let thread  = interactor-remote-thread(pane);
	  let stack-frame = interactor-stack-frame-context(pane);
	  let (complete?, warnings)
	    = project-valid-code?(project, text, thread, module: module,
				  stack-frame: stack-frame);
	  if (complete?)
	    //---*** Should construct a more specific message from the conditons
	    let message = if (empty?(warnings)) #f else "Syntax error." end;
	    values(#t, message);
	  else
	    values(#f, #f)
	  end
	end;
    end
  end
end method shell-parse-input;

define method interactor-empty-input?
    (pane :: <dylan-interactor>, text :: <string>) => (empty? :: <boolean>)
  block (return)
    for (char in text)
      unless (any-whitespace-char?(char))
	return(#f)
      end
    end;
    #t
  end
end method interactor-empty-input?;

//---*** Should be removed when the compiler gets its act together
define method interactor-terminated-code?
    (pane :: <dylan-interactor>, text :: <string>) => (terminated? :: <boolean>)
  block (return)
    for (char in text using backward-iteration-protocol)
      unless (any-whitespace-char?(char))	// skip whitespace
        if (char == ';')
          return(#t)
	else
	  return(#f)
	end
      end
    end;
    #f
  end
end method interactor-terminated-code?;

define method interactor-valid-command?
    (pane :: <dylan-interactor>, text :: <string>) => (valid? :: <boolean>)
  size(text) > 0 & text[0] = ':'
end method interactor-valid-command?;

define constant $interactor-compilation-timeout :: <integer> = 10;
define constant $waiting-message :: <byte-string> = "Waiting for return values...";

define constant $transaction-types :: <object-table> = make(<table>, weak: #"key");

define method shell-execute-code
    (pane :: <dylan-interactor>, text :: <string>, bp :: <basic-bp>) => ()
  let frame = sheet-frame(pane);
  with-busy-cursor (frame)
    case
      interactor-empty-input?(pane, text) =>
	#f;
      interactor-valid-command?(pane, text) =>
	interactor-process-command(pane, text, bp);
      otherwise =>
	interactor-do-execute-code(pane, text, bp);
    end;
    unless (interactor-empty-input?(pane, text))
      interactor-last-input(pane) := text
    end
  end
end method shell-execute-code;

define method interactor-do-execute-code
    (pane :: <dylan-interactor>, text :: <string>, bp :: <basic-bp>,
     #key transaction-type = #"evaluate") => ()
  let frame   = sheet-frame(pane);
  let section = line-section(bp-line(bp));
  when (shell-section?(section))
    let section :: <dylanworks-shell-section> = section;
    let line = section-output-line(section);
    when (line)
      insert!(line-start(line), $waiting-message);
      queue-redisplay(pane, $display-line, line: line, index: 0, centering: #f);
      redisplay-window(pane)
    end;
    let timeout = $interactor-compilation-timeout;
    with-compiler-locked (frame, timeout: timeout)
      let project = frame-current-project(frame);
      let module  = frame-ensure-project-database(frame) & frame-current-module(frame);
      when (module)
	// Execute the form
	let thread = interactor-remote-thread(pane);
	let stack-frame = interactor-stack-frame-context(pane);
	let transaction-id
	  = project-execute-code(project, text, thread, module: module,
				 stack-frame: stack-frame);
	$transaction-types[transaction-id] := transaction-type;
	// We'll use the transaction-id to figure out where to put the output
	section.%transaction-id := transaction-id
      end
    end
  end
end method interactor-do-execute-code;

define method interactor-receive-values
    (pane :: <dylan-interactor>, values :: <sequence>, #key transaction-id) => ()
  let project = frame-current-project(sheet-frame(pane));
  let type    = element($transaction-types, transaction-id, default: #f);
  select (type)
    #"describe" =>
      call-in-frame(sheet-frame(pane),
		    method ()
		      present-interactor-values
			(pane, values, transaction-id: transaction-id,
			 describe-function: interactor-do-show-contents)
		    end);
    otherwise =>
      // Add the source record to the buffer for this project
      let source-record
	= transaction-id-source-record(project, transaction-id);
      when (source-record)
	add-interactive-source-section(project, source-record)
      end;
      call-in-frame(sheet-frame(pane),
		    present-interactor-values,
		    pane, values, transaction-id: transaction-id);
  end;
  remove-key!($transaction-types, transaction-id)
end method interactor-receive-values;

define method interactor-receive-warnings
    (pane :: <dylan-interactor>, warnings :: <sequence>,
     #key transaction-id) => ()
  call-in-frame(sheet-frame(pane),
                present-interactor-warnings,
                pane, warnings, transaction-id: transaction-id)
end method interactor-receive-warnings;

//---*** What do we do if the evaluation resulted in an error?
define method present-interactor-values
    (pane :: <dylan-interactor>, name-value-pairs :: <sequence>,
     #key transaction-id, describe-function) => ()
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  let module  = frame-current-module(frame);
  with-editor-state-bound (buffer = pane)
    let section = find-section-for-transaction-id(buffer, transaction-id);
    when (section & section-output-line(section))
      let line   = section-output-line(section);
      let stream = make(<interval-stream>,
			interval: buffer,
			direction: #"output");
      // Delete the "Waiting..." message
      delete!(make-interval(line-start(line), line-end(line), in-order?: #t));
      stream-position(stream) := line-end(line);
      if (empty?(name-value-pairs))
        format(stream, "No values\n")
      else
        for (name-value-pair in name-value-pairs)
	  let name-label  = head(name-value-pair);
	  let value       = tail(name-value-pair);
	  let value-label
	    = print-environment-object-to-string
	        (project, value, namespace: module);
	  // Bind 'line' now before we insert a '\n'...
	  let line = bp-line(stream-position(stream));
	  format(stream, "%4s = %s\n", name-label, value-label);
	  // Set the contents properties after we've filled in the line
	  let property = list(#"object", value);
	  line-properties(line) := concatenate!(line-properties(line), property)
	end;
	when (describe-function)
	  let first-object = tail(name-value-pairs[0]);
	  let subset = interactor-make-subset(pane, first-object);
	  describe-function(pane, stream, subset)
	end
      end;
      queue-redisplay(pane, $display-text, centering: 1);
      redisplay-window(pane);
      // Ensure the output is read-only
      //--- Do this after redisplay because it messes up the modification ticks
      let node = section-node(section, buffer: buffer);
      interval-read-only?(node) := #t
    end
  end
end method present-interactor-values;

define method present-interactor-warnings
    (pane :: <dylan-interactor>, warning-objects :: <sequence>,
     #key transaction-id) => ()
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  let module  = frame-current-module(frame);
  with-editor-state-bound (buffer = pane)
    let section = find-section-for-transaction-id(buffer, transaction-id);
    when (section & section-output-line(section))
      let line   = section-output-line(section);
      let stream = make(<interval-stream>,
			interval: buffer,
			direction: #"output");
      // Insert the warnings before the "Waiting..." line
      stream-position(stream) := line-start(line);
      for (warning-object in warning-objects)
        let formatted-warning
	  = compiler-warning-full-message(project, warning-object);
	  //---*** Use this as the formatted message
	  /* = environment-object-display-name(project,
					       warning-object,
					       #f,	// no module context
					       full-message?: #t); */
	format(stream, "%s\n", formatted-warning);
	// Keep 'section-output-line' pointing to the correct line
	section-output-line(section) := bp-line(stream-position(stream))
      end;
      queue-redisplay(pane, $display-text, centering: 1);
      redisplay-window(pane)
    end
  end
end method present-interactor-warnings;

// Try to locate the section in this buffer for this transaction-id.
// Note that we can get transaction-ids from 'do-compile-to-core'
// that won't be found here; we just ignore them.
define function find-section-for-transaction-id
    (buffer :: <basic-buffer>, transaction-id)
 => (section :: false-or(<dylanworks-shell-section>))
  block (return)
    for (node = buffer-end-node(buffer) then node-previous(node),
	 until: ~node)
      let section = node-section(node);
      when (instance?(section, <dylanworks-shell-section>)
	    & section.%transaction-id = transaction-id)
	return(section)
      end
    end;
    #f
  end
end function find-section-for-transaction-id;

define constant $command-prefix-character = ':';
define constant $break-function-command   = "break";
define constant $describe-command         = "describe";
define constant $show-contents-command    = "show-contents";

define method interactor-process-command
    (pane :: <dylan-interactor>, text :: <string>, bp :: <basic-bp>) => ()
  let server  = pane.interactor-command-line-server;
  let text    = strip-interactor-command(text);
  let stream  = server.server-output-stream;
  let buffer  = pane.window-buffer;
  stream-position(stream) := buffer.interval-end-bp;
  //--- Nasty special case hack because the tokenizer isn't up to
  //--- handling the syntax of function names with embedded spaces
  let (command, arguments) = parse-command-name(text);
  select (command by \=)
    $break-function-command =>
      set-interactor-breakpoint(pane, arguments);
    $describe-command, $show-contents-command =>
      interactor-do-execute-code
	(pane, arguments, bp, transaction-type: #"describe");
    otherwise =>
      execute-command-line(server, text);
  end
end method interactor-process-command;

define function parse-command-name
    (text :: <string>)
 => (name :: false-or(<string>), remainder :: false-or(<string>))
  when (~empty?(text) & text[0] == $command-prefix-character)
    let pos = position(text, ' ');
    if (pos)
      values(copy-sequence(text, start: 1, end: pos),
	     copy-sequence(text, start: pos + 1))
    else
      values(text, #f)
    end
  end
end function parse-command-name;

define function strip-interactor-command
    (text :: <string>) => (stripped-text :: <string>)
  let _start = 0;
  let text-size = size(text);
  let _end = text-size;
  while (_start < _end & any-whitespace-char?(text[_start]))
    _start := _start + 1
  end;
  while (_end > _start & any-whitespace-char?(text[_end - 1]))
    _end := _end - 1;
  end;
  when (_end > _start & text[_end - 1] == ';')
    _end := _end - 1
  end;
  if (_start > 0 | _end < text-size)
    copy-sequence(text, start: _start, end: _end)
  else
    text
  end
end function strip-interactor-command;

define class <result-contents> (<object>)
  constant slot result-object :: <composite-object>,
    required-init-keyword: object:;
  constant slot result-names :: <sequence>,
    required-init-keyword: names:;
  constant slot result-values :: <sequence>,
    required-init-keyword: values:;
end class <result-contents>;

define class <result-subset> (<object>)
  constant slot result-contents :: <result-contents>,
    required-init-keyword: contents:;
  constant slot result-start :: <integer> = 0,
    init-keyword: start:;
end class <result-subset>;

define method interactor-show-contents
    (pane :: <dylan-interactor>, object :: <application-object>,
     #key count) => ()
  ignore(count);
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  let module  = frame-current-module(frame);
  let class   = application-object-class(project, object);
  let object-name
    = print-environment-object-to-string(project, object, namespace: module);
  let class-name
    = class & environment-object-display-name(project, class, module);
  do-with-interactor-output
    (method (stream :: <stream>)
       if (class-name & size(class-name) > 0)
	 let char    = if (size(class-name) > 1 & class-name[0] == '<') class-name[1]
		       else class-name[0] end;
	 let article = if (member?(char, "aeiouAEIOU")) "an" else "a" end;
	 format(stream, "%s is %s %s", object-name, article, class-name)
       else
	 format(stream, "%s is an object of unknown type", object-name, class-name)
       end
     end method,
     pane,
     format-to-string("Contents of %s", object-name))
end method interactor-show-contents;

//--- A slightly grubby function, maybe we can provide this functionality
//--- in Deuce in some higher level way.
define method do-with-interactor-output
    (function :: <function>, pane :: <dylan-interactor>, prefix :: <string>) => ()
  with-editor-state-bound (buffer = pane)
    let stream = make(<interval-stream>,
		      interval: buffer,
		      direction: #"output");
    write-line(stream, prefix);
    local method processor (mode, buffer, section, #key window)
	    ignore(mode, buffer, section, window);
	    function(stream)
	  end method;
    let section = line-section(bp-line(stream-position(stream)));
    process-shell-input(buffer-major-mode(buffer), buffer, section,
			window: pane, processor: processor);
    //---*** Remove the input property until we have a real command processor...
    put-property!(line-properties(section-start-line(section)), #"input", #f);
    close(stream);
    redisplay-window(pane);
    // Ensure the output is read-only
    //--- Do this after redisplay because it messes up the modification ticks
    let node = section-node(section, buffer: buffer);
    interval-read-only?(node) := #t
  end
end method do-with-interactor-output;

define constant $maximum-values :: <integer> = 20;

define method interactor-show-contents
    (pane :: <dylan-interactor>, object :: <composite-object>,
     #key count) => ()
  let subset = interactor-make-subset(pane, object);
  interactor-show-contents(pane, subset, count: count)
end method interactor-show-contents;

define method interactor-make-subset
    (pane :: <dylan-interactor>, object :: <composite-object>)
 => (subset :: <result-subset>)
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  let (names, values) = composite-object-contents(project, object);
  let result = make(<result-contents>,
		    object: object, names: names, values: values);
  make(<result-subset>, contents: result)
end method interactor-make-subset;

define method interactor-make-subset
    (pane :: <dylan-interactor>, object :: <collection-object>)
 => (subset :: <result-subset>)
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  let names   = collection-keys(project, object);
  let values  = collection-elements(project, object);
  let result = make(<result-contents>,
		    object: object, names: names, values: values);
  make(<result-subset>, contents: result)
end method interactor-make-subset;

define method interactor-show-contents
    (pane :: <dylan-interactor>, subset :: <result-subset>,
     #key count) => ()
  do-with-interactor-output
    (method (stream :: <stream>)
       interactor-do-show-contents(pane, stream, subset, count: count, show-prefix?: #f)
     end method,
     pane,
     interactor-object-contents-prefix(pane, subset))
end method interactor-show-contents;

define method interactor-object-contents-prefix
    (pane :: <dylan-interactor>, subset :: <result-subset>)
 => (prefix :: <string>)
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  let module  = frame-current-module(frame);
  let result  = subset.result-contents;
  let object  = result.result-object;
  let _start  = subset.result-start;
  let object-name
    = print-environment-object-to-string(project, object, namespace: module);
  format-to-string("Contents of %s%s",
		   object-name,
		   if (_start > 0) format-to-string(" [from %d]", _start)
		   else "" end)
end method interactor-object-contents-prefix;

define method interactor-do-show-contents
    (pane :: <dylan-interactor>, stream :: <stream>, subset :: <result-subset>,
     #key count, show-prefix? = #t) => ()
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  with-application-transaction (project)
    let module  = frame-current-module(frame);
    let thread  = interactor-remote-thread(pane);
    let result  = subset.result-contents;
    let object  = result.result-object;
    let names   = result.result-names;
    let values  = result.result-values;
    let total   = size(names);
    let _start  = subset.result-start;
    let count   = count | $maximum-values;
    let _end    = min(_start + count, total);
    let class   = application-object-class(project, object);
    let object-name
      = print-environment-object-to-string(project, object, namespace: module);
    let class-name
      = class & environment-object-display-name(project, class, module);
    when (show-prefix?)
      write-line(stream, interactor-object-contents-prefix(pane, subset))
    end;
    if (class-name)
      format(stream, "%s is a %s", object-name, class-name)
    else
      format(stream, "%s is an object of unknown type", object-name, class-name)
    end;
    let label-size :: <integer> = 0;
    let labels :: <simple-object-vector> = make(<vector>, size: _end - _start);
    // Compute the name labels first, so that we can make
    // the output have nice columnar format
    for (i :: <integer> from _start below _end,
	 offset from 0)
      let name = names[i];
      let name-label
	= select (name by instance?)
	    <environment-object> =>
	      environment-object-display-name(project, name, module);
	    <string> =>
	      name;
	    otherwise =>
	      format-to-string("%=", name);
	  end;
      label-size := max(size(name-label), label-size);
      labels[offset]  := name-label
    end;
    let spaces :: <byte-string>
      = make(<byte-string>, size: label-size + 1, fill: ' ');
    for (i :: <integer> from _start below _end,
	 name-label in labels)
      let value = values[i];
      let history-name
	= add-application-object-to-thread-history(project, thread, value);
      let history-label			// history label is 7 characters wide...
	= if (history-name) format-to-string("%4s = ", history-name)
	  else "       " end;
      let value-label
	= print-environment-object-to-string(project, value, namespace: module);
      write-element(stream, '\n');
      write(stream, history-label);
      write(stream, name-label);
      write(stream, spaces, end: label-size - size(name-label) + 1);
      write(stream, ": ");
      write(stream, value-label);
      // Set the contents properties after we've filled in the line
      let line = bp-line(stream-position(stream));
      let property = list(#"object", value);
      line-properties(line) := concatenate!(line-properties(line), property)
    end;
    if (_end < total)
      format(stream, "\n... [%d more]", total - _end);
      let next-subset
	= make(<result-subset>,
	       contents: result,
	       start: _end);
      interactor-next-subset(pane) := next-subset;
      // Set the contents properties after we've filled in the line
      let line = bp-line(stream-position(stream));
      let property = list(#"object", next-subset);
      line-properties(line) := concatenate!(line-properties(line), property)
    else
      interactor-next-subset(pane) := #f
    end
  end
end method interactor-do-show-contents;


/// Clipboard handling

define method paste-object?
    (pane :: <dylan-interactor>, object) => (paste? :: <boolean>)
  ignore(object);
  let frame = sheet-frame(pane);
  dylan-clipboard-object-available?(frame, <application-object>)
    & begin
	let project = frame-current-project(frame);
	let value = dylan-clipboard-object(frame);
	instance?(value, <application-object>)		// avoid race conditions!
	  & environment-object-home-server?(project, value)
	  & environment-object-exists?(project, value)
	  & application-object-proxy(value) ~= #f
      end
end method paste-object?;

define method paste-object
    (pane :: <dylan-interactor>, object) => ()
  ignore(object);
  let frame = sheet-frame(pane);
  let value = dylan-clipboard-object(frame);
  when (paste-object?(pane, value))
    let project = frame-current-project(frame);
    let module  = frame-current-module(frame);
    let buffer  = frame-buffer(frame);
    let thread  = interactor-remote-thread(pane);
    with-editor-state-bound (buffer = pane)
      let history-name
	= add-application-object-to-thread-history(project, thread, value);
      let last-node = buffer-end-node(buffer);
      let prev-node = node-previous(last-node);
      let section   = prev-node & node-section(prev-node);
      when (section & section-output-line(section))
	let line   = section-end-line(section);
	let stream = make(<interval-stream>,
			  interval: buffer,
			  direction: #"output");
	stream-position(stream) := line-end(line);
	let value-label
	  = print-environment-object-to-string(project, value, namespace: module);
	format(stream, "\n");
	let line = bp-line(stream-position(stream));
	format(stream, "%4s = %s\n", history-name, value-label);
	let property = list(#"object", value);
	line-properties(line) := concatenate!(line-properties(line), property);
	queue-redisplay(pane, $display-text, centering: 1);
	redisplay-window(pane)
      end
    end
  end
end method paste-object;


/// Breakpoints

define method set-interactor-breakpoint
    (pane :: <dylan-interactor>, name :: <string>) => ()
  let frame   = sheet-frame(pane);
  let project = frame-current-project(frame);
  let module  = frame-current-module(frame);
  with-editor-state-bound (buffer = pane)
    let stream = make(<interval-stream>,
		      interval: buffer,
		      direction: #"output");
    let object = find-environment-object(project, name);
    let set?
      = when (object)
	  let old-breakpoints = as(<vector>, environment-object-breakpoints(project));
	  make(<breakpoint-object>, project: project, object: object);
	  let new-breakpoints = as(<vector>, environment-object-breakpoints(project));
	  let seen-new-breakpoint? = #f;
	  for (breakpoint in new-breakpoints)
	    unless (member?(breakpoint, old-breakpoints))
	      seen-new-breakpoint? := #t;
	      format(stream, "Set breakpoint on '%s'\n",
		     print-environment-object-to-string
		       (project, breakpoint, namespace: module))
	    end
	  end;
	  seen-new-breakpoint?
	end;
    unless (set?)
      format(stream, "Function '%s' not found\n", name)
    end;
    queue-redisplay(pane, $display-text, centering: 1);
    redisplay-window(pane)
  end
end method set-interactor-breakpoint;
