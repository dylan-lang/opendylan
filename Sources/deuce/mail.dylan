Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple mail composition and sending

define sealed class <mail-buffer>
    (<non-file-buffer-mixin>, <basic-buffer>)
end class <mail-buffer>;

define sealed domain make (singleton(<mail-buffer>));
define sealed domain initialize (<mail-buffer>);

define variable *mail-buffer-count* :: <integer> = 0;

define function make-mail-buffer
    (#rest buffer-initargs,
     #key name, editor = frame-editor(*editor-frame*),
	  to, subject, body,
     #all-keys)
 => (buffer :: <mail-buffer>)
  ignore(editor);
  unless (name)
    inc!(*mail-buffer-count*);
    name := format-to-string("Mail %d", *mail-buffer-count*)
  end;
  with-keywords-removed (buffer-initargs = buffer-initargs, #[name:, to:, subject:, body:])
    let buffer = apply(make, <mail-buffer>,
		       name: name,
		       major-mode: find-mode(<mail-mode>),
		       buffer-initargs);
    initialize-mail-buffer(buffer, to: to, subject: subject, body: body);
    buffer
  end
end function make-mail-buffer;

define sealed method initialize-mail-buffer
    (buffer :: <mail-buffer>, #key to, subject, body: body-text) => ()
  // Make the header section
  let header  = make-empty-section(section-class: <mail-header-section>);
  let to      = make(<text-line>, contents: format-to-string("To: %s", to | ""));
  let subject = make(<text-line>, contents: format-to-string("Subject: %s", subject | ""));
  line-section(to) := header;
  section-start-line(header) := to;
  section-end-line(header)   := to;
  add-line!(header, subject);
  // Make the body section
  let body    = make-empty-section(section-class: <mail-body-section>);
  let divider = make(<divider-line>);
  let stream  = make(<string-stream>, contents: body-text | "");
  read-section-contents-from-stream(body, stream);
  add-line!(body, divider, after: #"start");
  // Build the mail buffer, which consists of the header followed by the body
  let header-node = make-section-node(buffer, header);
  let body-node   = make-section-node(buffer, body);
  node-buffer(header-node)  := buffer;
  buffer-start-node(buffer) := header-node;
  buffer-end-node(buffer)   := header-node;
  add-node!(buffer, body-node);
end method initialize-mail-buffer;

define sealed method revert-buffer
    (buffer :: <mail-buffer>,
     #key buffer-filler :: false-or(<function>) = #f, major-mode)
 => (reverted? :: <boolean>)
  ignore(buffer-filler, major-mode);
  initialize-mail-buffer(buffer);
  #t
end method revert-buffer;

define sealed method buffer-initial-point
    (buffer :: <mail-buffer>, #key point :: false-or(<basic-bp>) = #f)
 => (bp :: false-or(<basic-bp>))
  point | line-end(bp-line(interval-start-bp(buffer)))
end method buffer-initial-point;

define sealed method buffer-modified?
    (buffer :: <mail-buffer>) => (modified? :: <boolean>)
  // If there's any data in the body of the mail, claim that it's modified
  let header  = buffer-start-node(buffer);
  let body    = header & node-next(header);
  let section = body & node-section(body);
  when (section)
    let nlines = count-lines(section);
    let line   = nlines > 1 & line-next(section-start-line(section));
    nlines > 2 | (line & line-length(line) > 0)
  end
end method buffer-modified?;


/// Parsing the contents of a mail buffer

define sealed method parse-mail-buffer
    (buffer :: <mail-buffer>)
 => (from :: false-or(<byte-string>),
     to :: false-or(<byte-string>),
     cc :: false-or(<byte-string>),
     subject :: false-or(<byte-string>), body :: false-or(<byte-string>),
     other-headers :: <sequence>)
  let header-node = buffer-start-node(buffer);
  let body-node   = header-node & node-next(header-node);
  let header      = header-node & node-section(header-node);
  let body        = body-node   & node-section(body-node);
  let from :: false-or(<byte-string>) = #f;
  let to   :: false-or(<byte-string>) = #f;
  let cc   :: false-or(<byte-string>) = #f;
  let subj :: false-or(<byte-string>) = #f;
  let body :: false-or(<byte-string>) = body & as(<byte-string>, body);
  let other-headers :: <stretchy-object-vector> = make(<stretchy-vector>);
  //--- I admit it, this is a pretty cheesy header parser
  when (header)
    local method looking-at? (contents :: <byte-string>, length :: <integer>,
			      string :: <byte-string>)
	    let _start = 0;
	    let _end   = min(size(string), length);
	    string-equal?(contents, string, start1: _start, end1: _end)
	  end method;
    for (line = section-start-line(header) then line-next(line),
	 until: ~line)
      let contents = line-contents(line);
      let length   = line-length(line);
      case
	looking-at?(contents, length, "From:") =>
	  from := trim-whitespace(copy-sequence(contents, start: 5));
	looking-at?(contents, length, "To:") =>
	  to   := trim-whitespace(copy-sequence(contents, start: 3));
	looking-at?(contents, length, "cc:") =>
	  cc   := trim-whitespace(copy-sequence(contents, start: 3));
	looking-at?(contents, length, "Subject:") =>
	  subj := trim-whitespace(copy-sequence(contents, start: 8));
	otherwise =>
	  let colon = position(contents, ':');
	  when (colon)
	    let name  = copy-sequence(contents, end: colon);
	    let key   = as(<symbol>, name);
	    let value = trim-whitespace(copy-sequence(contents, start: colon + 1));
	    add!(other-headers, vector(key, name, value))
	  end;
      end
    end
  end;
  values(from, to, cc, subj, body, other-headers)
end method parse-mail-buffer;


/// Mail mode

define sealed class <mail-mode> (<fundamental-mode>)
end class <mail-mode>;

define sealed method initialize-major-mode
    (mode :: <mail-mode>, #key command-set = mode-command-set(mode)) => ()
  next-method();
  let control = $control-key;
  let command-set = copy-command-set(command-set);
  mode-command-set(mode) := command-set;
  select (command-set-name(command-set))
    #"emacs" =>
      let command-table = control-C-command-table(command-set);
      add-commands!(command-table,
		    vector('s', control, mail-send),
		    vector('c', control, mail-send-and-exit),
		    vector('w', control, mail-signature));
    #"windows" =>
      #f;
    otherwise =>
      #[];
  end
end method initialize-major-mode;

define sealed method mode-name
    (mode :: <mail-mode>) => (name :: <byte-string>)
  "Mail"
end method mode-name;

begin
  gethash(*keyword->major-mode*,   #"mail") := <mail-mode>;
  gethash(*file-type->major-mode*, #"mail") := <mail-mode>
end;


/// Mail header section

define sealed class <mail-header-section> (<basic-section>)
end class <mail-header-section>;

define sealed domain make (singleton(<mail-header-section>));
define sealed domain initialize (<mail-header-section>);


/// Mail body section

define sealed class <mail-body-section> (<basic-section>)
end class <mail-body-section>;

define sealed domain make (singleton(<mail-body-section>));
define sealed domain initialize (<mail-body-section>);


/// Divider lines

// This is a subclass of <diagram-line> because it is meant to be part of the
// structure of the buffer.  If it were a subclass of <structural-diagram-line>,
// users could accidently delete the line, which we don't want.
define sealed class <divider-line> (<diagram-line>)
end class <divider-line>;

define sealed domain make (singleton(<divider-line>));
define sealed domain initialize (<divider-line>);

define method initialize (line :: <divider-line>, #key)
  next-method();
  line-read-only?(line) := #t
end method initialize;

define sealed method do-characters
    (function :: <function>, line :: <divider-line>,
     #key start: _start, end: _end, from-end?, skip-test) => ()
  ignore(_start, _end, from-end?, skip-test);
  #f
end method do-characters;

define sealed method dump-line
    (line :: <divider-line>, stream :: <stream>) => ()
  write-line(stream, "--text follows this line--");
end method dump-line;

define variable $divider-line-color = make-color(150, 150, 150);

define sealed method display-line
    (line :: <divider-line>, mode :: <major-mode>, window :: <basic-window>,
     x :: <integer>, y :: <integer>,
     #key start: _start :: <integer> = 0, end: _end, align-y = #"top") => ()
  ignore(_start, _end);
  let (width, height) = window-viewport-size(window);
  ignore(height);
  let (fw, height, baseline, fd) = font-metrics(window, window-default-font(window));
  ignore(fw, fd);
  when (align-y == #"baseline")
    dec!(y, baseline)
  end;
  let offset = 5;
  let line-y = y + floor/(height, 2);
  draw-line(window,
	    x + offset, line-y, x + width - offset, line-y,
	    thickness: 1, color: $divider-line-color);
end method display-line;

define sealed method line-size
    (line :: <divider-line>, mode :: <major-mode>, window :: <basic-window>,
     #key start: _start, end: _end)
 => (width :: <integer>, height :: <integer>, baseline :: <integer>)
  ignore(_start, _end);
  let (width, height) = window-viewport-size(window);
  ignore(height);
  let (fw, height, baseline, fd) = font-metrics(window, window-default-font(window));
  ignore(fw, fd);
  values(width, height, baseline)
end method line-size;

define sealed method line-for-display-only?
    (line :: <divider-line>) => (display-only? :: <boolean>)
  #t
end method line-for-display-only?;


/// Mail sending commands

define command send-mail (frame, #key to, subject, body)
    "Compose and send a mail message."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <mail-buffer>  = make-mail-buffer(to: to, subject: subject, body: body);
  select-buffer-in-appropriate-window(window, buffer);
  frame-last-command-type(frame) := #"mail"
end command send-mail;

define command mail-send (frame)
    "Send the current mail message."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  when (instance?(buffer, <mail-buffer>))
    let (from, to, cc, subject, body, other-headers)
      = parse-mail-buffer(buffer);
    let (success?, message)
      = do-send-mail(window, to, subject, body,
		     from: from, cc: cc, other-headers: other-headers);
    if (success?)
      display-message(window, "Mail sent")
    else
      warning-dialog(window, "Mail failed:\n%s", message | "No reason given");
      command-error("Mail failed")
    end
  end;
  frame-last-command-type(frame) := #"mail"
end command mail-send;

define command mail-send-and-exit (frame)
    "Send the current mail message and close the mail buffer."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  when (instance?(buffer, <mail-buffer>))
    mail-send(frame);
    if (fixed-frame-buffer?(editor-policy(frame-editor(frame))))
      // Kill the buffer and close the window
      kill-buffer(buffer)
    else
      // Or in Emacs mode, just "bury" this buffer at the bottom
      //--- Too bad 'select-buffer' doesn't know how to bury it!
      let prev-buffer = previously-selected-buffer(window, 1);
      when (prev-buffer)
	select-buffer-in-appropriate-window(window, prev-buffer)
      end
    end
  end;
  frame-last-command-type(frame) := #"mail"
end command mail-send-and-exit;

define command mail-signature (frame)
    "Insert a signature."
  //---*** Insert the signature
  frame-last-command-type(frame) := #"mail"
end command mail-signature;


// For specialization by back-ends
define open generic do-send-mail
    (window :: <window>, to :: <string>, subject :: <string>, body :: <string>,
     #key from :: false-or(<string>), cc :: false-or(<string>), other-headers)
 => (success? :: <boolean>, message :: false-or(<string>));

define method do-send-mail
    (window :: <window>, to :: <string>, subject :: <string>, body :: <string>,
     #key from :: false-or(<string>), cc :: false-or(<string>), other-headers)
 => (success? :: <boolean>, message :: false-or(<string>))
  values(#f, "Don't know how to send mail!")
end method do-send-mail;
