Module:       duim-deuce-internals
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM back-end for choosing from a menu

define sealed method deuce/choose-from-menu
    (window :: <deuce-pane>, items :: <sequence>,
     #key title, value, label-key, value-key,
	  width, height, multiple-sets? = #f)
 => (value :: false-or(<object>), success? :: <boolean>)
  let (value, success? :: <boolean>)
    = choose-from-menu(items,
		       title: title,
		       label-key: label-key, value-key: value-key,
		       value: value,
		       owner: sheet-frame(window),
		       width:  width,
		       height: height,
		       multiple-sets?: multiple-sets?);
  values(success? & value, success?)
end method deuce/choose-from-menu;

define sealed method deuce/choose-from-dialog
    (window :: <deuce-pane>, items :: <sequence>,
     #key title, value, label-key, value-key,
	  width, height, selection-mode = #"single")
 => (value :: false-or(<object>), success? :: <boolean>,
     width :: false-or(<integer>), height :: false-or(<integer>))
  let (value, success? :: <boolean>, width, height)
    = choose-from-dialog(items,
			 title: title,
			 label-key: label-key, value-key: value-key,
			 value: value,
			 owner: sheet-frame(window),
			 width:  width,
			 height: height,
			 selection-mode: selection-mode);
  values(success? & value, success?, width, height)
end method deuce/choose-from-dialog;


/// DUIM back-end for various dialogs

define sealed method information-dialog
    (window :: <deuce-pane>, format-string :: <string>, #rest format-args) => ()
  let frame = sheet-frame(window);
  notify-user(apply(format-to-string, format-string, format-args),
	      owner: frame, style: #"information")
end method information-dialog;

define sealed method warning-dialog
    (window :: <deuce-pane>, format-string :: <string>, #rest format-args) => ()
  let frame = sheet-frame(window);
  notify-user(apply(format-to-string, format-string, format-args),
	      owner: frame, style: #"warning")
end method warning-dialog;

define sealed method yes-or-no-dialog
    (window :: <deuce-pane>, format-string :: <string>, #rest format-args)
 => (result :: <boolean>)
  let frame = sheet-frame(window);
  let (value, exit-type)
    = notify-user(apply(format-to-string, format-string, format-args),
		  owner: frame, style: #"question", exit-style: #"yes-no");
  ignore(value);
  select (exit-type)
    #"yes"    => #t;
    #"no"     => #f;
  end
end method yes-or-no-dialog;

define sealed method yes-no-or-cancel-dialog
    (window :: <deuce-pane>, format-string :: <string>, #rest format-args)
 => (result :: type-union(<boolean>, singleton(#"cancel")))
  let frame = sheet-frame(window);
  let (value, exit-type)
    = notify-user(apply(format-to-string, format-string, format-args),
		  owner: frame, style: #"question", exit-style: #"yes-no-cancel");
  ignore(value);
  select (exit-type)
    #"yes"    => #t;
    #"no"     => #f;
    #"cancel" => #"cancel";
  end
end method yes-no-or-cancel-dialog;


define sealed method open-file-dialog
    (window :: <deuce-pane>, #key default, default-type)
 => (pathname :: false-or(<pathname>))
  let frame = sheet-frame(window);
  choose-file(owner: frame,
	      direction: #"input",
	      if-does-not-exist: #"ask",
	      default: default & as(<string>, default),
	      default-type: default-type)
end method open-file-dialog;

define sealed method new-file-dialog
    (window :: <deuce-pane>, #key default, default-type)
 => (pathname :: false-or(<pathname>))
  let frame = sheet-frame(window);
  choose-file(owner: frame,
	      direction: #"output",
	      if-exists: #"overwrite",		// Deuce will take care of this
	      default: default & as(<string>, default), 
	      default-type: default-type)
end method new-file-dialog;


// These can be tweaked by the user...
define variable $buffer-box-width  :: <integer> = 500;
define variable $buffer-box-height :: <integer> = 300;

define sealed method save-buffers-dialog
    (window :: <deuce-pane>,
     #key exit-label :: false-or(<string>), reason :: false-or(<string>),
	  buffers :: false-or(<sequence>), default-buffers :: false-or(<sequence>))
 => (buffers :: type-union(<sequence>, singleton(#f), singleton(#"cancel")),
     no-buffers? :: <boolean>)
  // Note that sheet-frame ~= window-frame for <deuce-gadget>
  let frame  = sheet-frame(window);
  let editor = frame-editor(window-frame(window));
  do-save-buffers-dialog(frame, editor,
			 exit-label: exit-label,
			 reason: reason,
			 buffers: buffers,
			 default-buffers: default-buffers)
end method save-buffers-dialog;

define sealed method do-save-buffers-dialog
    (frame :: <frame>, editor :: <editor>,
     #key exit-label :: false-or(<string>), reason :: false-or(<string>),
	  buffers :: false-or(<sequence>), default-buffers :: false-or(<sequence>))
 => (buffers :: type-union(<sequence>, singleton(#f), singleton(#"cancel")),
     no-buffers? :: <boolean>)
  // List all buffers but initially select only modified or non-file buffers.
  // Only show the dialog if there are files needing to be saved.
  //--- It would be nice to inform the user if either (1) an unmodified file
  //--- was deleted from the file system and offer to re-save it, or (2) an
  //--- unmodified file was changed on the file system
  // Keep the set of buffers sorted...
  editor-buffers(editor)
    := sort!(editor-buffers(editor),
	     test: method (b1, b2)
		     string-less?(buffer-name(b1), buffer-name(b2))
		   end);
  // Use buffers passed in (if any), else all modified non-anonymous buffers
  let all-buffers
    = buffers
      | choose(method (b) ~buffer-anonymous?(b) end,
	       editor-buffers(editor));
  let default-buffers
    = default-buffers
      | choose(method (b) buffer-modified?(b) & ~special-purpose-buffer?(b) end,
	       all-buffers);
  if (empty?(default-buffers))
    values(#f, #t)
  else
    let framem = frame-manager(frame);
    let save-label   = "&Save";
    let all-label    = "Save &All";
    let cancel-label = "Cancel";
    with-frame-manager (framem)
      let result = default-buffers;
      let reason-labels = reason & string-to-labels(reason);
      let reason-layout = reason-labels
			  & ~empty?(reason-labels)
			  & make(<column-layout>, children: reason-labels);
      let buffers-box
	= make(<list-box>,
	       items: all-buffers,
	       value: default-buffers,
	       label-key: buffer-name,
	       selection-mode: #"multiple",
	       activate-callback:
		 method (b)
		   result := gadget-value(b);
		   exit-dialog(b)
		 end method);
      let reason-and-buffers
	= if (reason-layout)
	    make(<column-layout>, 
		 y-spacing: 2,
		 children: vector(reason-layout, buffers-box));
	  else
	    buffers-box
	  end;
      let save-button
	= make(<push-button>,
	       label: save-label,
	       activate-callback:
		 method (b)
		   result := gadget-value(buffers-box);
		   exit-dialog(b)
		 end method);
      let all-button
	= make(<push-button>,
	       label: all-label,
	       activate-callback:
		 method (b)
		   // OK, so it doesn't really save them all,
		   // it just saves all of the modified buffers
		   result := default-buffers;
		   exit-dialog(b)
		 end method);
      let exit-button
	= make(<push-button>,
	       label: exit-label,
	       activate-callback:
		 method (b)
		   result := #f;
		   exit-dialog(b)
		 end method);
      let cancel-button
	= make(<push-button>,
	       label: cancel-label,
	       activate-callback:
		 method (b)
		   result := #"cancel";
		   exit-dialog(b)
		 end method);
      let layout
	= make(<column-layout>,
	       y-spacing: 2,
	       x-alignment: #"right",
	       children:
		 vector(reason-and-buffers,
			make(<row-layout>,
			     x-alignment: #"right",
			     x-spacing: 2, equalize-widths?: #t,
			     children:
			       if (exit-label)
				 vector(save-button, all-button, exit-button, cancel-button)
			       else
				 vector(save-button, all-button, cancel-button)
			       end)));
      let dialog = make(<dialog-frame>,
			title: "Save Files",
			layout: layout,
			input-focus: buffers-box,
			exit-buttons?: #f,	// we'll do our own exit buttons
			exit-button:   exit-button,
			cancel-button: cancel-button,
			mode: #"modal",
			owner: frame,
			// Be consistent with 'choose-buffer-dialog'
			width: max($buffer-box-width, 300),
			height: max($buffer-box-height, 200));
      frame-default-button(dialog) := save-button;
      let exit-status = start-dialog(dialog);
      when (exit-status)
	let (width, height) = frame-size(dialog);
	$buffer-box-width  := width;
	$buffer-box-height := height;
      end;
      values(exit-status & result, #f)
    end
  end
end method do-save-buffers-dialog;

// Return a sequence of <label>s, one per line in the string
define function string-to-labels
    (string :: <string>) => (labels :: false-or(<stretchy-object-vector>))
  let string-stream = make(<string-stream>, contents: string);
  let lines :: <stretchy-object-vector> = make(<stretchy-vector>);
  for (line = read-line(string-stream, on-end-of-stream: #f)
	 then read-line(string-stream, on-end-of-stream: #f),
       until: ~line)
    add!(lines, make(<label>, label: line))
  end;
  lines
end function string-to-labels;


define sealed method choose-buffer-dialog
    (window :: <deuce-pane>,
     #key title = "Choose Document", ok-label :: <string> = "OK",
	  buffer :: false-or(<basic-buffer>) = #f, buffers = #f)
 => (buffer :: false-or(<basic-buffer>))
  do-choose-buffers-dialog(window,
			   title: title,
			   ok-label: ok-label, all-label: #f,
			   buffers: buffers, buffer: buffer,
			   selection-mode: #"single")
end method choose-buffer-dialog;

define sealed method choose-buffers-dialog
    (window :: <deuce-pane>,
     #key title = "Choose Documents",
	  ok-label :: <string> = "OK", all-label :: false-or(<string>) = #f,
	  buffer :: false-or(<basic-buffer>) = #f, buffers = #f)
 => (buffers :: false-or(<sequence>))
  do-choose-buffers-dialog(window,
			   title: title,
			   ok-label: ok-label, all-label: all-label,
			   buffers: buffers, buffer: buffer,
			   selection-mode: #"multiple")
end method choose-buffers-dialog;

define sealed method do-choose-buffers-dialog
    (window :: <deuce-pane>,
     #key title = "Choose Document",
	  ok-label :: <string> = "OK", all-label :: false-or(<string>) = #f,
	  buffer :: false-or(<basic-buffer>) = #f, buffers = #f,
	  selection-mode = #"single")
 => (result :: false-or(type-union(<buffer>, <sequence>)))
  let frame  = sheet-frame(window);		// ~= window-frame for <deuce-gadget>
  let framem = frame-manager(frame);
  let buffer = buffer | window-buffer(window);
  let editor = frame-editor(window-frame(window));
  // Keep the set of buffers sorted...
  editor-buffers(editor)
    := sort!(editor-buffers(editor),
	     test: method (b1, b2)
		     string-less?(buffer-name(b1), buffer-name(b2))
		   end);
  let buffers = buffers | choose(complement(buffer-anonymous?), editor-buffers(editor));
  let default = if (selection-mode == #"single") buffer else vector(buffer) end;
  with-frame-manager (framem)
    let result = default;
    let buffers-box
      = make(<list-box>,
	     items: buffers,
	     value: default,
	     label-key: buffer-name,
	     selection-mode: selection-mode,
	     value-changed-callback:
	       method (b)
		 result := gadget-value(b)
	       end method,
	     activate-callback:
	       method (b)
		 result := gadget-value(b);
		 exit-dialog(b)
	       end method);
    let ok-button
      = make(<push-button>,
	     label: ok-label,
	     activate-callback:
	       method (b)
		 result := gadget-value(buffers-box);
		 exit-dialog(b)
	       end method);
    let all-button
      = make(<push-button>,
	     label: all-label,
	     activate-callback:
	       method (b)
		 result := buffers;
		 exit-dialog(b)
	       end method);
    let cancel-button
      = make(<push-button>,
	     label: "Cancel",
	     activate-callback:
	       method (b)
		 result := #f;
		 exit-dialog(b)
	       end method);
    let layout
      = make(<column-layout>,
	     y-spacing: 2,
	     x-alignment: #"right",
	     children:
	       vector(buffers-box,
		      make(<row-layout>,
			   x-alignment: #"right",
			   x-spacing: 2, equalize-widths?: #t,
			   children:
			     if (all-label)
			       vector(ok-button, all-button, cancel-button)
			     else
			       vector(ok-button, cancel-button)
			     end)));
    let dialog = make(<dialog-frame>,
		      title: title,
		      layout: layout,
		      input-focus: buffers-box,
		      exit-buttons?: #f,	// we'll do our own exit buttons
		      exit-button:   ok-button,
		      cancel-button: cancel-button,
		      mode: #"modal",
		      owner: frame,
		      // Be consistent with 'save-buffers-dialog'
		      width: max($buffer-box-width, 300),
                      height: max($buffer-box-height, 200));
    frame-default-button(dialog) := ok-button;
    let exit-status = start-dialog(dialog);
    when (exit-status)
      let (width, height) = frame-size(dialog);
      $buffer-box-width  := width;
      $buffer-box-height := height;
    end;
    exit-status & result
  end
end method do-choose-buffers-dialog;


// This can be tweaked by the user...
define variable $choose-string-field-width :: <integer> = 300;

define sealed method choose-string-dialog
    (window :: <deuce-pane>,
     #key default = "", prompt = "String:", title = "Choose String")
 => (string :: false-or(<string>))
  let framem = frame-manager(window);
  with-frame-manager (framem)
    let text-field
      = make(<text-field>,
             min-width: $choose-string-field-width,
	     value: default,
	     activate-callback: exit-dialog);
    let layout
      = make(<table-layout>,
	     x-spacing: 8, y-spacing: 2,
	     contents: vector(vector(make(<label>, label: prompt),
				     text-field)));
    let dialog = make(<dialog-frame>,
		      title: title,
		      layout: layout,
		      input-focus: text-field,
		      mode: #"modal",
		      owner: sheet-frame(window));
    text-selection(text-field) := #t;
    let exit-status = start-dialog(dialog);
    when (exit-status)
      let (width, height) = sheet-size(text-field);
      ignore(height);
      $choose-string-field-width := width
    end;
    exit-status & gadget-value(text-field)
  end
end method choose-string-dialog;

define sealed method new-buffer-dialog
    (window :: <deuce-pane>,
     #key title = "New Buffer")
 => (buffer-name :: false-or(<string>))
  choose-string-dialog(window, title: title, prompt: "Buffer name:")
end method new-buffer-dialog;

define sealed method edit-definition-dialog
    (window :: <deuce-pane>, name :: <string>,
     #key title = "Edit Definition")
 => (definition)
  choose-string-dialog(window, title: title, prompt: "Definition:", default: name)
end method edit-definition-dialog;


define sealed method hack-matching-lines-dialog
    (window :: <deuce-pane>)
 => (string :: false-or(<string>), operation)
  let framem = frame-manager(window);
  with-frame-manager (framem)
    let text-field
      = make(<text-field>,
             min-width: $choose-string-field-width,
	     value: "",
	     activate-callback: exit-dialog);
    let radio-box
      = make(<radio-box>,
	     value: #"show-matching",
	     items: #[#[#"show-matching",     "Show &matching lines"],
                      #[#"show-non-matching", "Show &non-matching lines"]],
             value-key: first,
             label-key: second,
	     orientation: #"vertical");
    let layout
      = make(<table-layout>,
	     x-spacing: 8, y-spacing: 2,
	     contents: vector(vector(make(<label>, label: "String:"),
				     text-field),
                              vector(make(<label>, label: "Operation:"),
				     radio-box)));
    let dialog = make(<dialog-frame>,
		      title: "Do Matching Lines",
		      layout: layout,
		      input-focus: text-field,
		      mode: #"modal",
		      owner: sheet-frame(window));
    text-selection(text-field) := #t;
    let exit-status = start-dialog(dialog);
    when (exit-status)
      let (width, height) = sheet-size(text-field);
      ignore(height);
      $choose-string-field-width := width
    end;
    if (exit-status)
      values(gadget-value(text-field), gadget-value(radio-box))
    else
      values(#f, #f)
    end
  end
end method hack-matching-lines-dialog;


define sealed method goto-position-dialog
    (window :: <deuce-pane>, what :: <goto-target-type>)
 => (number :: false-or(<integer>), what :: false-or(<goto-target-type>))
  let framem = frame-manager(window);
  with-frame-manager (framem)
    let number-label
      = make(<label>, label: "&Number:");
    let number-field
      = make(<text-field>,
	     value-type: <integer>,
	     value-changing-callback:
	       method (gadget)
		 let dialog = sheet-frame(gadget);
		 dialog-exit-enabled?(dialog)
		   := gadget-value(gadget) & gadget-value(gadget) > 0
	       end method);
    local method focus-on-number-field (sheet)
	    frame-input-focus(sheet-frame(sheet)) := number-field
	  end method;
    let what-label
      = make(<label>, label: "Go to What:");
    let what-box
      = make(<radio-box>,
	     value: what,
	     items: #[#"line", #"character"],
	     label-key: method (symbol)
			  let string = as(<string>, symbol);
			  string[0] := as-uppercase(string[0]);
			  concatenate("&", string)
			end method,
	     orientation: #"horizontal",
	     value-changed-callback: focus-on-number-field,
	     activate-callback:      focus-on-number-field);
    let layout
      = make(<table-layout>,
	     x-spacing: 8, y-spacing: 2,
	     contents: vector(vector(what-label, what-box),
			      vector(number-label, number-field)));
    let dialog = make(<dialog-frame>,
		      title: "Go To",
		      layout: layout,
		      input-focus: number-field,
		      mode: #"modal",
		      owner: sheet-frame(window));
    let exit-status = start-dialog(dialog);
    let number = gadget-value(number-field);
    if (exit-status & number)
      values(number, gadget-value(what-box))
    else
      values(#f, #f)
    end
  end
end method goto-position-dialog;


/// Search and Replace dialogs

// This can be tweaked by the user...
define variable $search-string-field-width :: <integer> = 250;

define sealed method string-search-dialog
    (window :: <deuce-pane>,
     #key string :: false-or(<byte-string>) = #f,
	  reverse? :: <boolean> = #f,
	  case-sensitive? :: <boolean> = #f,
	  whole-word? :: <boolean> = #f) => ()
  let frame  = window-frame(window);		// ~= sheet-frame for <deuce-gadget>
  let framem = frame-manager(window);
  let editor :: <basic-editor> = frame-editor(frame);
  with-frame-manager (framem)
    let search-label
      = make(<label>, label: "Fi&nd what:");
    let search-string
      = make(<text-field>,
	     min-width: $search-string-field-width,
             value: string | "",
	     value-changing-callback:
	       method (tf) 
		 editor-search-string(editor) := gadget-value(tf)
	       end method,
	     value-changed-callback:
	       method (tf) 
		 window-note-search-string(window, gadget-value(tf))
	       end method,
	     activate-callback:
	       method (tf)
		 ignore(tf);
		 with-editor-state-bound (window)
		   find-next-or-previous-string(frame);
		   redisplay-window(window)
	         end
	       end method);
    let reverse-button
      = make(<check-button>,
	     value: reverse?,
	     label: "Search &backwards",
	     value-changed-callback:
	       method (b)
		 editor-reverse-search?(editor) := gadget-value(b)
	       end method);
    let whole-word-button
      = make(<check-button>,
	     value: whole-word?,
	     label: "Match &whole word only",
	     value-changed-callback:
	       method (b)
		 editor-whole-word-search?(editor) := gadget-value(b)
	       end method);
    let case-button
      = make(<check-button>,
	     value: case-sensitive?,
	     label: "Match &case",
	     value-changed-callback:
	       method (b)
		 editor-case-sensitive-search?(editor) := gadget-value(b)
	       end method);
    let next-button
      = make(<push-button>,
	     label: "&Find Next",
	     activate-callback:
	       method (b)
		 ignore(b);
		 with-editor-state-bound (window)
		   find-next-or-previous-string(frame);
		   redisplay-window(window)
		 end
	       end method);
    let cancel-button
      = make(<push-button>,
	     label: "Cancel",
	     activate-callback:
	       method (b)
		 window-note-search-string(window, #f);
		 cancel-dialog(b)
	       end method);
    let replace-button
      = make(<push-button>,
	     label: "&Replace...",
	     activate-callback:
	       method (b)
		 // Hide this dialog, start the next, then exit this.
		 let dialog = sheet-frame(b);
		 frame-mapped?(dialog) := #f;
		 string-replace-dialog
		   (window,
		    string:   editor-search-string(editor),
		    reverse?: editor-reverse-search?(editor),
		    whole-word?:     editor-whole-word-search?(editor),
		    case-sensitive?: editor-case-sensitive-search?(editor));
		 exit-dialog(dialog)
	       end method);
    let layout
      = make(<row-layout>,
	     x-spacing: 8,
	     children: vector(make(<table-layout>,
				   x-spacing: 8, y-spacing: 2,
				   x-alignment: #[#"right", #"left"],
				   y-alignment: #"center",
				   contents: vector(vector(search-label, search-string),
						    vector(#f, reverse-button),
						    vector(#f, whole-word-button),
						    vector(#f, case-button))),
			      make(<column-layout>,
				   y-spacing: 2,
				   equalize-widths?: #t,
				   children: vector(next-button,
						    replace-button,
						    cancel-button))));
    let dialog = make(<dialog-frame>,
		      title: "Find",
		      layout: layout,
		      input-focus:   search-string,
		      exit-buttons?: #f,	// we'll do our own exit buttons
		      cancel-button: cancel-button,
		      mode: #"modeless",
		      owner: sheet-frame(window));
    text-selection(search-string) := #t;
    frame-default-button(dialog)  := next-button;
    //--- The "nested" modeless dialog disappears on search failures in CAPI
    start-dialog(dialog);
    let (width, height) = sheet-size(search-string);
    ignore(height);
    $search-string-field-width := width;
    // If the search string is empty, set it back to #f
    when (editor-search-string(editor) & empty?(editor-search-string(editor)))
      editor-search-string(editor) := #f;
      window-note-search-string(window, #f)
    end
  end
end method string-search-dialog;

define sealed method string-replace-dialog
    (window :: <deuce-pane>,
     #key string  :: false-or(<byte-string>) = #f,
	  replace :: false-or(<byte-string>) = #f,
	  reverse? :: <boolean> = #f,
	  case-sensitive? :: <boolean> = #f,
	  whole-word? :: <boolean> = #f) => ()
  local method replace-or-find (frame :: <editor-state-mixin>)
	  if (frame-search-string-found?(frame) == frame-buffer(frame))
	    replace-next-or-previous-string(frame);
	    find-next-or-previous-string(frame)
	  else
	    // If the string hasn't been found yet, just find it, so that
	    // we don't unexpectedly replace the first occurrence
	    find-next-or-previous-string(frame)
	  end
	end method;
  let frame  = window-frame(window);		// ~= sheet-frame for <deuce-gadget>
  let framem = frame-manager(window);
  let editor :: <basic-editor> = frame-editor(frame);
  with-frame-manager (framem)
    let search-label
      = make(<label>, label: "Fi&nd what:");
    let search-string
      = make(<text-field>,
	     min-width: $search-string-field-width,
             value: string | "",
	     value-changing-callback:
	       method (tf)
		 editor-search-string(editor) := gadget-value(tf)
	       end method,
	     value-changed-callback:
	       method (tf) 
		 window-note-search-string(window, gadget-value(tf))
	       end method,
	     activate-callback:
	       method (tf)
		 ignore(tf);
		 with-editor-state-bound (window)
		   find-next-or-previous-string(frame);
		   redisplay-window(window)
		 end
	       end method);
    let replace-label
      = make(<label>, label: "Re&place with:");
    let replace-string
      = make(<text-field>,
	     min-width: $search-string-field-width,
             value: replace | "",
	     value-changing-callback:
	       method (tf)
		 editor-replace-string(editor) := gadget-value(tf)
	       end method,
	     activate-callback:
	       method (tf)
		 ignore(tf);
		 with-editor-state-bound (window)
		   replace-or-find(frame);
		   redisplay-window(window)
	         end
	       end method);
    let reverse-button
      = make(<check-button>,
	     value: reverse?,
	     label: "Search &backwards",
	     value-changed-callback:
	       method (b)
		 editor-reverse-search?(editor) := gadget-value(b)
	       end method);
    let whole-word-button
      = make(<check-button>,
	     value: whole-word?,
	     label: "Match &whole word only",
	     value-changed-callback:
	       method (b)
		 editor-whole-word-search?(editor) := gadget-value(b)
	       end method);
    let case-button
      = make(<check-button>,
	     value: case-sensitive?,
	     label: "Match &case",
	     value-changed-callback:
	       method (b)
		 editor-case-sensitive-search?(editor) := gadget-value(b)
	       end method);
    let next-button
      = make(<push-button>,
	     label: "&Find Next",
	     activate-callback:
	       method (b)
		 ignore(b);
		 with-editor-state-bound (window)
		   find-next-or-previous-string(frame);
		   redisplay-window(window)
		 end
	       end method);
    let cancel-button
      = make(<push-button>,
	     label: "Cancel",
	     activate-callback:
	       method (b)
		 window-note-search-string(window, #f);
		 cancel-dialog(b)
	       end method);
    let replace-button
      = make(<push-button>,
	     label: "&Replace",
	     activate-callback:
	       method (b)
		 ignore(b);
		 with-editor-state-bound (window)
		   replace-or-find(frame);
		   redisplay-window(window)
		 end
	       end method);
    let replace-all-button
      = make(<push-button>,
	     label: "Replace &All",
	     activate-callback:
	       method (b)
		 ignore(b);
		 with-editor-state-bound (window)
		   replace-next-or-previous-string(frame, replace-all?: #t);
		   redisplay-window(window)
	         end
	       end method);
    let layout
      = make(<row-layout>,
	     x-spacing: 8,
	     children: vector(make(<table-layout>,
				   x-spacing: 8, y-spacing: 2,
				   x-alignment: #[#"right", #"left"],
				   y-alignment: #"center",
				   contents: vector(vector(search-label, search-string),
						    vector(replace-label, replace-string),
						    vector(#f, reverse-button),
						    vector(#f, whole-word-button),
						    vector(#f, case-button))),
			      make(<column-layout>,
				   y-spacing: 2,
				   equalize-widths?: #t,
				   children: vector(next-button,
						    replace-button,
						    replace-all-button,
						    cancel-button))));
    let dialog = make(<dialog-frame>,
		      title: "Replace",
		      layout: layout,
		      input-focus:   search-string,
		      exit-buttons?: #f,	// we'll do our own exit buttons
		      cancel-button: cancel-button,
		      mode: #"modeless",
		      owner: sheet-frame(window));
    text-selection(search-string) := #t;
    frame-default-button(dialog)  := next-button;
    //--- The "nested" modeless dialog disappears on search failures in CAPI
    start-dialog(dialog);
    let (width, height) = sheet-size(search-string);
    ignore(height);
    $search-string-field-width := width;
    // If the search string is empty, set it back to #f
    when (editor-search-string(editor) & empty?(editor-search-string(editor)))
      editor-search-string(editor) := #f;
      window-note-search-string(window, #f)
    end;
    when (editor-replace-string(editor) & empty?(editor-replace-string(editor)))
      editor-replace-string(editor) := #f
    end
  end
end method string-replace-dialog;


/// Configuration dialog

define pane <input-policy-page> ()
  sealed slot %command-set-policy :: <command-set-policy> = #"emacs",
    init-keyword: command-set-policy:;
  sealed slot %alt-key-is-meta? :: <boolean> = #t,
    init-keyword: alt-key-is-meta?:;
  sealed slot %initial-click-moves-point? :: <boolean> = #f,
    init-keyword: initial-click-moves-point?:;
  pane %command-set-pane (pane)
    make(<option-box>,
	 value: pane.%command-set-policy,
	 value-changed-callback:
	   method (box) pane.%command-set-policy := gadget-value(box) end,
	 items: #[#"emacs", #"windows"],
	 label-key:
	   method (symbol) string-capitalize(as(<string>, symbol)) end);
  pane %alt-key-pane (pane)
    make(<check-button>,
	 value: pane.%alt-key-is-meta?,
	 label: "Use &Alt key like Emacs 'meta' key",
	 value-changed-callback:
	   method (button) pane.%alt-key-is-meta? := gadget-value(button) end);
  pane %initial-click-pane (pane)
    make(<check-button>,
	 value: pane.%initial-click-moves-point?,
	 label: "Reposition the cursor on the &first mouse click in a window",
	 value-changed-callback:
	   method (button) pane.%initial-click-moves-point? := gadget-value(button) end);
  layout (pane)
    vertically (spacing: 2)
      labelling ("&Keyboard layout:")
	pane.%command-set-pane
      end;
      pane.%alt-key-pane;
      pane.%initial-click-pane;
    end;
end pane <input-policy-page>;

define pane <editing-policy-page> ()
  sealed slot %clipboard-policy :: <boolean> = #t,
    init-keyword: clipboard-policy:;
  sealed slot %typing-replaces-selection? :: <boolean> = #f,
    init-keyword: typing-replaces-selection?:;
  sealed slot %unselected-copy-policy :: <unselected-copy-policy> = #"copy-line",
    init-keyword: unselected-copy-policy:;
  sealed slot %next-line-adds-newline? :: <boolean> = #t,
    init-keyword: next-line-adds-newline?:;
  sealed slot %undo-past-save-policy :: <boolean> = #f,
    init-keyword: undo-past-save-policy:;
  sealed slot %confirm-kill-buffer? :: <boolean> = #f,
    init-keyword: confirm-kill-buffer?:;
  sealed slot %new-file-buffer? :: <boolean> = #t,
    init-keyword: new-file-buffer?:;
  pane %clipboard-pane (pane)
    make(<check-button>,
	 value: pane.%clipboard-policy,
	 label: "Use the &clipboard as well as the 'kill ring' for 'Cut' and 'Copy'",
	 value-changed-callback:
	   method (button) pane.%clipboard-policy := gadget-value(button) end);
  pane %typing-pane (pane)
    make(<check-button>,
	 value: pane.%typing-replaces-selection?,
	 label: "&Replace the selection when a character is typed",
	 value-changed-callback:
	   method (button) pane.%typing-replaces-selection? := gadget-value(button) end);
  pane %unselected-pane (pane)
    make(<check-button>,
	 value: pane.%unselected-copy-policy == #"copy-line",
	 label: "Copy the current &line with 'Copy' if there is no selection",
	 value-changed-callback:
	   method (button)
	     pane.%unselected-copy-policy
	       := if (gadget-value(button)) #"copy-line" else #"nothing" end
	   end);
  pane %next-line-pane (pane)
    make(<check-button>,
	 value: pane.%next-line-adds-newline?,
	 label: "Add a blank line when the cursor &moves down at the end of a file",
	 value-changed-callback:
	   method (button) pane.%next-line-adds-newline? := gadget-value(button) end);
  pane %undo-pane (pane)
    make(<check-button>,
	 value: pane.%undo-past-save-policy,
	 label: "Maintain files' &undo/redo history after saving them",
	 value-changed-callback:
	   method (button) pane.%undo-past-save-policy := gadget-value(button) end);
  pane %confirm-pane (pane)
    make(<check-button>,
	 value: pane.%confirm-kill-buffer?,
	 label: "Offer a choice of file when cl&osing a file from the keyboard",
	 value-changed-callback:
	   method (button) pane.%confirm-kill-buffer? := gadget-value(button) end);
  pane %new-file-pane (pane)
    make(<check-button>,
	 value: pane.%new-file-buffer?,
	 label: "Ask for a file name when creating a &new file",
	 value-changed-callback:
	   method (button) pane.%new-file-buffer? := gadget-value(button) end);
  layout (pane)
    vertically (spacing: 2)
      pane.%clipboard-pane;
      pane.%typing-pane;
      pane.%unselected-pane;
      pane.%next-line-pane;
      pane.%undo-pane;
      pane.%confirm-pane;
      pane.%new-file-pane;
    end;
end pane <editing-policy-page>;

define function describe-font
    (font :: <font>)
 => (description :: <string>)
  let integer-size = font.font-point-size;
  let weight = font.font-weight;
  let slant = font.font-slant;
  concatenate(font.font-name | "(Unspecified font)",
	      if (instance?(weight, <symbol>) & weight ~== #"normal")
		concatenate(" ", as(<string>, weight))
	      else "" end,
	      if (instance?(slant, <symbol>)
		  & ~(slant == #"normal" | slant == #"roman"))
		concatenate(" ", as(<string>, slant))
	      else "" end,
	      ", ",
	      if (integer-size)
		integer-to-string(integer-size)
	      else
		"(Unspecified size)"
	      end)
end function describe-font;

define pane <display-policy-page> ()
  sealed slot %marking-policy :: <marking-policy> = #"end-of-line",
    init-keyword: marking-policy:;
  sealed slot %scrolling-moves-point? :: <boolean> = #t,
    init-keyword: scrolling-moves-point?:;
  sealed slot %fixed-frame-buffer? :: <boolean> = #f,
    init-keyword: fixed-frame-buffer?:;
  sealed slot %font = $default-font,
    init-keyword: default-font:;
  sealed slot %tab-stop-size :: limited(<integer>, min: 1, max: 10) = 8,
    init-keyword: tab-stop-size:;
  sealed slot %show-separators? :: <boolean> = #t,
    init-keyword: show-separators?:;
  sealed slot %show-path-in-title? :: <boolean> = #t,
    init-keyword: show-path-in-title?:;
  pane %marking-pane (pane)
    make(<option-box>,
	 value: pane.%marking-policy,
	 value-changed-callback:
	   method (box) pane.%marking-policy := gadget-value(box) end,
	 items: #[#[#"end-of-line", "the last character in each line."],
		  #[#"right-margin", "the right edge of the visible area."]],
	 value-key: first, label-key: second);
  pane %font-description-pane (pane)
    make(<text-field>,
	 value: describe-font(pane.%font),
	 read-only?: #t);
  pane %choose-font-pane (pane)
    make(<push-button>, label: "Choose &Font...",
	 activate-callback:
	   method (button)
	     let new-text-style
	       = choose-text-style
		   (owner: sheet-frame(pane),
		    default:
		      standardize-text-style
			(port(pane), make-duim-text-style(#f, pane.%font)),
		    fixed-width-only?: #t);
	     when (new-text-style)
	       pane.%font := make-font-from-duim-text-style(new-text-style);
	       gadget-value(pane.%font-description-pane)
	         := describe-font(pane.%font);
	     end;
	   end);
  //--- We don't support this yet, so don't advertise it
  /* pane %tab-pane (pane)
       make(<spin-box>,
	    value: pane.%tab-stop-size,
	    value-changed-callback:
	      method (box) pane.%tab-stop-size := gadget-value(box) end,
	    items: range(from: 1, to: 10)); */
  pane %scrolling-pane (pane)
    make(<check-button>,
	 value: pane.%scrolling-moves-point?,
	 label: "Keep the cursor within the visible area when &scrolling with the scroll bar",
	 value-changed-callback:
	   method (button) pane.%scrolling-moves-point? := gadget-value(button) end);
  pane %fixed-frame-pane (pane)
    make(<check-button>,
	 value: pane.%fixed-frame-buffer?,
	 label: "Associate each opened file with a separate &window",
	 value-changed-callback:
	   method (button) pane.%fixed-frame-buffer? := gadget-value(button) end);
  pane %show-separators-pane (pane)
    make(<check-button>,
	 value: pane.%show-separators?,
	 label: "Show separator &lines between Dylan definitions",
	 value-changed-callback:
	   method (button) pane.%show-separators? := gadget-value(button) end);
  pane %show-path-pane (pane)
    make(<check-button>,
	 value: pane.%show-path-in-title?,
	 label: "Show the &path with the file name in editor window titles",
	 value-changed-callback:
	   method (button) pane.%show-path-in-title? := gadget-value(button) end);
  layout (pane)
    vertically (spacing: 2)
      make(<table-layout>,
	   x-spacing: 2, y-spacing: 2,
	   y-alignment: #"center",
	   contents: vector(vector(make(<label>, label: "&Highlight selection to:"),
				   pane.%marking-pane),
			    vector(make(<label>, label: "Default font:"),
				   horizontally (x-spacing: 4)
				     pane.%font-description-pane;
				     pane.%choose-font-pane;
				   end) /*,
			    vector(make(<label>, label: "&Default tab width:"),
				   pane.%tab-pane) */));
      pane.%scrolling-pane;
      pane.%fixed-frame-pane;
      pane.%show-separators-pane;
      pane.%show-path-pane;
    end;
end pane <display-policy-page>;

define pane <search-policy-page> ()
  sealed slot %wrap-searches? :: <boolean> = #t,
    init-keyword: wrap-searches?:;
  sealed slot %use-isearch? :: <boolean> = #t,
    init-keyword: use-isearch?:;
  pane %wrap-pane (pane)
    make(<check-button>,
	 value: pane.%wrap-searches?,
	 label: "Restart &searching from the top/bottom of the file when a search fails",
	 value-changed-callback:
	   method (button) pane.%wrap-searches? := gadget-value(button) end);
  pane %isearch-pane (pane)
    make(<check-button>,
	 value: pane.%use-isearch?,
	 label: "Use Emacs-style &incremental search as the default search",
	 value-changed-callback:
	   method (button) pane.%use-isearch? := gadget-value(button) end);
  layout (pane)
    vertically (spacing: 2)
      pane.%wrap-pane;
      pane.%isearch-pane;
    end;
end pane <search-policy-page>;

//--- It would be so nice to put this _outside_ of the property pages...
define pane <restore-defaults-pane> ()
  sealed slot %input-page :: <input-policy-page>,
    required-init-keyword: input-page:;
  sealed slot %editing-page :: <editing-policy-page>,
    required-init-keyword: editing-page:;
  sealed slot %display-page :: <display-policy-page>,
    required-init-keyword: display-page:;
  sealed slot %search-page :: <search-policy-page>,
    required-init-keyword: search-page:;
  pane %windows-defaults-pane (pane)
    make(<push-button>, label: "Restore &Windows defaults",
         activate-callback:
           method (button)
             ignore(button);
             update-pages-from-policy($windows-editor-policy, pane)
           end);
  pane %emacs-defaults-pane (pane)
    make(<push-button>, label: "Restore &Emacs defaults",
         activate-callback:
           method (button)
             ignore(button);
             update-pages-from-policy($emacs-editor-policy, pane)
           end);
  layout (pane)
    vertically (y-spacing: 2, equalize-widths?: #t)
      make(<label>, label: "Select an editor style to reset all options:");
      pane.%windows-defaults-pane;
      pane.%emacs-defaults-pane;
    end;
end pane <restore-defaults-pane>;

define sealed method update-pages-from-policy
    (policy :: <editor-policy>, pane :: <restore-defaults-pane>) => ()
  let (input, editing, display, search)
    = values(pane.%input-page,   pane.%editing-page,
	     pane.%display-page, pane.%search-page);
  gadget-value(input.%command-set-pane, do-callback?: #t)
    := command-set-policy(policy);
  gadget-value(input.%alt-key-pane, do-callback?: #t)
    := alt-key-is-meta?(policy);
  gadget-value(input.%initial-click-pane, do-callback?: #t)
    := initial-click-moves-point?(policy);
  gadget-value(editing.%clipboard-pane, do-callback?: #t)
    := clipboard-policy(policy);
  gadget-value(editing.%typing-pane, do-callback?: #t)
    := typing-replaces-selection?(policy);
  gadget-value(editing.%unselected-pane, do-callback?: #t)
    := (unselected-copy-policy(policy) == #"copy-line");
  gadget-value(editing.%next-line-pane, do-callback?: #t)
    := next-line-adds-newline?(policy);
  gadget-value(editing.%undo-pane, do-callback?: #t)
    := undo-past-save-policy(policy);
  gadget-value(editing.%confirm-pane, do-callback?: #t)
    := confirm-kill-buffer?(policy);
  gadget-value(editing.%new-file-pane, do-callback?: #t)
    := new-file-buffer?(policy);
  gadget-value(display.%marking-pane, do-callback?: #t)
    := marking-policy(policy);
  gadget-value(display.%scrolling-pane, do-callback?: #t)
    := scrolling-moves-point?(policy);
  gadget-value(display.%font-description-pane, do-callback?: #t)
    := describe-font(default-font(policy));
  /* gadget-value(display.%tab-pane, do-callback?: #t)
       := tab-stop-size(policy); */
  gadget-value(display.%fixed-frame-pane, do-callback?: #t)
    := fixed-frame-buffer?(policy);
  gadget-value(display.%show-separators-pane, do-callback?: #t)
    := show-section-separators?(policy);
  gadget-value(display.%show-path-pane, do-callback?: #t)
    := show-path-in-title?(policy);
  gadget-value(search.%wrap-pane, do-callback?: #t)
    := wrap-searches?(policy);
  gadget-value(search.%isearch-pane, do-callback?: #t)
    := use-isearch?(policy);
end method update-pages-from-policy;

define constant $configuration-dialog-mode = #"modal";

define sealed method configuration-dialog
    (window :: <deuce-pane>) => (policy :: false-or(<editor-policy>))
  // Note that sheet-frame ~= window-frame for <deuce-gadget>
  let frame  = sheet-frame(window);
  let editor = frame-editor(window-frame(window));
  let framem = frame-manager(window);
  let policy = editor-policy(editor);
  with-frame-manager (framem)
    let input-policy-page
      = make(<input-policy-page>,
	     command-set-policy: command-set-policy(policy),
	     alt-key-is-meta?: alt-key-is-meta?(policy),
	     initial-click-moves-point?: initial-click-moves-point?(policy));
    let editing-policy-page
      = make(<editing-policy-page>,
	     clipboard-policy: clipboard-policy(policy),
	     typing-replaces-selection?: typing-replaces-selection?(policy),
	     unselected-copy-policy: unselected-copy-policy(policy),
	     next-line-adds-newline?: next-line-adds-newline?(policy),
	     undo-past-save-policy: undo-past-save-policy(policy),
	     confirm-kill-buffer?: confirm-kill-buffer?(policy),
	     new-file-buffer?: new-file-buffer?(policy));
    let display-policy-page
      = make(<display-policy-page>,
	     marking-policy: marking-policy(policy),
	     scrolling-moves-point?: scrolling-moves-point?(policy),
	     default-font: default-font(policy),
	     tab-stop-size: tab-stop-size(policy),
	     fixed-frame-buffer?: fixed-frame-buffer?(policy),
	     show-separators?: show-section-separators?(policy),
	     show-path-in-title?: show-path-in-title?(policy));
    let search-policy-page
      = make(<search-policy-page>,
	     wrap-searches?: wrap-searches?(policy),
	     use-isearch?:   use-isearch?(policy));
    let restore-defaults-pane
      = make(<restore-defaults-pane>,
             input-page:   input-policy-page,
             editing-page: editing-policy-page,
             display-page: display-policy-page,
             search-page:  search-policy-page);
    let pages = vector(make(<property-page>,
			    child: input-policy-page,     label: "&Input"),
		       make(<property-page>,
			    child: editing-policy-page,   label: "&Editing"),
		       make(<property-page>,
			    child: display-policy-page,   label: "&Display"),
		       make(<property-page>,
			    child: search-policy-page,    label: "&Search"),
		       make(<property-page>,
			    child: restore-defaults-pane, label: "&Restore"));
    local method make-policy (dialog)
	    ignore(dialog);
	    make(<editor-policy>,
		 command-set-policy: input-policy-page.%command-set-policy,
		 alt-key-is-meta?: input-policy-page.%alt-key-is-meta?,
		 initial-click-moves-point?: input-policy-page.%initial-click-moves-point?,
		 clipboard-policy: editing-policy-page.%clipboard-policy,
		 typing-replaces-selection?: editing-policy-page.%typing-replaces-selection?,
		 unselected-copy-policy: editing-policy-page.%unselected-copy-policy,
		 next-line-adds-newline?: editing-policy-page.%next-line-adds-newline?,
		 undo-past-save-policy: editing-policy-page.%undo-past-save-policy,
		 confirm-kill-buffer?: editing-policy-page.%confirm-kill-buffer?,
		 new-file-buffer?: editing-policy-page.%new-file-buffer?,
		 marking-policy: display-policy-page.%marking-policy,
		 scrolling-moves-point?: display-policy-page.%scrolling-moves-point?,
		 default-font: display-policy-page.%font,
		 tab-stop-size: display-policy-page.%tab-stop-size,
		 fixed-frame-buffer?: display-policy-page.%fixed-frame-buffer?,
		 show-section-separators?: display-policy-page.%show-separators?,
		 show-path-in-title?: display-policy-page.%show-path-in-title?,
		 wrap-searches?: search-policy-page.%wrap-searches?,
		 use-isearch?:   search-policy-page.%use-isearch?)
	  end method;
    //---*** How do we get a "Revert" button, now that we can support reversion?
    //--- We can't, as a <property-frame> maps onto a Win32 custom control,
    //--- which doesn't allow you to add things except inside the pages :-(
    let dialog = make(<property-frame>,
		      title: "Editor Options",
		      pages: pages,
		      min-width: 450, min-height: 250,
		      apply-callback:
			$configuration-dialog-mode == #"modeless"
			& method (dialog)
			    editor-policy(editor) := make-policy(dialog)
			  end method,
		      mode: $configuration-dialog-mode,
		      owner: sheet-frame(window));
    let exit-status = start-dialog(dialog);
    exit-status & make-policy(dialog)
  end
end method configuration-dialog;
