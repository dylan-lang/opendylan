Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Text editing gadgets

// Selection change notifications
define protocol <<selection-change-protocol>> (<<gadget-protocol>>)
  function execute-text-selection-changed-callback
    (gadget :: <abstract-gadget>, client, id) => ();
  function do-execute-text-selection-changed-callback
    (gadget :: <abstract-gadget>, client, id) => ();
  getter gadget-text-selection-changed-callback
    (gadget :: <abstract-gadget>) => (callback :: <callback-type>);
  setter gadget-text-selection-changed-callback-setter
    (callback :: <callback-type>, gadget :: <abstract-gadget>)
 => (callback :: <callback-type>);
end protocol <<selection-change-protocol>>;

define open abstract class <selection-change-mixin> (<abstract-gadget>)
  sealed slot gadget-text-selection-changed-callback :: <callback-type> = #f,
    init-keyword: text-selection-changed-callback:;
end class <selection-change-mixin>;

define method execute-text-selection-changed-callback
    (gadget :: <selection-change-mixin>, client, id) => ()
  ignore(client, id);
  let callback = gadget-text-selection-changed-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget)
  else
    do-execute-text-selection-changed-callback(gadget, client, id)
  end
end method execute-text-selection-changed-callback;

define method do-execute-text-selection-changed-callback
    (gadget :: <selection-change-mixin>, client, id) => ()
  ignore(client, id);
  #f
end method do-execute-text-selection-changed-callback;


// Protection notifications
define protocol <<protected-gadget-protocol>> (<<gadget-protocol>>)
  function execute-protection-callback
    (gadget :: <abstract-gadget>, client, id, range) => ();
  function do-execute-protection-callback
    (gadget :: <abstract-gadget>, client, id, range) => ();
  getter gadget-protection-callback
    (gadget :: <abstract-gadget>) => (callback :: <callback-type>);
  setter gadget-protection-callback-setter
    (callback :: <callback-type>, gadget :: <abstract-gadget>)
 => (callback :: <callback-type>);
end protocol <<protected-gadget-protocol>>;

define open abstract class <protected-gadget-mixin> (<abstract-gadget>)
  sealed slot gadget-protection-callback :: <callback-type> = #f,
    init-keyword: protection-callback:;
end class <protected-gadget-mixin>;

define method execute-protection-callback
    (gadget :: <protected-gadget-mixin>, client, id, range) => ()
  ignore(client, id);
  let callback = gadget-protection-callback(gadget);
  if (callback)
    execute-callback(gadget, callback, gadget, range)
  else
    do-execute-protection-callback(gadget, client, id, range)
  end
end method execute-protection-callback;

define method do-execute-protection-callback
    (gadget :: <protected-gadget-mixin>, client, id, range) => ()
  ignore(client, id, range);
  #f
end method do-execute-protection-callback;


/// Text gadget protocols and support classes

define open abstract class <text-gadget> (<value-gadget>) end;

define open abstract class <text-range> (<object>) end;

define protocol <<text-field-protocol>> (<<value-gadget-protocol>>)
  getter gadget-text
    (gadget :: <text-gadget>) => (text :: <string>);
  setter gadget-text-setter
    (text :: <string>, gadget :: <text-gadget>, #key do-callback?)
 => (text :: <string>);
  getter gadget-text-buffer
    (gadget :: <text-gadget>) => (text :: <string>);
  setter gadget-text-buffer-setter
    (text :: <string>, gadget :: <text-gadget>)
 => (text :: <string>);
  function note-gadget-text-changed
    (gadget :: <text-gadget>) => ();
  function gadget-text-parser
    (type :: <type>, text :: <string>) => (value);
  function gadget-value-printer
    (type :: <type>, value) => (text :: <string>);
  // Get and set the current selection
  function text-selection
    (gadget :: <text-gadget>) => (range :: type-union(<text-range>, one-of(#f)));
  function text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)), gadget :: <text-gadget>)
 => (range :: type-union(<text-range>, one-of(#t, #f)));
  // Return or replace the current selection.  Note that this works
  // on all sorts of sheets, not just text gadgets
  function selected-text
    (gadget :: <abstract-sheet>) => (string :: false-or(<string>));
  function selected-text-setter
    (string :: false-or(<string>), gadget :: <abstract-sheet>)
 => (string :: false-or(<string>));
  function text-field-modified?
    (gadget :: <text-gadget>) => (modified? :: <boolean>);
  function text-field-modified?-setter
    (modified? :: <boolean>, gadget :: <text-gadget>)
 => (modified? :: <boolean>);
  // These can be more efficient than using 'gadget-value'
  function text-field-size
    (gadget :: <text-gadget>) => (size :: <integer>);
  function text-field-text
    (gadget :: <text-gadget>, range :: <text-range>)
 => (string :: false-or(<string>));
  // Get and set the current caret position
  function text-caret-position
    (gadget :: <text-gadget>) => (index :: false-or(<integer>));
  function text-caret-position-setter
    (index :: false-or(<integer>), gadget :: <text-gadget>)
 => (index :: false-or(<integer>));
  // Mapping indices to caret positions, and vice-versa
  function character-position
    (gadget :: <text-gadget>, x, y) => (index :: <integer>);
  function position-character
    (gadget :: <text-gadget>, index :: <integer>) => (x, y);
end protocol <<text-field-protocol>>;


// Default methods for these are no-ops
define method selected-text
    (sheet :: <sheet>) => (string :: false-or(<string>))
  #f
end method selected-text;

define method selected-text-setter
    (string :: false-or(<string>), sheet :: <sheet>)
 => (string :: false-or(<string>))
  string
end method selected-text-setter;


define sealed class <simple-text-range> (<text-range>)
  sealed slot text-range-start :: <integer>,
    required-init-keyword: start:;
  sealed slot text-range-end :: <integer>,
    required-init-keyword: end:;
end class <simple-text-range>;

define sealed domain make (singleton(<simple-text-range>));
define sealed domain initialize (<simple-text-range>);

define sealed inline method make
    (class == <text-range>, #rest initargs, #key)
 => (range :: <simple-text-range>)
  dynamic-extent(initargs);
  apply(make, <simple-text-range>, initargs)
end method make;

define sealed class <active-text-range> (<simple-text-range>)
  sealed slot text-range-object = #f,
    init-keyword: object:;
end class <active-text-range>;

define sealed domain make (singleton(<active-text-range>));
define sealed domain initialize (<active-text-range>);


/// Text gadget state

define sealed class <text-gadget-state> (<gadget-state>)
  sealed constant slot %state-text,
    required-init-keyword: text:;
end class <text-gadget-state>;

define sealed domain make (singleton(<text-gadget-state>));
define sealed domain initialize (<text-gadget-state>);

define method gadget-state
    (gadget :: <text-gadget>) => (state :: <text-gadget-state>)
  make(<text-gadget-state>,
       text: gadget-text(gadget))
end method gadget-state;

define method gadget-state-setter
    (state :: <text-gadget-state>, gadget :: <text-gadget>)
 => (state :: <text-gadget-state>)
  gadget-text(gadget) := state.%state-text;
  state
end method gadget-state-setter;


/// Text gadget classes

// A single-line text editing field
// The callbacks are as follows:
//  - value-changing callback during "casual" typing
//  - value-changed callback when the change is "committed"
//  - activate callback when some sort of activation gesture is seen
// The "commit" and "activation" gestures are defined by the back-end
define open abstract class <text-field>
    (<bordered-gadget-mixin>,
     <action-gadget-mixin>,
     <changing-value-gadget-mixin>,
     <value-gadget-mixin>,
     <text-gadget>,
     <basic-gadget>)
  // We maintain the text buffer separately from 'gadget-text' in case
  // the internal representation is different from the external represenation,
  // as is the case with Windows multi-line text editors
  slot gadget-text-buffer :: <string> = "",
    init-keyword: text:;
  constant slot gadget-value-type :: <type> = <string>,
    init-keyword: value-type:;
  sealed slot text-field-maximum-size :: false-or(<integer>) = #f,
    init-keyword: maximum-size:;
end class <text-field>;

define method initialize
    (gadget :: <text-field>,
     #key x-alignment = #"left", case: text-case = #f, auto-scroll? = #f)
  next-method();
  let xa = select (x-alignment)
	     #"left"  => %x_alignment_left;
	     #"right" => %x_alignment_right;
	     #"center", #"centre" => %x_alignment_center;
	   end;
  let c = select (text-case)
	    #f       => %text_case_false;
	    #"lower" => %text_case_lower;
	    #"upper" => %text_case_upper;
	  end;
  let scroll = if (auto-scroll?) %auto_scroll else 0 end;
  gadget-flags(gadget)
    := logior(logand(gadget-flags(gadget), lognot(%x_alignment_mask)),
	      xa + c + scroll)
end method initialize;

define constant $text-field-cases :: <simple-object-vector>
    = #[#f, #"lower", #"upper"];

define sealed inline method text-field-case
    (gadget :: <text-field>) => (text-case)
  let index = ash(logand(gadget-flags(gadget), %text_case_mask),
		  -%text_case_shift);
  $text-field-cases[index]
end method text-field-case;

define sealed inline method text-field-auto-scroll?
    (gadget :: <text-field>) => (auto-scroll? :: <boolean>)
  logand(gadget-flags(gadget), %auto_scroll) = %auto_scroll
end method text-field-auto-scroll?;

define method viewport-fencepost? (sheet :: <text-field>) => (true? :: <boolean>)
  #t
end method viewport-fencepost?;


// Back-ends where 'gadget-text' and 'gadget-text-buffer' use different
// representations should specialize 'gadget-text' and 'gadget-text-setter'
define method gadget-text
    (gadget :: <text-gadget>) => (text :: <string>)
  gadget-text-buffer(gadget)
end method gadget-text;

define method gadget-text-setter
    (text :: <string>, gadget :: <text-gadget>, #key do-callback? = #f)
 => (text :: <string>)
  gadget-text-buffer(gadget) := text;
  when (do-callback?)
    execute-value-changed-callback(gadget, gadget-client(gadget), gadget-id(gadget))
  end;
  note-gadget-text-changed(gadget);
  note-gadget-value-changed(gadget);
  text
end method gadget-text-setter;

define method note-gadget-text-changed
    (gadget :: <text-gadget>) => ()
  #f
end method note-gadget-text-changed;


define method gadget-value
    (gadget :: <text-gadget>) => (value)
  gadget-text-parser(gadget-value-type(gadget), gadget-text(gadget))
end method gadget-value;

define method do-gadget-value-setter
    (gadget :: <text-gadget>, value) => ()
  let text = gadget-value-printer(gadget-value-type(gadget), value);
  unless (text = gadget-text(gadget))
    gadget-text(gadget) := text
  end
end method do-gadget-value-setter;


/// 'accept' and 'present', ha ha ha

define method gadget-text-parser
    (type :: subclass(<string>), text :: <string>) => (value :: <string>)
  text
end method gadget-text-parser;

define method gadget-value-printer
    (type :: subclass(<string>), value :: <string>) => (text :: <string>)
  value
end method gadget-value-printer;


define method gadget-text-parser
    (type :: subclass(<symbol>), text :: <string>) => (value :: <symbol>)
  as(<symbol>, text)
end method gadget-text-parser;

define method gadget-value-printer
    (type :: subclass(<symbol>), value :: <symbol>) => (text :: <string>)
  as(<string>, value)
end method gadget-value-printer;


define method gadget-text-parser
    (type :: subclass(<integer>), text :: <string>) => (value :: false-or(<integer>))
  block ()
    let (value, next) = string-to-integer(text);
    next = size(text) & value
  exception (<error>)
    #f
  end
end method gadget-text-parser;

define method gadget-value-printer
    (type :: subclass(<integer>), value :: false-or(<integer>)) => (text :: <string>)
  if (value)
    integer-to-string(value)
  else
    ""
  end
end method gadget-value-printer;


/*
define method gadget-text-parser
    (type :: subclass(<float>), text :: <string>) => (value :: false-or(<float>))
  block ()
    let (value, next) = string-to-float(text);
    next = size(text) & value
  exception (<error>)
    #f
  end
end method gadget-text-parser;

define method gadget-value-printer
    (type :: subclass(<float>), value :: false-or(<float>)) => (text :: <string>)
  if (value)
    float-to-string(value)
  else
    ""
  end
end method gadget-value-printer;
*/


/// Password fields

define open abstract class <password-field> (<text-field>)
end class <password-field>;


/// Multi-line text editors

define protocol <<text-editor-protocol>> (<<text-field-protocol>>)
  getter text-field-word-wrap?
    (gadget :: <text-gadget>) => (word-wrap? :: <boolean>);
  // Hacking lines
  function current-line
    (gadget :: <text-gadget>) => (line :: false-or(<integer>));
  function line-length
    (gadget :: <text-gadget>, line :: <integer>) => (length :: false-or(<integer>));
  function get-line
    (gadget :: <text-gadget>, line :: <integer>) => (line :: false-or(<string>));
  // Mapping indices to lines, and vice-versa
  function index-line
    (gadget :: <text-gadget>, index :: <integer>) => (line :: false-or(<integer>));
  function line-index
    (gadget :: <text-gadget>, line :: <integer>) => (index :: false-or(<integer>));
  // Protection
  function text-range-protected?
    (gadget :: <text-gadget>, range :: <text-range>) => (protected? :: <boolean>);
  function text-range-protected?-setter
    (protected? :: <boolean>, gadget :: <text-gadget>, range :: <text-range>)
 => (protected? :: <boolean>);
  // Searching
  function find-text (gadget :: <text-gadget>, string :: <string>)
 => (index :: false-or(<integer>));
end protocol <<text-editor-protocol>>;

// A multi-line text editing field
define open abstract class <text-editor>
    (<scrolling-gadget-mixin>, <text-field>)
  sealed constant slot gadget-lines   :: false-or(<integer>) = #f,	// i.e., bottomless
    init-keyword: lines:;
  sealed constant slot gadget-columns :: false-or(<integer>) = #f,
    init-keyword: columns:;
  sealed slot text-field-word-wrap? :: <boolean> = #t,
    init-keyword: word-wrap?:;
end class <text-editor>;


/// "Rich" text editors

define protocol <<rich-text-editor-protocol>> (<<text-editor-protocol>>)
  getter character-format
    (gadget :: <text-gadget>) => (sd :: false-or(<style-descriptor>));
  // Set the default character format, or set the format for the indicated range
  setter character-format-setter
    (sd :: false-or(<style-descriptor>), gadget :: <text-gadget>,
     #key range :: false-or(<text-range>))
 => (sd :: false-or(<style-descriptor>));
  getter paragraph-format
    (gadget :: <text-gadget>, #key range) => (paragraph-format);
  setter paragraph-format-setter
    (paragraph-format, gadget :: <text-gadget>, #key range) => (paragraph-format);
end protocol <<rich-text-editor-protocol>>;

define open abstract class <rich-text-editor> (<text-editor>)
  sealed slot %paragraph-format = #f,
    init-keyword: paragraph-format:;
  sealed slot %word-break-policy = #f,
    init-keyword: word-break-policy:;
  sealed slot %line-break-policy = #f,
    init-keyword: line-break-policy:
end class <rich-text-editor>;

define method paragraph-format
    (gadget :: <rich-text-editor>, #key range) => (paragraph-format)
  //---*** What to do about the range?
  gadget.%paragraph-format
end method paragraph-format;

define method paragraph-format-setter
    (paragraph-format, gadget :: <rich-text-editor>, #key range) => (paragraph-format)
  //---*** What to do about the range?
  gadget.%paragraph-format := paragraph-format
end method paragraph-format-setter;


/// Text changed events

define sealed class <text-changing-gadget-event> (<gadget-event>)
  sealed constant slot event-text :: <string>,
    required-init-keyword: text:;
end class <text-changing-gadget-event>;

define sealed domain make (singleton(<text-changing-gadget-event>));
define sealed domain initialize (<text-changing-gadget-event>);

define method handle-event
    (gadget :: <text-gadget>, event :: <text-changing-gadget-event>) => ()
  gadget-text-buffer(gadget) := event-text(event);
  execute-value-changing-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define function distribute-text-changing-callback
    (gadget :: <text-gadget>, text :: <string>) => ()
  distribute-event(port(gadget),
		   make(<text-changing-gadget-event>,
			gadget: gadget,
			text: text))
end function distribute-text-changing-callback;


define sealed class <text-changed-gadget-event> (<text-changing-gadget-event>)
end class <text-changed-gadget-event>;

define sealed domain make (singleton(<text-changed-gadget-event>));
define sealed domain initialize (<text-changed-gadget-event>);

define method handle-event
    (gadget :: <text-gadget>, event :: <text-changed-gadget-event>) => ()
  gadget-text-buffer(gadget) := event-text(event);
  execute-value-changed-callback(gadget, gadget-client(gadget), gadget-id(gadget))
end method handle-event;

define function distribute-text-changed-callback
    (gadget :: <text-gadget>, text :: <string>) => ()
  distribute-event(port(gadget),
		   make(<text-changed-gadget-event>,
			gadget: gadget,
			text: text))
end function distribute-text-changed-callback;
