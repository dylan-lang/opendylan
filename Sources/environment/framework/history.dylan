Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// History mechanism

define open abstract class <frame-history-mixin> (<frame>)
  constant slot %history = make(<deque>);
  constant slot %forward-history = make(<deque>);
  slot %history-back-button = #f;
  slot %history-forward-button = #f;
end class <frame-history-mixin>;

// Some label strings

define constant $history-back-title        = "Back";
define constant $history-back-to-prefix    = "Back to ";
define constant $history-forward-title     = "Forward";
define constant $history-forward-to-prefix = "Forward to ";


define open generic note-frame-history-changed (frame :: <frame>) => ();

define open generic frame-select-previous-object
    (frame :: <frame-history-mixin>) => (object, success? :: <boolean>);

define open generic frame-select-next-object
    (frame :: <frame-history-mixin>) => (object, success? :: <boolean>);

define open generic frame-view-history 
    (frame :: <frame-history-mixin>) => ();

define method initialize (frame :: <frame-history-mixin>, #key) => ()
  next-method();
  update-frame-commands(frame);
end method initialize;

define method frame-history
    (frame :: <frame-history-mixin>)
 => (history :: <deque>)
  frame.%history
end method frame-history;

define method update-frame-commands (frame :: <frame>) => ()
  let previous-history? = frame-has-previous-history?(frame);
  let next-history?     = frame-has-next-history?(frame);
  command-enabled?(frame-select-previous-object, frame) := previous-history?;
  command-enabled?(frame-select-next-object, frame)     := next-history?;
  command-enabled?(frame-view-history, frame)
    := ~empty?(frame-history(frame));

  // Update documentation strings to include the destination name
  gadget-documentation(frame.%history-back-button)
    := if (previous-history?)
	 let previous-object
	   = frame-coerce-raw-object(frame, second(frame.%history));
         concatenate($history-back-to-prefix, 
		     frame-primary-object-name(frame, previous-object))
       else
         $history-back-title
       end if;
  gadget-documentation(frame.%history-forward-button)
    := if (next-history?)
	 let next-object
	   = frame-coerce-raw-object(frame, first(frame.%forward-history));
         concatenate($history-forward-to-prefix, 
		     frame-primary-object-name(frame, next-object))
       else
         $history-forward-title
       end if;
end method update-frame-commands;

define method note-frame-history-changed (frame :: <frame>) => ()
  #f
end method note-frame-history-changed;

define method frame-select-object (frame :: <frame>, object) => ()
  update-frame-commands(frame);
  #f
end method frame-select-object;

define method frame-add-to-history
    (frame :: <frame-history-mixin>, object) => ()
  let history = frame.%history;
  let coerced-object = frame-coerce-raw-object(frame, object);
  let coerced-history-object = ~empty?(history) & frame-coerce-raw-object(frame, history[0]);
  unless (coerced-object = coerced-history-object)
    push(history, object);
    size(frame.%forward-history) := 0;
    frame-select-object(frame, object);
    note-frame-history-changed(frame)
  end;
end method frame-add-to-history;

define method frame-remove-from-history
    (frame :: <frame-history-mixin>, object) => ()
  let history = frame.%history;
  if (size(history) > 1)
    frame-select-previous-object(frame);
    remove!(frame.%forward-history, object)
  else
    size(history) := 0;
    frame-select-next-object(frame);
    if (size(history) = 0)
      note-frame-last-object-closed(frame);
    end
  end;
  note-frame-history-changed(frame)
end method frame-remove-from-history;

define method note-frame-last-object-closed
     (frame :: <frame-history-mixin>) => ()
  update-frame-commands(frame)
end method note-frame-last-object-closed;

define method frame-has-previous-history? 
    (frame :: <frame-history-mixin>) => (previous? :: <boolean>)
  size(frame.%history) > 1
end method frame-has-previous-history?;

define method frame-has-next-history? 
    (frame :: <frame-history-mixin>) => (next? :: <boolean>)
  ~empty?(frame.%forward-history)
end method frame-has-next-history?;

define method frame-select-previous-object
    (frame :: <frame-history-mixin>) => (object, success? :: <boolean>)
  let history = frame.%history;
  if (frame-has-previous-history?(frame))
    let old-item = pop(history);
    let item = history[0];
    push(frame.%forward-history, old-item);
    frame-select-object(frame, item);
    note-frame-history-changed(frame);
    values(item, #t)
  end;
end method frame-select-previous-object;

define method frame-select-next-object
    (frame :: <frame-history-mixin>) => (object, success? :: <boolean>)
  let forward-history = frame.%forward-history;
  if (frame-has-next-history?(frame))
    let item = pop(forward-history);
    push(frame.%history, item);
    frame-select-object(frame, item);
    note-frame-history-changed(frame);
    values(item, #t)
  end;
end method frame-select-next-object;

define constant $frame-history-limit = 30;

define method frame-most-recent-objects 
    (frame :: <frame-history-mixin>, #key count = $frame-history-limit)
  let history 
    = remove-duplicates(concatenate(frame.%forward-history, frame.%history),
			test: method (raw1, raw2) => (equal? :: <boolean>)
				let coerced1 = frame-coerce-raw-object(frame, raw1);
				let coerced2 = frame-coerce-raw-object(frame, raw2);
				coerced1 = coerced2
			      end);
  if (count & size(history) > count)
    copy-sequence(history, end: count)
  else
    history
  end
end method frame-most-recent-objects;

define variable $view-history-dialog-width  :: false-or(<integer>) = #f;
define variable $view-history-dialog-height :: false-or(<integer>) = #f;

define method frame-view-history
    (frame :: <frame-history-mixin>) => ()
  let (object, success?, width, height)
    = choose-from-dialog
        (frame-most-recent-objects(frame, count: #f),
         label-key: method (raw-object)
		      let object = frame-coerce-raw-object(frame, raw-object);
		      frame-primary-object-name(frame, object)
		    end,
         default-item: frame-raw-primary-object(frame),
         title: "History",
         owner: frame,
	 width:  $view-history-dialog-width,
	 height: $view-history-dialog-height);
  if (success? & object)
    $view-history-dialog-width  := width;
    $view-history-dialog-height := height;
    frame-primary-object(frame) := object
  end
end method frame-view-history;


/// History command table

define variable $back-bitmap    :: <label-type> = "<";
define variable $forward-bitmap :: <label-type> = ">";

define constant $back-doc    = "Goes back one step.";
define constant $forward-doc = "Goes forward one step.";
define constant $history-doc = "Displays the history.";

define command-table *history-movement-command-table* (*global-command-table*)
  menu-item $history-back-title    = frame-select-previous-object,
    accelerator:   make-keyboard-gesture(#"left", #"alt"),
    documentation: $back-doc,
    image: $back-bitmap;
  menu-item $history-forward-title = frame-select-next-object,
    accelerator:   make-keyboard-gesture(#"right", #"alt"),
    documentation: $forward-doc,
    image: $forward-bitmap;
end command-table *history-movement-command-table*;

define command-table *history-command-table* (*global-command-table*)
  include *history-movement-command-table*;
  separator;
  menu-item "View History" = frame-view-history,
    documentation: $history-doc;
end command-table *history-command-table*;

define method make-history-tool-bar-buttons
    (frame :: <frame-history-mixin>)
 => (buttons :: <sequence>)
  frame.%history-back-button
    := make(<button>,
           label: $back-bitmap,
           documentation: $history-back-title,
           command: frame-select-previous-object,
           activate-callback: method (sheet)
                                let frame = sheet-frame(sheet);
                                frame-select-previous-object(frame)
                              end);
  frame.%history-forward-button
    := make(<button>, 
           label: $forward-bitmap,
           documentation: $history-forward-title,
           command: frame-select-next-object,
           activate-callback: method (sheet)
                                let frame = sheet-frame(sheet);
                                frame-select-next-object(frame)
                              end);
  vector(frame.%history-back-button, frame.%history-forward-button)
end method make-history-tool-bar-buttons;
