Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Progress notes

// The current progress note
define thread variable *progress-note* = #f;

define open generic clear-progress-note
    (framem :: <abstract-frame-manager>, note) => ();
define open generic display-progress-note
    (framem :: <abstract-frame-manager>, note) => ();
define open generic raise-progress-note
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>)
 => (sheet :: false-or(<sheet>));
define open generic lower-progress-note
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>, sheet) => ();


define sealed class <progress-note> (<object>)
  sealed slot progress-note-sheet = #f,
    init-keyword: sheet:;
  sealed slot progress-note-frame = #f,
    init-keyword: frame:;
  sealed slot progress-note-label = #f,
    init-keyword: label:,
    setter: %label-setter;
  sealed slot %numerator = 0;
  sealed slot %denominator = 1;
end class <progress-note>;
    
define sealed domain make (singleton(<progress-note>));
define sealed domain initialize (<progress-note>);

define method progress-note-label-setter (label, note :: <progress-note>) => (label)
  note.%label := label;
  let frame = progress-note-frame(note);
  when (frame)
    display-progress-note(frame-manager(frame), note)
  end;
  label
end method progress-note-label-setter;


define macro noting-progress
  { noting-progress (?label:expression) ?:body end }
    => { begin
	   let noting-progress-body = method () ?body end;
	   do-noting-progress(#f, ?label, noting-progress-body)
	 end }
  { noting-progress (?sheet:expression, ?label:expression) ?:body end }
    => { begin
	   let noting-progress-body = method () ?body end;
	   do-noting-progress(?sheet, ?label, noting-progress-body)
	 end }
end macro noting-progress;

define method do-noting-progress
    (sheet :: <sheet>, label, continuation :: <function>,
     #key frame = sheet-frame(sheet), cursor)
 => (#rest values)
  let old-note = *progress-note*;
  let new-note = make(<progress-note>,
		      label: label,
		      sheet: sheet,
		      frame: frame);
  let pointer = port(sheet) & port-pointer(port(frame));
  let old-cursor = frame-cursor-override(frame);
  let framem = frame-manager(frame);
  local
    method initialize-progress
	() => ()
      when (cursor)
	frame-cursor-override(frame) := cursor
      end;
      display-progress-note(framem, new-note)
    end method initialize-progress,

    method finish-progress
	() => ()
      // If there was an old note, restore it, otherwise get rid
      // of the progress note display
      when (cursor)
	frame-cursor-override(frame) := old-cursor
      end;
      if (old-note)
	display-progress-note(framem, old-note);
      else
	lower-progress-note(framem, frame, sheet)
      end
    end method finish-progress;

  dynamic-bind (*progress-note* = new-note)
    block ()
      call-in-frame(frame, initialize-progress);
      continuation()
    cleanup
      call-in-frame(frame, finish-progress)
    end
  end
end method do-noting-progress;

define method do-noting-progress
    (sheet == #f, label, continuation :: <function>,
     #key frame = current-frame(), cursor)
 => (#rest values)
  let sheet = raise-progress-note(frame-manager(frame), frame);
  if (sheet)
    do-noting-progress(sheet, label, continuation,
                       frame: frame, cursor: cursor)
  else
    continuation()
  end
end method do-noting-progress;

define method do-noting-progress
    (frame :: <frame>, label, continuation :: <function>,
     #key frame: _frame = frame, cursor) => (#rest values)
  ignore(_frame);
  do-noting-progress(#f, label, continuation,
                     frame: frame, cursor: cursor)
end method do-noting-progress;


define method note-progress
    (numerator, denominator,
     #key note = *progress-note*, label, cursor) => ()
  when (note)
    let frame = progress-note-frame(note);
    let framem = frame-manager(frame);
    let pointer = port-pointer(port(frame));
    when (pointer & cursor)
      //--- This won't 'stick', should we use frame-override-cursor?
      pointer-cursor(pointer) := cursor
    end;
    when (label)
      note.%label := label
    end;
    note.%numerator := numerator;
    note.%denominator := denominator;
    call-in-frame(frame, display-progress-note, framem, note)
  end
end method note-progress;

define method note-progress-in-phases
    (numerator, denominator,
     #key note = *progress-note*, label, phase-number = 0, n-phases = 1) => ()
  when (note)
    note-progress(denominator * phase-number + numerator,
		  denominator * n-phases,
		  note: note, label: label)
  end
end method note-progress-in-phases;


/// Default implementation of progress notes, using the status bar

define method raise-progress-note
    (framem :: <basic-frame-manager>, frame :: <frame>)
 => (sheet :: false-or(<sheet>))
  frame-status-bar(frame)
end method raise-progress-note;

define method clear-progress-note
    (framem :: <basic-frame-manager>, note :: <progress-note>) => ()
  let sheet = progress-note-sheet(note);
  when (sheet)
    gadget-value(sheet) := #f
  end
end method clear-progress-note;

define method display-progress-note
    (framem :: <basic-frame-manager>, note :: <progress-note>) => ()
  let sheet = progress-note-sheet(note);
  let denominator = note.%denominator;
  when (denominator > 0)
    gadget-value-range(sheet) := range(from: 0, to: denominator)
  end;
  gadget-label(sheet) := progress-note-label(note);
  gadget-value(sheet) := note.%numerator
end method display-progress-note;

define method lower-progress-note
    (framem :: <basic-frame-manager>, frame :: <frame>,
     gadget :: <status-bar>)
 => ()
  // Switch off the progress control, but leave the last message
  gadget-value(gadget) := #f
end method lower-progress-note;
