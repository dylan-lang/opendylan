Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Primary object API

// #f always means "no object"

define open abstract class <frame-primary-object-mixin> (<frame-history-mixin>)
  sealed slot frame-raw-primary-object :: <object> = #f,
    init-keyword: object:;
  sealed slot frame-name-selector :: false-or(<gadget>) = #f;
  sealed slot frame-last-name :: false-or(<string>) = #f;
  sealed slot frame-last-callback :: false-or(<symbol>) = #f;
end class <frame-primary-object-mixin>;

//---*** Restricting these to <frame-primary-object-mixin> loses,
//       which seems too bad

define open generic frame-primary-object
    (frame :: <frame>)
 => (object :: <object>);

define open generic frame-default-primary-object
    (frame :: <frame>)
 => (object :: <object>);

define open generic frame-coerce-raw-object
    (frame :: <frame>, object :: <object>)
 => (object :: <object>);

define open generic frame-coerce-primary-object
    (frame :: <frame>, object :: <object>)
 => (object :: <object>);

define open generic frame-coerce-object
    (frame :: <frame>, object :: <object>)
 => (object :: <object>);

define open generic frame-primary-object-name
    (frame :: <frame>, object :: <object>)
 => (name :: <string>);

define open generic note-primary-object-changed
    (frame :: <frame>, old-object :: <object>) => ();

define open generic note-raw-primary-object-replaced
    (frame :: <frame>, old-object :: <object>) => ();

define open generic frame-primary-object-class
    (frame :: <frame>) => (class :: <class>);

define open generic frame-select-primary-object
    (frame :: <frame>, #key title, label, prompt)
 => ();


/// Initialization

define method handle-event
    (frame :: <frame-primary-object-mixin>, event :: <frame-mapped-event>)
 => ()
  next-method();
  // Get the raw primary object (set in initialize), and set the
  // primary object, which will call notification functions, etc.
  let object = frame.frame-raw-primary-object;
  case
    object =>
      frame-raw-primary-object(frame) := #f;
      frame-primary-object(frame)     := object;
    ~frame-raw-primary-object(frame) =>
      let default-object = frame-default-primary-object(frame);
      if (default-object)
        frame-primary-object(frame) := default-object
      end;
    //--- cpage: 1998.03.10 I don't think the otherwise case can occur.
    //           Consider deleting this.
    otherwise =>
      /* Do nothing */;
  end case;
end method handle-event;

define method reinitialize-frame
    (frame :: <frame-primary-object-mixin>, #key object) => ()
  next-method();
  if (object)
    //---*** cpage: 1998.09.01 This line plays havoc with functions that want to
    //              know whether the new object is the same as the old. Before
    //              frame-raw-primary-object was created, this line used to
    //              reset %object. I suspect it did so to ensure that the frame
    //              frame was refreshed. I'm leaving this here for a while, till
    //              we have tested this change.
    // frame.frame-raw-primary-object := #f;
    frame-primary-object(frame) := object
  end;
end method reinitialize-frame;

define method frame-coerce-raw-object
    (frame :: <frame-primary-object-mixin>, object :: <object>)
 => (object :: <object>)
  object
end method frame-coerce-raw-object;
 
define method frame-primary-object
    (frame :: <frame-primary-object-mixin>)
 => (object :: <object>)
  frame-coerce-raw-object(frame, frame-raw-primary-object(frame))
end method frame-primary-object;

define method frame-default-primary-object
    (frame :: <frame-primary-object-mixin>)
 => (object :: <object>)
  #f
end method frame-default-primary-object;

define method frame-coerce-object
    (frame :: <frame-primary-object-mixin>, object :: <object>)
 => (object :: <object>)
  instance?(object, frame-primary-object-class(frame)) & object
end method frame-coerce-object;

define method frame-coerce-object
    (frame :: <frame-primary-object-mixin>, name :: <string>)
 => (object :: false-or(<environment-object>))
  find-named-object(frame, name)
end method frame-coerce-object;

define method frame-coerce-primary-object
    (frame :: <frame-primary-object-mixin>, object :: <object>)
 => (object :: <object>)
  frame-coerce-object(frame, object)
end method frame-coerce-primary-object;

define method note-primary-object-changed
    (frame :: <frame>, old-object :: <object>) => ()
  #f
end method note-primary-object-changed;

define method note-raw-primary-object-replaced
    (frame :: <frame>, old-object :: <object>) => ()
  #f
end method note-raw-primary-object-replaced;

define method frame-primary-object-setter
    (object :: <object>, frame :: <frame-primary-object-mixin>)
 => (object :: <object>)
  let old-raw-object = frame-raw-primary-object(frame);
  let old-object = frame-primary-object(frame);
  let raw-object = object & frame-coerce-primary-object(frame, object);
  when (object & ~raw-object)
    let message
      = format-to-string(if (instance?(object, <string>))
			   "Cannot find an object named '%s'."
			 else
			   "Cannot find object %=."
			 end,
			 object);
    notify-user(message, owner: frame);
  end;
  when (raw-object & raw-object ~== old-raw-object)
    frame.frame-raw-primary-object := raw-object;
    raw-object & frame-add-to-history(frame, raw-object);
    old-raw-object & note-raw-primary-object-replaced(frame, old-raw-object);
    note-primary-object-changed(frame, old-object);
  end;
  object
end method frame-primary-object-setter;

define method frame-select-object
    (frame :: <frame-primary-object-mixin>, object :: <object>) => ()
  next-method();
  frame-primary-object(frame) := object
end method frame-select-object;

define method note-frame-last-object-closed
    (frame :: <frame-primary-object-mixin>) => ()
  next-method();
  frame.frame-raw-primary-object := #f;
end method note-frame-last-object-closed;

define method frame-primary-object-class
    (frame :: <frame-primary-object-mixin>) => (class :: <class>)
  <object>
end method frame-primary-object-class;

define method make-frame-primary-object-selector 
    (frame :: <frame-primary-object-mixin>, #key label = "Object:")
 => (sheet :: <sheet>)
  local method find-named-object-in-history
	    (name :: <string>) => (object)
	  let name = as-lowercase(name);
	  block (return)
	    for (raw-object in frame-history(frame))
	      let object = frame-coerce-raw-object(frame, raw-object);
	      let object-name 
		= as-lowercase(frame-primary-object-name(frame, object));
	      if (name = object-name) return(raw-object) end
	    end
	  end
	end method find-named-object-in-history;
  local method update-primary-object
	    (gadget :: <combo-box>, callback :: <symbol>) => ()
	  let frame = sheet-frame(gadget);
	  with-busy-cursor (frame)
	    let name = gadget-value(gadget);
	    //--- This is a grotesque hack to avoid updating twice,
	    //--- first from the value changed callback and then
	    //--- again from the activate callback.
	    unless (name == frame.frame-last-name
		      & callback == #"activate"
		      & frame.frame-last-callback == #"value-changed")
	      let object = find-named-object-in-history(name) | name;
	      frame-primary-object(frame) := object
	    end;
	    frame.frame-last-name     := name;
	    frame.frame-last-callback := callback;
	  end
        end method update-primary-object;
  let object = frame-primary-object(frame);
  horizontally (spacing: 2, y-alignment: #"center")
    make(<label>, label: label);
    frame-name-selector(frame)
      := make(<combo-box>,
	      value: object & frame-primary-object-name(frame, object),
	      min-width: 100,
	      value-changed-callback: rcurry(update-primary-object, #"value-changed"),
              activate-callback: rcurry(update-primary-object, #"activate"))
  end
end method make-frame-primary-object-selector;

define method note-primary-object-changed
    (frame :: <frame-primary-object-mixin>,
     old-object :: <object>)
 => ()
  next-method();
  refresh-name-selector(frame)
end method note-primary-object-changed;

define method note-frame-history-changed
    (frame :: <frame-primary-object-mixin>) => ()
  next-method();
  let name-selector = frame-name-selector(frame);
  when (name-selector)
    let history
      = map(method (raw-object)
	      let object = frame-coerce-raw-object(frame, raw-object);
	      frame-primary-object-name(frame, object)
	    end,
	    frame-most-recent-objects(frame));
    // ---*** hughg, 1997/01/22: HACK to work around bug on \= for <deque>s
    // gadget-items(name-selector) := history
    gadget-items(name-selector) := as(<vector>, history)
  end
end method note-frame-history-changed;

define method refresh-frame (frame :: <frame-primary-object-mixin>) => ()
  next-method();
  refresh-name-selector(frame)
end method refresh-frame;

define method refresh-name-selector
    (frame :: <frame-primary-object-mixin>) => ()
  let name-selector = frame-name-selector(frame);
  if (name-selector)
    let object = frame-primary-object(frame);
    gadget-value(name-selector) 
      := if (object) frame-primary-object-name(frame, object) end
  end
end method refresh-name-selector;


/// Select an object by name

define variable $select-object-dialog-width :: <integer> = 350;

define frame <select-object-dialog> (<dialog-frame>)
  constant slot select-object-dialog-label,
    init-keyword: label:;
  constant slot select-object-dialog-prompt,
    init-keyword: prompt:;
  slot select-object-dialog-value = #f,
    init-keyword: value:;
  constant slot select-object-dialog-items = vector(#f),
    init-keyword: items:;
  pane %object-selecter (frame)
    make(<combo-box>,
	 value: frame.select-object-dialog-value,
	 items: frame.select-object-dialog-items,
	 value-changing-callback:
	   method (gadget)
	     let dialog = sheet-frame(gadget);
	     let value = gadget-value(gadget);
	     dialog-exit-enabled?(dialog) := ~empty?(value)
	   end,
	 value-changed-callback:
	   method (gadget)
	     let dialog = sheet-frame(gadget);
	     let value = gadget-value(gadget);
	     dialog.select-object-dialog-value := value;
	     dialog-exit-enabled?(dialog) := ~empty?(value)
	   end,
	 activate-callback:
	   method (gadget)
	     let dialog = sheet-frame(gadget);
	     when (dialog-exit-enabled?(dialog))
	       exit-dialog(dialog)
	     end
	   end);
  layout (frame)
    make(<table-layout>,
         y-spacing: 14,
         y-alignment: #"center",
         contents:
           list(list(make(<null-pane>), //--- cpage: 1997.09.14 Put an icon here.
                     make(<label>, label: frame.select-object-dialog-prompt)),
                list(make(<label>, label: frame.select-object-dialog-label),
		     frame.%object-selecter)));
end frame <select-object-dialog>;

define method handle-event
    (dialog :: <select-object-dialog>, event :: <frame-focus-in-event>) => ()
  frame-input-focus(dialog) := dialog.%object-selecter
end method handle-event;

define method frame-select-primary-object
    (frame :: <frame-primary-object-mixin>,
     #key title  = "Browse",
          label  = "Object:",
          prompt = "Type the name of an object to browse.")
 => ()
  let object = frame-primary-object(frame);
  let dialog
    = make(<select-object-dialog>,
	   owner: frame,
	   title: title,
	   label: label,
	   prompt: prompt,
	   value: object & frame-primary-object-name(frame, object),
	   items: gadget-items(frame-name-selector(frame)),
	   width: max($select-object-dialog-width, 300));
  frame-input-focus(dialog) := dialog.%object-selecter;
  when (start-dialog(dialog))
    let (width, height) = frame-size(dialog);
    $select-object-dialog-width := width;
    with-busy-cursor (frame)
      let value = dialog.select-object-dialog-value;
      frame-primary-object(frame) := value
    end;
  end
end method frame-select-primary-object;
