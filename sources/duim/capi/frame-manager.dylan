Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// CAPI frame manager

define sealed class <capi-frame-manager> (<basic-frame-manager>)
end class <capi-frame-manager>;

define method make-frame-manager
  (_port :: <capi-port>,
   #key palette, class = <capi-frame-manager>, #all-keys)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


define sealed method note-frame-iconified
    (framem :: <capi-frame-manager>, frame :: <simple-frame>) => ()
  next-method();		// update the frame's state
  let _port  = port(frame);
  let sheet  = _port & top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    //--- This is probably not the right thing
    lower-mirror(_port, sheet, mirror)
  end
end method note-frame-iconified;

define sealed method note-frame-deiconified
    (framem :: <capi-frame-manager>, frame :: <simple-frame>) => ()
  next-method();		// update the frame's state
  let _port  = port(frame);
  let sheet  = _port & top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    //--- Is this the right thing?
    raise-mirror(_port, sheet, mirror)
  end
end method note-frame-deiconified;


/// The standard dialogs

// Values for style (per PW, following the Motif spec) are:
//  :ERROR, :INFORMATION, :QUESTION, :WORKING, :WARNING.
define method do-notify-user 
    (framem :: <capi-frame-manager>, owner :: <sheet>,
     message :: <string>, style :: <notification-style>,
     #key frame, title, exit-style,
     #all-keys)
 => (value :: <boolean>, exit-type)
  ignore(owner, frame, exit-style);
  block (return)
    select (style)
      #"information" =>
	display-message("~A", message);
      #"question" =>
	let value = lisp-true?(confirm-yes-or-no("~A", message));
	return(value, if (value) #"yes" else #"no" end);
      #"warning" =>
	display-message("~A", message);
      #"error" =>
	display-message("~A", message);
      #"serious-error" =>
	display-message("Serious error: ~A", message);
      #"fatal-error" =>
	display-message("Fatal error: ~A", message);
    end;
    values(#t, #"ok")
  end
end method do-notify-user;

define method do-choose-file
    (framem :: <capi-frame-manager>, 
     owner :: <sheet>,
     direction :: one-of(#"input", #"output"),
     #key title, documentation, exit-boxes,
	  if-exists, if-does-not-exist,
	  default, default-type, filters, default-filter, selection-mode = #"single",
     #all-keys)
 => (locator :: false-or(type-union(<string>, <sequence>)),
     filter :: false-or(<integer>))
  ignore(documentation, exit-boxes,
	 if-exists, if-does-not-exist,
	 default-type, filters, default-filter);
  let file = prompt-for-file(title | #(), pathname: default | #());
  unless (instance?(file, <list>))
    select (selection-mode)
      #"single"   => cl-namestring(file);
      #"multiple" => vector(cl-namestring(file));
    end
  end
end method do-choose-file;

define method do-choose-color
    (framem :: <capi-frame-manager>, 
     owner :: <sheet>,
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (color :: false-or(<color>))
  ignore(documentation, exit-boxes, name, default);
  let color = prompt-for-color(title | #());
  unless (instance?(color, <list>))
    let rep = sheet-mirror(owner);
    capi-color->color(rep, unconvert-color(rep, convert-color(rep, color)))
  end
end method do-choose-color;

define method do-choose-text-style
    (framem :: <capi-frame-manager>, 
     owner :: <sheet>,
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (text-style :: false-or(<text-style>))
  ignore(documentation, exit-boxes, name, default);
  //---*** Do something with prompt-for-font...
  #f
end method do-choose-text-style;
