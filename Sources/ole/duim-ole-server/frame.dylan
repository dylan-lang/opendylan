Module:    DUIM-OLE-server
Synopsis:  OLE server frame class and methods
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open generic ole-frame-class-id (frame) => (class-id);

define open abstract class <embeddable-frame> ( duim/<simple-frame> )
  slot ole-frame-doc-obj :: false-or(<DUIM-server-doc>) = #f;
  slot embedded-data-changed? :: <boolean> = #t; // data not saved to storage?
  slot ole-frame-class-id = #f, init-keyword: class-id:; // GUID or string
  constant slot ole-frame-usage :: <fixnum> = $REGCLS-SINGLEUSE,
    init-keyword: usage:;
  slot frame-in-place-active? :: <boolean> = #f;
  constant slot ole-frame-prog-id :: false-or(<string>) = #f,
    init-keyword: prog-id:;
  slot ole-frame-object-title :: false-or(<string>) = #f,
    init-keyword: object-title:;
  constant slot ole-frame-short-title :: false-or(<string>) = #f,
    init-keyword: short-title:;
  slot ole-frame-status-message :: false-or(<string>) = #f; // in status bar

  // Returns the OLE IUnknown interface pointer for a user who wants to 
  // access the OLE API directly.  Note that this does not call AddRef.
  slot frame-ole-object :: false-or(<basic-DUIM-OLE-server>) = #f;
end class;


// Return amount of space allocated in the container window:
define method frame-embedded-size ( frame :: <embeddable-frame> )
 => ( width :: <integer>, height :: <integer> );
  OLE-util-current-size(frame-ole-object(frame))
end method;

// Called by the application program to notify the OLE container that the
// application data has changed.
define method note-embedded-data-changed ( frame :: <embeddable-frame> ) => ();
  frame.embedded-data-changed? := #t;	// store persistent data
  note-embedded-image-changed(frame);	// update image in container
end method;

// Update the image in the container:
define method note-embedded-image-changed ( frame :: <embeddable-frame> ) => ()
  let doc = frame.ole-frame-doc-obj;
  if ( doc )
    // This seems a little clumsy.  Probably there should eventually be
    // a separate frame class for an in-process server, and then it can have
    // a separate method.		???
    if ( started-by-ole?(doc.get-app) |
	  instance?(doc.get-object, <ole-in-process-server>) )
      OLE-util-view-changed(doc.get-object);
    end if;
  end if;
  values()
end method;

// user can call this to get name of container application
define function frame-container-name ( frame :: <embeddable-frame> )
    => ( application :: false-or(<string>), document :: false-or(<string>) );
  OLE-util-container-name(frame-ole-object(frame))
end;

define open generic frame-embedded-sheet (frame)
 => (sheet :: false-or(duim/<sheet>));

// default method, user can override.
define method frame-embedded-sheet (frame :: <embeddable-frame>)
 => (sheet :: false-or(duim/<sheet>));
  #f
end method;

define open generic note-embedded-region-changed (frame, width, height)
 => (ok :: <boolean>);

// default method, user can override.
define method note-embedded-region-changed (frame :: <embeddable-frame>,
					  width, height) => (ok :: <boolean>)
  #t
end method;


define open generic frame-embedded-size-requested ( frame )
		=> ( width :: <integer>, height :: <integer> );

// default method
define method frame-embedded-size-requested ( frame :: <embeddable-frame> )
		=> ( width :: <integer>, height :: <integer> )
  let doc = frame.ole-frame-doc-obj;
  let sheet = doc.doc-sheet;
  duim/box-size(duim/sheet-region(sheet))
end method;


define open generic frame-ole-object-class( frame )
 => (class :: <class>, args :: <sequence>);

// Default method can be overridden by the user.
define method frame-ole-object-class( frame :: duim/<frame> )
 => (class :: <class>, args :: <vector>);

  values( <DUIM-OLE-server>, vector(frame: frame) )
end method;

define method duim/note-win32-frame-destroyed (frame ::  <embeddable-frame>)
 => ();
  let obj = frame.frame-ole-object;
  if ( obj ) // unless destroy-frame called from make-ocx-factory
    // For an in-process server, do nothing.
    unless ( instance?(obj, <ole-in-process-server>) )
      next-method(); // does PostQuitMessage to terminate the server process.
    end;
  end if;
end method duim/note-win32-frame-destroyed;



// status bar support 

define method duim/frame-status-message
    (frame :: <embeddable-frame>) => (message :: false-or(<string>))
  if ( frame-in-place-active?(frame) )
    // Using the container's status bar.
    frame.ole-frame-status-message
  else // Using the application's own status bar.
    next-method()
  end if;
end method duim/frame-status-message;

define method duim/frame-status-message-setter
    (message :: false-or(<string>), frame :: <embeddable-frame>)
 => (message :: false-or(<string>))

  if ( frame-in-place-active?(frame) )
    // Use the container's status bar.
    if ( message | frame.ole-frame-status-message )
      let status = OLE-util-set-status-text(frame-ole-object(frame), message);
      frame.ole-frame-status-message := SUCCEEDED?(status) & message
    else
      #f
    end if
  else // Use the application's own status bar.
    next-method()
  end if
end method duim/frame-status-message-setter;

// This is used by DUIM for describing menu items:
define method duim/update-frame-documentation
      (frame :: <embeddable-frame>, message :: false-or(<string>)) => ();
  if ( frame-in-place-active?(frame) )
    // Use the container's status bar.
    duim/frame-status-message-setter(message, frame);
  else // Use a "simple" message in the application's own status bar.
    next-method();
  end if;
end method duim/update-frame-documentation;


// dummy methods for ordinary frame:

define method note-embedded-data-changed ( frame :: duim/<frame> ) => ();
  values()
end method;

define method embedded-data-changed? ( frame :: duim/<frame> )
			=> dirty? :: <boolean>;
  #t
end method;

define method embedded-data-changed?-setter ( value :: <boolean>,
				        frame :: duim/<frame> );
  value
end method;

define method ole-frame-doc-obj-setter ( value, frame :: duim/<frame> );
  value
end method;


define open generic note-in-place-activation( frame ) => ();

// default method does nothing.
define method note-in-place-activation( frame :: duim/<frame> ) => ();
  values()
end method;

define open generic note-in-place-deactivation (frame ) => ();

// default method does nothing.
define method note-in-place-deactivation( frame :: duim/<frame> ) => ();
  values()
end method;
