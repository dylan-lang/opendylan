Module:    DUIM-OLE-server
Synopsis:  Program start-up, event loop, and termination.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// The application's main program should look like:
//
//	define frame <foo-frame> (<embeddable-frame>) ...
//    ...
//	define constant $foo-ID = make-GUID(...);
//    ...
//	start-frame(make(<foo-frame>, class-id: $foo-ID, title: "Foo"));


// The first argument below should be specialized on duim/<win32-port>,
// but we're having to use duim/<port> instead to work around:
//	BUG 1052: ``specializers ... cannot be computed at compile-time''

define method duim/port-start-frame
    (port :: duim/<port>, frame :: <embeddable-frame>)
 => (status-code :: false-or(<integer>))

  unless ( frame.ole-frame-class-id )
    error("`class-id:' not specified for %=", frame);
  end unless;
  if ( OLE-util-register-only?() ) // just [un]register and terminate
    if ( OLE-util-unregister?() )
      frame-unregister-server(frame);
    else
      frame-register-server(frame);
    end if;
  else

    let App :: <DUIM-server-app> = make(<DUIM-server-app>, frame: frame);
    let ( class :: <class>, class-args :: <sequence> ) =
      frame-ole-object-class(frame); // usually <DUIM-OLE-server>
    let doc = App.get-doc;

    // if not started by OLE, then show the Window, and create a "fake"
    // object, else Register a pointer to IClassFactory so that OLE can 
    // instruct us to make an object at the appropriate time.
    if ( ~ App.started-by-ole? )
      let obj = apply(make, class, doc: doc, class-args);
      App.dummy-object := obj;
      AddRef(obj);
      // May just want to use `next-method()' at this point ???
      show-app-window(App);
    else
      App.get-factory := apply(make, <class-factory>,
			       clsid: frame.ole-frame-class-id,
			       class: class,
			       connection-flags: frame.ole-frame-usage,
			       doc: doc,
			       class-args);
    end if;

    let code = ole-event-loop(frame);
    if ( App.started-by-ole? )
      close-app(App);
    end if;
    code
  end if
end method duim/port-start-frame;

define method duim/make-event-queue
    (framem :: duim/<frame-manager>, frame :: <embeddable-frame>)
 => (event-queue :: false-or(duim/<event-queue>))
  // Ensure that DUIM does not queue events since we are not using
  // the DUIM event loop.
  #f
end method duim/make-event-queue;



define method frame-registration-parameters( frame :: <embeddable-frame> )
 => (prog-id, title, short-title);
  let title = frame.ole-frame-object-title;
  unless ( title )
    title := frame.duim/frame-title;
    unless ( title )
      error("No title for registration of %=", frame);
    end unless;
  end unless;
  let short-title = frame.ole-frame-short-title | frame.duim/frame-title;
  if ( short-title )
    if (size(title) < size(short-title) ) short-title := title; end if;
    if ( size(short-title) > 15 )
      short-title := copy-sequence(short-title, end: 15);
    end if;
  end if;
  let prog-id = frame.ole-frame-prog-id |
    invent-prog-id(as(<REFCLSID>, frame.ole-frame-class-id), title);
  values(prog-id, title, short-title)
end frame-registration-parameters;

// These two should return either no values, #f, or an <HRESULT>.
define open generic frame-register-server (frame);
define open generic frame-unregister-server (frame);

define method frame-register-server( frame :: <embeddable-frame> );
  let ( prog-id, title, short-title ) = frame-registration-parameters(frame);
  register-ole-server(frame.ole-frame-class-id, prog-id,
		      title, short-name: short-title, full-name: title);
end method;

define method frame-unregister-server( frame :: <embeddable-frame> );
  let class-id = frame.ole-frame-class-id;
  let prog-id = frame-registration-parameters(frame);
  unregister-ole-server(class-id, prog-id);
end method;


define method ole-event-loop ( frame :: <embeddable-frame> )
 => code :: <integer>;

  let top-sheet :: duim/<sheet> = duim/top-level-sheet(frame);
  let accelerators = duim/accelerator-table(top-sheet);
  with-stack-structure( msg :: <LPMSG> )
    // message loop
      while ( GetMessage(msg, $NULL-HWND, 0, 0) )

	// Only key messages need to be sent to OleTranslateAccelerator.
	let message :: <fixnum> = msg.message-value;
	unless ((message >= $WM-KEYFIRST) & (message <= $WM-KEYLAST) &
		  // Check the server's accelerator keys first:
		  ((accelerators &
		      ~ zero?(TranslateAccelerator(msg.hwnd-value,
						   accelerators, msg))) |
		     ( frame-in-place-active?(frame) &
			// Check the container's accelerator keys:
			OLE-util-translate-accelerator(frame-ole-object(frame),
						       msg))) )
	  // Message is not an accelerator key.
	  TranslateMessage(msg);    // Translates virtual key codes
	  DispatchMessage(msg);     // Dispatches message to window
	end unless;
      end while;
      msg.wParam-value    // Returns the value from PostQuitMessage
  end with-stack-structure
end method ole-event-loop;



// (Originally, these actions were in response to a WM_CLOSE message.)
define method close-app ( App :: <DUIM-server-app> ) => ();

  Output-Debug-String("close-app\r\n");
  
  // if there is still a document
  let doc = App.get-doc;
  if ( doc )
    // if there is still an object within a document
    let obj = doc.get-object;
    unless ( null?(obj) )
      // this case occurs if there is still
      // an outstanding Ref count on the object
      // when the app is trying to go away.
      // typically this case will occur in
      // the "open" editing mode.
      //  Close the document
      OLE-util-close-server(obj);
    end unless;
    App.get-doc := #f;
  end if;

  // if we were started by OLE, unregister the class factory, otherwise
  // remove the ref count on our dummy OLE object
  if ( App.started-by-ole? )
    revoke-registration(App.get-factory);
  else
    Release(App.dummy-object);
  end if;
  values()
end method;


define method invent-prog-id (class-id :: <REFCLSID>, title :: <string>)
 => prog-id :: <byte-string>;
  // If the user has not supplied a "prog-id", make one up.
  // Since this is only used as an internal identifier, there is no reason
  // why it needs to be particularly readable.
  // We create an ID string by combining portions of the title and class ID.
  // For example, if the window title is "My Foo & Bar" and the class ID is
  // "{01234567-89AB-CDEF-EDCB-A9876543210F}" then the generated program ID is:
  // "MyFooBarxxxxxxxxA9876543210F01234567"

  let prog-id :: <byte-string> = make(<byte-string>, size: 36, fill: 'x');
  let class-id-string :: <byte-string> = as(<string>, class-id);
  let id-size :: <fixnum> = size(class-id-string);
  let i :: <fixnum> = 16;
  for ( k from id-size - 13  below id-size - 1 ) // host ID portion of GUID
    prog-id[i] := class-id-string[k];
    i := i + 1;
  end for;
  for ( k from 1 below 9 ) // timestamp portion of GUID
    prog-id[i] := class-id-string[k];
    i := i + 1;
  end for;
  i := 0;
  let j :: <fixnum> = 0;
  let title-size :: <fixnum> = size(title);
  // must begin with a letter
  // (since this code is only for MS Windows, we can assume ASCII)
  while ( j < title-size & as-lowercase(title[j]) < 'a' ) j := j + 1; end;
  // copy the first 24 alphanumeric characters from the title:
  while ( i < 24 & j < title-size )
    let c :: <character> = title[j];
    if ( ( c >= '0' & c <= '9' ) | (c >= 'A' & as-uppercase(c) <= 'Z') )
      prog-id[i] := c;
      i := i + 1;
    end if;
    j := j + 1;
  end while;
  prog-id
end invent-prog-id;
