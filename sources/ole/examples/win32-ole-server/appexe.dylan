Module:    sample-OLE-server
Synopsis:  local server support
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Definitions needed to support a local server.  Everything in this
// file can be safely omitted if only an in-process server is needed.

// application class ID for local server
define constant $GUID-LOCAL = "{1FDDEC70-005B-11D0-89FD-02070119F639}";

// Local server OLE object class
define COM-interface <local-simple-server> (<simple-server>) end;

// There's really only ever one object in a local server, so this is
// a little overgeneral...
define function active-obj () => (app :: false-or(<simple-server>))
  any?(method (app :: <simple-server-app>)
	 let obj = app.app-ole-object;
	 obj & OLE-util-in-place-active?(obj) & obj
       end, *app-instances*);
end;

define function top-level-loop () => (code :: <integer>);
  with-stack-structure (msg :: <LPMSG>)
    // message loop
    while (GetMessage(msg, $NULL-HWND, 0, 0))
      // OleTranslateAccelerator MUST be called, even though this application
      // does not have an accelerator table. This has to be done in order
      // for the mnemonics for the top level menu items to work properly.
      // Note that it's ok to call this even if current-obj() is #f.
      unless (OLE-util-translate-accelerator(active-obj(), msg))
	TranslateMessage(msg); // Translates virtual key codes
	DispatchMessage(msg);  // Dispatches message to window
      end unless;
    end while;
    msg.wParam-value // Returns the value from PostQuitMessage
  end with-stack-structure;
end top-level-loop;


define method main-program ()
  if (OLE-util-in-process-startup?())
    Output-Debug-String("Entered in-process server");
  elseif (OLE-util-register-only?()) // just [un]register and terminate
    register-ole-server($GUID-LOCAL,
			"HQN.DylanSample.0",
			"Dylan simple local OLE Server", // name for "insert object"
			short-name: "Dylan OLE Server"); // name for menus
  elseif (~OLE-util-started-by-OLE?())
    // Not started by OLE. Create an OLE object as if a container created it.
    let object = make(<local-simple-server>,
		      clsid: $GUID-LOCAL,
		      started-by-ole?: #f);
    AddRef(object);
    show-app-frame(object.server-app, application-show-window());
    top-level-loop();
  else // Normal local server case.
    with-class-factory (clsid: $GUID-LOCAL,
			class: <local-simple-server>,
			context: $CLSCTX-LOCAL-SERVER,
			flags: $REGCLS-SINGLEUSE)
      top-level-loop();
    end;
  end if;
end main-program;


main-program();

