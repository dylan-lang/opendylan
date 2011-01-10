Module:    sample-automation-server
Synopsis:  Server startup/exit
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $class-id = "{ef462960-bb53-11cf-89f8-02070119f639}";

// In-process support

define COM-interface <in-process-painter> (<painter>) end;

in-process-automation-server(class: <in-process-painter>,
			     typeinfo:  <in-process-painter>.type-information,
			     class-id: $class-id,
			     prog-id: "HQNexamples.DPaintApp",
			     title: "Dylan OLE Automation in-proc server",
			     versioned-prog-id: "HQNexamples.DPaintApp.1");


define method terminate (this :: <in-process-painter>) => ()
  next-method();
  let window = this.window-handle;
  unless (null-handle?(window))
    this.window-handle := $NULL-HWND;
    DestroyWindow(window);
  end unless;
end;


// Out-of-process support

define COM-interface <local-painter> (<painter>) end;

define method terminate (this :: <local-painter>) => ()
  next-method();
  PostQuitMessage(0);
end;


define method main-program () => ();

  if (OLE-util-register-only?()) // just [un]register and terminate
    register-automation-server($class-id,
			       "HQNexamples.DPaintApp",
			       "Dylan OLE Automation local server",
			       versioned-prog-id: "HQNexamples.DPaintApp.1");
  else
    let hWnd = application-init-window();
    when (hWnd)
      with-class-factory (clsid: $class-id,
			  class: <local-painter>,
			  typeinfo: <local-painter>.type-information,
			  window-handle: hWnd)
	let pmsg :: <PMSG> = make(<PMSG>);
	while (GetMessage(pmsg, $NULL-HWND, 0, 0))
	  TranslateMessage(pmsg);
	  DispatchMessage(pmsg);
	end while;
      end with-class-factory;
    end when;
  end if;

end main-program;

// Run the main program, but only if invoked out-of-process
unless (OLE-util-in-process-startup?())
  main-program();
end;
