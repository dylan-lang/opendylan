Module:    select-viewer-server
Synopsis:  This file is the main program for a server in a separate process.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $window-title = "Dylan Select Viewer OLE Automation server";

/*
define method initialize (this :: <select-viewer>, #rest ignore, #key) => ();
  next-method();
  values()
end method initialize;
*/

define method terminate (this :: <select-viewer>) => ()
  next-method();
  // post ourselves a "close" message to end the program:
  quit-server();
  values()
end method terminate ;

define method main-program () => ();

  if ( OLE-util-register-only?() ) // just [un]register and terminate
    register-automation-server($select-viewer-class-id,
                               "HQNexamples.SelectViewer",
			       "Dylan OLE Automation SelectViewer",
			       versioned-prog-id: "HQNexamples.SelectViewer.1");
  else
    let hinst :: <HINSTANCE> = application-instance-handle();
    if ( InitApplication(hinst) )
      let hWnd = InitInstance(hinst, application-show-window());
      if ( hWnd )
	let factory = make(<class-factory>,
			   class: <select-viewer>,
			   typeinfo: type-information(<select-viewer>),
			   clsid: $select-viewer-class-id);

	let pmsg :: <PMSG> = make(<PMSG>);
	while( GetMessage(pmsg, $NULL-HWND, 0, 0) )
	  TranslateMessage(pmsg);
	  DispatchMessage(pmsg);
	end while;

	revoke-registration(factory);
      end if;
    end if;
    values()
  end if;
end method main-program;

define function quit-server () => ()
  // post ourselves a "close" message to end the program:
  PostQuitMessage(0);
end;

// Run the main program.
main-program();
