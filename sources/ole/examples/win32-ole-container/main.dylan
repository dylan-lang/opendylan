Module:    sample-OLE-container
Synopsis:  Main program for OLE container example.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function main-program () => ()

  *sample-app* := make(<sample-container-app>);

  // We will add one ref count on our App.  Later when we want to destroy
  // the App object, we will release this ref count.  When the App's ref
  // count goes to 0, it will be terminated.
  AddRef(*sample-app*);

  // app initialization
  let hInstance = application-instance-handle();
  init-application(*sample-app*, hInstance);

  // instance initialization
  init-instance(*sample-app*, hInstance, application-show-window());

  // message loop
  let msg :: <LPMSG> = make(<LPMSG>);
  while( GetMessage(msg, $NULL-HWND, 0, 0) )
    unless ( container-handle-accelerators(*sample-app*, msg) )
      TranslateMessage(msg);    /* Translates virtual key codes */ 
      DispatchMessage(msg);     /* Dispatches message to window */ 
    end unless;
  end while;

  // Release the ref count added on the App above. this will make
  // the App's ref count go to 0, and the App object will be terminated.
  Release(*sample-app*);

end main-program;

// initiate execution.
main-program();
