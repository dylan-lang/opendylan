Module:   status-buttons
Synopsis: Noddy status switching buttons demo
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define frame <status-buttons-frame> (<simple-frame>)
  pane ok-button (frame)
    make(<push-button>, 
         label: "OK",
         max-width: $fill,
         max-height: $fill,
         activate-callback:
           method (gadget)
             display-status-ok(tip: "Status: OK");
           end);
  pane warning-button (frame)
    make(<push-button>, 
         label: "Warning",
         max-width: $fill,
         max-height: $fill,
         activate-callback:
           method (gadget)
             display-status-warning(tip: "Status: Warning");
           end);
  pane error-button (frame)
    make(<push-button>, 
         label: "Error",
         max-width: $fill,
         max-height: $fill,
         activate-callback:
           method (gadget)
             display-status-error(tip: "Status: Error");
           end);
  layout (frame)
    vertically (spacing: 2)
      ok-button(frame);
      warning-button(frame);
      error-button(frame);
    end;
  keyword width: = 500;
  keyword height: = 500;
  keyword title: = "Dylan Status Buttons";
end frame;

define method main () => ()
  block ()
    start-frame(make(<status-buttons-frame>));
  cleanup
    stop-status-display();
  end;
end method main;

begin
  main();
end;

// eof

