module:        devel-dbg-ui
synopsis:      Stuff for the "stop" button
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *stop-button* :: <boolean> = #f;
define variable *stop-button-signalled* :: <boolean> = #f;
define variable *stop-thread-active?* :: <boolean> = #f;

define method signal-stop-button()
  *stop-button-signalled* := #t;
  ignore(c-signal-stop-button);
  disable-stop-thread();
end method;

define C-callable-wrapper c-signal-stop-button of signal-stop-button
  C-name: "c_signal_stop_button";
end C-callable-wrapper;

define method check-stop-button (application :: <application>)
                                    => (pressed? :: <boolean>)

  if (*stop-button-signalled*)
    stop-application(application);
    *stop-button-signalled* := #f;
    #t;
  else
    #f;
  end if;
end method;

define method thread-process-stop-button()
  create-and-process-stop-button()
end method;

keyboard-interrupt-polling?() := #f;

define method thread-process-keyboard-interrupt()
  while (#t)
    until (*stop-thread-active?*) sleep(.2) end;
    until (keyboard-interrupt?()) sleep(.2) end;
    keyboard-interrupt?() := #f;
    signal-stop-button();
  end;
end method;

define method initialize-stop-button()
  *stop-button-signalled* := #f;
  disable-stop-thread();

  make(<thread>,
       name: "Stop Button Thread",
       function:
	 if (*stop-button*) thread-process-stop-button
	 else thread-process-keyboard-interrupt end,
       priority: $high-priority);
end method;

define method disable-stop-thread()
  *stop-thread-active?* := #f
end method;

define method enable-stop-thread()
  *stop-thread-active?* := #t
end method;
