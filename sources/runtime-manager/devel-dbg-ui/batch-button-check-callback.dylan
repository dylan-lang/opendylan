module:     devel-dbg-ui
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *stop-button* :: <boolean> = #f;
define variable *stop-button-signalled* :: <boolean> = #f;

define method signal-stop-button()
  *stop-button-signalled* := #t
end method;

define C-callable-wrapper c-signal-stop-button of signal-stop-button
  C-name: "c_signal_stop_button";
end C-callable-wrapper;

define method check-stop-button (application :: <application>)
                                    => (pressed? :: <boolean>)
  #f
end method;


define method enable-stop-thread()
  #f
end method;
