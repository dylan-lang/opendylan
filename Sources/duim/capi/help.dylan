Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stub 'display-help' methods for CAPI backend

define method display-help
    (framem :: <capi-frame-manager>, frame :: <frame>, command :: <help-command>) => ()
  notify-user(format-to-string("DBG: help invoked."),
	      owner: frame)
end method display-help;

define method display-help
    (framem :: <capi-frame-manager>, frame :: <frame>, command :: <help-on-help>) => ()
  notify-user(format-to-string("DBG: help on help invoked."),
	      owner: frame)
end method display-help;

define method display-help
    (framem :: <capi-frame-manager>, frame :: <frame>, command :: <help-on-topics>) => ()
  notify-user(format-to-string("DBG: help on topics invoked: %=",
			       sheet-help-source(command)),
	      owner: frame)
end method display-help;

define method display-help
    (framem :: <capi-frame-manager>, frame :: <frame>, command :: <help-on-context>) => ()
  notify-user(format-to-string("DBG: help on context invoked: %=, %=",
                               sheet-help-source(command), help-context(command)),
              owner: frame)
end method display-help;

define method display-help
    (framem :: <capi-frame-manager>, frame :: <frame>, command :: <help-on-keyword>) => ()
  notify-user(format-to-string("DBG: help on keyword invoked: %=, %=",
                               sheet-help-source(command), help-keyword(command)),
              owner: frame)
end method display-help;
