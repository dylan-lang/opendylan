Module:    emulator-environment
Synopsis:  Emulator environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// CAPI Interactor pane

// Replaced by Scott's new DUIM interactor
/*
define class <capi-interactor-pane>
    (<capi-gadget-mixin>,
     <interactor-control>,
     <leaf-pane>)
end class <capi-interactor-pane>;

define class <capi-interactor-pane-mirror>
    (<capi-mirror>,
     <capi-infix-listener-pane>)
end class <capi-interactor-pane-mirror>;

define method class-for-make-pane
    (framem :: <capi-frame-manager>, class == <interactor-control>,
     #rest pane-options)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-interactor-pane>, #f)
end method class-for-make-pane;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-interactor-pane>)
  make-capi-mirror(sheet, <capi-interactor-pane-mirror>);
end method do-make-mirror;

define method gadget-value (pane :: <capi-interactor-pane>) => (object)
  let mirror = sheet-mirror(pane);
  if (mirror)
    ensure-server-object(project, capi-infix-listener-pane-value(mirror))
  end
end method gadget-value;
*/
