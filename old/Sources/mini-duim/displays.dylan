Module:    mini-duim
Synopsis:  Mini-DUIM displays
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Displays

define open abstract class <display> (<object>)
end class <display>;

define open abstract class <basic-display>
    (<mirrored-sheet-mixin>,
     <basic-sheet>,
     <display>)
  slot display-pixel-width;
  slot display-pixel-height;
  slot display-mm-width;
  slot display-mm-height;
  slot display-pixels-per-point;
end class <basic-display>;

define sealed class <standard-display> (<basic-display>)
end class <standard-display>;

define open generic initialize-display 
    (_port :: <port>, _display :: <display>) => ();

define method initialize (_display :: <basic-display>, #key port: _port)
  next-method();
  initialize-display(_port, _display)
end method initialize;

define method sheet-device-transform
    (_display :: <basic-display>) => (transform :: <transform>)
  $identity-transform
end method sheet-device-transform;

define method find-display
    (#key port: _port) => (display :: <display>)
  unless (_port)
    _port := find-port();
  end;
  port-display(_port)
    | begin
	let display = make(port-display-class(_port),
			   port: _port);
	port-display(_port) := display;
	display
      end
end method find-display;

define method port-display-class (_port :: <port>) => (class)
  <standard-display>
end method port-display-class;
