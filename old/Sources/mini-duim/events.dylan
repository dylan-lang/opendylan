Module:    mini-duim
Synopsis:  Mini-DUIM events
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Events

define open abstract class <event> (<object>)
  slot event-modifier-state :: <integer> = 0,
    init-keyword: modifier-state:;
end class <event>;

define open abstract class <sheet-event> (<event>)
  slot event-sheet :: <sheet>,
    required-init-keyword: sheet:;
end class <sheet-event>;

define method event-client
    (event :: <sheet-event>) => (sheet :: <sheet>)
  event-sheet(event)
end method event-client;

define open abstract class <keyboard-event> (<sheet-event>)
  slot event-key-name = #f,
    init-keyword: key-name:;
  slot event-character :: false-or(<character>) = #f,
    init-keyword: character:;
end class <keyboard-event>;

define sealed class <key-press-event> (<keyboard-event>)
end class <key-press-event>;

define sealed class <key-release-event> (<keyboard-event>)
end class <key-release-event>;

define open abstract class <pointer-event> (<sheet-event>)
  slot event-pointer :: false-or(<pointer>) = #f,
    init-keyword: pointer:;
  slot event-x :: false-or(<integer>) = #f,
    init-keyword: x:;
  slot event-y :: false-or(<integer>) = #f,
    init-keyword: y:;
end class <pointer-event>;

define open abstract class <pointer-button-event> (<pointer-event>)
  slot event-button = #f,
    init-keyword: button:;
end class <pointer-button-event>;

define sealed class <button-press-event> (<pointer-button-event>)
end class <button-press-event>;

define sealed class <button-release-event> (<pointer-button-event>)
end class <button-release-event>;

define sealed class <double-click-event> (<button-press-event>)
end class <double-click-event>;

define sealed class <pointer-motion-event> (<pointer-event>)
end class <pointer-motion-event>;

define sealed class <pointer-drag-event>
    (<pointer-motion-event>, <pointer-button-event>)
end class <pointer-drag-event>;

define open abstract class <window-event> (<sheet-event>)
  slot event-region :: false-or(<region>) = #f,
    init-keyword: region:;
end class <window-event>;

define sealed class <window-configuration-event> (<window-event>)
end class <window-configuration-event>;

define sealed class <window-repaint-event> (<window-event>)
end class <window-repaint-event>;

define open abstract class <frame-event> (<event>)
  slot event-frame :: <frame>,
    required-init-keyword: frame:;
end class <frame-event>;

define method event-client
    (event :: <frame-event>) => (frame :: <frame>)
  event-frame(event)
end method event-client;

define sealed class <frame-created-event> (<frame-event>)
end class <frame-created-event>;

define sealed class <frame-destroyed-event> (<frame-event>)
end class <frame-destroyed-event>;

define sealed class <frame-exited-event> (<frame-event>)
end class <frame-exited-event>;

define sealed class <application-exited-event> (<frame-exited-event>)
  slot event-status-code :: <integer>,
    required-init-keyword: status-code:;
end class <application-exited-event>;


/// The main event loop

//---*** Temporary fix to get this to compile in Webster
define constant $left-button   = 256;
define constant $middle-button = 512;
define constant $right-button  = 1024;

define constant $shift-key     = 1;
define constant $control-key   = 2;
define constant $meta-key      = 4;

/*---*** Doesn't work in Webster :-(
define constant $left-button-base = 8;
define constant $left-button   :: <integer> = ash(1, $left-button-base);
define constant $middle-button :: <integer> = $left-button * 2;
define constant $right-button  :: <integer> = $left-button * 4;

define constant $shift-key   :: <integer> = ash(1, 0);
define constant $control-key :: <integer> = ash(1, 1);
define constant $meta-key    :: <integer> = ash(1, 2);
*/

define open generic handle-event
    (client, event :: <event>) => ();
define open generic handle-button-event
    (client, event :: <event>, button) => ();

define method handle-event
    (client, event :: <event>) => ()
  ignore(client, event);
  #f
end method handle-event;

define method handle-event 
    (sheet :: <sheet>, event :: <window-repaint-event>) => ()
  handle-repaint(sheet, sheet-medium(sheet), event-region(event))
end method handle-event;

define method handle-event
    (sheet :: <sheet>, event :: <pointer-button-event>) => ()
  handle-button-event(sheet, event, event-button(event))
end method handle-event;

define method handle-button-event
    (sheet :: <sheet>, event :: <pointer-button-event>, button) => ()
  #f
end method handle-button-event;
    
// Mini-DUIM has no event queuing, so we just handle it immediately
define method distribute-event
    (_port :: <port>, event :: <event>) => ()
  debug-message("Distributing %=", event);
  handle-event(event-client(event), event)
end method distribute-event;

// This takes one (or more) raw events from the window system and
// distributes zero (or more) DUIM events to the appopriate queue(s)
define open generic process-next-event
    (port :: <port>, #key timeout)
 => (exit? :: <boolean>, status-code);

//---*** Do we need this?
define open generic process-pending-events
    (_port :: <port>)
 => (exit? :: <boolean>, status-code);


/// Repainting

define open generic handle-repaint
    (sheet :: <sheet>, medium :: false-or(<medium>), region :: <region>) => ();

define method handle-repaint
    (sheet :: <sheet>, medium :: false-or(<medium>), region :: <region>) => ()
  ignore(sheet, medium, region);
  #f
end method handle-repaint;

define method repaint-sheet
    (sheet :: <basic-sheet>, region :: <region>) => ()
  clear-box(sheet, region);
  handle-repaint(sheet, sheet-medium(sheet), region)
end method repaint-sheet;


/// Fake distribution functions

define method distribute-function-event
    (sheet :: <sheet>, function :: <function>)
  function(sheet-frame(sheet))
end method distribute-function-event;
