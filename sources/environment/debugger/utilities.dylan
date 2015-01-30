Module:    environment-debugger
Author:    Bill Chiles, Jason Trenouth
Synopsis:  Generally useful stuff and temporary hacks
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// WITHIN-FRAME (internal)
///
/// Macro to insert work into frame's event loop

define macro within-frame

  { within-frame (?name:variable = ?frame:expression) ?body:body end }
    =>
  { local method _do-the-body_ (?name)
            ?body;
          end method;
    let _frame_ = ?frame;
    call-in-frame(_frame_, _do-the-body_, _frame_); }

  { within-frame (?name:name) ?body:body end }
    =>
  { local method _do-the-body_ (?name)
            ?body;
          end method;
    let _frame_ = ?name;
    call-in-frame(_frame_, _do-the-body_, _frame_); }

end macro;


/// CHOOSE-INTO (internal)
/*
define method choose-into (into :: <mutable-collection>, test :: <function>, outof :: <collection>)
  => (into :: <mutable-collection>)
  // ---*** LANGUAGE: remove-all-keys! should work on stretchy-vectors
  into.size := 0;
  local method maybe-add!(x)
          test(x) & add!(into, x)
        end method;
  do(maybe-add!, outof);
  into;
end method;
*/

/// NON-EMPTY-STRING (internal)

define function non-empty-string
    (string :: <string>) => (result :: false-or(<string>))
  ~empty?(string) & string
end function non-empty-string;


/// $VERTICAL-SPACING

define constant $vertical-spacing :: <integer> = 8;


/// REDISPLAY-DEBUGGER-EDITOR-WINDOW (internal)

define function redisplay-debugger-editor-window
    (window :: <gadget>, #key refresh? :: <boolean> = #f) => ()
  if (sheet-mapped?(window))
    with-editor-state-bound (window)
      if (refresh?)
        queue-redisplay(window, $display-all)
      else
        queue-redisplay(window, $display-text, centering: 1)
      end;
      redisplay-window(window)
    end
  end
end function redisplay-debugger-editor-window;
