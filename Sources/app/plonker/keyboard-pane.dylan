Module:    plonker
Synopsis:  Play with MIDI
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// A keyboard pane

define class <keyboard-pane> (<drawing-pane>)
  // User supplied stuff.
  slot keyboard-octaves :: <integer> = 4,
    init-keyword: octaves:;
  slot keyboard-activate-callback :: <function> = always(#f),
    init-keyword: activate-callback:;
  slot keyboard-deactivate-callback :: <function> = always(#f),
    init-keyword: deactivate-callback:;
  slot keyboard-key-map-callback :: <function> = always(#f),
    init-keyword: key-map-callback:;
  // Internal state.
  slot keyboard-key-down :: false-or(<integer>) = #f;
  slot keyboard-key-press-states :: <table> = make(<table>);
end class;

define method play-keyboard-note
    (pane :: <keyboard-pane>, note :: <integer>) => ()
  keyboard-key-down(pane) := note;
  if (note) keyboard-activate-callback(pane)(pane, note) end;
end method;

define method release-keyboard-note
    (pane :: <keyboard-pane>, note :: <integer>) => ()
  keyboard-key-down(pane) := #f;
  if (note) keyboard-deactivate-callback(pane)(pane, note) end;
end method;

//// Key geometry.

define method edges-to-polygon 
    (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
 => (coords :: <simple-object-vector>)
  vector(left, top, left, bottom, right, bottom, right, top)
end method;

define method natural-key-region 
    (natural :: <integer>, naturals :: <integer>, 
       board-width :: <integer>, board-height :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let lhs = floor/(natural * board-width, naturals);
  let rhs = floor/((natural + 1) * board-width, naturals) - 1;
  values(lhs, 0, rhs, board-height)
end method;

define method sharp-key-region 
    (natural :: <integer>, naturals :: <integer>, 
       board-width :: <integer>, board-height :: <integer>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let (left, top, right, bottom) 
    = natural-key-region(natural, naturals, board-width, board-height);
  let key-space = floor/((right - left) * 1, 3);
  values(right - key-space, 0, right + key-space + 2, floor/(board-height * 2, 3))
end method;

//// Event handling.

define method handle-repaint
    (sheet :: <keyboard-pane>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(sheet);
  let naturals = keyboard-octaves(sheet) * 7 /* + 1 */;
  for (natural from 0 below naturals)
    let (k-left, k-top, k-right, k-bottom)
      = natural-key-region(natural, naturals, right - 1, bottom - 1);
    draw-polygon
      (medium, edges-to-polygon(k-left, k-top, k-right, k-bottom), filled?: #f);
  end;
  for (natural from 0 below naturals,
         natural-info in $major-scale-naturals-cycle)
    if (member?(#"sharp", natural-info))
      let (k-left, k-top, k-right, k-bottom)
        = sharp-key-region(natural, naturals, right - 1, bottom - 1);
      draw-polygon
        (medium, edges-to-polygon(k-left, k-top, k-right, k-bottom), filled?: #t);
    end;
  end;
end method;

define method handle-button-event
    (sheet :: <keyboard-pane>, event :: <button-press-event>, button == $left-button)
 => ()
  let note = compute-hit-note-for-event(sheet, event);
  if (note) play-keyboard-note(sheet, note) end;
end method;

define method handle-button-event
    (sheet :: <keyboard-pane>, event :: <button-release-event>, button == $left-button)
 => ()
  let note = compute-hit-note-for-event(sheet, event);
  let note-playing = keyboard-key-down(sheet);
  if (note & note == note-playing) 
    release-keyboard-note(sheet, note) 
  end;
end method;

// If you don't like the Stylophone semantics, remove this method!

define method handle-button-event
    (sheet :: <keyboard-pane>, event :: <pointer-drag-event>, button == $left-button)
 => ()
  let note = compute-hit-note-for-event(sheet, event);
  let note-playing = keyboard-key-down(sheet);
  if (note-playing ~== note)
    // We've dragged somewhere new. If we were playing something, release it.
    if (note-playing)
      release-keyboard-note(sheet, keyboard-key-down(sheet));
    end;
    // If we've dragged into another key, then play it.
    if (note)
      play-keyboard-note(sheet, note);
    end;
  end;
end method;

define method handle-event
    (sheet :: <keyboard-pane>, event :: <pointer-exit-event>)
 => ()
  let note-playing = keyboard-key-down(sheet);
  if (note-playing) 
    // If we've left the keyboard entirely and a note was playing, release it
    // unless there's a key down. This is a hack: we need to record what's
    // playing what!
    let states = keyboard-key-press-states(sheet);
    if (empty?(states))
      release-keyboard-note(sheet, note-playing); 
    end;
  end;
end method;

define method handle-event
    (sheet :: <keyboard-pane>, event :: <key-press-event>)
 => ()
  let key = event-key-name(event);
  // format-out("Key press: %=\n", key);
  if (key & ~element(keyboard-key-press-states(sheet), key, default: #f))
    element(keyboard-key-press-states(sheet), key) := #t;
    let note = keyboard-key-map-callback(sheet)(sheet, key);
    if (note) play-keyboard-note(sheet, note) end;
  end;
end method;

define method handle-event
    (sheet :: <keyboard-pane>, event :: <key-release-event>)
 => ()
  let key = event-key-name(event);
  // format-out("Key release: %=\n", key);
  if (key)  
    element(keyboard-key-press-states(sheet), key) := #f;
    let note = keyboard-key-map-callback(sheet)(sheet, key);
    if (note) release-keyboard-note(sheet, note) end;
  end;
end method;

//// Hit testing.

define method compute-hit-note-for-event
    (sheet :: <keyboard-pane>, event :: <pointer-event>)
 => (note :: false-or(<integer>))
  compute-hit-note(sheet, event-x(event), event-y(event));
end method;

define method compute-hit-note
    (sheet :: <keyboard-pane>, x :: <integer>, y :: <integer>) 
 => (note :: false-or(<integer>))
  // Brute force "which key"... eek!
  let (left, top, right, bottom) = box-edges(sheet);
  let naturals = keyboard-octaves(sheet) * 7 /* + 1 */;
  block (return)
    let note = 0;
    for (natural from 0 below naturals,
           natural-info in $major-scale-naturals-cycle)
      let has-sharp? = member?(#"sharp", natural-info);
      if (has-sharp?)
        let (k-left, k-top, k-right, k-bottom)
          = sharp-key-region(natural, naturals, right - 1, bottom - 1);
        // There must be DUIM stuff for this...
        if (k-left <= x & x <= k-right & k-top <= y & y <= k-bottom)        
          // format-out("Hit %= sharp\n", natural);
          return(note + 1);
        end;
      end;
      let (k-left, k-top, k-right, k-bottom)
        = natural-key-region(natural, naturals, right - 1, bottom - 1);      
      if (k-left <= x & x <= k-right & k-top <= y & y <= k-bottom)        
        // format-out("Hit %= natural\n", natural);
        return(note);
      end;
      note := note + if (has-sharp?) 2 else 1 end;
    end;
    // If I've computed regions correctly we should never drop out here since
    // it's an exhaustive partition, but I've probably got it wrong so...
    #f
  end;
end method;

// eof

