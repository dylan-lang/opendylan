Module:       duim-deuce-internals
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Event handling

define macro with-command-error-handling
  { with-command-error-handling () ?:body end }
    => { block ()
           ?body
         exception (e :: <command-error>)
           when (command-error-format-string(e))
             apply(deuce/display-error-message,
                   command-error-window(e),
                   command-error-format-string(e), command-error-format-arguments(e))
           end
         end }
end macro with-command-error-handling;

define sealed method handle-event
    (sheet :: <deuce-pane>, event :: <button-press-event>) => ()
  let frame = window-frame(sheet);                // ~= sheet-frame for <deuce-gadget>
  with-editor-state-bound (buffer = sheet)
    when (frame-isearch-direction(frame))
      finish-incremental-search(frame)
    end;
    let pointer = event-pointer(event);
    pointer-grabbed?(pointer) := sheet;                // grab pointer for auto-scrolling
    let x = event-x(event);
    let y = event-y(event);
    let button    = event-button(event);
    let modifiers = make-deuce-modifiers(event-modifier-state(event));
    let (presentation, bp, move-point?)
      = presentation-at-position(sheet, x, y,
                                 button: button, modifiers: modifiers,
                                 event-type: #"press");
    deuce/display-message(sheet, "");                // clear the message pane
    local method clear-mark-and-move-point (bp :: <basic-bp>, degree) => ()
            let mode = buffer & buffer-major-mode(buffer);
            window-goal-x-position(sheet)
              := if (mode) index->position(bp-line(bp), mode, sheet, bp-index(bp))
                 else 0 end;
            clear-mark!(window: sheet);
            move-point!(bp, window: sheet);
            queue-redisplay(sheet, degree);
            redisplay-window(sheet)
          end method;
    with-command-error-handling ()
      case
        presentation =>
          let object = presentation-object(presentation);
          let type   = presentation-type(presentation);
          when (move-point? & ~diagram-line?(bp-line(bp)))
            clear-mark-and-move-point(bp, $display-point)
          end;
          handle-presentation-event(sheet, object, type,
                                    bp: bp, x: x, y: y,
                                    button: button, modifiers: modifiers,
                                    event-type: #"press");
        bp & gesture-matches?($move-gesture, button, modifiers) =>
          // Move the point to the selected position
          when (~diagram-line?(bp-line(bp)))
            clear-mark-and-move-point(bp, $display-point)
          end;
        bp & gesture-matches?($copy-gesture, button, modifiers) =>
          // Insert the atom under the mouse into the current position
          when (~diagram-line?(bp-line(bp)))
            let (sbp, ebp) = atom-under-bp(bp);
            let bp = window-point(sheet);
            check-read-only(bp);
            with-change-recording (buffer, <paste-change-record>, start-bp: bp)
              when (~start-of-line?(bp)
                    & ~whitespace-char?(bp-character-before(bp))
                    & list-syntax(bp-character-before(bp)) ~= $list-open)
                insert-moving!(bp, ' ')
              end;
              insert-moving!(bp, make-interval(sbp, ebp, in-order?: #t));
              when (~end-of-line?(bp)
                    & ~whitespace-char?(bp-character(bp))
                    & list-syntax(bp-character(bp)) ~= $list-close)
                insert-moving!(bp, ' ')
              end
            end;
            clear-mark-and-move-point(bp, $display-text)
          end;
        otherwise =>
          #f;
      end
    end;
    sheet.%last-event := event
  end
end method handle-event;

define sealed method handle-event
    (sheet :: <deuce-pane>, event :: <double-click-event>) => ()
  let frame = window-frame(sheet);                // ~= sheet-frame for <deuce-gadget>
  with-editor-state-bound (buffer = sheet)
    when (frame-isearch-direction(frame))
      finish-incremental-search(frame)
    end;
    let pointer = event-pointer(event);
    pointer-grabbed?(pointer) := #f;                // lose the pointer grab
    let x = event-x(event);
    let y = event-y(event);
    let button    = event-button(event);
    let modifiers = make-deuce-modifiers(event-modifier-state(event));
    deuce/display-message(sheet, "");                // clear the message pane
    with-command-error-handling ()
      when (gesture-matches?($move-gesture, button, modifiers))
        // Mark the atom under the mouse
        let (presentation, bp, move-point?)
          = presentation-at-position(sheet, x, y,
                                     button: button, modifiers: modifiers,
                                     event-type: #"double-click");
        ignore(move-point?);
        when (bp & ~presentation & ~line-for-display-only?(bp-line(bp)))
          select-atom-under-bp(sheet, bp);
          let mode = buffer & buffer-major-mode(buffer);
          let bp   = window-point(sheet);
          window-goal-x-position(sheet)
            := if (mode) index->position(bp-line(bp), mode, sheet, bp-index(bp))
               else 0 end;
          redisplay-window(sheet)
        end
      end
    end;
    sheet.%last-event := event
  end
end method handle-event;

define sealed method handle-event
    (sheet :: <deuce-pane>, event :: <button-release-event>) => ()
  let frame = window-frame(sheet);                // ~= sheet-frame for <deuce-gadget>
  with-editor-state-bound (buffer = sheet)
    when (frame-isearch-direction(frame))
      finish-incremental-search(frame)
    end;
    let pointer = event-pointer(event);
    pointer-grabbed?(pointer) := #f;                // lose the pointer grab
    let x = event-x(event);
    let y = event-y(event);
    let button    = event-button(event);
    let modifiers = make-deuce-modifiers(event-modifier-state(event));
    let (presentation, bp, move-point?)
      = presentation-at-position(sheet, x, y,
                                 button: button, modifiers: modifiers,
                                 event-type: #"release");
    ignore(move-point?);
    with-command-error-handling ()
      if (instance?(sheet.%last-event, <pointer-drag-event>)
          & gesture-matches?($move-gesture, button, modifiers))
        // Finish marking the selected region.
        when (bp & ~line-for-display-only?(bp-line(bp)))
          let mode = buffer & buffer-major-mode(buffer);
          window-goal-x-position(sheet)
            := if (mode) index->position(bp-line(bp), mode, sheet, bp-index(bp))
               else 0 end;
          move-point!(bp, window: sheet);
          queue-redisplay(sheet, $display-point);
          redisplay-window(sheet)
        end
      else
        when (presentation)
          let object = presentation-object(presentation);
          let type   = presentation-type(presentation);
          handle-presentation-event(sheet, object, type,
                                    bp: bp, x: x, y: y,
                                    button: button, modifiers: modifiers,
                                    event-type: #"release")
        end
      end
    end;
    sheet.%last-event := event
  end
end method handle-event;

define sealed method handle-event
    (sheet :: <deuce-pane>, event :: <pointer-motion-event>) => ()
  with-editor-state-bound (sheet)
    let x = event-x(event);
    let y = event-y(event);
    let cursor = cursor-at-position(sheet, x, y) | #"i-beam";
    sheet-cursor(sheet) := cursor;
    sheet.%last-event := event
  end
end method handle-event;

define sealed method handle-event
    (sheet :: <deuce-pane>, event :: <pointer-drag-event>) => ()
  with-editor-state-bound (sheet)
    let x = event-x(event);
    let y = event-y(event);
    let cursor = cursor-at-position(sheet, x, y) | #"i-beam";
    sheet-cursor(sheet) := cursor;
    let button    = event-button(event);
    let modifiers = make-deuce-modifiers(event-modifier-state(event));
    with-command-error-handling ()
      when (gesture-matches?($move-gesture, button, modifiers))
        // If the pointer went outside of the window, auto-scroll
        //---*** How can we do this without requiring continual moving of the mouse?
        let (left, top, right, bottom) = box-edges(sheet-viewport-region(sheet));
        let dx = if (x < left) x - left elseif (x > right)  x - right  else 0 end;
        let dy = if (y < top)  y - top  elseif (y > bottom) y - bottom else 0 end;
        when (dx ~= 0 | dy ~= 0)
          if (abs(dx) < abs(dy))
            let n = if (y < top) -1 else 1 end;
            scroll-n-lines(sheet, n, move-point?: #t);
          else
            let n = if (x < left) -line-scroll-amount(sheet, #"horizontal")
                    else line-scroll-amount(sheet, #"horizontal") end;
            let (sx, sy) = scroll-position(sheet);
            set-scroll-position(sheet, max(0, sx + n), sy);
          end
        end;
        // Continue marking the selected region
        let (presentation, bp, move-point?)
          = presentation-at-position(sheet, x, y,
                                     button: button, modifiers: modifiers,
                                     event-type: #"drag");
        ignore(move-point?);
        when (bp & ~presentation & bp ~= window-point(sheet))
          // If no mark, set it now so that we get a region
          unless (window-mark(sheet))
            move-mark!(window-point(sheet), window: sheet, volatile?: #t);
          end;
          // Move the point to extend the region.  But first, make sure
          // point is on a real line, otherwise 'update-caret-position'
          // will try to move it!
          when (line-for-display-only?(bp-line(bp)))
            bp := move-over-lines(bp, if (y < top) 1 else -1 end);
          end;
          move-point!(bp, window: sheet);
          queue-redisplay(sheet, $display-region);
          redisplay-window(sheet)
        end
      end
    end;
    sheet.%last-event := event
  end
end method handle-event;


define inline function modifier-key? (keysym) => (modifier? :: <boolean>)
  member?(keysym, #[#"shift", #"control", #"meta", #"alt", #"super", #"hyper",
                    #"alt-gr", #"caps-lock", #"num-lock"])
end function modifier-key?;

// Handle key press events by calling the appropriate Deuce command
define sealed method handle-event
    (sheet :: <deuce-pane>, event :: <key-press-event>) => ()
  let frame = window-frame(sheet);                // ~= sheet-frame for <deuce-gadget>
  with-editor-state-bound (buffer = sheet)
    let keysym    = event-key-name(event);
    let char      = event-character(event);
    let modifiers = make-deuce-modifiers(event-modifier-state(event));
    // Ignore modifier keys, because they're not ever command bindings
    when (char | (keysym & ~modifier-key?(keysym)))
      case
        char & alpha-char?(char)
        & ~zero?(logand(modifiers, deuce/$shift-key)) =>
          // If the shift key is on, make sure the character is upper case
          char := as-uppercase(char);
        char & as(<integer>, char) < as(<integer>, ' ') =>
          // If the character is below the printing ISO Latin-1 range, lose it
          char := #f;
      end;
      frame-command-character(frame) := char;
      frame-command-modifiers(frame) := modifiers;
      with-command-error-handling ()
        when (~frame-isearch-direction(frame)
              | continue-incremental-search(frame, keysym: keysym))
          // Pick up the command table from the command reader state
          let comtab = frame-command-state(frame);
          // Turn off the shift key state for ordinary characters so as
          // not to confuse matters during gesture matching
          let state   = if (char) logand(modifiers, lognot(deuce/$shift-key))
                        else modifiers end;

          let gesture = vector(char | keysym, state);
          let command = find-command(comtab, gesture);
          case
            instance?(command, deuce/<command-table>) =>
              // It's a command prefix (c-X), so change the state
              frame-command-state(frame) := command;
              let modifier = $modifier-key-names[state];
              deuce/display-message(sheet, "%s%s-",
                                    modifier, char | string-capitalize(as(<string>, keysym)));
            command & command-enabled?(command, sheet-frame(sheet)) =>
              // We got a real command, execute it
              when (buffer)
                let mode = buffer & buffer-major-mode(buffer);
                deuce/execute-command(mode, frame, command);
                frame-command-character(frame) := #f;
                frame-command-modifiers(frame) := 0;
              end;
            otherwise =>
              // No such command, reset all of the state
              deuce/display-error-message(sheet, "");
              frame-last-command-type(frame) := #f;
              frame-numeric-arg(frame) := 1;
              frame-numeric-arg-state(frame) := #f;
              frame-last-command(frame)  := cancel-command;
              frame-command-state(frame) := standard-command-table(frame-command-set(frame));
          end
        end
      end
    end;
    sheet.%last-event := event
  end
end method handle-event;


/// A little bit of streams for you...

define sealed method read-character
    (pane :: <deuce-pane>) => (character :: <character>)
  let character = #f;
  until (character)
    let event = read-event(pane);
    when (instance?(event, <key-press-event>))
      let keysym    = event-key-name(event);
      let char      = event-character(event);
      let modifiers = logand(event-modifier-state(event),
                             logior($shift-key, $control-key, $meta-key, $super-key));
      when (char | (keysym & ~modifier-key?(keysym)))
        when (char & alpha-char?(char)
              & ~zero?(logand(modifiers, $shift-key)))
          char      := as-uppercase(char);
          modifiers := logand(modifiers, lognot($shift-key))
        end;
        when (zero?(logand(modifiers, logior($meta-key, $super-key))))
          case
            char & ~zero?(logand(modifiers, $control-key)) =>
              let code = as(<integer>, char);
              case
                code >= as(<integer>, 'a') & code <= as(<integer>, 'z') =>
                  char := as(<byte-character>, code - as(<integer>, 'a') + 1);
                code >= as(<integer>, 'A') & code <= as(<integer>, 'A') =>
                  char := as(<byte-character>, code - as(<integer>, 'A') + 1);
                code = as(<integer>, '@') =>
                  char := as(<byte-character>, #o00);
                code = as(<integer>, '[') =>
                  char := as(<byte-character>, #o33);
                code = as(<integer>, '\\') =>
                  char := as(<byte-character>, #o34);
                code = as(<integer>, ']') =>
                  char := as(<byte-character>, #o35);
                code = as(<integer>, '^') =>
                  char := as(<byte-character>, #o36);
                code = as(<integer>, '_') =>
                  char := as(<byte-character>, #o37);
              end;
            ~char & keysym == #"tab"     & modifiers = 0 =>
              char := '\t';
            ~char & keysym == #"return"  & modifiers = 0 =>
              char := '\r';
            ~char & keysym == #"return"  & modifiers = $shift-key =>
              char := '\n';
            ~char & keysym == #"newline" & modifiers = 0 =>
              char := '\n';
          end;
          character := char
        end
      end
    end
  end;
  character
end method read-character;

define sealed method read-gesture
    (pane :: <deuce-pane>) => (keysym, char, modifiers)
  let gesture? = #f;
  let (keysym, char, modifiers) = values(#f, #f, #f);
  until (gesture?)
    let event = read-event(pane);
    when (instance?(event, <key-press-event>))
      keysym    := event-key-name(event);
      char      := event-character(event);
      modifiers := logand(event-modifier-state(event),
                          logior($shift-key, $control-key, $meta-key, $super-key));
      when (char | (keysym & ~modifier-key?(keysym)))
        when (char & alpha-char?(char)
              & ~zero?(logand(modifiers, $shift-key)))
          // If the shift key is on, make sure the character is upper case
          char      := as-uppercase(char);
          modifiers := logand(modifiers, lognot($shift-key))
        end;
        gesture? := #t
      end
    end
  end;
  values(keysym, char, make-deuce-modifiers(modifiers))
end method read-gesture;


/// DUIM -> Deuce conversions

assert($control-key == deuce/$control-key * 2
       & $meta-key  == deuce/$meta-key * 2
       & $super-key == deuce/$super-key * 2,
       "Deuce and DUIM modifier key constants inconsistent");

assert($left-button     == deuce/$left-button
       & $middle-button == deuce/$middle-button
       & $right-button  == deuce/$right-button,
       "Deuce and DUIM mouse button constants inconsistent");

define function make-deuce-modifiers
    (modifier-state :: <integer>) => (modifiers :: <integer>)
  // Shift the DUIM modifier state to get a Deuce modifier state,
  // then set in the shift key if necessary
  let modifiers = ash(logand(modifier-state,
                             logior($control-key, $meta-key, $super-key)),
                      -1);
  when (logand(modifier-state, $shift-key) ~= 0)
    modifiers := logior(modifiers, deuce/$shift-key)
  end;
  // we gratitiously ignore Hyper, AltGr, CapsLock
  modifiers := logand(modifiers, #xf);
  modifiers
end function make-deuce-modifiers;
