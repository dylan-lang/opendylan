Module:    plonker
Synopsis:  Play with MIDI
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Application state.

define class <keyboard> (<object>)
  slot keyboard-midi-device,
    required-init-keyword: midi-device:;
  slot keyboard-midi-channel,
    required-init-keyword: midi-channel:;
  slot keyboard-base-note :: <integer> = 40,
    init-keyword: base-note:;
  slot keyboard-volume :: <integer> = 127,
    init-keyword: volume:;
end class;

define method initialize (keyboard :: <keyboard>, #key) => ()
  next-method();
  keyboard-instrument(keyboard) := midi-name(find-midi-instrument(0));
end method;

define method keyboard-instrument-setter
    (name :: <byte-string>, keyboard :: <keyboard>) => (name :: <byte-string>)
  let instrument = find-midi-instrument(name);
  select-midi-instrument
    (keyboard-midi-device(keyboard), keyboard-midi-channel(keyboard),
       instrument);
  name                    
end method;

define method keyboard-instruments 
    (keyboard :: <keyboard>) => (instruments :: <list>)
  let instruments = #();
  do-midi-instruments(method (instrument) instruments := pair(midi-name(instrument), instruments) end);
  reverse!(instruments);
end method;

define method keyboard-play-note 
    (keyboard :: <keyboard>, note :: <integer>) => ()
  midi-on
    (keyboard-midi-device(keyboard), keyboard-midi-channel(keyboard),
       keyboard-base-note(keyboard) + note,
       keyboard-volume(keyboard));
end method;

define method keyboard-release-note 
    (keyboard :: <keyboard>, note :: <integer>) => ()
  midi-off
    (keyboard-midi-device(keyboard), keyboard-midi-channel(keyboard),
       keyboard-base-note(keyboard) + note);
end method;

define method keyboard-volume-update
    (keyboard :: <keyboard>, vol :: <integer>) => ()
  send-midi-message
    (keyboard-midi-device(keyboard), $MIDI-parameter, 
       keyboard-midi-channel(keyboard), 7 /* $midi-volume-parameter */, vol);
end method;

define method keyboard-bend-note 
    (keyboard :: <keyboard>, bend :: <integer>) => ()
  midi-pitch-bend
    (keyboard-midi-device(keyboard), keyboard-midi-channel(keyboard),
       bend);
end method;

define method keyboard-balance
    (keyboard :: <keyboard>, balance :: <integer>) => ()
  send-midi-message
    (keyboard-midi-device(keyboard), $MIDI-parameter, 
       keyboard-midi-channel(keyboard), $midi-balance-parameter, balance);
end method;

define method compute-qwerty-key-map () => (table :: <table>)
  let row1 = "\\zxcvbnm,./";
  let row2 = "asdfghjkl;'#";
  let row3 = "qwertyuiop[]";
  let row4 = "234567890-=";
  let table = make(<table>);
  let note = 0;
  let scale-cursor = $major-scale-naturals-cycle;
  local method symbolize (c :: <character>) => (sym :: <symbol>)
    as(<symbol>, make(<byte-string>, size: 1, fill: c))
  end method;
  local method do-row-pair (row1, row2)
    for (natural in row1, accidental in row2)
      let note-info = scale-cursor.head;
      table[symbolize(natural)] := note;
      note := note + 1; scale-cursor := scale-cursor.tail;
      if (member?(#"sharp", note-info))
        table[symbolize(accidental)] := note;
        note := note + 1;
      end;
    end;
  end method;
  do-row-pair(row1, row2);
  do-row-pair(row3, row4);
  table
end method;

define constant $qwerty-key-map = compute-qwerty-key-map();

define method qwerty-key-map (key :: <symbol>) => (note :: false-or(<integer>))
  element($qwerty-key-map, key, default: #f)
end method;

//// Application frame.

define frame <keyboard-frame> (<simple-frame>)

  // Slots.

  slot frame-model,
    required-init-keyword: model:;

  // Panes.

  pane file-menu (frame)
    make(<menu>,
         label: "&File",
         children:
           vector(make(<menu-button>,
                       label: "New Keyboard",
                       activate-callback: 
                         method (#rest args)
                           spawn-keyboard-frame();
                         end)));

  pane instruments-pane (frame)
    make(<option-box>,
         documentation: "Instrument",
         items: keyboard-instruments(frame-model(frame)),
         scroll-bars: #"vertical", 
         accepts-focus?: #f,
         value-changed-callback:
           method (pane :: <option-box>)
             keyboard-instrument(frame-model(sheet-frame(pane))) 
               := gadget-value(pane);
             frame-input-focus(frame) := keyboard-pane(frame);
           end);

  pane keyboard-pane (frame)
    frame-input-focus(frame)
      := make(<keyboard-pane>, 
              activate-callback: 
                method (sheet :: <keyboard-pane>, note :: <integer>) => ()
                  keyboard-play-note(frame-model(sheet-frame(sheet)), note);
                end,
              deactivate-callback: 
              method (sheet :: <keyboard-pane>, note :: <integer>) => ()
                keyboard-release-note(frame-model(sheet-frame(sheet)), note);
              end,
              key-map-callback:
                method (sheet :: <keyboard-pane>, key :: <symbol>) 
                 => (note :: false-or(<integer>))
                  qwerty-key-map(key)
                end);

  pane volume-pane (frame)
    make(<slider>, 
         documentation: "Volume",
         orientation: #"vertical", 
         value-range: range(from: 127, to: 0, by: -1),
         value: 127,
         accepts-focus?: #f,
         value-changing-callback:
           method (sheet :: <slider>) => ()
             keyboard-volume-update
               (frame-model(sheet-frame(sheet)), gadget-value(sheet));
           end);

  pane pitch-bend-pane (frame)
    make(<slider>, 
         documentation: "Pitch bend",
         orientation: #"vertical", 
         value-range: range(from: #x3FFF, to: 1, by: -1),
         value: #x2000,
         accepts-focus?: #f,
         value-changing-callback:
           method (sheet :: <slider>) => ()
             keyboard-bend-note
               (frame-model(sheet-frame(sheet)), gadget-value(sheet));
           end);

  pane balance-pane (frame)
    make(<slider>, 
         documentation: "Balance",
         orientation: #"horizontal", 
         value-range: 
           range(from: $midi-balance-full-left, to: $midi-balance-full-right),
         value: $midi-balance-central,
         accepts-focus?: #f,
         value-changing-callback:
           method (sheet :: <slider>) => ()
             keyboard-balance
               (frame-model(sheet-frame(sheet)), gadget-value(sheet));
           end);

  pane status-bar (frame)
    make(<status-bar>);

  pane main-layout (frame)
    vertically (spacing: 2)
      instruments-pane(frame);
      horizontally (spacing: 2)
        volume-pane(frame);
        pitch-bend-pane(frame);
        vertically (spacing: 2)
          keyboard-pane(frame);
          balance-pane(frame);
        end;
      end;
    end;

  // Standard element install.

  menu-bar (frame)
    make(<menu-bar>,
         children: vector(file-menu(frame)));
  
  layout (frame)
    main-layout(frame);

  status-bar (frame)
    status-bar(frame);

end frame;

// TODO: Locking, channel re-use, max channel check.

define variable *next-free-channel* = 0;

define method keyboard-frame (#key channel = #f)
  let dev = default-midi-device();
  let channel-no = *next-free-channel*;
  let channel = midi-channels(dev)[channel-no];
  *next-free-channel* := *next-free-channel* + 1;
  // with-open-midi-device (dev)
    let state = make(<keyboard>, midi-device: dev, midi-channel: channel);
    let frame = make(<keyboard-frame>, 
                     title: format-to-string("MIDI channel %s", channel-no),
                     model: state,
                     width: 540, height: 190);
    start-frame(frame);
  // end;
end method;

define method spawn-keyboard-frame (#rest args)
  make-application-thread(function: keyboard-frame);
end method;

define method first-keyboard-frame ()
  let dev = default-midi-device();
  let open = #f;
  block ()
    open-midi-device(dev);
    open := #t;
    spawn-keyboard-frame();
    wait-for-shutdown();
  cleanup
    if (open) close-midi-device(dev) end;
  end;
end method;
