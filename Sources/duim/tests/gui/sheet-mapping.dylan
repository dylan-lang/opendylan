Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sheet mapping test

define constant $initial-selection = #[1];

define frame <sheet-mapping-frame> (<simple-frame>)
  pane mapping-buttons (frame)
    make(<check-box>,
         items: #("One", "Two", "Three", "Four", "Five"),
         selection: $initial-selection,
         value-changed-callback: method (gadget)
                                   update-sheet-mapping(sheet-frame(gadget))
                                 end);
  pane sheet-one (frame)
    make(<radio-box>, items: #(1, 2, 3),
         x:  50,   y: 50, withdrawn?: #t);
  pane sheet-two (frame)
    make(<text-field>, text: "Sheet Two",
         x: 250,  y: 50, withdrawn?: #f);
  pane sheet-three (frame)
    make(<label>, label: "Sheet Three",
         x:  50,  y: 150,  withdrawn?: #t);
  pane sheet-four (frame)
    make(<progress-bar>, value: 10, value-range: range(from: 0, to: 100),
         x: 250,  y: 150,  withdrawn?: #t);
  pane sheet-five (frame)
    make(<push-button>, label: "Sheet Five",
         x:  50,  y: 250,  withdrawn?: #t);
  pane pinboard (frame)
    make(<pinboard-layout>,
         width: 400, height: 300,
         children: vector(frame.sheet-one,
                          frame.sheet-two,
                          frame.sheet-three,
                          frame.sheet-four,
                          frame.sheet-five));
  pane main-layout (frame)
    vertically (spacing: 5, x-alignment: #"centre")
      frame.mapping-buttons;
      with-border (type: #"sunken")
        frame.pinboard
      end;
    end;
  layout (frame) frame.main-layout;
end frame <sheet-mapping-frame>;

define method update-sheet-mapping
    (frame :: <sheet-mapping-frame>) => ()
  let buttons = frame.mapping-buttons;
  let pinboard = frame.pinboard;
  let selection = gadget-selection(buttons);
  for (child in sheet-children(pinboard),
       index from 0)
    let mapped? = member?(index, selection);
    sheet-withdrawn?(child) := ~mapped?;
  end;
  relayout-children(pinboard);
  for (child in sheet-children(pinboard),
       index from 0)
    let mapped? = member?(index, selection);
    if (mapped?)
      sheet-mapped?(child) := #t
    end;
  end;
end method update-sheet-mapping;

install-test(<sheet-mapping-frame>, "Sheet mapping");
