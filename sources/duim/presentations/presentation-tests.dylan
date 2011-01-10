Module:       DUIM-Presentations-Internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Presentations test harness

//---*** This is all stolen intact from duim/examples/harness.dylan

define variable *example-frames* = make(<stretchy-vector>);

define method find-example-class (class :: <class>)
  block (return)
    for (example in *example-frames*)
      if (example[0] = class)
        return(example)
      end
    end
  end
end method find-example-class;

define method install-example 
    (frame-class :: <class>, title :: <string>)
  let example = find-example-class(frame-class);
  if (example)
    example[1] := title;
  else
    add!(*example-frames*, vector(frame-class, title))
  end;
  frame-class
end method install-example;

define method start-example-frame 
    (frame :: <frame>, class :: <class>, 
     #rest args,
     #key frame-manager: framem)
 => (example-frame :: <frame>)
  let example-frame
    = with-busy-cursor (frame)
        let example = find-example-class(class);
        let frame-class = example[0];
        let title = example[1];
        let example-frame
          = if (example)
	      apply(make, frame-class, owner: frame, title: title, args)
	    else
	      apply(make, class, owner: frame, title: "Example", args)
	    end;
        frame-mapped?(example-frame) := #t;
        example-frame
      end;
  start-frame(example-frame);
  example-frame
end method start-example-frame;

define method sorted-example-frames ()
  sort(*example-frames*,
       test: method (example1, example2)
               example1[1] < example2[1]
             end)
end method sorted-example-frames;

define frame <examples-harness> (<simple-frame>)
  pane update (frame)
    make(<push-button>,
	      label: "Update",
	      activate-callback: update-examples-harness);
  pane examples (frame)
    make(<list-control>,
         items: sorted-example-frames(),
         label-key: second,
         value-key: first,
         activate-callback: method (sheet :: <sheet>)
                              let frame = sheet-frame(sheet);
                              let example = gadget-value(sheet);
                              start-example-frame(frame, example)
                            end);
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.update;
      frame.examples;
    end;
  layout (frame) frame.main-layout;
end frame <examples-harness>;

define method update-examples-harness 
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  gadget-items(frame.examples) := sorted-example-frames()
end method update-examples-harness;

define method start-examples 
    () => (status-code :: false-or(<integer>))
  let frame = make(<examples-harness>, title: "Presentations Examples");
  start-frame(frame);
  #f
end method start-examples;


/// Simple presentation tests


/// Initialization

start-examples();

// end
