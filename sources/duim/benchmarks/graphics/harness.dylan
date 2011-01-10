Module:    duim-graphics-benchmarks
Author:    Andy Armstrong
Synopsis:  Interactive benchmarks for DUIM graphics
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A simple harness for all of the benchmarks

define variable *benchmark-frames* = make(<stretchy-vector>);

define method find-benchmark-class (class :: <class>)
  block (return)
    for (benchmark in *benchmark-frames*)
      if (benchmark[0] = class)
        return(benchmark)
      end
    end
  end
end method find-benchmark-class;

define method install-benchmark 
    (frame-class :: <class>, title :: <string>)
  let benchmark = find-benchmark-class(frame-class);
  if (benchmark)
    benchmark[1] := title;
  else
    add!(*benchmark-frames*, vector(frame-class, title))
  end;
  frame-class
end method install-benchmark;

define method start-benchmark-frame 
    (frame :: <frame>, class :: <class>, 
     #rest args,
     #key frame-manager: framem)
 => (thread :: <thread>)
  with-busy-cursor (frame)
    let benchmark = find-benchmark-class(class);
    let frame-class = benchmark[0];
    let title = benchmark[1];
    local method create-benchmark-frame () => ()
	    with-abort-restart ()
	      let benchmark-frame
	        = if (benchmark)
		    apply(make, frame-class, title: title, args)
		  else
		    apply(make, class, title: "Benchmark", args)
		  end;
	      start-frame(benchmark-frame)
	    end
	  end method create-benchmark-frame;
    make(<thread>,
	 name: title,
	 function: create-benchmark-frame)
  end
end method start-benchmark-frame;

define method sorted-benchmark-frames ()
  sort(*benchmark-frames*,
       test: method (benchmark1, benchmark2)
               benchmark1[1] < benchmark2[1]
             end)
end method sorted-benchmark-frames;


/// A frame to show them in

define frame <benchmarks-harness> (<simple-frame>)
  pane update (frame)
    make(<push-button>,
	 label: "Update",
	 documentation: "Update the list of available benchmarks",
	 activate-callback: update-benchmarks-harness);
  pane benchmarks (frame)
    begin
      let frames = sorted-benchmark-frames();
      make(<list-box>,
	   documentation: "Double-click on a benchmark name to run it",
	   items: frames,
	   lines: size(frames),
	   label-key: second,
	   value-key: first,
	   activate-callback: method (sheet :: <sheet>)
				let frame = sheet-frame(sheet);
				let benchmark = gadget-value(sheet);
				start-benchmark-frame(frame, benchmark)
			      end)
    end;
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.update;
      frame.benchmarks;
    end;
  layout (frame) frame.main-layout;
end frame <benchmarks-harness>;

define method update-benchmarks-harness 
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  gadget-items(frame.benchmarks) := sorted-benchmark-frames()
end method update-benchmarks-harness;

define method start-benchmarks 
    () => (status-code :: <integer>)
  let frame = make(<benchmarks-harness>, title: "Graphics Benchmarks");
  frame-input-focus(frame) := frame.benchmarks;
  start-frame(frame)
end method start-benchmarks;
