Module:    environment-profiler
Synopsis:  The profiling tool provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some useful constants (move into environment-tools?)

define constant $y-spacing = 6;
define constant $x-spacing = 20;


/// Profiler summary

define frame <profiler-summary> (<simple-frame>)
  slot %valid? :: <boolean> = #f;
  pane summary-page (frame)
    make(<tab-control-page>,
	 label: "Summary",
	 id:    #"summary",
	 child: frame.summary-layout);
  pane summary-layout (frame)
    make(<column-layout>, y-spacing: $y-spacing);
end frame <profiler-summary>;

define method refresh-profiler-page
    (frame :: <profiler-summary>, page == #"summary",
     #key clean? = #f, new-thread? = #t)
 => ()
  let project = frame.ensure-frame-project;
  let application = project.project-application;
  let profile = frame.profiler-application-profile;
  let thread = frame.frame-current-thread;
  let layout = frame.summary-layout;
  debug-message("Refreshing summary page (clean?: %=)", clean?);
  if (clean? | ~frame.%valid?)
    let property-gadgets = make(<stretchy-vector>);
    let group-items :: <stretchy-vector> = make(<stretchy-vector>);
    local method make-stretchy-label
	      (name :: <string>) => (label :: <label>)
	    make(<label>,
		 label:     name,
		 mnemonic:  #f,
		 min-width: 200, max-width: $fill)
	  end method make-stretchy-label,

          method start-new-section
	      () => ()
	    if (size(property-gadgets) > 0)
	      add!(property-gadgets, make(<separator>));
	    end;
	    group-items.size := 0
	  end method start-new-section,

          method add-label-and-value
	      (label, value :: <string>) => ()
	    let label
	      = if (instance?(label, <sheet>))
		  label
		else
		  make(<label>, label: label, mnemonic: #f)
		end;
	    add!(group-items, 
		 vector(label, make-stretchy-label(value)));
	  end add-label-and-value,

          method finish-section
	      (#key y-alignment = #"top") => ()
	    add!(property-gadgets,
		 make(<table-layout>,
		      contents: copy-sequence(group-items),
		      columns: 2,
		      x-spacing: $x-spacing,
		      y-spacing: $y-spacing,
		      x-alignment: #[#"left", #"left"],
		      y-alignment: y-alignment))
	  end finish-section;

    // Name group
    begin
      start-new-section();
      let filename = application & application-filename(application);
      add-label-and-value("Application:",
			  if (filename)
			    as(<string>, filename)
			  else
			    "[none]"
			  end);
      finish-section()
    end;

    // Profiling results group
    if (profile)
      start-new-section();
      let profile = frame.profiler-application-profile;
      add-label-and-value("Threads:", 
			  integer-to-string(profile.application-profile-threads.size));
      add-label-and-value("Snapshots:", 
			  integer-to-string(profile.application-profile-snapshots.size));
      add-label-and-value("Elapsed time:", 
			  time-to-string(profile.application-total-wall-time));
      add-label-and-value("Page faults:", 
			  integer-to-string(profile.application-total-page-faults));
      finish-section()
    end;

    layout.sheet-children := property-gadgets;
    let mapped? = sheet-mapped?(layout);
    //---*** andrewa: what should we really be doing here?
    unless (relayout-parent(layout))
      relayout-children(layout)
    end;
    when (mapped?)
      sheet-mapped?(layout) := #t
    end;
    frame.%valid? := #t
  end
end method refresh-profiler-page;

define method invalidate-profiler-page
    (frame :: <profiler-summary>, page == #"summary") => ()
  frame.%valid? := #f
end method invalidate-profiler-page;
