Module:    environment-reports
Author:    Andy Armstrong
Synopsis:  Profile report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profile reports

define abstract class <profile-report> (<project-report>)
  constant slot report-qualify-names? :: <boolean> = #f,
    init-keyword: qualify-names?:;
//---*** Not currently used...
//  slot report-profile-results :: false-or(<profile-results>) = #f;
//---*** Not currently used...
//  constant slot report-include-source? :: <boolean> = #t,
//    init-keyword: include-source?:;
end class <profile-report>;

/*---*** Not currently used...
define abstract class <profile-results> (<object>)
end class <profile-results>;
*/

define class <raw-profile-report> (<profile-report>)
end class <raw-profile-report>;

install-report(#"raw-profile", "Raw profile", 
	       <raw-profile-report>);

define class <profile-summary-report> (<profile-report>)
end class <profile-summary-report>;

install-report(#"profile-summary", "Profile summary", 
	       <profile-summary-report>);

define class <profile-call-history-report> (<profile-report>)
  //---*** Not currently used!
  // constant slot report-cut-off-percentage :: <float> = 3.0,
  //   init-keyword: cut-off-percentage:;
end class <profile-call-history-report>;

/*---*** Not yet finished!
install-report(#"profile-call-history", "Profile call history", 
	       <profile-call-history-report>);
*/

/*---*** Not yet implemented!
define class <full-profile-report> (<profile-report>)
end class <full-profile-report>;

install-report(#"full-profile", "Full profile", <full-profile-report>);
*/


/// Raw profile

define method write-report-as
    (stream :: <stream>, report :: <raw-profile-report>, _format == #"text")
 => ()
  let project = report.report-project;
  let application = project.project-application;
  let profile = project.project-last-profile;
  let snapshot-index :: <integer> = 1;
  let elapsed-wall-time :: <integer> = 0;
  let elapsed-page-faults :: <integer> = 0;
  let total-allocation :: <integer> = 0;
  let thread-totals :: <object-table> = make(<object-table>);
  format(stream, "Profile results for %s\n\n",
	 application.application-filename);
  format(stream, "      Snapshots: %d\n",
	 profile.application-total-snapshots);
  format(stream, "      Wall time: %d\n",
	 profile.application-total-wall-time);
  format(stream, "     Page faults: %d\n",
	 profile.application-total-page-faults);
  new-line(stream);
  do-application-profile-snapshots
    (method (snapshot :: <application-snapshot>)
       format(stream, "%s\n", $report-separator);
       let wall-time   = snapshot.application-snapshot-wall-time;
       let page-faults = snapshot.application-snapshot-page-faults;
       increment!(elapsed-wall-time,   wall-time);
       increment!(elapsed-page-faults, page-faults);
       format(stream, "Snapshot %d\n", snapshot-index);
       format(stream, "       Wall time: %s [total %s]\n",
	      integer-to-string(wall-time, size: 7, fill: ' '), 
	      integer-to-string(elapsed-wall-time, size: 7, fill: ' '));
       format(stream, "     Page faults: %s [total %s]\n",
	      integer-to-string(page-faults, size: 7, fill: ' '),
	      integer-to-string(elapsed-page-faults, size: 7, fill: ' '));
       new-line(stream);
       do-application-snapshot-thread-snapshots
	 (method (snapshot :: <thread-snapshot>)
	    let thread = snapshot.thread-snapshot-thread;
	    format(stream, "Snapshot: %s\n", 
		   profile-object-name(report, thread));
	    let cpu-time   = snapshot.thread-snapshot-cpu-time;
	    let allocation = snapshot.thread-snapshot-allocation;
	    let class      = snapshot.thread-snapshot-allocated-class;
	    let thread-elapsed-cpu-time 
	      = element(thread-totals, #"cpu", default: 0);
	    let thread-total-allocation
	      = element(thread-totals, #"allocation", default: 0);
	    element(thread-totals, #"cpu")
	      := thread-elapsed-cpu-time + cpu-time;
	    element(thread-totals, #"allocation")
	      := thread-total-allocation + allocation;
	    increment!(total-allocation, allocation);
	    format(stream, "        CPU time: %s [thread total %s]\n",
		   integer-to-string(cpu-time, size: 7, fill: ' '),
		   integer-to-string(thread-elapsed-cpu-time, size: 7, fill: ' '));
	    format(stream, "      Allocation: %s [thread total %s] [total %s]\n",
		   integer-to-string(allocation, size: 7, fill: ' '),
		   integer-to-string(thread-total-allocation, size: 7, fill: ' '),
		   integer-to-string(total-allocation, size: 7, fill: ' '));
	    if (class)
	      format(stream, "           Class: %s\n",
		     profile-object-name(report, class))
	    end;
	    new-line(stream);
	    let frame-index :: <integer> = 1;
	    do-thread-snapshot-functions
	      (method 
		   (form :: <application-code-object>,
		    location :: false-or(<source-location>))
		 let name = profile-object-name(report, form);
		 format(stream, "%s. %s\n",
			integer-to-string(frame-index, size: 5, fill: ' '),
			name);
		 increment!(frame-index)
	       end,
	       application, snapshot);
	    format(stream, "\n\n");
	  end,
	  snapshot);
       increment!(snapshot-index)
     end,
     profile)
end method write-report-as;

/*
define method write-report-as
    (stream :: <stream>, report :: <raw-profile-report>, _format == #"html")
 => ()
  let title = format-to-string("%s Bug Report", release-product-name());
  with-html-output (stream, title)
    for (section in $profile-report-sections)
      let section-title   = section[0];
      let section-keyword = section[1];
      write-html(stream,
		 #"h2", section-title, #"/h2", '\n');
      write-html(stream,
		 '\n', #"p", '\n');
      write-html-profile-report-section(stream, report, section-keyword)
    end
  end
end method write-report-as;
*/


/// Profile summary

define abstract class <profile-info> (<object>)
end class <profile-info>;

define sealed domain make (subclass(<profile-info>));
define sealed domain initialize (<profile-info>);

define class <profile-summary-info> (<profile-info>)
  constant slot info-threads :: <simple-object-vector>,
    required-init-keyword: threads:;
  constant slot info-count :: <integer> = 0,
    required-init-keyword: count:;
  constant slot info-wall-time :: <integer>,
    required-init-keyword: wall-time:;
  constant slot info-page-faults :: <integer>,
    required-init-keyword: page-faults:;
end class <profile-summary-info>;

define class <profile-thread-info> (<profile-info>)
  constant slot info-thread :: <thread-object>,
    required-init-keyword: thread:;
  constant slot info-objects :: <object-table> = make(<object-table>);
  slot info-count :: <integer> = 0;
  slot info-cpu-time :: <integer> = 0;
  slot info-allocation :: <integer> = 0;
end class <profile-thread-info>;

define abstract class <profile-object-info> (<profile-info>)
  slot info-count :: <integer> = 0;
  slot info-cpu-time :: <integer> = 0;
  slot info-allocation :: <integer> = 0;
end class <profile-object-info>;

define class <profile-function-info> (<profile-object-info>)
  constant slot info-function :: <environment-object>,
    required-init-keyword: function:;
end class <profile-function-info>;

define class <profile-class-info> (<profile-object-info>)
  constant slot info-class :: <class-object>,
    required-init-keyword: class:;
end class <profile-class-info>;

define method process-profile-summary
    (project :: <project-object>, profile :: <application-profile>,
     #key type :: <symbol> = #"function",
          show-foreign-functions? :: <boolean> = #f)
 => (summary :: <profile-summary-info>)
  let application = project.project-application;
  let threads :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let total-count       :: <integer> = 0;
  let total-wall-time   :: <integer> = 0;
  let total-page-faults :: <integer> = 0;
  do-application-profile-threads
    (method (thread :: <thread-object>)
       let thread-info = make(<profile-thread-info>, thread: thread);
       add!(threads, thread-info);
       let objects = thread-info.info-objects;
       do-thread-profile-snapshots
	 (method 
	      (application-snapshot :: <application-snapshot>,
	       thread-snapshot :: <thread-snapshot>)
	    let cpu-time = thread-snapshot.thread-snapshot-cpu-time;
	    let wall-time = application-snapshot.application-snapshot-wall-time;
	    let allocation = thread-snapshot.thread-snapshot-allocation;
	    let page-faults = application-snapshot.application-snapshot-page-faults;
	    increment!(total-wall-time,   wall-time);
	    increment!(total-page-faults, page-faults);
	    increment!(thread-info.info-count);
	    increment!(thread-info.info-cpu-time,   cpu-time);
	    increment!(thread-info.info-allocation, allocation);
	    select (type)
	      #"function" =>
	        if (cpu-time >= 0 | allocation >= 0)
		  block (return)
		    do-thread-snapshot-functions
		      (method 
			   (form :: <environment-object>,
			    location :: false-or(<source-location>))
			 unless (hidden-function?
				   (project, form,
				    show-foreign-functions?: 
				      show-foreign-functions?))
			   let info
			     = element(objects, form, default: #f)
			         | begin
				     objects[form]
				       := make(<profile-function-info>,
					       function: form)
				   end;
			   increment!(info.info-count);
			   increment!(info.info-cpu-time,   cpu-time);
			   increment!(info.info-allocation, allocation);
			   return()
			 end
		       end,
		       application, thread-snapshot)
		  end
		end;
	      #"class" =>
		let class = thread-snapshot.thread-snapshot-allocated-class;
		if (class)
		  let info
		    = element(objects, class, default: #f)
		        | begin
			    objects[class]
			      := make(<profile-class-info>,
				      class: class)
			  end;
		  increment!(info.info-count);
		  increment!(info.info-cpu-time,   cpu-time);
		  increment!(info.info-allocation, allocation);
		end;
	    end
	  end,
	  application, profile, thread)
     end,
     profile);
  make(<profile-summary-info>,
       threads:     as(<simple-object-vector>, threads),
       total-count: total-count,
       wall-time:   total-wall-time,
       page-faults: total-page-faults)
end method process-profile-summary;

define method write-report-as
    (stream :: <stream>, report :: <profile-summary-report>, _format == #"text")
 => ()
  let project = report.report-project;
  let profile = project.project-last-profile;
  let info = process-profile-summary(project, profile);
  let count = 20;
  format(stream, "Profile summary\n\n");
  format(stream, "  Totals:\n\n");
  format(stream, "    Wall time: %d\n", info.info-wall-time);
  format(stream, "  Page faults: %d\n", info.info-page-faults);
  for (thread-info :: <profile-thread-info> in info.info-threads)
    let thread = thread-info.info-thread;
    format(stream, "%s\n", $report-separator);
    format(stream, "%s\n",
	   profile-object-name(report, thread));
    format(stream, "  Totals:\n\n");
    format(stream, "      Samples: %d\n", thread-info.info-count);
    format(stream, "     CPU time: %d\n", thread-info.info-cpu-time);
    format(stream, "   Allocation: %d\n", thread-info.info-allocation);
    let functions = as(<simple-object-vector>, thread-info.info-objects);
    local
      method print-summary
	  (title :: <string>, getter :: <function>) => ()
	format(stream, "\n  %s:\n\n", title);
	let functions
	  = sort(functions,
		 test: method 
			   (f1 :: <profile-function-info>,
			    f2 :: <profile-function-info>)
			 f1.getter > f2.getter
		       end);
	for (function-info :: <profile-function-info> in functions,
	    index from 1 to count)
	  format(stream, "  %s. %s [%s] %s\n",
		 integer-to-string(index, size: 5, fill: ' '),
		 integer-to-string(function-info.getter, size: 7, fill: ' '),
		 integer-to-string(function-info.info-count, size: 5, fill: ' '),
		 profile-object-name(report, function-info.info-function))
	end
      end method print-summary;

    print-summary("Exclusive stack summary",      info-cpu-time);
    print-summary("Exclusive allocation summary", info-allocation);
  end
end method write-report-as;

/*
define method write-report-as
    (stream :: <stream>, report :: <profile-summary-report>, _format == #"html") => ()
  let title = format-to-string("%s Bug Report", release-product-name());
  with-html-output (stream, title)
    for (section in $profile-report-sections)
      let section-title   = section[0];
      let section-keyword = section[1];
      write-html(stream,
		 #"h2", section-title, #"/h2", '\n');
      write-html(stream,
		 '\n', #"p", '\n');
      write-html-profile-report-section(stream, report, section-keyword)
    end
  end
end method write-report-as;
*/


/// Time line report

define method write-report-as
    (stream :: <stream>, report :: <profile-call-history-report>, _format == #"text") => ()
  let project = report.report-project;
  let application = project.project-application;
  let profile = project.project-last-profile;
  let thread-index :: <integer> = 1;
  do-application-profile-threads
    (method (thread :: <thread-object>)
       format(stream, "%s\n", $report-separator);
       format(stream, "%s\n\n", 
	      profile-object-name(report, thread));
       write-thread-report(stream, report, thread, _format);
       increment!(thread-index)
     end,
     profile)
end method write-report-as;

/*
define method write-report-as
    (stream :: <stream>, report :: <profile-call-history-report>,
     _format == #"html")
 => ()
  let title = format-to-string("%s Bug Report", release-product-name());
  with-html-output (stream, title)
    for (section in $profile-report-sections)
      let section-title   = section[0];
      let section-keyword = section[1];
      write-html(stream,
		 #"h2", section-title, #"/h2", '\n');
      write-html(stream,
		 '\n', #"p", '\n');
      write-html-profile-report-section(stream, report, section-keyword)
    end
  end
end method write-report-as;
*/

define class <profile-call-history> (<object>)
  sealed constant slot call-history-root-references :: <simple-object-vector>,
    required-init-keyword: root-references:;
  sealed constant slot call-history-total-cpu-time :: <integer>,
    required-init-keyword: total-cpu-time:;
  sealed constant slot call-history-total-wall-time :: <integer>,
    required-init-keyword: total-wall-time:;
end class <profile-call-history>;

define sealed domain make (subclass(<profile-call-history>));
define sealed domain initialize (<profile-call-history>);

define class <profile-frame-history> (<object>)
  sealed constant slot profile-frame-frame :: <thread-frame-snapshot>,
    required-init-keyword: frame:;
  sealed constant slot profile-frame-references :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  sealed constant slot profile-frame-start-cpu-time :: <integer> = 0,
    init-keyword: start-cpu-time:;
  sealed constant slot profile-frame-start-wall-time :: <integer> = 0,
    init-keyword: start-wall-time:;
  sealed slot profile-frame-cpu-time :: <integer> = 0,
    init-keyword: cpu-time:;
  sealed slot profile-frame-wall-time :: <integer> = 0,
    init-keyword: wall-time:;
  sealed slot profile-frame-allocation :: <integer> = 0,
    init-keyword: allocation:;
end class <profile-frame-history>;

define sealed domain make (subclass(<profile-frame-history>));
define sealed domain initialize (<profile-frame-history>);

define class <profile-frame-allocated-class> (<object>)
  sealed constant slot profile-frame-allocated-class :: <class-object>,
    required-init-keyword: allocated-class:;
  sealed constant slot profile-frame-source-location :: false-or(<source-location>),
    required-init-keyword: source-location:;
end class <profile-frame-allocated-class>;

define sealed domain make (subclass(<profile-frame-allocated-class>));
define sealed domain initialize (<profile-frame-allocated-class>);

define method process-profile-call-history
    (project :: <project-object>, profile :: <application-profile>,
     thread :: <thread-object>,
     #key show-foreign-functions? :: <boolean> = #f)
 => (history :: <profile-call-history>)
  let profile = project.project-last-profile;
  let application = project.project-application;
  let stack :: <deque> = make(<deque>);
  let elapsed-cpu-time :: <integer> = 0;
  let elapsed-wall-time :: <integer> = 0;
  let root-references :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  do-application-profile-snapshots
    (method (snapshot :: <application-snapshot>)
       let thread-snapshot 
	 = application-snapshot-thread-snapshot(snapshot, thread);
       let wall-time   = snapshot.application-snapshot-wall-time;
       let page-faults = snapshot.application-snapshot-page-faults;
       increment!(elapsed-wall-time, wall-time);
       if (thread-snapshot)
	 let cpu-time   = thread-snapshot.thread-snapshot-cpu-time;
	 let allocation = thread-snapshot.thread-snapshot-allocation;
	 let class      = thread-snapshot.thread-snapshot-allocated-class;
	 let frames     = thread-snapshot-frame-snapshots(application, thread-snapshot);
	 let stack-index :: <integer> = stack.size - 1;
	 let frame-index :: <integer> = frames.size - 1;
	 increment!(elapsed-cpu-time, cpu-time);
	 // Increment the counts for all shared items on the stack
	 block (return)
	   while (stack-index >= 0 & frame-index >= 0)
	     let frame = frames[frame-index];
	     let info :: <profile-frame-history> = stack[stack-index];
	     if (info.profile-frame-frame ~== frame)
	       return()
	     end;
	     increment!(info.profile-frame-cpu-time,   cpu-time);
	     increment!(info.profile-frame-allocation, allocation);
	     increment!(info.profile-frame-wall-time,  wall-time);
	     decrement!(frame-index);
	     decrement!(stack-index)
	   end
	 end;
	 // Remove any obsolete frames from the stack
	 while (stack-index >= 0)
	   pop(stack);
	   decrement!(stack-index)
	 end;
	 let top-of-stack = ~empty?(stack) & stack[0];
	 // Now pop on the new ones
	 while (frame-index >= 0)
	   let frame = frames[frame-index];
	   let info
	     = make(<profile-frame-history>, 
		    frame:           frame,
		    start-wall-time: elapsed-wall-time,
		    start-cpu-time:  elapsed-cpu-time,
		    cpu-time:        cpu-time,
		    wall-time:       wall-time,
		    allocation:      allocation);
	   if (top-of-stack)
	     add!(top-of-stack.profile-frame-references, info)
	   else
	     add!(root-references, info)
	   end;
	   top-of-stack := info;
	   push(stack, info);
	   decrement!(frame-index)
	 end;
	 // Add any allocated class to the first non-internal call on
	 // the stack.
	 if (class & top-of-stack)
	   block (return)
	     for (index :: <integer> from 0,
		  function-call :: <profile-frame-history> in stack)
	       if (index > 0)
		 let frame = function-call.profile-frame-frame;
		 unless (hidden-function?
			   (project, frame.frame-snapshot-function,
			    show-foreign-functions?: show-foreign-functions?))
		   add!(function-call.profile-frame-references,
			make(<profile-frame-allocated-class>,
			     allocated-class: class,
			     source-location: frame.frame-snapshot-source-location));
		   return()
		 end
	       end
	     end
	   end
	 end
       else
	 for (function-call :: <profile-frame-history> in stack)
	   increment!(function-call.profile-frame-wall-time, wall-time)
	 end
       end
     end,
     profile);
  make(<profile-call-history>,
       root-references: as(<simple-object-vector>, root-references),
       total-cpu-time:  elapsed-cpu-time,
       total-wall-time: elapsed-wall-time)
end method process-profile-call-history;

define method write-thread-report
    (stream :: <stream>, report :: <profile-call-history-report>,
     thread :: <thread-object>, _format == #"text")
 => ()
  error("Not yet implemented!")
end method write-thread-report;


/// Full profile

/*
define method write-report-as
    (stream :: <stream>, report :: <full-profile-report>, _format == #"text") => ()
  format(stream, "%s\n", $profile-report-first-line);
  for (section in $profile-report-sections)
    let section-title   = section[0];
    let section-keyword = section[1];
    format(stream, "%s\n", $report-separator);
    format(stream, "%s:\n\n", as-uppercase(section-title));
    write-profile-report-section(stream, report, section-keyword)
  end;
  format(stream, "%s\n", $report-separator);
  format(stream, "%s\n", $profile-report-last-line)
end method write-report-as;

define method write-report-as
    (stream :: <stream>, report :: <full-profile-report>, _format == #"html") => ()
  let title = format-to-string("%s Bug Report", release-product-name());
  with-html-output (stream, title)
    for (section in $profile-report-sections)
      let section-title   = section[0];
      let section-keyword = section[1];
      write-html(stream,
		 #"h2", section-title, #"/h2", '\n');
      write-html(stream,
		 '\n', #"p", '\n');
      write-html-profile-report-section(stream, report, section-keyword)
    end
  end
end method write-report-as;


/// Profile processing

define constant $profile-results = make(<table>, weak?: #t);

define class <profile-results> (<object>)
  constant slot application-profile :: <application-profile>,
    required-init-keyword: profile:;
  constant slot thread-profile-results :: <sequence>,
    required-init-keyword: results:;
end class <profile-results>;

define sealed domain make (subclass(<profile-results>));
define sealed domain initialize (<profile-results>);

define class <thread-profile-results> (<object>)
end class <thread-profile-results>;

define sealed domain make (subclass(<thread-profile-results>));
define sealed domain initialize (<thread-profile-results>);

define method profile-results
    (project :: <project-object>, profile :: <application-profile>)
 => (results :: <profile-results>)
  element($profile-results, profile, default: #f)
    | begin
	let results = process-profile-results(project, profile);
	$profile-results[profile] := results
      end
end method profile-results;

define method process-profile-results
    (project :: <project-object>, profile :: <application-profile>)
 => (results :: <profile-results>)
  let thread-profile-results = make(<stretchy-vector>);
  do-application-profile-threads
    (method (thread :: <thread-object>)
       add!(thread-profile-results,
	    process-thread-profile-results(project, thread))
     end,
     profile);
  make(<profile-results>,
       profile: profile,
       results: thread-profile-results)
end method process-profile-results;

define method process-thread-profile-results
    (project :: <project-object>, thread :: <thread-object>)
 => (results :: <thread-profile-results>)
  make(<thread-profile-results>)
end method process-thread-profile-results;
*/


/// Utilities

define method profile-object-name
    (report :: <profile-report>, object :: <environment-object>)
 => (name :: <string>)
  let project = report.report-project;
  let qualify-names? = report.report-qualify-names?;
  environment-object-display-name
    (project, object, #f, qualify-names?: qualify-names?)
end method profile-object-name;

define method hidden-function?
    (project :: <project-object>, function :: <environment-object>,
     #key show-foreign-functions? :: <boolean> = #f)
 => (hidden? :: <boolean>)
  instance?(function, <internal-method-object>)
    | (~show-foreign-functions? & instance?(function, <foreign-object>))
end method hidden-function?;

/*
define method thread-profile-total-value
    (application :: <application>, thread :: <thread-object>, type :: <symbol>)
 => (total-value :: <integer>)
  let total-value :: <integer> = 0;
  do-thread-profile-snapshots
    (method 
	 (application-snapshot :: <application-snapshot>,
	  thread-snapshot :: <thread-snapshot>)
       let value = thread-snapshot-value(application, snapshot, type);
       increment!(total-value, value)
     end,
     application, profile, thread);
  total-value
end method thread-profile-total-value;
*/
