Module:    devel-dbg-ui
Synopsis:  Profiler handling for the console debugger
Author:    Keith Dennison, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $default-profiling-interval = 50;

define constant <profiler-style>
  = one-of(#"cpu-time", #"allocation", #"class-allocation", 
	   #"wall-clock-time", #"page-faults");


define class <profiler-options> (<object>)
  constant slot profiler-style :: <profiler-style>,
    required-init-keyword: style:;

  constant slot profiler-interval :: <integer>
      = $default-profiling-interval,
    init-keyword: interval:;

  constant slot profiler-depth :: false-or(<integer>) = #f,
    init-keyword: depth:;

  constant slot asking-for-threads? :: <boolean> = #f,
    init-keyword: asking-for-threads?:;

  constant slot profiler-threads :: <stretchy-vector> 
    = make(<stretchy-vector>),
    init-keyword: threads:;
end class;

define class <profiler-results> (<object>)
  constant slot counter-groups-table :: <table> = make(<table>);
end class;

define class <profiler-run> (<object>)
  constant slot profiler-options :: <profiler-options>,
    required-init-keyword: options:;

  slot profiler-results :: false-or(<profiler-results>) = #f;
end class;

define function start-profiling-with-options
    (application :: <application>, options :: <profiler-options>)
 => ()
  application.application-profiler-run
    := make(<profiler-run>, options: options);
  let style = options.profiler-style;
  let class-profiling? = style == #"class-allocation";
  let interval = ~class-profiling? & options.profiler-interval;
  let depth = options.profiler-depth;
  let threads = options.profiler-threads;
  let thread-set
    = if (~empty?(threads))
	as-thread-sequence(application, threads);
      end if;
  if (thread-set)
    start-profiling(application, 
		    interval: interval,
		    threads: thread-set,
		    stack-depth: depth,
		    class-profiling?: class-profiling?)
  else
    start-profiling(application, 
		    interval: interval,
		    stack-depth: depth,
		    class-profiling?: class-profiling?)
  end
end function start-profiling-with-options;

define function maybe-profile-new-thread
    (application :: <application>, thread :: <remote-thread>)
 => ()
  let profiler-run = application.application-profiler-run;
  if (profiler-run)
    let options = profiler-run.profiler-options;
    if (options.asking-for-threads?)
      let valid-answer = #f;
      let answer = #f;
      debugger-message("*** Profiling is currently activated.");
      debugger-message("    Do you wish to add this thread to the profile (Y/N)?");
      let yes?
	= block (return)
	    while (#t)
	      format-out("%s", "        [Y or N] > ");
	      answer := read-line(*standard-input*);
	      format-out("\n");
	      let first-letter
		= instance?(answer, <string>)
		    & (size(answer) > 0)
		    & as-uppercase(answer[0]);
	      select (first-letter)
		'Y'       => return(#t);
		'N'       => return(#f);
		otherwise => #f;
	      end select;
	    end while;
	  end block;
      if (yes?)
	let threads = options.profiler-threads;
	let style = options.profiler-style;
	let class-profiling? = style == #"class-allocation";
	let interval = ~class-profiling? & options.profiler-interval;
	add!(threads, thread);
	control-profiling(application,
			  reset?: #f,
			  interval: interval,
			  stack-depth: options.profiler-depth,
			  threads: threads);
	debugger-message("Added thread.");
      end if;
    end if;
  end if;
end function maybe-profile-new-thread;
