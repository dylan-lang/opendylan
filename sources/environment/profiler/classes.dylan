Module:    environment-profiler
Synopsis:  The profiling tool provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profiler classes

define frame <profiler-classes> (<simple-frame>)
  pane classes-page (frame)
    make(<tab-control-page>,
	 label: "Classes",
	 id:    #"classes",
	 child: frame.classes-displayer);
  pane classes-displayer (frame)
    make(<table-control-displayer>,
	 element-label: "form",
	 information-available?-function: curry(profiler-has-class-results?, frame),
	 transaction-function: curry(perform-profiler-transaction, frame),
	 children-generator: curry(frame-class-profile-info, frame),
	 headings: #["Class", "Count", "Amount", "%"],
	 alignments: #[#"left", #"right", #"right", #"right"],
	 widths: #[200, 60, 60, 40],
	 generators: vector(wrapper-object,
			    wrapper-count,
			    wrapper-amount,
			    wrapper-percentage),
	 sort-orders: #[#"class", #"count", #"amount", #"percentage"],
	 sort-order: #"amount",
	 sort-function: curry(frame-sort-class-profile-info, frame),
	 label-key: curry(frame-default-object-name, frame),
	 items-changed-callback: note-profiler-classes-updated,
	 value-changed-callback: note-profiler-selected-classes-changed,
         activate-callback: curry(profiler-activate-callback, frame));
end frame <profiler-classes>;

define method refresh-profiler-page
    (frame :: <profiler-classes>, page == #"classes",
     #key clean? = #f, new-thread? = #t)
 => ()
  let project = frame.ensure-frame-project;
  let displayer = frame.classes-displayer;
  let thread = frame.frame-current-thread;
  debug-message("Refreshing classes page (clean?: %=)", clean?);
  unless (~thread & ~displayer.displayer-object)
    displayer-object(displayer, clean?: clean?, new-thread?: new-thread?)
      := thread
  end
end method refresh-profiler-page;

define method invalidate-profiler-page
    (frame :: <profiler-classes>, page == #"classes") => ()
  // frame.classes-displayer.displayer-valid? := #f
  invalidate-frame-property-page(frame, frame.classes-displayer, #f, page)
end method invalidate-profiler-page;

define method frame-class-profile-info
    (frame :: <profiler-classes>, thread :: <thread-object>)
 => (wrappers :: <sequence>)
  let project = frame.ensure-frame-project;
  let profile = frame.profiler-application-profile;
  let show-foreign-functions? = frame.profiler-show-foreign-functions?;
  let profile-info
    = process-profile-summary
        (project, profile,
	 type: #"class",
	 show-foreign-functions?: show-foreign-functions?);
  let wrappers :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let total :: <integer> = 0;
  for (thread-info in profile-info.info-threads)
    if (thread-info.info-thread == thread)
      for (class-info :: <profile-class-info> in thread-info.info-objects)
	let amount = class-info.info-allocation;
	total := total + amount;
	add!(wrappers,
	     make(<profile-info-wrapper>,
		  object: class-info.info-class,
		  count:  class-info.info-count,
		  amount: amount))
      end
    end
  end;
  for (wrapper :: <profile-info-wrapper> in wrappers)
    let amount = wrapper.wrapper-amount;
    wrapper.wrapper-percentage := percentage(amount, total)
  end;
  debug-message("Found %d classes", wrappers.size);
  wrappers
end method frame-class-profile-info;

define method frame-sort-class-profile-info
    (frame :: <profiler-classes>, wrappers :: <sequence>,
     order :: <symbol>)
 => (wrappers :: <sequence>)
  select (order)
    #"class" =>
      frame-sort-items(frame, wrappers, key: wrapper-object);
    otherwise =>
      let key
	= select (order)
	    #"count"      => wrapper-count;
	    #"amount"     => wrapper-amount;
	    #"percentage" => wrapper-percentage;
	  end;
      keyed-sort(wrappers, key: key, test: \>);
  end
end method frame-sort-class-profile-info;

define method note-profiler-classes-updated
    (displayer :: <table-control-displayer>)
 => ()
  update-profiler-classes-status(displayer)
end method note-profiler-classes-updated;

define method note-profiler-selected-classes-changed
    (displayer :: <table-control-displayer>, value :: <sequence>)
 => ()
  ignore(value);
  update-profiler-classes-status(displayer)
end method note-profiler-selected-classes-changed;

define method update-profiler-classes-status
    (displayer :: <table-control-displayer>)
 => ()
  let frame = sheet-frame(displayer);
  update-profiler-status
    ("class", displayer,
     available?:       frame.profiler-has-class-results?,
     amount-to-string: allocation-to-string)
end method update-profiler-classes-status;

define constant $kilobyte = 1024;
define constant $megabyte = 1024 * 1024;
// define constant $gigabyte = 1024 * 1024 * 1024;

define method allocation-to-string
    (integer :: <integer>) => (string :: <string>)
  local method one-decimal-point-string
	    (type :: <string>, value :: <single-float>)
	 => (string :: <string>)
	  let (whole, part) = floor/(round(value * 10), 10);
	  format-to-string("%d.%d %s",
			   whole, part,
			   type)
	end method one-decimal-point-string;
  let value = as(<single-float>, integer);
  select (integer by \>)
//    10 * $gigabyte =>
//      one-decimal-point-string("GB", value / $gigabyte);
    10 * $megabyte =>
      one-decimal-point-string("MB", value / $megabyte);
    10 * $kilobyte =>
      one-decimal-point-string("KB", value / $kilobyte);
    otherwise =>
      format-to-string("%d bytes", integer);
  end
end method allocation-to-string;

/*---*** Add filtering next...
define method filter-profile-info
    (frame :: <environment-frame>, libraries :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let project = frame.ensure-frame-project;
  let no-filter? = empty?(substring-filter);
  let library = project-library(project);
  let used-libraries
    = if (library)
	source-form-used-definitions(project, library)
      else
	#[]
      end;
  local method object-matches-type-filter? 
	    (library :: <library-object>) => (matches? :: <boolean>)
	  select (type-filter)
	    #"libraries"      => #t;
	    #"used-libraries" => 
	      member?(library, used-libraries);
	  end
	end method object-matches-type-filter?;
  local method object-matches-substring-filter?
	    (library :: <library-object>) => (matches? :: <boolean>)
	  no-filter?
	    | begin
		let name = frame-default-object-name(frame, library);
		subsequence-position(name, substring-filter) ~= #f
	      end
	end method object-matches-substring-filter?;
  local method show-object?
	    (library :: <library-object>) => (show? :: <boolean>)
	  object-matches-type-filter?(library)
	    & object-matches-substring-filter?(library)
	end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in libraries)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-profile-info;
*/
